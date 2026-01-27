// State management
let currentGame = null;
let selectedOutcome = null;
let wheelCanvas = null;
let wheelCtx = null;
let wheelAngle = 0;
let wheelSpinning = false;
let wheelAnimationId = null;
let userBalance = 0;

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Handle real-time odds updates for current game
    if (data.opcode === 'odds_update' && currentGame && data.game_id === currentGame.game_id) {
        // Update odds display
        currentGame.odd1 = data.odd1;
        currentGame.odd2 = data.odd2;
        currentGame.cap_opt1 = data.cap_opt1;
        currentGame.cap_opt2 = data.cap_opt2;
        
        // Refresh the display
        displayGameDetails();
    } else if (data.opcode === 'balance_update') {
        // Update user balance
        userBalance = data.balance;
        updateBalanceDisplay();
    } else if (data.opcode === 'bet_confirmed' && currentGame && data.game_id === currentGame.game_id) {
        // Show bet confirmation
        const choiceText = data.choice === 'opt1' ? currentGame.opt1_text : currentGame.opt2_text;
        alert(`Bet placed successfully!\n\nAmount: $${data.amount.toFixed(2)}\nOutcome: ${choiceText}\nOdds: ${data.odd.toFixed(2)}x\nNew Balance: $${data.balance.toFixed(2)}`);
    } else if (data.opcode === 'betting_closed' && currentGame && data.game_id === currentGame.game_id) {
        currentGame.betting_open = false;
        displayGameDetails();
    } else if (data.opcode === 'game_result' && currentGame && data.game_id === currentGame.game_id) {
        currentGame.result = data.result;
        currentGame.betting_open = false;
        displayGameDetails();
    }
});

// Check authentication and load bet
document.addEventListener('DOMContentLoaded', async () => {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) {
        window.location.href = 'login.html';
        return;
    }
    
    // Show admin link if user is admin
    const user = JSON.parse(currentUser);
    if (user.isAdmin) {
        document.getElementById('admin-link').style.display = 'inline';
    }
    
    // Connect WebSocket
    connectWebSocket();
    
    // Get game ID from URL
    const urlParams = new URLSearchParams(window.location.search);
    const gameId = urlParams.get('id');
    
    if (!gameId) {
        window.location.href = 'dashboard.html';
        return;
    }
    
    // Load user balance
    await loadBalance();
    
    // Load game details
    await loadGameDetail(gameId);
    
    // Add event listener for amount input
    const amountInput = document.getElementById('bet-amount');
    if (amountInput) {
        amountInput.addEventListener('input', updatePotentialReturn);
    }
});

// Load user balance
async function loadBalance() {
    try {
        userBalance = await fetchBalance();
        updateBalanceDisplay();
    } catch (error) {
        console.error('Error loading balance:', error);
    }
}

// Update balance display
function updateBalanceDisplay() {
    const balanceEl = document.querySelector('.balance-display');
    if (balanceEl) {
        balanceEl.textContent = `Balance: $${userBalance.toFixed(2)}`;
    }
}

// Load game detail
async function loadGameDetail(gameId) {
    try {
        currentGame = await fetchGameDetail(gameId);
        displayGameDetails();
        
        // Initialize spinning wheel for virtual events if needed
        // For now, we don't have a category field, so we skip this
        document.getElementById('spinning-wheel-section').style.display = 'none';
    } catch (error) {
        console.error('Error loading game details:', error);
        alert('Failed to load game details');
        window.location.href = 'dashboard.html';
    }
}

// Display game details
function displayGameDetails() {
    if (!currentGame) return;
    
    // Populate game details
    document.getElementById('detail-category').textContent = 'real'; // Default category
    document.getElementById('detail-category').className = 'bet-category';
    document.getElementById('detail-title').textContent = currentGame.question_text;
    
    const totalVolume = (currentGame.tot_opt1 || 0) + (currentGame.tot_opt2 || 0);
    document.getElementById('detail-volume').textContent = `$${totalVolume.toFixed(2)}`;
    document.getElementById('detail-participants').textContent = '-'; // Backend doesn't track this
    
    // Load outcome buttons
    const outcomesContainer = document.getElementById('outcome-options');
    outcomesContainer.innerHTML = '';
    
    // Option 1
    const btn1 = document.createElement('div');
    btn1.className = 'outcome-btn';
    btn1.onclick = () => selectOutcome('opt1');
    
    const label1 = document.createElement('span');
    label1.className = 'outcome-btn-label';
    label1.textContent = currentGame.opt1_text;
    
    const odds1 = document.createElement('span');
    odds1.className = 'outcome-btn-odds';
    odds1.textContent = currentGame.odd1 != null ? `${currentGame.odd1.toFixed(2)}x` : 'N/A';
    
    btn1.appendChild(label1);
    btn1.appendChild(odds1);
    outcomesContainer.appendChild(btn1);
    
    // Option 2
    const btn2 = document.createElement('div');
    btn2.className = 'outcome-btn';
    btn2.onclick = () => selectOutcome('opt2');
    
    const label2 = document.createElement('span');
    label2.className = 'outcome-btn-label';
    label2.textContent = currentGame.opt2_text;
    
    const odds2 = document.createElement('span');
    odds2.className = 'outcome-btn-odds';
    odds2.textContent = currentGame.odd2 != null ? `${currentGame.odd2.toFixed(2)}x` : 'N/A';
    
    btn2.appendChild(label2);
    btn2.appendChild(odds2);
    outcomesContainer.appendChild(btn2);
    
    // Disable betting if closed or has result
    if (!currentGame.betting_open || currentGame.result) {
        const placeBetBtn = document.querySelector('button[onclick="placeBet()"]');
        if (placeBetBtn) {
            placeBetBtn.disabled = true;
            placeBetBtn.textContent = currentGame.result ? 'Game Finished' : 'Betting Closed';
        }
    }
    
    // Show result if available
    if (currentGame.result) {
        const resultText = currentGame.result === 'opt1' ? currentGame.opt1_text : currentGame.opt2_text;
        const resultDiv = document.createElement('div');
        resultDiv.style.cssText = 'background: #4CAF50; color: white; padding: 15px; border-radius: 8px; margin-top: 20px; text-align: center; font-weight: bold;';
        resultDiv.textContent = `Result: ${resultText}`;
        document.querySelector('.bet-form-section').prepend(resultDiv);
    }
}

// Select outcome
function selectOutcome(choice) {
    if (!currentGame.betting_open || currentGame.result) {
        alert('Betting is closed for this game');
        return;
    }
    
    selectedOutcome = choice;
    
    // Update button states
    document.querySelectorAll('.outcome-btn').forEach((btn, i) => {
        if ((i === 0 && choice === 'opt1') || (i === 1 && choice === 'opt2')) {
            btn.classList.add('selected');
        } else {
            btn.classList.remove('selected');
        }
    });
    
    // Update potential return
    updatePotentialReturn();
}

// Update potential return calculation
function updatePotentialReturn() {
    const amountInput = document.getElementById('bet-amount');
    const amount = parseFloat(amountInput.value) || 0;
    
    if (selectedOutcome && amount > 0 && currentGame) {
        const odds = selectedOutcome === 'opt1' ? currentGame.odd1 : currentGame.odd2;
        const cap = selectedOutcome === 'opt1' ? currentGame.cap_opt1 : currentGame.cap_opt2;
        
        if (odds != null) {
            const potentialReturn = amount * odds;
            document.getElementById('potential-return').textContent = 
                '$' + potentialReturn.toFixed(2);
            
            // Show cap warning if needed
            if (cap != null && amount > cap) {
                const warningEl = document.getElementById('cap-warning');
                if (!warningEl) {
                    const warning = document.createElement('div');
                    warning.id = 'cap-warning';
                    warning.style.cssText = 'color: #ff9800; margin-top: 10px; font-size: 14px;';
                    warning.textContent = `Warning: Maximum bet for this option is $${cap.toFixed(2)}`;
                    amountInput.parentElement.appendChild(warning);
                }
            } else {
                const warningEl = document.getElementById('cap-warning');
                if (warningEl) warningEl.remove();
            }
        }
    } else {
        document.getElementById('potential-return').textContent = '$0.00';
        const warningEl = document.getElementById('cap-warning');
        if (warningEl) warningEl.remove();
    }
}

// Place bet
async function placeBet() {
    const amount = parseFloat(document.getElementById('bet-amount').value);
    
    if (!amount || amount <= 0) {
        alert('Please enter a valid amount');
        return;
    }
    
    if (!selectedOutcome) {
        alert('Please select an outcome');
        return;
    }
    
    if (!currentGame.betting_open || currentGame.result) {
        alert('Betting is closed for this game');
        return;
    }
    
    const cap = selectedOutcome === 'opt1' ? currentGame.cap_opt1 : currentGame.cap_opt2;
    if (cap != null && amount > cap) {
        alert(`Maximum bet for this option is $${cap.toFixed(2)}`);
        return;
    }
    
    if (amount > userBalance) {
        alert('Insufficient balance');
        return;
    }
    
    try {
        const result = await submitBetAPI(currentGame.game_id, amount, selectedOutcome);
        
        // Update balance
        userBalance = result.new_balance;
        updateBalanceDisplay();
        
        // Show success message
        const choiceText = selectedOutcome === 'opt1' ? currentGame.opt1_text : currentGame.opt2_text;
        alert(`Bet placed successfully!\n\nAmount: $${amount.toFixed(2)}\nOutcome: ${choiceText}\nOdds: ${result.bet_odd.toFixed(2)}x\nPotential Return: $${(amount * result.bet_odd).toFixed(2)}\nNew Balance: $${result.new_balance.toFixed(2)}`);
        
        // Update odds
        if (result.new_odd1 != null && result.new_odd2 != null) {
            currentGame.odd1 = result.new_odd1;
            currentGame.odd2 = result.new_odd2;
            currentGame.cap_opt1 = result.new_cap_opt1;
            currentGame.cap_opt2 = result.new_cap_opt2;
            displayGameDetails();
        }
        
        // Reset form
        document.getElementById('bet-amount').value = '';
        selectedOutcome = null;
        document.querySelectorAll('.outcome-btn').forEach(btn => {
            btn.classList.remove('selected');
        });
        updatePotentialReturn();
        
    } catch (error) {
        alert(`Failed to place bet: ${error.message}`);
    }
}
        
        const time = document.createElement('span');
        time.className = 'activity-time';
        time.textContent = activity.time;

// Initialize spinning wheel (removed - not needed for now)
// Load activity (removed - backend doesn't provide this data)

