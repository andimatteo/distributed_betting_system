// State management
let currentGame = null;
let selectedOutcome = null;
let wheelCanvas = null;
let wheelCtx = null;
let wheelAngle = 0;
let wheelSpinning = false;
let wheelAnimationId = null;
let userBalance = 0;
let pendingBalanceUpdate = null; // Store pending balance update during wheel spin

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
        // Check if this balance update is related to the current virtual game being viewed
        const category = currentGame ? (currentGame.category || 'real') : 'real';
        const isCurrentVirtualGame = data.game_id && currentGame && 
                                      data.game_id === currentGame.game_id && 
                                      category === 'virtual';
        
        if (isCurrentVirtualGame && wheelSpinning) {
            // Delay balance update until wheel stops spinning
            pendingBalanceUpdate = data.balance;
        } else {
            // Update balance immediately
            userBalance = data.balance;
            updateBalanceDisplay();
        }
    } else if (data.opcode === 'bet_confirmed' && currentGame && data.game_id === currentGame.game_id) {
        // Show bet confirmation (balance will be updated via balance_update message)
        const choiceText = data.choice === 'opt1' ? currentGame.opt1_text : currentGame.opt2_text;
        alert(`Bet placed successfully!\n\nAmount: $${data.amount.toFixed(2)}\nOutcome: ${choiceText}\nOdds: ${data.odd.toFixed(2)}x\nPotential Return: $${(data.amount * data.odd).toFixed(2)}`);
    } else if (data.opcode === 'betting_closed' && currentGame && data.game_id === currentGame.game_id) {
        currentGame.betting_open = false;
        displayGameDetails();
    } else if (data.opcode === 'game_result' && currentGame && data.game_id === currentGame.game_id) {
        // Store the result data
        const resultData = {
            result: data.result,
            winners_count: data.winners_count,
            total_paid: data.total_paid
        };
        
        // If virtual game, spin the wheel first
        const category = currentGame.category || 'real';
        if (category === 'virtual') {
            // Map result to color
            const resultColor = data.result === 'opt1' ? 'red' : 'black';
            
            // Start spinning wheel
            spinWheelWithCallback(resultColor, () => {
                // This callback runs after wheel stops spinning
                currentGame.result = resultData.result;
                currentGame.betting_open = false;
                displayGameDetails();
                
                // Reload user's bets to show outcome
                loadMyBetsForGame(currentGame.game_id);
                
                // Apply pending balance update if there is one
                if (pendingBalanceUpdate !== null) {
                    userBalance = pendingBalanceUpdate;
                    updateBalanceDisplay();
                    pendingBalanceUpdate = null;
                } else {
                    // Reload balance if no pending update
                    loadBalance();
                }
            });
        } else {
            // For real games, update immediately
            currentGame.result = resultData.result;
            currentGame.betting_open = false;
            displayGameDetails();
            
            // Reload user's bets to show outcome
            loadMyBetsForGame(currentGame.game_id);
        }
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
    const balanceEl = document.querySelector('.balance-amount');
    if (balanceEl) {
        balanceEl.textContent = `$${userBalance.toFixed(2)}`;
    }
}

// Load game detail
async function loadGameDetail(gameId) {
    try {
        currentGame = await fetchGameDetail(gameId);
        displayGameDetails();
        
        // Load user's bets for this game
        loadMyBetsForGame(gameId);
        
        // Initialize spinning wheel for virtual events if needed
        const category = currentGame.category || 'real';
        if (category === 'virtual') {
            document.getElementById('spinning-wheel-section').style.display = 'block';
            initWheel();
        } else {
            document.getElementById('spinning-wheel-section').style.display = 'none';
        }
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
    const category = currentGame.category || 'real';
    document.getElementById('detail-category').textContent = category;
    document.getElementById('detail-category').className = `bet-category ${category}`;
    document.getElementById('detail-title').textContent = currentGame.question_text;
    
    const totalVolume = (currentGame.tot_opt1 || 0) + (currentGame.tot_opt2 || 0);
    document.getElementById('detail-volume').textContent = `$${totalVolume.toFixed(2)}`;
    
    // Load outcome buttons
    const outcomesContainer = document.getElementById('outcome-options');
    outcomesContainer.innerHTML = '';
    
    // Option 1
    const btn1 = document.createElement('div');
    btn1.className = 'outcome-btn';
    if (!currentGame.result) {
        btn1.onclick = () => selectOutcome('opt1');
    } else {
        btn1.classList.add('disabled');
        btn1.style.cursor = 'not-allowed';
        btn1.style.opacity = '0.6';
    }
    
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
    if (!currentGame.result) {
        btn2.onclick = () => selectOutcome('opt2');
    } else {
        btn2.classList.add('disabled');
        btn2.style.cursor = 'not-allowed';
        btn2.style.opacity = '0.6';
    }
    
    const label2 = document.createElement('span');
    label2.className = 'outcome-btn-label';
    label2.textContent = currentGame.opt2_text;
    
    const odds2 = document.createElement('span');
    odds2.className = 'outcome-btn-odds';
    odds2.textContent = currentGame.odd2 != null ? `${currentGame.odd2.toFixed(2)}x` : 'N/A';
    
    btn2.appendChild(label2);
    btn2.appendChild(odds2);
    outcomesContainer.appendChild(btn2);
    
    // Show/hide bet form based on betting status
    const betFormGroup = document.querySelector('.bet-form .form-group');
    const potentialReturnDiv = document.querySelector('.bet-form .potential-return');
    const isClosed = !currentGame.betting_open || currentGame.result;
    
    if (betFormGroup) {
        betFormGroup.style.display = isClosed ? 'none' : 'block';
    }
    if (potentialReturnDiv) {
        potentialReturnDiv.style.display = isClosed ? 'none' : 'flex';
    }
    
    // Disable betting if closed or has result
    if (isClosed) {
        const placeBetBtn = document.querySelector('button[onclick="placeBet()"]');
        if (placeBetBtn) {
            placeBetBtn.disabled = true;
            placeBetBtn.textContent = currentGame.result ? 'Game Finished' : 'Betting Closed';
            placeBetBtn.style.cursor = 'not-allowed';
        }
    } else {
        const placeBetBtn = document.querySelector('button[onclick="placeBet()"]');
        if (placeBetBtn) {
            placeBetBtn.disabled = false;
            placeBetBtn.textContent = 'Place Bet';
        }
    }
    
    // Show result if available
    if (currentGame.result) {
        const resultText = currentGame.result === 'opt1' ? currentGame.opt1_text : currentGame.opt2_text;
        
        // Check if result div already exists to avoid duplicates
        let resultDiv = document.getElementById('game-result-display');
        if (!resultDiv) {
            resultDiv = document.createElement('div');
            resultDiv.id = 'game-result-display';
            resultDiv.style.cssText = 'background: #4CAF50; color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px; text-align: center; font-weight: bold;';
            
            const bettingSection = document.querySelector('.betting-section');
            if (bettingSection) {
                // Insert at the beginning of betting section
                bettingSection.insertBefore(resultDiv, bettingSection.firstChild);
            }
        }
        
        if (resultDiv) {
            resultDiv.textContent = `Result: ${resultText}`;
        }
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

// Initialize spinning wheel
function initWheel() {
    wheelCanvas = document.getElementById('wheel-canvas');
    if (!wheelCanvas) return;
    
    wheelCtx = wheelCanvas.getContext('2d');
    drawWheel();
}

// Draw the spinning wheel
function drawWheel() {
    const centerX = wheelCanvas.width / 2;
    const centerY = wheelCanvas.height / 2;
    const radius = 180;
    
    // Clear canvas
    wheelCtx.clearRect(0, 0, wheelCanvas.width, wheelCanvas.height);
    
    // Save context
    wheelCtx.save();
    
    // Rotate canvas
    wheelCtx.translate(centerX, centerY);
    wheelCtx.rotate(wheelAngle * Math.PI / 180);
    wheelCtx.translate(-centerX, -centerY);
    
    // Draw segments (alternating red and black)
    const segments = 20;
    const anglePerSegment = (2 * Math.PI) / segments;
    
    for (let i = 0; i < segments; i++) {
        wheelCtx.beginPath();
        wheelCtx.moveTo(centerX, centerY);
        wheelCtx.arc(
            centerX,
            centerY,
            radius,
            i * anglePerSegment,
            (i + 1) * anglePerSegment
        );
        wheelCtx.closePath();
        
        // Alternate between red and black
        wheelCtx.fillStyle = i % 2 === 0 ? '#ef4444' : '#1e293b';
        wheelCtx.fill();
        
        // Add border
        wheelCtx.strokeStyle = '#f1f5f9';
        wheelCtx.lineWidth = 2;
        wheelCtx.stroke();
        
        // Add number to segment
        wheelCtx.save();
        const angle = i * anglePerSegment + anglePerSegment / 2;
        const textRadius = radius * 0.75;
        const textX = centerX + Math.cos(angle) * textRadius;
        const textY = centerY + Math.sin(angle) * textRadius;
        
        wheelCtx.translate(textX, textY);
        wheelCtx.rotate(angle + Math.PI / 2);
        
        wheelCtx.fillStyle = '#ffffff';
        wheelCtx.font = 'bold 18px Arial';
        wheelCtx.textAlign = 'center';
        wheelCtx.textBaseline = 'middle';
        wheelCtx.fillText(i.toString(), 0, 0);
        
        wheelCtx.restore();
    }
    
    // Draw center circle
    wheelCtx.beginPath();
    wheelCtx.arc(centerX, centerY, 20, 0, 2 * Math.PI);
    wheelCtx.fillStyle = '#4f46e5';
    wheelCtx.fill();
    wheelCtx.strokeStyle = '#f1f5f9';
    wheelCtx.lineWidth = 3;
    wheelCtx.stroke();
    
    // Restore context
    wheelCtx.restore();
}

// Spin the wheel and stop at specified color
function spinWheel(stopColor) {
    spinWheelWithCallback(stopColor, null);
}

// Spin the wheel with a callback when finished
function spinWheelWithCallback(stopColor, callback) {
    if (wheelSpinning) return;
    
    console.log('=== SPIN WHEEL DEBUG ===');
    console.log('Target color:', stopColor);
    
    wheelSpinning = true;
    
    // Calculate target angle based on color
    // Red segments are at even positions (0, 2, 4, ..., 18)
    // Black segments are at odd positions (1, 3, 5, ..., 19)
    const segments = 20;
    const anglePerSegment = 360 / segments; // 18 degrees per segment
    
    console.log('Angle per segment:', anglePerSegment);
    
    // Choose a random segment of the target color
    let targetSegment;
    if (stopColor === 'red') {
        const redSegments = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18];
        targetSegment = redSegments[Math.floor(Math.random() * redSegments.length)];
    } else {
        const blackSegments = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
        targetSegment = blackSegments[Math.floor(Math.random() * blackSegments.length)];
    }
    
    console.log('Target segment index:', targetSegment);
    console.log('Target segment should be:', targetSegment % 2 === 0 ? 'RED' : 'BLACK');
    
    // The wheel is rotated, and pointer is at the TOP
    // In our drawing, segments start from 0 degrees (right side) when wheelAngle = 0
    // Pointer is at 270 degrees in canvas coords (top of circle)
    
    // Segment center in the original (un-rotated) wheel
    const segmentStartAngle = targetSegment * anglePerSegment;
    const segmentCenterInWheel = segmentStartAngle + anglePerSegment / 2;
    
    console.log('Segment center angle (in wheel):', segmentCenterInWheel);
    
    // To align this segment center with the pointer at the top (270 degrees),
    // we need to rotate the wheel by: (270 - segmentCenterInWheel) degrees
    // But since our rotation is in the opposite direction, it's: segmentCenterInWheel - 270
    let targetAngle = 270 - segmentCenterInWheel;
    
    // Normalize to 0-360 range
    while (targetAngle < 0) targetAngle += 360;
    while (targetAngle >= 360) targetAngle -= 360;
    
    console.log('Target rotation angle:', targetAngle);
    
    // Add multiple rotations for effect (5-7 full rotations)
    const numRotations = Math.floor(5 + Math.random() * 3); // 5, 6, or 7 complete rotations
    const extraRotations = numRotations * 360;
    const totalRotation = extraRotations + targetAngle;
    
    console.log('Number of rotations:', numRotations);
    console.log('Extra rotations:', extraRotations);
    console.log('Total rotation:', totalRotation);
    
    // Animation parameters
    const duration = 5000;
    const startTime = Date.now();
    const startAngle = wheelAngle % 360;
    
    console.log('Starting from angle:', startAngle);
    
    // Calculate how much we need to rotate from current position
    let angleDifference = targetAngle - startAngle;
    while (angleDifference < 0) angleDifference += 360;
    
    const finalAngle = startAngle + extraRotations + angleDifference;
    
    console.log('Final target angle:', finalAngle);
    
    function animate() {
        const currentTime = Date.now();
        const elapsed = currentTime - startTime;
        const progress = Math.min(elapsed / duration, 1);
        
        // Easing function (ease-out)
        const easeOut = 1 - Math.pow(1 - progress, 3);
        
        wheelAngle = startAngle + ((finalAngle - startAngle) * easeOut);
        drawWheel();
        
        if (progress < 1) {
            wheelAnimationId = requestAnimationFrame(animate);
        } else {
            wheelSpinning = false;
            const finalNormalizedAngle = wheelAngle % 360;
            wheelAngle = finalNormalizedAngle;
            
            console.log('Final wheel angle:', finalNormalizedAngle);
            
            // Verify which segment we landed on
            const pointerAngle = 270; // Top position
            const segmentAtPointer = Math.floor(((pointerAngle - finalNormalizedAngle + 360) % 360) / anglePerSegment) % segments;
            const colorAtPointer = segmentAtPointer % 2 === 0 ? 'RED' : 'BLACK';
            
            console.log('Segment at pointer:', segmentAtPointer);
            console.log('Color at pointer:', colorAtPointer);
            console.log('Expected color:', stopColor.toUpperCase());
            console.log('=== END DEBUG ===');
            
            // Execute callback if provided
            if (callback && typeof callback === 'function') {
                setTimeout(() => {
                    callback();
                }, 500); // Small delay to let user see the result
            } else {
                // Show result alert (original behavior for test spins)
                setTimeout(() => {
                    alert(`Wheel stopped on ${colorAtPointer}!\nExpected: ${stopColor.toUpperCase()}`);
                }, 100);
            }
        }
    }
    
    animate();
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
        
        // Reload user's bets for this game
        loadMyBetsForGame(currentGame.game_id);
        
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

// Load and display user's bets for the current game
async function loadMyBetsForGame(gameId) {
    const activityList = document.getElementById('activity-list');
    
    try {
        const response = await fetchUserBets(gameId);
        const bets = response.bets || [];
        
        if (bets.length === 0) {
            activityList.innerHTML = '<div class="no-activity">You haven\'t placed any bets on this game yet.</div>';
            return;
        }
        
        // Sort by placed_at descending
        bets.sort((a, b) => b.placed_at - a.placed_at);
        
        activityList.innerHTML = '';
        
        bets.forEach(bet => {
            const betItem = document.createElement('div');
            betItem.className = 'activity-item';
            
            // Add status class
            if (bet.won === true) {
                betItem.classList.add('won');
            } else if (bet.won === false) {
                betItem.classList.add('lost');
            } else {
                betItem.classList.add('pending');
            }
            
            const choiceText = bet.choice === 'opt1' ? bet.opt1_text : bet.opt2_text;
            
            let statusHTML = '';
            if (bet.won === null) {
                statusHTML = '<span class="status-badge pending">Pending</span>';
            } else if (bet.won === true) {
                statusHTML = `<span class="status-badge won">Won: $${bet.payout.toFixed(2)}</span>`;
            } else {
                statusHTML = '<span class="status-badge lost">Lost</span>';
            }
            
            betItem.innerHTML = `
                <div class="bet-info-row">
                    <div>
                        <div class="bet-choice">${choiceText}</div>
                        <div class="bet-details-small">$${bet.amount.toFixed(2)} at ${bet.odd.toFixed(2)}x odds</div>
                        <div class="bet-timestamp">${formatEuropeanDateTime(bet.placed_at)}</div>
                    </div>
                    <div class="bet-status-col">
                        ${statusHTML}
                        <div class="potential-payout">Potential: $${(bet.amount * bet.odd).toFixed(2)}</div>
                    </div>
                </div>
            `;
            
            activityList.appendChild(betItem);
        });
    } catch (error) {
        console.error('Error loading user bets:', error);
        activityList.innerHTML = '<div class="error-message">Failed to load your bets.</div>';
    }
}
