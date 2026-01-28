// State management
let allBets = [];
let currentFilter = 'all';
let lastBalanceTimestamp = 0;

// WebSocket message handler
registerWSMessageHandler((data) => {
    if (data.opcode === 'betting_closed') {
        // Update status badge for bets on this game (surgical update)
        handleBettingClosedDOM(data.game_id);
    } else if (data.opcode === 'game_result') {
        // Update bet cards for this game with results (surgical update)
        handleGameResultDOM(data.game_id, data.result);
    } else if (data.opcode === 'bet_confirmed') {
        // Reload to show the new bet
        loadMyBets();
    } else if (data.opcode === 'balance_update') {
        // Only update if this message is newer than the last one
        if (data.timestamp && data.timestamp > lastBalanceTimestamp) {
            lastBalanceTimestamp = data.timestamp;
            const balanceElement = document.querySelector('.balance-amount');
            if (balanceElement && data.balance != null) {
                balanceElement.textContent = `$${data.balance.toFixed(2)}`;
            }
        }
    }
});

// Check authentication
document.addEventListener('DOMContentLoaded', () => {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) {
        window.location.href = 'login.html';
        return;
    }
    
    // Check if JWT is expired
    if (!checkAuthExpiration()) {
        return; // checkAuthExpiration handles redirect
    }
    
    const user = JSON.parse(currentUser);
    
    // Guests cannot access My Bets page
    if (user.isGuest) {
        window.location.href = 'dashboard.html';
        return;
    }
    
    // Show admin link if user is admin
    if (user.isAdmin) {
        document.getElementById('admin-link').style.display = 'inline';
    }
    
    // Connect WebSocket
    connectWebSocket();
    
    loadMyBets();
    loadBalance();
});

// Load user's bets
async function loadMyBets() {
    const container = document.getElementById('bets-container');
    container.innerHTML = '<div class="loading">Loading your bets...</div>';
    
    try {
        const response = await fetchUserBets();
        allBets = response.bets || [];
        
        if (allBets.length === 0) {
            container.innerHTML = '<div class="no-bets">You haven\'t placed any bets yet.</div>';
            return;
        }
        
        // Sort by placed_at descending
        allBets.sort((a, b) => b.placed_at - a.placed_at);
        
        renderBets();
    } catch (error) {
        console.error('Error loading bets:', error);
        container.innerHTML = '<div class="error">Failed to load your bets. Please try again.</div>';
    }
}

// Render bets based on current filter
function renderBets() {
    const container = document.getElementById('bets-container');
    
    let filteredBets = allBets;
    if (currentFilter === 'pending') {
        filteredBets = allBets.filter(bet => bet.won === null);
    } else if (currentFilter === 'won') {
        filteredBets = allBets.filter(bet => bet.won === true);
    } else if (currentFilter === 'lost') {
        filteredBets = allBets.filter(bet => bet.won === false);
    }
    
    if (filteredBets.length === 0) {
        container.innerHTML = `<div class="no-bets">No ${currentFilter === 'all' ? '' : currentFilter} bets found.</div>`;
        return;
    }
    
    container.innerHTML = '';
    
    filteredBets.forEach(bet => {
        const betCard = createBetCard(bet);
        container.appendChild(betCard);
    });
}

// Surgical DOM update: Handle betting closed
function handleBettingClosedDOM(gameId) {
    // Update all bet cards for this game
    const betCards = document.querySelectorAll(`.bet-card[data-game-id="${gameId}"]`);
    
    betCards.forEach(card => {
        // Only update if bet is still pending
        if (card.classList.contains('pending')) {
            const statusBadge = card.querySelector('.status-badge.pending');
            if (statusBadge) {
                statusBadge.textContent = 'Waiting for Result';
            }
            
            // Update bet data in allBets array
            const betId = parseInt(card.dataset.betId);
            const bet = allBets.find(b => b.id === betId);
            if (bet) {
                bet.betting_open = false;
            }
        }
    });
}

// Surgical DOM update: Handle game result
async function handleGameResultDOM(gameId, result) {
    // Find all bets on this game
    const betCards = document.querySelectorAll(`.bet-card[data-game-id="${gameId}"]`);
    
    if (betCards.length === 0) return;
    
    // Fetch updated bet data to get payout information
    try {
        const response = await fetchUserBets();
        const updatedBets = response.bets || [];
        
        betCards.forEach(card => {
            const betId = parseInt(card.dataset.betId);
            const bet = allBets.find(b => b.id === betId);
            const updatedBet = updatedBets.find(b => b.id === betId);
            
            if (!bet || !updatedBet) return;
            
            // Update bet data in allBets array
            Object.assign(bet, updatedBet);
            
            // Determine if won or lost
            const won = updatedBet.won;
            
            // Remove old status class
            card.classList.remove('pending', 'won', 'lost');
            
            // Add new status class
            if (won === true) {
                card.classList.add('won');
            } else if (won === false) {
                card.classList.add('lost');
            } else {
                card.classList.add('pending');
            }
            
            // Update status div
            const statusDiv = card.querySelector('.bet-status');
            if (statusDiv) {
                statusDiv.innerHTML = '';
                
                if (won === true) {
                    const statusBadge = document.createElement('span');
                    statusBadge.className = 'status-badge won';
                    statusBadge.textContent = `Won: $${updatedBet.payout.toFixed(2)}`;
                    statusDiv.appendChild(statusBadge);
                    
                    const resultText = updatedBet.game_result === 'opt1' ? updatedBet.opt1_text : updatedBet.opt2_text;
                    const resultDiv = document.createElement('div');
                    resultDiv.className = 'result-text';
                    resultDiv.textContent = `Result: ${resultText}`;
                    statusDiv.appendChild(resultDiv);
                } else if (won === false) {
                    const statusBadge = document.createElement('span');
                    statusBadge.className = 'status-badge lost';
                    statusBadge.textContent = 'Lost';
                    statusDiv.appendChild(statusBadge);
                    
                    const resultText = updatedBet.game_result === 'opt1' ? updatedBet.opt1_text : updatedBet.opt2_text;
                    const resultDiv = document.createElement('div');
                    resultDiv.className = 'result-text';
                    resultDiv.textContent = `Result: ${resultText}`;
                    statusDiv.appendChild(resultDiv);
                }
            }
            
            // If current filter would exclude this bet, hide it with animation
            if (currentFilter === 'pending' && won !== null) {
                card.style.opacity = '0';
                card.style.transform = 'translateX(-20px)';
                setTimeout(() => {
                    card.style.display = 'none';
                }, 300);
            }
        });
    } catch (error) {
        console.error('Error fetching updated bet data:', error);
    }
}


// Create bet card element
function createBetCard(bet) {
    const card = document.createElement('div');
    card.className = 'bet-card';
    card.dataset.gameId = bet.game_id; // Add game ID for targeting
    card.dataset.betId = bet.id; // Add bet ID for targeting
    
    // Add status class
    if (bet.won === true) {
        card.classList.add('won');
    } else if (bet.won === false) {
        card.classList.add('lost');
    } else {
        card.classList.add('pending');
    }
    
    // Header with game info
    const header = document.createElement('div');
    header.className = 'bet-card-header';
    
    const categoryBadge = document.createElement('span');
    categoryBadge.className = `bet-category ${bet.game_category}`;
    categoryBadge.textContent = bet.game_category;
    
    const gameTitle = document.createElement('h3');
    gameTitle.textContent = bet.game_question;
    gameTitle.onclick = () => window.location.href = `bet-detail.html?id=${bet.game_id}`;
    gameTitle.style.cursor = 'pointer';
    
    header.appendChild(categoryBadge);
    header.appendChild(gameTitle);
    
    // Bet details
    const details = document.createElement('div');
    details.className = 'bet-details';
    
    const choiceText = bet.choice === 'opt1' ? bet.opt1_text : bet.opt2_text;
    
    details.innerHTML = `
        <div class="bet-detail-row">
            <span class="label">Your Choice:</span>
            <span class="value choice">${choiceText}</span>
        </div>
        <div class="bet-detail-row">
            <span class="label">Amount:</span>
            <span class="value">$${bet.amount.toFixed(2)}</span>
        </div>
        <div class="bet-detail-row">
            <span class="label">Odds:</span>
            <span class="value">${bet.odd.toFixed(2)}x</span>
        </div>
        <div class="bet-detail-row">
            <span class="label">Potential Return:</span>
            <span class="value">$${(bet.amount * bet.odd).toFixed(2)}</span>
        </div>
    `;
    
    // Status and result
    const statusDiv = document.createElement('div');
    statusDiv.className = 'bet-status';
    
    if (bet.won === null) {
        // Pending
        const statusBadge = document.createElement('span');
        statusBadge.className = 'status-badge pending';
        statusBadge.textContent = bet.betting_open ? 'Betting Open' : 'Waiting for Result';
        statusDiv.appendChild(statusBadge);
    } else if (bet.won === true) {
        // Won
        const statusBadge = document.createElement('span');
        statusBadge.className = 'status-badge won';
        statusBadge.textContent = `Won: $${bet.payout.toFixed(2)}`;
        statusDiv.appendChild(statusBadge);
        
        const resultText = bet.game_result === 'opt1' ? bet.opt1_text : bet.opt2_text;
        const resultDiv = document.createElement('div');
        resultDiv.className = 'result-text';
        resultDiv.textContent = `Result: ${resultText}`;
        statusDiv.appendChild(resultDiv);
    } else {
        // Lost
        const statusBadge = document.createElement('span');
        statusBadge.className = 'status-badge lost';
        statusBadge.textContent = 'Lost';
        statusDiv.appendChild(statusBadge);
        
        const resultText = bet.game_result === 'opt1' ? bet.opt1_text : bet.opt2_text;
        const resultDiv = document.createElement('div');
        resultDiv.className = 'result-text';
        resultDiv.textContent = `Result: ${resultText}`;
        statusDiv.appendChild(resultDiv);
    }
    
    // Timestamp
    const timestamp = document.createElement('div');
    timestamp.className = 'bet-timestamp';
    timestamp.textContent = `Placed: ${formatEuropeanDateTime(bet.placed_at)}`;
    
    card.appendChild(header);
    card.appendChild(details);
    card.appendChild(statusDiv);
    card.appendChild(timestamp);
    
    return card;
}

// Filter bets
function filterBets(filter) {
    currentFilter = filter;
    
    // Update button states
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    event.target.classList.add('active');
    
    renderBets();
}

// Load balance
async function loadBalance() {
    try {
        const balance = await fetchBalance();
        document.querySelector('.balance-amount').textContent = `$${balance.toFixed(2)}`;
    } catch (error) {
        console.error('Error loading balance:', error);
    }
}
