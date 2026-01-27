// State management
let allBets = [];
let currentFilter = 'all';

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Reload bets when there's a game result or bet confirmed
    if (data.opcode === 'game_result' || data.opcode === 'bet_confirmed') {
        loadMyBets();
    } else if (data.opcode === 'balance_update') {
        // Update user balance in real-time
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement && data.balance != null) {
            balanceElement.textContent = `$${data.balance.toFixed(2)}`;
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
    
    // Show admin link if user is admin
    const user = JSON.parse(currentUser);
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

// Create bet card element
function createBetCard(bet) {
    const card = document.createElement('div');
    card.className = 'bet-card';
    
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
    timestamp.textContent = `Placed: ${new Date(bet.placed_at).toLocaleString()}`;
    
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
