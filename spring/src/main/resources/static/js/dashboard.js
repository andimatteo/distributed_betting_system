// State management
let currentFilter = 'all';
let allGames = [];

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Handle odds updates, new games, etc.
    if (data.opcode === 'odds_update') {
        updateGameOdds(data.game_id, data.odd1, data.odd2, data.cap_opt1, data.cap_opt2);
    } else if (data.opcode === 'new_game') {
        loadBets(); // Reload all games when a new one is created
    } else if (data.opcode === 'betting_closed' || data.opcode === 'game_result') {
        loadBets(); // Reload to show updated status
    } else if (data.opcode === 'balance_update') {
        // Update user balance in real-time
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement && data.balance != null) {
            balanceElement.textContent = `$${data.balance.toFixed(2)}`;
        }
    }
});

// Update odds for a specific game in real-time
function updateGameOdds(gameId, odd1, odd2, capOpt1, capOpt2) {
    const game = allGames.find(g => g.game_id === gameId);
    if (game) {
        game.odd1 = odd1;
        game.odd2 = odd2;
        game.cap_opt1 = capOpt1;
        game.cap_opt2 = capOpt2;
        loadBets(); // Refresh the display
    }
}

// Check authentication
document.addEventListener('DOMContentLoaded', () => {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) {
        window.location.href = 'login.html';
        return;
    }
    
    const user = JSON.parse(currentUser);
    
    // Update navbar for guests
    if (user.isGuest) {
        // Show Login/Register, hide My Bets, Deposit, and Logout
        document.getElementById('my-bets-link').style.display = 'none';
        document.getElementById('add-balance-link').style.display = 'none';
        document.getElementById('logout-link').style.display = 'none';
        document.getElementById('login-link').style.display = 'inline';
        
        // Hide balance for guests
        const balanceCard = document.querySelector('.balance-card');
        if (balanceCard) balanceCard.style.display = 'none';
    } else {
        // Show Logout and My Bets, hide Login/Register
        document.getElementById('login-link').style.display = 'none';
        
        // Show admin link if user is admin
        if (user.isAdmin) {
            document.getElementById('admin-link').style.display = 'inline';
        }
    }
    
    // Connect WebSocket
    connectWebSocket();
    
    loadBets();
    
    // Load balance only for authenticated users
    if (!user.isGuest) {
        loadBalance();
    }
});

// Load and display bets
async function loadBets() {
    const grid = document.getElementById('bets-grid');
    grid.innerHTML = '<div style="grid-column: 1/-1; text-align: center; padding: 20px;">Loading games...</div>';
    
    try {
        allGames = await fetchGames();
        
        if (allGames.length === 0) {
            grid.innerHTML = '<div style="grid-column: 1/-1; text-align: center; padding: 20px;">No games available</div>';
            return;
        }
        
        const filteredGames = currentFilter === 'all' 
            ? allGames 
            : allGames.filter(game => getCategoryFromGame(game) === currentFilter);
        
        grid.innerHTML = '';
        
        filteredGames.forEach(game => {
            const card = createBetCard(game);
            grid.appendChild(card);
        });
    } catch (error) {
        console.error('Error loading games:', error);
        grid.innerHTML = '<div style="grid-column: 1/-1; text-align: center; padding: 20px; color: red;">Failed to load games</div>';
    }
}

// Determine category based on game properties
function getCategoryFromGame(game) {
    return game.category || 'real';
}

// Create bet card element
function createBetCard(game) {
    const card = document.createElement('div');
    card.className = 'bet-card';
    card.onclick = () => viewBetDetail(game.game_id);
    
    // Create card header
    const header = document.createElement('div');
    header.className = 'bet-card-header';
    
    const categoryBadge = document.createElement('div');
    const category = getCategoryFromGame(game);
    categoryBadge.className = `bet-category ${category}`;
    categoryBadge.textContent = category;
    
    const title = document.createElement('h3');
    title.textContent = game.question_text;
    
    header.appendChild(categoryBadge);
    header.appendChild(title);
    
    // Create outcomes
    const outcomesDiv = document.createElement('div');
    outcomesDiv.className = 'bet-outcomes';
    
    // Option 1
    const outcome1Div = document.createElement('div');
    outcome1Div.className = 'outcome';
    
    const label1 = document.createElement('span');
    label1.className = 'outcome-label';
    label1.textContent = game.opt1_text;
    
    const odds1 = document.createElement('span');
    odds1.className = 'outcome-odds';
    odds1.textContent = game.odd1 != null ? `${game.odd1.toFixed(2)}x` : 'N/A';
    
    outcome1Div.appendChild(label1);
    outcome1Div.appendChild(odds1);
    outcomesDiv.appendChild(outcome1Div);
    
    // Option 2
    const outcome2Div = document.createElement('div');
    outcome2Div.className = 'outcome';
    
    const label2 = document.createElement('span');
    label2.className = 'outcome-label';
    label2.textContent = game.opt2_text;
    
    const odds2 = document.createElement('span');
    odds2.className = 'outcome-odds';
    odds2.textContent = game.odd2 != null ? `${game.odd2.toFixed(2)}x` : 'N/A';
    
    outcome2Div.appendChild(label2);
    outcome2Div.appendChild(odds2);
    outcomesDiv.appendChild(outcome2Div);
    
    // Create info
    const info = document.createElement('div');
    info.className = 'bet-info';
    
    const volume = document.createElement('span');
    const totalVolume = (game.tot_opt1 || 0) + (game.tot_opt2 || 0);
    volume.textContent = `Volume: $${totalVolume.toFixed(2)}`;
    
    const status = document.createElement('span');
    if (game.result) {
        status.textContent = `Result: ${game.result === 'opt1' ? game.opt1_text : game.opt2_text}`;
        status.style.fontWeight = 'bold';
        status.style.color = '#4CAF50';
    } else if (!game.betting_open) {
        status.textContent = 'Betting Closed';
        status.style.color = '#ff9800';
    } else {
        status.textContent = 'Betting Open';
        status.style.color = '#2196F3';
    }
    
    info.appendChild(volume);
    info.appendChild(status);
    
    // Assemble card
    card.appendChild(header);
    card.appendChild(outcomesDiv);
    card.appendChild(info);
    
    return card;
}

// Filter bets by category
function filterBets(category) {
    currentFilter = category;
    
    // Update active filter button
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    event.target.classList.add('active');
    
    loadBets();
}

// Navigate to bet detail page
function viewBetDetail(gameId) {
    window.location.href = `bet-detail.html?id=${gameId}`;
}

// Load user balance
async function loadBalance() {
    try {
        const balance = await fetchBalance();
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement) {
            balanceElement.textContent = `$${balance.toFixed(2)}`;
        }
    } catch (error) {
        console.error('Error loading balance:', error);
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement) {
            balanceElement.textContent = 'Error';
        }
    }
}

