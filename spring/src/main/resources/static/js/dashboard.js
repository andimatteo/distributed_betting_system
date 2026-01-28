// State management
let currentFilter = 'all';
let allGames = [];

// WebSocket message handler
registerWSMessageHandler((data) => {
    if (data.opcode === 'odds_update') {
        updateGameOddsDOM(data.game_id, data.odd1, data.odd2, data.cap_opt1, data.cap_opt2, data.total_volume);
    } else if (data.opcode === 'new_game') {
        addNewGameDOM(data);
    } else if (data.opcode === 'betting_closed') {
        handleBettingClosedDOM(data.game_id);
    } else if (data.opcode === 'game_result') {
        handleGameResultDOM(data.game_id, data.result);
    } else if (data.opcode === 'balance_update') {
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement && data.balance != null) {
            balanceElement.textContent = `$${data.balance.toFixed(2)}`;
        }
    }
});

// Update odds directly in the DOM
function updateGameOddsDOM(gameId, odd1, odd2, capOpt1, capOpt2, totalVolume) {
    // Update in-memory state
    const game = allGames.find(g => g.game_id === gameId);
    if (game) {
        game.odd1 = odd1;
        game.odd2 = odd2;
        game.cap_opt1 = capOpt1;
        game.cap_opt2 = capOpt2;
        game.total_volume = totalVolume;
    }
    
    // Update DOM
    const card = document.getElementById(`game-card-${gameId}`);
    if (!card) return;
    
    // Update odds
    const odds1Elem = card.querySelector('.outcome-odds-1');
    const odds2Elem = card.querySelector('.outcome-odds-2');
    if (odds1Elem) odds1Elem.textContent = `${odd1.toFixed(2)}x`;
    if (odds2Elem) odds2Elem.textContent = `${odd2.toFixed(2)}x`;
    
    // Update volume
    const volumeElem = card.querySelector('.bet-volume');
    if (volumeElem) volumeElem.textContent = `Volume: $${totalVolume.toFixed(2)}`;
}

// Add new game to DOM
function addNewGameDOM(gameData) {
    const game = {
        game_id: gameData.game_id,
        question_text: gameData.question_text,
        opt1_text: gameData.opt1_text,
        opt2_text: gameData.opt2_text,
        category: gameData.category,
        betting_open: gameData.betting_open,
        odd1: gameData.odd1,
        odd2: gameData.odd2,
        cap_opt1: gameData.cap_opt1,
        cap_opt2: gameData.cap_opt2,
        tot_opt1: 0,
        tot_opt2: 0,
        result: null
    };
    
    // Add to state
    allGames.push(game);
    
    // Check if game matches current filter
    const category = getCategoryFromGame(game);
    if (currentFilter !== 'all' && category !== currentFilter) {
        return; // Don't display if filtered out
    }
    
    // Create card
    const card = createBetCard(game);
    card.style.opacity = '0';
    card.style.transform = 'translateY(-20px)';
    
    // Find or create active section
    const grid = document.getElementById('bets-grid');
    let activeHeader = grid.querySelector('.section-header.active-header');
    
    if (!activeHeader) {
        activeHeader = document.createElement('div');
        activeHeader.className = 'section-header active-header';
        activeHeader.textContent = 'Active Bets';
        grid.insertBefore(activeHeader, grid.firstChild);
    }
    
    // Insert after active header (at the beginning of active games)
    const nextElement = activeHeader.nextElementSibling;
    if (nextElement && nextElement.classList.contains('bet-card')) {
        grid.insertBefore(card, nextElement);
    } else {
        grid.insertBefore(card, nextElement);
    }
    
    // Animate in
    setTimeout(() => {
        card.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
        card.style.opacity = '1';
        card.style.transform = 'translateY(0)';
    }, 10);
}

// Handle betting closed
function handleBettingClosedDOM(gameId) {
    // Update state
    const game = allGames.find(g => g.game_id === gameId);
    if (game) {
        game.betting_open = false;
    }
    
    // Move card from active to closed section
    moveCardToClosed(gameId, 'Betting Closed', '#ff9800');
}

// Handle game result
function handleGameResultDOM(gameId, result) {
    // Update state
    const game = allGames.find(g => g.game_id === gameId);
    if (game) {
        game.result = result;
        game.betting_open = false;
    }
    
    // Move card and update result
    const resultText = game ? `Result: ${result === 'opt1' ? game.opt1_text : game.opt2_text}` : `Result: ${result}`;
    moveCardToClosed(gameId, resultText, '#4CAF50', true);
}

// Move card from active to closed section
function moveCardToClosed(gameId, statusText, statusColor, isResult = false) {
    const card = document.getElementById(`game-card-${gameId}`);
    if (!card) return;
    
    const grid = document.getElementById('bets-grid');
    
    // Update status
    const statusElem = card.querySelector('.bet-status');
    if (statusElem) {
        statusElem.textContent = statusText;
        statusElem.style.color = statusColor;
        if (isResult) {
            statusElem.style.fontWeight = 'bold';
        }
    }
    
    // Find or create closed section
    let closedHeader = grid.querySelector('.section-header.closed-header');
    
    if (!closedHeader) {
        closedHeader = document.createElement('div');
        closedHeader.className = 'section-header closed-header';
        closedHeader.textContent = 'Closed Bets';
        grid.appendChild(closedHeader);
    }
    
    // Animate out and move
    card.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
    card.style.opacity = '0';
    card.style.transform = 'translateX(-20px)';
    
    setTimeout(() => {
        // Move to closed section (insert after header)
        const nextElement = closedHeader.nextElementSibling;
        if (nextElement && nextElement.classList.contains('bet-card')) {
            grid.insertBefore(card, nextElement);
        } else {
            grid.appendChild(card);
        }
        
        // Animate in
        setTimeout(() => {
            card.style.opacity = '1';
            card.style.transform = 'translateX(0)';
        }, 10);
    }, 300);
}

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
        
        // Separate active and inactive games
        const activeGames = filteredGames.filter(game => game.betting_open && !game.result);
        const inactiveGames = filteredGames.filter(game => !game.betting_open || game.result);
        
        // Sort by game_id descending (newest first)
        activeGames.sort((a, b) => b.game_id - a.game_id);
        inactiveGames.sort((a, b) => b.game_id - a.game_id);
        
        grid.innerHTML = '';
        
        // Display active games
        if (activeGames.length > 0) {
            const activeHeader = document.createElement('div');
            activeHeader.className = 'section-header active-header';
            activeHeader.textContent = 'Active Bets';
            grid.appendChild(activeHeader);
            
            activeGames.forEach(game => {
                const card = createBetCard(game);
                grid.appendChild(card);
            });
        }
        
        // Display inactive games
        if (inactiveGames.length > 0) {
            const inactiveHeader = document.createElement('div');
            inactiveHeader.className = 'section-header closed-header';
            inactiveHeader.textContent = 'Closed Bets';
            grid.appendChild(inactiveHeader);
            
            inactiveGames.forEach(game => {
                const card = createBetCard(game);
                grid.appendChild(card);
            });
        }
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
    card.id = `game-card-${game.game_id}`;
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
    odds1.className = 'outcome-odds outcome-odds-1';
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
    odds2.className = 'outcome-odds outcome-odds-2';
    odds2.textContent = game.odd2 != null ? `${game.odd2.toFixed(2)}x` : 'N/A';
    
    outcome2Div.appendChild(label2);
    outcome2Div.appendChild(odds2);
    outcomesDiv.appendChild(outcome2Div);
    
    // Create info
    const info = document.createElement('div');
    info.className = 'bet-info';
    
    const volume = document.createElement('span');
    volume.className = 'bet-volume';
    const totalVolume = game.total_volume ?? ((game.tot_opt1 || 0) + (game.tot_opt2 || 0));
    volume.textContent = `Volume: $${totalVolume.toFixed(2)}`;
    
    const status = document.createElement('span');
    status.className = 'bet-status';
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

