// State management
let currentFilter = 'all';

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Handle odds updates, new bets, etc.
    if (data.type === 'odds_update' || data.type === 'bet_update') {
        loadBets(); // Reload bets when updates arrive
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
    
    loadBets();
});

// Load and display bets
function loadBets() {
    const grid = document.getElementById('bets-grid');
    grid.innerHTML = '';
    
    const filteredBets = currentFilter === 'all' 
        ? mockBets 
        : mockBets.filter(bet => bet.category === currentFilter);
    
    filteredBets.forEach(bet => {
        const card = createBetCard(bet);
        grid.appendChild(card);
    });
}

// Create bet card element
function createBetCard(bet) {
    const card = document.createElement('div');
    card.className = 'bet-card';
    card.onclick = () => viewBetDetail(bet.id);
    
    // Create card header
    const header = document.createElement('div');
    header.className = 'bet-card-header';
    
    const categoryBadge = document.createElement('div');
    categoryBadge.className = 'bet-category';
    categoryBadge.textContent = bet.category;
    
    const title = document.createElement('h3');
    title.textContent = bet.title;
    
    header.appendChild(categoryBadge);
    header.appendChild(title);
    
    // Create outcomes
    const outcomesDiv = document.createElement('div');
    outcomesDiv.className = 'bet-outcomes';
    
    bet.outcomes.slice(0, 2).forEach(outcome => {
        const outcomeDiv = document.createElement('div');
        outcomeDiv.className = 'outcome';
        
        const label = document.createElement('span');
        label.className = 'outcome-label';
        label.textContent = outcome.label;
        
        const odds = document.createElement('span');
        odds.className = 'outcome-odds';
        odds.textContent = `${outcome.odds}x`;
        
        outcomeDiv.appendChild(label);
        outcomeDiv.appendChild(odds);
        outcomesDiv.appendChild(outcomeDiv);
    });
    
    // Create info
    const info = document.createElement('div');
    info.className = 'bet-info';
    
    const volume = document.createElement('span');
    volume.textContent = `Volume: ${bet.volume}`;
    
    const participants = document.createElement('span');
    participants.textContent = `${bet.participants} participants`;
    
    info.appendChild(volume);
    info.appendChild(participants);
    
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
function viewBetDetail(betId) {
    window.location.href = `bet-detail.html?id=${betId}`;
}
