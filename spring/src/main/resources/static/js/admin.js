// State management
let allGames = [];

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Handle admin-specific updates
    if (data.opcode === 'new_game' || data.opcode === 'odds_update' || 
        data.opcode === 'betting_closed' || data.opcode === 'game_result') {
        loadAdminGames(); // Reload admin table
    }
});

// Check authentication and admin status
document.addEventListener('DOMContentLoaded', () => {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) {
        window.location.href = 'login.html';
        return;
    }
    
    // Check if user is admin
    const user = JSON.parse(currentUser);
    if (!user.isAdmin) {
        alert('Access denied. Admin privileges required.');
        window.location.href = 'dashboard.html';
        return;
    }
    
    // Connect WebSocket
    connectWebSocket();
    
    loadAdminGames();
});

// Load games in admin table
async function loadAdminGames() {
    const tbody = document.getElementById('admin-bets-list');
    tbody.innerHTML = '<tr><td colspan="6" style="text-align: center;">Loading games...</td></tr>';
    
    try {
        allGames = await fetchGames();
        
        tbody.innerHTML = '';
        
        if (allGames.length === 0) {
            tbody.innerHTML = '<tr><td colspan="6" style="text-align: center;">No games available</td></tr>';
            return;
        }
        
        allGames.forEach(game => {
            const row = document.createElement('tr');
            
            // ID cell
            const idCell = document.createElement('td');
            idCell.textContent = game.game_id.substring(0, 8) + '...'; // Shorten the ref
            idCell.title = game.game_id; // Show full ID on hover
            
            // Category cell (not in backend, default to 'real')
            const categoryCell = document.createElement('td');
            const categoryBadge = document.createElement('span');
            categoryBadge.className = 'category-badge real';
            categoryBadge.textContent = 'real';
            categoryCell.appendChild(categoryBadge);
            
            // Title cell
            const titleCell = document.createElement('td');
            titleCell.textContent = game.question_text;
            
            // Status cell
            const statusCell = document.createElement('td');
            const statusBadge = document.createElement('span');
            statusBadge.className = 'status-badge';
            
            if (game.result) {
                statusBadge.classList.add('closed');
                statusBadge.textContent = 'finished';
            } else if (game.betting_open) {
                statusBadge.classList.add('open');
                statusBadge.textContent = 'open';
            } else {
                statusBadge.classList.add('closed');
                statusBadge.textContent = 'closed';
            }
            
            statusCell.appendChild(statusBadge);
            
            // Result cell
            const resultCell = document.createElement('td');
            if (game.result) {
                const resultText = game.result === 'opt1' ? game.opt1_text : game.opt2_text;
                resultCell.textContent = resultText;
                resultCell.style.fontWeight = 'bold';
                resultCell.style.color = '#4CAF50';
            } else {
                resultCell.textContent = '-';
            }
            
            // Actions cell
            const actionsCell = document.createElement('td');
            actionsCell.className = 'actions-cell';
            
            if (game.betting_open && !game.result) {
                const stopBtn = document.createElement('button');
                stopBtn.className = 'btn-small btn-warning';
                stopBtn.textContent = 'Stop Betting';
                stopBtn.onclick = () => stopBettingForGame(game.game_id);
                actionsCell.appendChild(stopBtn);
            }
            
            if (!game.result) {
                const resultBtn = document.createElement('button');
                resultBtn.className = 'btn-small btn-primary';
                resultBtn.textContent = 'Set Result';
                resultBtn.onclick = () => openResultModal(game);
                actionsCell.appendChild(resultBtn);
            }
            
            row.appendChild(idCell);
            row.appendChild(categoryCell);
            row.appendChild(titleCell);
            row.appendChild(statusCell);
            row.appendChild(resultCell);
            row.appendChild(actionsCell);
            
            tbody.appendChild(row);
        });
    } catch (error) {
        console.error('Error loading games:', error);
        tbody.innerHTML = '<tr><td colspan="6" style="text-align: center; color: red;">Failed to load games</td></tr>';
    }
}

// Create new game
async function createBet(event) {
    event.preventDefault();
    
    const questionText = document.getElementById('bet-title').value;
    const opt1Text = document.getElementById('bet-option1').value;
    const opt2Text = document.getElementById('bet-option2').value;
    
    if (!questionText || !opt1Text || !opt2Text) {
        alert('Please fill in all fields');
        return;
    }
    
    try {
        const result = await createGameAPI(questionText, opt1Text, opt2Text);
        
        // Show success message
        alert(`Game created successfully!\nID: ${result.game_id}\nQuestion: ${questionText}`);
        
        // Reset form
        document.getElementById('create-bet-form').reset();
        
        // Reload table
        loadAdminGames();
    } catch (error) {
        alert(`Failed to create game: ${error.message}`);
    }
}

// Stop betting on a specific game
async function stopBettingForGame(gameId) {
    const game = allGames.find(g => g.game_id === gameId);
    if (!game) return;
    
    if (confirm(`Stop betting on "${game.question_text}"?`)) {
        try {
            await stopBettingAPI(gameId);
            alert('Betting stopped successfully!');
            loadAdminGames();
        } catch (error) {
            alert(`Failed to stop betting: ${error.message}`);
        }
    }
}

// Open result modal
let currentResultGame = null;

function openResultModal(game) {
    if (!game) return;
    
    currentResultGame = game;
    
    // Set modal title
    document.getElementById('modal-bet-title').textContent = `Game: ${game.question_text}`;
    
    // Load result options
    const resultOptions = document.getElementById('result-options');
    resultOptions.innerHTML = '';
    
    // Option 1
    const btn1 = document.createElement('button');
    btn1.className = 'btn btn-large result-option-btn';
    btn1.textContent = game.opt1_text;
    btn1.onclick = () => setResult(game.game_id, 'opt1', game.opt1_text);
    resultOptions.appendChild(btn1);
    
    // Option 2
    const btn2 = document.createElement('button');
    btn2.className = 'btn btn-large result-option-btn';
    btn2.textContent = game.opt2_text;
    btn2.onclick = () => setResult(game.game_id, 'opt2', game.opt2_text);
    resultOptions.appendChild(btn2);
    
    // Show modal
    document.getElementById('result-modal').style.display = 'flex';
}

// Close result modal
function closeResultModal() {
    document.getElementById('result-modal').style.display = 'none';
    currentResultGame = null;
}

// Set result for a game
async function setResult(gameId, result, resultText) {
    const game = allGames.find(g => g.game_id === gameId);
    if (!game) return;
    
    if (confirm(`Set result to "${resultText}" for "${game.question_text}"?`)) {
        try {
            const response = await setGameResultAPI(gameId, result);
            
            closeResultModal();
            loadAdminGames();
            
            alert(`Result set successfully!\nGame: ${game.question_text}\nWinner: ${resultText}\nWinners: ${response.winners_count}\nTotal Paid: $${response.total_paid.toFixed(2)}`);
        } catch (error) {
            alert(`Failed to set result: ${error.message}`);
        }
    }
}

// Close modal when clicking outside
window.onclick = function(event) {
    const modal = document.getElementById('result-modal');
    if (event.target === modal) {
        closeResultModal();
    }
}
