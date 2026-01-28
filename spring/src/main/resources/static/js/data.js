// API Configuration
const API_BASE_URL = '/api';

// Error Modal Functions
function showErrorModal(message) {
    // Create modal if it doesn't exist
    let modal = document.getElementById('error-modal');
    if (!modal) {
        modal = document.createElement('div');
        modal.id = 'error-modal';
        modal.className = 'error-modal';
        modal.innerHTML = `
            <div class="error-modal-content">
                <div class="error-modal-header">
                    <div class="error-modal-icon">!</div>
                    <div class="error-modal-title">Error</div>
                </div>
                <div class="error-modal-message" id="error-modal-message"></div>
                <button class="error-modal-button" onclick="closeErrorModal()">OK</button>
            </div>
        `;
        document.body.appendChild(modal);
        
        // Close on background click
        modal.addEventListener('click', (e) => {
            if (e.target === modal) {
                closeErrorModal();
            }
        });
    }
    
    // Set message and show
    document.getElementById('error-modal-message').textContent = message;
    modal.classList.add('active');
}

function closeErrorModal() {
    const modal = document.getElementById('error-modal');
    if (modal) {
        modal.classList.remove('active');
    }
}

// Helper function to check if current user is a guest
function isGuest() {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) return false;
    try {
        const user = JSON.parse(currentUser);
        return user.isGuest === true;
    } catch {
        return false;
    }
}

// Helper function to format timestamp in European format (local timezone)
function formatEuropeanDateTime(timestampInSeconds) {
    const date = new Date(timestampInSeconds * 1000); // Convert seconds to milliseconds
    const day = String(date.getDate()).padStart(2, '0');
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${day}/${month}/${year}, ${hours}:${minutes}:${seconds}`;
}

// Helper function to get auth headers
function getAuthHeaders() {
    const token = localStorage.getItem('authToken');
    return {
        'Content-Type': 'application/json',
        'Authorization': token ? `Bearer ${token}` : ''
    };
}

// API Functions
async function fetchGames() {
    try {
        const response = await fetch(`${API_BASE_URL}/games`, {
            method: 'GET',
            headers: getAuthHeaders()
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`fetchGames failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            throw new Error('Failed to fetch games');
        }
        
        const data = await response.json();
        return data.games || [];
    } catch (error) {
        console.error('Error fetching games:', error);
        return [];
    }
}

async function fetchGameDetail(gameId) {
    try {
        const response = await fetch(`${API_BASE_URL}/games/${gameId}`, {
            method: 'GET',
            headers: getAuthHeaders()
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`fetchGameDetail failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            throw new Error('Failed to fetch game details');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error fetching game details:', error);
        throw error;
    }
}

async function submitBetAPI(gameId, amount, choice) {
    try {
        const response = await fetch(`${API_BASE_URL}/bet`, {
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify({
                game_id: gameId,
                amount: amount,
                choice: choice
            })
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`submitBet failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            const errorData = await response.json();
            throw new Error(errorData.error || 'Failed to place bet');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error placing bet:', error);
        throw error;
    }
}

async function fetchBalance() {
    try {
        const response = await fetch(`${API_BASE_URL}/balance`, {
            method: 'GET',
            headers: getAuthHeaders()
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`fetchBalance failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            throw new Error('Failed to fetch balance');
        }
        
        const data = await response.json();
        return data.balance || 0;
    } catch (error) {
        console.error('Error fetching balance:', error);
        return 0;
    }
}

// Fetch user's bets
async function fetchUserBets(gameId = null) {
    try {
        const url = gameId 
            ? `${API_BASE_URL}/user/bets/${gameId}`
            : `${API_BASE_URL}/user/bets`;
            
        const response = await fetch(url, {
            method: 'GET',
            headers: getAuthHeaders()
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`fetchUserBets failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            throw new Error('Failed to fetch bets');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error fetching user bets:', error);
        throw error;
    }
}

// Deposit to user account
async function addBalanceAPI(amount) {
    try {
        const response = await fetch(`${API_BASE_URL}/balance`, {
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify({ amount: amount })
        });
        
        if (!response.ok) {
            const erlangNode = response.headers.get('x-erlang-node');
            console.error(`addBalance failed [${response.status}] from node: ${erlangNode || 'unknown'}`);
            const error = await response.json().catch(() => ({}));
            throw new Error(error.error || 'Failed to Deposit');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error adding balance:', error);
        throw error;
    }
}

// Logout function (shared across pages)
function logout() {
    localStorage.removeItem('currentUser');
    window.location.href = 'login.html';
}
