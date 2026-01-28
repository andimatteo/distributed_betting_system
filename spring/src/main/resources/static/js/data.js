// API Configuration
const API_BASE_URL = '/api';

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

// Admin API Functions
async function createGameAPI(questionText, opt1Text, opt2Text, category) {
    try {
        const response = await fetch(`${API_BASE_URL}/admin/game`, {
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify({
                question_text: questionText,
                opt1_text: opt1Text,
                opt2_text: opt2Text,
                category: category
            })
        });
        
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || 'Failed to create game');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error creating game:', error);
        throw error;
    }
}

async function stopBettingAPI(gameId) {
    try {
        const response = await fetch(`${API_BASE_URL}/admin/stop_betting`, {
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify({
                game_id: gameId
            })
        });
        
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || 'Failed to stop betting');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error stopping betting:', error);
        throw error;
    }
}

async function setGameResultAPI(gameId, result) {
    try {
        const response = await fetch(`${API_BASE_URL}/admin/start_game`, {
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify({
                game_id: gameId,
                result: result
            })
        });
        
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || 'Failed to set game result');
        }
        
        return await response.json();
    } catch (error) {
        console.error('Error setting game result:', error);
        throw error;
    }
}

// Logout function (shared across pages)
function logout() {
    localStorage.removeItem('currentUser');
    window.location.href = 'login.html';
}
