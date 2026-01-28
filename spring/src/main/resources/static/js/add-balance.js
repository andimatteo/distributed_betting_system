// WebSocket message handler
registerWSMessageHandler((data) => {
    // Update balance when receiving balance_update message
    if (data.opcode === 'balance_update') {
        const balanceElement = document.querySelector('.balance-amount');
        if (balanceElement && data.balance != null) {
            balanceElement.textContent = `$${data.balance.toFixed(2)}`;
        }
    }
});

// Check authentication
document.addEventListener('DOMContentLoaded', async () => {
    const currentUser = localStorage.getItem('currentUser');
    if (!currentUser) {
        window.location.href = 'login.html';
        return;
    }
    
    // Check if JWT is expired
    if (!checkAuthExpiration()) {
        return; // checkAuthExpiration handles redirect
    }
    
    // Show admin link if user is admin
    if (user.isAdmin) {
        document.getElementById('admin-link').style.display = 'inline';
    }
    
    // Connect WebSocket
    connectWebSocket();
    
    // Load and display current balance
    await loadCurrentBalance();
});

// Load current balance
async function loadCurrentBalance() {
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

// Handle Deposit
async function handleAddBalance(event) {
    event.preventDefault();
    const amount = parseFloat(document.getElementById('amount').value);
    
    if (!amount || amount <= 0) {
        showErrorModal('Please enter a valid amount');
        return;
    }
    
    try {
        await addBalanceAPI(amount);
        
        // Reset form (balance will be updated via WebSocket)
        document.getElementById('add-balance-form').reset();
    } catch (error) {
        showErrorModal(`Failed to Deposit: ${error.message}`);
    }
}

