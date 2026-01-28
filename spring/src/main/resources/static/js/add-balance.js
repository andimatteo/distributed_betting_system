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
    
    const user = JSON.parse(currentUser);
    
    // Guests cannot Deposit
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

