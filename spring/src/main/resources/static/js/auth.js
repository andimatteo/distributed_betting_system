// Auth tab switching
function switchAuthTab(tab) {
    const loginForm = document.getElementById('login-form');
    const registerForm = document.getElementById('register-form');
    const tabs = document.querySelectorAll('.tab-btn');
    
    tabs.forEach(btn => btn.classList.remove('active'));
    
    if (tab === 'login') {
        loginForm.style.display = 'block';
        registerForm.style.display = 'none';
        tabs[0].classList.add('active');
    } else {
        loginForm.style.display = 'none';
        registerForm.style.display = 'block';
        tabs[1].classList.add('active');
    }
}

// Handle login
function handleLogin(event) {
    event.preventDefault();
    const username = document.getElementById('login-username').value;
    const password = document.getElementById('login-password').value;

    fetch('/api/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password })
    })
        .then(async (response) => {
            if (!response.ok) {
                const error = await response.json().catch(() => ({}));
                throw new Error(error.error || 'Login failed');
            }
            return response.json();
        })
        .then((data) => {
            const user = { id: data.id, username, isAdmin: data.isAdmin };
            localStorage.setItem('currentUser', JSON.stringify(user));
            localStorage.setItem('authToken', data.token);
            localStorage.setItem('authExpiry', String(data.expiryTimeEpochSeconds));
            window.location.href = 'dashboard.html';
        })
        .catch((err) => showErrorModal(err.message));
}

// Handle registration
function handleRegister(event) {
    event.preventDefault();
    const username = document.getElementById('register-username').value;
    const password = document.getElementById('register-password').value;
    const confirm = document.getElementById('register-confirm').value;

    if (password !== confirm) {
        showErrorModal('Passwords do not match!');
        return;
    }

    fetch('/api/auth/register', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password })
    })
        .then(async (response) => {
            if (!response.ok) {
                const error = await response.json().catch(() => ({}));
                throw new Error(error.error || 'Registration failed');
            }
            return response.json();
        })
        .then((data) => {
            const user = { id: data.id, username, isAdmin: data.isAdmin };
            localStorage.setItem('currentUser', JSON.stringify(user));
            localStorage.setItem('authToken', data.token);
            localStorage.setItem('authExpiry', String(data.expiryTimeEpochSeconds));
            window.location.href = 'dashboard.html';
        })
        .catch((err) => showErrorModal(err.message));
}

// Continue as guest
function continueAsGuest() {
    localStorage.setItem('currentUser', JSON.stringify({ isGuest: true }));
    window.location.href = 'dashboard.html';
}

// Check if already logged in
document.addEventListener('DOMContentLoaded', () => {
    const currentUser = localStorage.getItem('currentUser');
    if (currentUser && !JSON.parse(currentUser).isGuest) {
        window.location.href = 'dashboard.html';
    }
});
