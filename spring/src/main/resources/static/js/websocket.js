// Common WebSocket connection management
let ws = null;
let wsMessageHandlers = [];

function connectWebSocket() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const wsUrl = `${protocol}//${window.location.host}/ws`;
    
    ws = new WebSocket(wsUrl);
    
    ws.onopen = () => {
        console.log('WebSocket connected');
        // Send auth token
        const token = localStorage.getItem('authToken');
        if (token) {
            ws.send(JSON.stringify({ type: 'auth', token }));
        }
    };
    
    ws.onmessage = (event) => {
        console.log('WebSocket message:', event.data);
        try {
            const data = JSON.parse(event.data);
            // Call all registered message handlers
            wsMessageHandlers.forEach(handler => {
                try {
                    handler(data);
                } catch (e) {
                    console.error('Error in WebSocket message handler:', e);
                }
            });
        } catch (e) {
            console.error('Error parsing WebSocket message:', e);
        }
    };
    
    ws.onerror = (error) => {
        console.error('WebSocket error:', error);
    };
    
    ws.onclose = () => {
        console.log('WebSocket disconnected');
        // Attempt to reconnect after 3 seconds
        setTimeout(connectWebSocket, 3000);
    };
}

// Register a message handler
function registerWSMessageHandler(handler) {
    if (typeof handler === 'function') {
        wsMessageHandlers.push(handler);
    }
}

// Send a message through WebSocket
function sendWSMessage(data) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(data));
    } else {
        console.warn('WebSocket is not connected');
    }
}

// Get WebSocket connection status
function isWSConnected() {
    return ws && ws.readyState === WebSocket.OPEN;
}
