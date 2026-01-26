// State management
let currentBet = null;
let selectedOutcome = null;
let wheelCanvas = null;
let wheelCtx = null;
let wheelAngle = 0;
let wheelSpinning = false;
let wheelAnimationId = null;

// WebSocket message handler
registerWSMessageHandler((data) => {
    // Handle real-time odds updates for current bet
    if (data.type === 'odds_update' && currentBet && data.betId === currentBet.id) {
        // Update odds display
        updateOddsDisplay(data.odds);
    }
});

function updateOddsDisplay(odds) {
    // Update the odds in the current bet view
    if (currentBet && odds) {
        currentBet.outcomes.forEach((outcome, index) => {
            if (odds[index]) {
                outcome.odds = odds[index];
            }
        });
        // Refresh the display
        const urlParams = new URLSearchParams(window.location.search);
        const betId = parseInt(urlParams.get('id'));
        if (betId) {
            loadBetDetail(betId);
        }
    }
}

// Check authentication and load bet
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
    
    // Get bet ID from URL
    const urlParams = new URLSearchParams(window.location.search);
    const betId = parseInt(urlParams.get('id'));
    
    if (!betId) {
        window.location.href = 'dashboard.html';
        return;
    }
    
    loadBetDetail(betId);
    
    // Add event listener for amount input
    const amountInput = document.getElementById('bet-amount');
    if (amountInput) {
        amountInput.addEventListener('input', updatePotentialReturn);
    }
});

// Load bet detail
function loadBetDetail(betId) {
    currentBet = mockBets.find(bet => bet.id === betId);
    if (!currentBet) {
        window.location.href = 'dashboard.html';
        return;
    }
    
    // Populate bet details
    document.getElementById('detail-category').textContent = currentBet.category;
    document.getElementById('detail-category').className = 'bet-category';
    document.getElementById('detail-title').textContent = currentBet.title;
    document.getElementById('detail-volume').textContent = currentBet.volume;
    document.getElementById('detail-participants').textContent = currentBet.participants;
    
    // Load outcome buttons
    const outcomesContainer = document.getElementById('outcome-options');
    outcomesContainer.innerHTML = '';
    
    currentBet.outcomes.forEach((outcome, index) => {
        const btn = document.createElement('div');
        btn.className = 'outcome-btn';
        btn.onclick = () => selectOutcome(index);
        
        const label = document.createElement('span');
        label.className = 'outcome-btn-label';
        label.textContent = outcome.label;
        
        const odds = document.createElement('span');
        odds.className = 'outcome-btn-odds';
        odds.textContent = `${outcome.odds}x`;
        
        btn.appendChild(label);
        btn.appendChild(odds);
        outcomesContainer.appendChild(btn);
    });
    
    // Load activity
    loadActivity();
    
    // Initialize spinning wheel for virtual events
    if (currentBet.category === 'virtual') {
        document.getElementById('spinning-wheel-section').style.display = 'block';
        initWheel();
    } else {
        document.getElementById('spinning-wheel-section').style.display = 'none';
    }
}

// Select outcome
function selectOutcome(index) {
    selectedOutcome = index;
    
    // Update button states
    document.querySelectorAll('.outcome-btn').forEach((btn, i) => {
        if (i === index) {
            btn.classList.add('selected');
        } else {
            btn.classList.remove('selected');
        }
    });
    
    // Update potential return
    updatePotentialReturn();
}

// Update potential return calculation
function updatePotentialReturn() {
    const amountInput = document.getElementById('bet-amount');
    const amount = parseFloat(amountInput.value) || 0;
    
    if (selectedOutcome !== null && amount > 0) {
        const odds = currentBet.outcomes[selectedOutcome].odds;
        const potentialReturn = amount * odds;
        document.getElementById('potential-return').textContent = 
            '$' + potentialReturn.toFixed(2);
    } else {
        document.getElementById('potential-return').textContent = '$0.00';
    }
}

// Place bet
function placeBet() {
    const amount = parseFloat(document.getElementById('bet-amount').value);
    
    if (!amount || amount <= 0) {
        alert('Please enter a valid amount');
        return;
    }
    
    if (selectedOutcome === null) {
        alert('Please select an outcome');
        return;
    }
    
    const outcome = currentBet.outcomes[selectedOutcome];
    const potentialReturn = amount * outcome.odds;
    
    // Mock bet placement - in real app, this would call backend API
    alert(`Bet placed successfully!\n\nAmount: $${amount.toFixed(2)}\nOutcome: ${outcome.label}\nPotential Return: $${potentialReturn.toFixed(2)}`);
    
    // Reset form
    document.getElementById('bet-amount').value = '';
    selectedOutcome = null;
    document.querySelectorAll('.outcome-btn').forEach(btn => {
        btn.classList.remove('selected');
    });
    updatePotentialReturn();
}

// Load activity
function loadActivity() {
    const activityList = document.getElementById('activity-list');
    activityList.innerHTML = '';
    
    mockActivity.forEach(activity => {
        const item = document.createElement('div');
        item.className = 'activity-item';
        
        const header = document.createElement('div');
        header.className = 'activity-header';
        
        const details = document.createElement('span');
        details.className = 'activity-details';
        details.textContent = activity.action;
        
        const time = document.createElement('span');
        time.className = 'activity-time';
        time.textContent = activity.time;
        
        header.appendChild(details);
        header.appendChild(time);
        item.appendChild(header);
        
        activityList.appendChild(item);
    });
}

// Initialize spinning wheel
function initWheel() {
    wheelCanvas = document.getElementById('wheel-canvas');
    if (!wheelCanvas) return;
    
    wheelCtx = wheelCanvas.getContext('2d');
    drawWheel();
}

// Draw the spinning wheel
function drawWheel() {
    const centerX = wheelCanvas.width / 2;
    const centerY = wheelCanvas.height / 2;
    const radius = 180;
    
    // Clear canvas
    wheelCtx.clearRect(0, 0, wheelCanvas.width, wheelCanvas.height);
    
    // Save context
    wheelCtx.save();
    
    // Rotate canvas
    wheelCtx.translate(centerX, centerY);
    wheelCtx.rotate(wheelAngle * Math.PI / 180);
    wheelCtx.translate(-centerX, -centerY);
    
    // Draw segments (alternating red and black)
    const segments = 20;
    const anglePerSegment = (2 * Math.PI) / segments;
    
    for (let i = 0; i < segments; i++) {
        wheelCtx.beginPath();
        wheelCtx.moveTo(centerX, centerY);
        wheelCtx.arc(
            centerX,
            centerY,
            radius,
            i * anglePerSegment,
            (i + 1) * anglePerSegment
        );
        wheelCtx.closePath();
        
        // Alternate between red and black
        wheelCtx.fillStyle = i % 2 === 0 ? '#ef4444' : '#1e293b';
        wheelCtx.fill();
        
        // Add border
        wheelCtx.strokeStyle = '#f1f5f9';
        wheelCtx.lineWidth = 2;
        wheelCtx.stroke();
        
        // Add number to segment
        wheelCtx.save();
        const angle = i * anglePerSegment + anglePerSegment / 2;
        const textRadius = radius * 0.75;
        const textX = centerX + Math.cos(angle) * textRadius;
        const textY = centerY + Math.sin(angle) * textRadius;
        
        wheelCtx.translate(textX, textY);
        wheelCtx.rotate(angle + Math.PI / 2);
        
        wheelCtx.fillStyle = '#ffffff';
        wheelCtx.font = 'bold 18px Arial';
        wheelCtx.textAlign = 'center';
        wheelCtx.textBaseline = 'middle';
        wheelCtx.fillText(i.toString(), 0, 0);
        
        wheelCtx.restore();
    }
    
    // Draw center circle
    wheelCtx.beginPath();
    wheelCtx.arc(centerX, centerY, 20, 0, 2 * Math.PI);
    wheelCtx.fillStyle = '#4f46e5';
    wheelCtx.fill();
    wheelCtx.strokeStyle = '#f1f5f9';
    wheelCtx.lineWidth = 3;
    wheelCtx.stroke();
    
    // Restore context
    wheelCtx.restore();
}

// Spin the wheel and stop at specified color
function spinWheel(stopColor) {
    if (wheelSpinning) return;
    
    console.log('=== SPIN WHEEL DEBUG ===');
    console.log('Target color:', stopColor);
    
    wheelSpinning = true;
    
    // Calculate target angle based on color
    // Red segments are at even positions (0, 2, 4, ..., 18)
    // Black segments are at odd positions (1, 3, 5, ..., 19)
    const segments = 20;
    const anglePerSegment = 360 / segments; // 18 degrees per segment
    
    console.log('Angle per segment:', anglePerSegment);
    
    // Choose a random segment of the target color
    let targetSegment;
    if (stopColor === 'red') {
        const redSegments = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18];
        targetSegment = redSegments[Math.floor(Math.random() * redSegments.length)];
    } else {
        const blackSegments = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
        targetSegment = blackSegments[Math.floor(Math.random() * blackSegments.length)];
    }
    
    console.log('Target segment index:', targetSegment);
    console.log('Target segment should be:', targetSegment % 2 === 0 ? 'RED' : 'BLACK');
    
    // The wheel is rotated, and pointer is at the TOP
    // In our drawing, segments start from 0 degrees (right side) when wheelAngle = 0
    // Pointer is at 270 degrees in canvas coords (top of circle)
    
    // Segment center in the original (un-rotated) wheel
    const segmentStartAngle = targetSegment * anglePerSegment;
    const segmentCenterInWheel = segmentStartAngle + anglePerSegment / 2;
    
    console.log('Segment center angle (in wheel):', segmentCenterInWheel);
    
    // To align this segment center with the pointer at the top (270 degrees),
    // we need to rotate the wheel by: (270 - segmentCenterInWheel) degrees
    // But since our rotation is in the opposite direction, it's: segmentCenterInWheel - 270
    let targetAngle = 270 - segmentCenterInWheel;
    
    // Normalize to 0-360 range
    while (targetAngle < 0) targetAngle += 360;
    while (targetAngle >= 360) targetAngle -= 360;
    
    console.log('Target rotation angle:', targetAngle);
    
    // Add multiple rotations for effect (5-7 full rotations)
    const numRotations = Math.floor(5 + Math.random() * 3); // 5, 6, or 7 complete rotations
    const extraRotations = numRotations * 360;
    const totalRotation = extraRotations + targetAngle;
    
    console.log('Number of rotations:', numRotations);
    console.log('Extra rotations:', extraRotations);
    console.log('Total rotation:', totalRotation);
    
    // Animation parameters
    const duration = 5000;
    const startTime = Date.now();
    const startAngle = wheelAngle % 360;
    
    console.log('Starting from angle:', startAngle);
    
    // Calculate how much we need to rotate from current position
    let angleDifference = targetAngle - startAngle;
    while (angleDifference < 0) angleDifference += 360;
    
    const finalAngle = startAngle + extraRotations + angleDifference;
    
    console.log('Final target angle:', finalAngle);
    
    function animate() {
        const currentTime = Date.now();
        const elapsed = currentTime - startTime;
        const progress = Math.min(elapsed / duration, 1);
        
        // Easing function (ease-out)
        const easeOut = 1 - Math.pow(1 - progress, 3);
        
        wheelAngle = startAngle + ((finalAngle - startAngle) * easeOut);
        drawWheel();
        
        if (progress < 1) {
            wheelAnimationId = requestAnimationFrame(animate);
        } else {
            wheelSpinning = false;
            const finalNormalizedAngle = wheelAngle % 360;
            wheelAngle = finalNormalizedAngle;
            
            console.log('Final wheel angle:', finalNormalizedAngle);
            
            // Verify which segment we landed on
            const pointerAngle = 270; // Top position
            const segmentAtPointer = Math.floor(((pointerAngle - finalNormalizedAngle + 360) % 360) / anglePerSegment) % segments;
            const colorAtPointer = segmentAtPointer % 2 === 0 ? 'RED' : 'BLACK';
            
            console.log('Segment at pointer:', segmentAtPointer);
            console.log('Color at pointer:', colorAtPointer);
            console.log('Expected color:', stopColor.toUpperCase());
            console.log('=== END DEBUG ===');
            
            // Show result
            setTimeout(() => {
                alert(`Wheel stopped on ${colorAtPointer}!\nExpected: ${stopColor.toUpperCase()}`);
            }, 100);
        }
    }
    
    animate();
}

// Test function for buttons
function testSpin(color) {
    spinWheel(color);
}
