#!/bin/bash

# Deployment script for Erlang betting nodes
# Usage: ./deploy_erlang.sh <node_number>
# Example: ./deploy_erlang.sh 1

set -e

# Load configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.conf"

NODE_NUM=$1

if [ -z "$NODE_NUM" ]; then
    echo "Usage: $0 <node_number> (1, 2, or 3)"
    exit 1
fi

# Set node-specific variables
case $NODE_NUM in
    1)
        NODE_IP=$ERLANG1_IP
        NODE_NAME="betting_node1"
        ;;
    2)
        NODE_IP=$ERLANG2_IP
        NODE_NAME="betting_node2"
        ;;
    3)
        NODE_IP=$ERLANG3_IP
        NODE_NAME="betting_node3"
        ;;
    *)
        echo "Invalid node number. Must be 1, 2, or 3"
        exit 1
        ;;
esac

echo "Deploying to Node $NODE_NUM at $NODE_IP..."

# Deploy on remote node
echo "Deploying on remote node..."
ssh "root@${NODE_IP}" << EOF
    set -e
    
    # Ensure application directory exists
    mkdir -p /opt/betting_system
    cd /opt/betting_system
    
    # Ensure code is pulled (user assumes git repo already cloned)
    if [ -d .git ]; then
        echo "Pulling latest code..."
        git pull origin main
    else
        echo "Git repository not found. Ensure code is already cloned at /opt/betting_system"
        exit 1
    fi
    
    # Install Erlang if needed
    if ! command -v erl &> /dev/null; then
        echo "Installing Erlang..."
        apt-get update
        apt-get install -y erlang
    fi
    
    # Install rebar3 if needed
    if ! command -v rebar3 &> /dev/null; then
        echo "Installing rebar3..."
        apt-get update
        apt-get install -y rebar3
    fi
    
    # Navigate to erlang directory and compile
    cd erlang
    echo "Compiling application..."
    rebar3 compile
    
    echo "Application compiled successfully"
EOF

echo ""
echo "Deployment to Node $NODE_NUM completed!"
echo ""
echo "Next steps:"
if [ "$NODE_NUM" == "1" ]; then
    echo "  1. SSH to $NODE_IP:"
    echo "     ssh root@${NODE_IP}"
    echo ""
    echo "  2. Start the master node (Node 1 must start first):"
    echo "     cd /opt/betting_system/erlang"
    echo "     erl -sname ${NODE_NAME} -setcookie betting_cookie \\"
    echo "         -pa _build/default/lib/*/ebin \\"
    echo "         -config sys.config \\"
    echo "         -eval \"application:start(betting_node)\""
    echo ""
    echo "  3. After Node 1 is running, deploy and start Nodes 2 and 3"
else
    echo "  1. SSH to $NODE_IP:"
    echo "     ssh root@${NODE_IP}"
    echo ""
    echo "  2. Start this worker node:"
    echo "     cd /opt/betting_system/erlang"
    echo "     erl -sname ${NODE_NAME} -setcookie betting_cookie \\"
    echo "         -pa _build/default/lib/*/ebin \\"
    echo "         -config sys.config \\"
    echo "         -eval \"application:start(betting_node)\""
    echo ""
    echo "  Note: Make sure Node 1 (10.2.1.62) is running first!"
fi
echo ""
echo "For detached mode (background), add '-detached' to the erl command above"
echo ""
