#!/usr/bin/env bash
set -euo pipefail
source deploy/config.conf

# Deployment script for Erlang betting nodes
# Usage: ./deploy_erlang.sh <node_number>
# Example: ./deploy_erlang.sh 1
# Assumes git repository is cloned at ~/distributed_betting_system on each node

ERLANG_PASSWORD="${ERLANG_PASSWORD:?missing ERLANG_PASSWORD}"
JWT_SECRET="${JWT_SECRET:?missing JWT_SECRET}"
SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
ERLANG_USER="dsmt"

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

echo "== Deploying Erlang Node $NODE_NUM ($NODE_NAME) to $NODE_IP =="

# Deploy on remote node
sshpass -p "$ERLANG_PASSWORD" ssh $SSH_OPTS "${ERLANG_USER}@${NODE_IP}" \
    "NODE_NAME='${NODE_NAME}' NODE_IP='${NODE_IP}' JWT_SECRET='${JWT_SECRET}' bash -s" <<'REMOTE'
set -euo pipefail

cd ~/distributed_betting_system

# Pull latest code
echo "Pulling latest code..."
git pull origin main

cd erlang

echo "Compiling application..."
rebar3 compile

# Create a startup script with JWT_SECRET
cat > start_node.sh << STARTUP
#!/bin/bash
export JWT_SECRET='${JWT_SECRET}'
cd ~/distributed_betting_system/erlang
erl -name ${NODE_NAME}@${NODE_IP} -setcookie betting_cookie \\
    -pa _build/default/lib/*/ebin \\
    -config sys.config \\
    -eval "case application:ensure_all_started(betting_node) of {ok, _} -> io:format(\"~n✅ Application betting_node started successfully~n~n\"); {error, Reason} -> io:format(\"~n❌ Failed to start application: ~p~n~n\", [Reason]) end" \\
    "\$@"
STARTUP
chmod +x start_node.sh

echo "Application compiled successfully on $NODE_NAME"
echo "Startup script created: ~/distributed_betting_system/erlang/start_node.sh"
REMOTE

echo "✅ Erlang Node $NODE_NUM deployed to $NODE_IP"
echo ""
echo "To start the node, SSH to $NODE_IP and run:"
echo "  cd ~/distributed_betting_system/erlang"
echo "  ./start_node.sh              # foreground mode"
echo "  ./start_node.sh -detached    # background mode"
echo ""
echo "The node will start as: ${NODE_NAME}@${NODE_IP}"
if [ "$NODE_NUM" == "1" ]; then
    echo ""
    echo "Note: Node 1 (master) must be started FIRST before other nodes."
else
    echo ""
    echo "Note: Make sure Node 1 ($ERLANG1_IP) is running first!"
fi
