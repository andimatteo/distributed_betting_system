#!/usr/bin/env bash
set -euo pipefail
source deploy/config.conf

# Stop all Erlang nodes before redeploying
# This ensures node 1 doesn't connect to old processes from previous runs

ERLANG_PASSWORD="${ERLANG_PASSWORD:?missing ERLANG_PASSWORD}"
SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
ERLANG_USER="dsmt"

echo "== Stopping all Erlang nodes =="

# Stop node 1
echo "Stopping node 1 at $ERLANG1_IP..."
sshpass -p "$ERLANG_PASSWORD" ssh $SSH_OPTS "${ERLANG_USER}@${ERLANG1_IP}" \
    "pkill -f 'name betting_node1@' || true; rm -rf ~/distributed_betting_system/erlang/mnesia_data" || true

# Stop node 2
echo "Stopping node 2 at $ERLANG2_IP..."
sshpass -p "$ERLANG_PASSWORD" ssh $SSH_OPTS "${ERLANG_USER}@${ERLANG2_IP}" \
    "pkill -f 'name betting_node2@' || true; rm -rf ~/distributed_betting_system/erlang/mnesia_data" || true

# Stop node 3
echo "Stopping node 3 at $ERLANG3_IP..."
sshpass -p "$ERLANG_PASSWORD" ssh $SSH_OPTS "${ERLANG_USER}@${ERLANG3_IP}" \
    "pkill -f 'name betting_node3@' || true; rm -rf ~/distributed_betting_system/erlang/mnesia_data" || true

# Wait for processes to fully terminate
sleep 3

echo "✅ All Erlang nodes stopped"
