#!/usr/bin/env bash
set -euo pipefail
source deploy/config.conf

ROOT_PASSWORD="${ROOT_PASSWORD:?missing ROOT_PASSWORD}"
SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

LOCAL_CONF="nginx/betting.conf"
REMOTE_CONF="${NGINX_REMOTE_CONF}"
REMOTE_SITE_NAME="${NGINX_SITE_NAME}"

if [[ ! -f "${LOCAL_CONF}" ]]; then
  echo "Missing local nginx config: ${LOCAL_CONF}" >&2
  exit 1
fi

echo "== Copy nginx config to load balancer =="
sshpass -p "$ROOT_PASSWORD" scp $SSH_OPTS "${LOCAL_CONF}" "root@${LOAD_BALANCER_IP}:${REMOTE_CONF}"

echo "== Enable site (if needed) =="
sshpass -p "$ROOT_PASSWORD" ssh $SSH_OPTS "root@${LOAD_BALANCER_IP}" "\
  if [[ ! -e /etc/nginx/sites-enabled/${REMOTE_SITE_NAME} ]]; then
    ln -s ${REMOTE_CONF} /etc/nginx/sites-enabled/${REMOTE_SITE_NAME};
  fi
"

echo "== Test and restart nginx =="
sshpass -p "$ROOT_PASSWORD" ssh $SSH_OPTS "root@${LOAD_BALANCER_IP}" "\
  nginx -t && systemctl restart nginx && systemctl --no-pager status nginx | head -n 25 \
"

echo "✅ Nginx deployed"
