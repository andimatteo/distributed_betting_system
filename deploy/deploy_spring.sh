#!/usr/bin/env bash
set -euo pipefail
source deploy/config.conf

# Ensure JAVA_HOME is valid/accessible and java is on PATH
if [ -n "${JAVA_HOME:-}" ] && [ ! -x "$JAVA_HOME/bin/java" ]; then
  echo "WARN: JAVA_HOME is set but not usable: $JAVA_HOME (falling back to system java)" >&2
  unset JAVA_HOME
fi
if [ -z "${JAVA_HOME:-}" ]; then
  export JAVA_HOME="$(dirname "$(dirname "$(readlink -f "$(command -v java)")")")"
fi
export PATH="$JAVA_HOME/bin:$PATH"

ROOT_PASSWORD="${ROOT_PASSWORD:?missing ROOT_PASSWORD}"
SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

echo "== Build Spring =="
mvn -q -f spring/pom.xml clean package -DskipTests

JAR="$(ls spring/target/*.jar | grep -v plain | head -n 1)"
echo "Jar: $JAR"

echo "== Ensure remote path exists =="
sshpass -p "$ROOT_PASSWORD" ssh $SSH_OPTS "root@${WEB_SERVER_IP}" "\
  mkdir -p \"$(dirname "${SPRING_REMOTE_JAR}")\" \
"

echo "== Copy jar to server =="
sshpass -p "$ROOT_PASSWORD" scp $SSH_OPTS "$JAR" "root@${WEB_SERVER_IP}:${SPRING_REMOTE_JAR}"

echo "== Stop & Rerun Spring (container) =="
sshpass -p "$ROOT_PASSWORD" ssh $SSH_OPTS "root@${WEB_SERVER_IP}" \
  "SPRING_REMOTE_JAR='${SPRING_REMOTE_JAR}' SPRING_SERVICE='${SPRING_SERVICE}' bash -s" <<'REMOTE'
set -euo pipefail

APP_JAR="$SPRING_REMOTE_JAR"
LOG_DIR="$(dirname "$APP_JAR")"
LOG="${LOG_DIR}/${SPRING_SERVICE}.log"

mkdir -p "$LOG_DIR"

echo "Stopping app (if running)..."
pkill -f "java.*${APP_JAR}" || true

echo "Starting app..."
nohup java -jar "$APP_JAR" > "$LOG" 2>&1 &

sleep 1

echo "Checking process..."
pgrep -f "java.*${APP_JAR}" >/dev/null

echo "OK: app is running"
echo "Last logs:"
tail -n 50 "$LOG" || true
REMOTE

echo "✅ Spring deployed"
