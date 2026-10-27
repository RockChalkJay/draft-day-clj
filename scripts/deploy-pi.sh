#!/usr/bin/env bash
# Build on the laptop, ship the jar and units to the Pi, restart the server.
#   scripts/deploy-pi.sh                  # PI defaults to pi@raspberrypi.local
#   PI=me@192.168.1.50 scripts/deploy-pi.sh
# SKIP_BUILD=1 re-ships the last jar.
set -euo pipefail
cd "$(dirname "$0")/.."

PI="${PI:-pi@raspberrypi.local}"
export JAVA_HOME="${JAVA_HOME:-/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home}"

if [ -z "${SKIP_BUILD:-}" ]; then
  npm run release
  lein with-profile +with-dev uberjar   # ships dev/ too, for the harvesters
fi

jar=$(ls -t target/uberjar/*-standalone.jar | head -1)
rsync -av "$jar" "$PI:/tmp/draft-day.jar"
rsync -av deploy/ "$PI:/tmp/draft-day-units/"

ssh "$PI" 'bash -s' <<'R'
set -euo pipefail
sudo install -o draftday -g draftday -m 644 /tmp/draft-day.jar /opt/draft-day/draft-day.jar
sudo install -m 644 /tmp/draft-day-units/* /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable draft-day.service harvest-trends.timer harvest-projections.timer
sudo systemctl restart draft-day.service
sudo systemctl start harvest-trends.timer harvest-projections.timer
systemctl list-timers 'harvest-*' --no-pager
R
echo "http://${PI#*@}:8080"
