#!/usr/bin/env bash
# Build on the laptop, ship the jar and units to the Pi, restart the server.
#   scripts/deploy-pi.sh                  # PI defaults to pi@raspberrypi.local
#   PI=me@192.168.1.50 scripts/deploy-pi.sh
# SKIP_BUILD=1 re-ships the last jar. SEED_DATA=1 copies this machine's saved
# trends and projections across (once, so the Pi's reports see the full record).
set -euo pipefail
cd "$(dirname "$0")/.."

PI="${PI:-pi@raspberrypi.local}"
export JAVA_HOME="${JAVA_HOME:-/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home}"

if [ -z "${SKIP_BUILD:-}" ]; then
  npm run release
  lein uberjar
fi

jar=$(ls -t target/uberjar/*-standalone.jar | head -1)
rsync -av "$jar" "$PI:/tmp/draft-day.jar"
rsync -av deploy/ "$PI:/tmp/draft-day-units/"

if [ -n "${SEED_DATA:-}" ]; then
  rsync -a data/trends data/projections "$PI:/tmp/draft-day-seed/"
fi

ssh "$PI" 'bash -s' <<'R'
set -euo pipefail
sudo install -o draftday -g draftday -m 644 /tmp/draft-day.jar /opt/draft-day/draft-day.jar
sudo install -m 644 /tmp/draft-day-units/* /etc/systemd/system/
if [ -d /tmp/draft-day-seed ]; then
  sudo rsync -a --ignore-existing --chown=draftday:draftday /tmp/draft-day-seed/ /opt/draft-day/data/
  rm -rf /tmp/draft-day-seed
fi
sudo systemctl daemon-reload
sudo systemctl enable draft-day.service harvest-trends.timer harvest-projections.timer
sudo systemctl restart draft-day.service
sudo systemctl start harvest-trends.timer harvest-projections.timer
systemctl list-timers 'harvest-*' --no-pager
R
echo "http://${PI#*@}:8080"
