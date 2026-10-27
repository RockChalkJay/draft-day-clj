#!/usr/bin/env bash
# One-time Pi bootstrap, run on the Pi as a sudo-capable user:
#   ssh pi@raspberrypi.local 'bash -s' < scripts/pi-setup.sh
# TZ=America/Chicago to also set the time zone, so the timers' local times match
# the laptop's crontab. LAN=192.168.1.0/24 to limit port 8080 to that network.
set -euo pipefail

sudo apt-get update
sudo apt-get install -y default-jre-headless rsync

id draftday >/dev/null 2>&1 || sudo useradd --system --home /opt/draft-day --shell /usr/sbin/nologin draftday
sudo install -d -o draftday -g draftday /opt/draft-day /opt/draft-day/data

if [ -n "${TZ:-}" ]; then sudo timedatectl set-timezone "$TZ"; fi

if [ -n "${LAN:-}" ]; then
  sudo apt-get install -y ufw
  sudo ufw allow ssh
  sudo ufw allow from "$LAN" to any port 8080 proto tcp
  sudo ufw --force enable
fi

java -version
echo "ready: run scripts/deploy-pi.sh from the laptop"
