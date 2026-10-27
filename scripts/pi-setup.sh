#!/usr/bin/env bash
# One-time Pi bootstrap, run on the Pi as a sudo-capable user:
#   ssh pi@raspberrypi.local 'bash -s' < scripts/pi-setup.sh
# Put TZ=America/Chicago in the remote command ('TZ=... bash -s') to also set the
# time zone, so the timers' local times match the laptop's crontab, and
# LAN=192.168.1.0/24 to limit port 8080 to that network. ssh does not carry
# variables set in front of it.
set -euo pipefail

# The server needs Java 21 or later: http-kit starts virtual threads, which 17
# lacks. Use the OS's own package when it has one, else Adoptium's Temurin.
sudo apt-get update
sudo apt-get install -y rsync
if apt-cache policy openjdk-21-jre-headless | grep -q 'Candidate: [0-9]'; then
  sudo apt-get install -y openjdk-21-jre-headless
else
  sudo apt-get install -y wget gpg apt-transport-https
  wget -qO - https://packages.adoptium.net/artifactory/api/gpg/key/public \
    | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/adoptium.gpg
  echo "deb https://packages.adoptium.net/artifactory/deb $(. /etc/os-release; echo "$VERSION_CODENAME") main" \
    | sudo tee /etc/apt/sources.list.d/adoptium.list
  sudo apt-get update
  sudo apt-get install -y temurin-21-jre
fi
# The units run /usr/bin/java; point it at 21 even with 17 still installed.
sudo update-alternatives --set java "$(ls -d /usr/lib/jvm/*21*/bin/java | head -1)"

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
