#!/usr/bin/env bash
# Merge the Pi's saved snapshots into this machine's data/. File names are UTC
# stamps, so nothing collides; --ignore-existing never overwrites, and there is
# no --delete, so nothing here is removed.
set -euo pipefail
cd "$(dirname "$0")/.."

PI="${PI:-pi@raspberrypi.local}"
for d in trends projections; do
  rsync -av --ignore-existing "$PI:/opt/draft-day/data/$d/" "data/$d/"
done
