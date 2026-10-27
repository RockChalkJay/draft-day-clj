# Running Draft Day on a Raspberry Pi

The Pi does two jobs from one jar: it serves the app on your home network, and it
runs the harvesters (`tools.trends`, `tools.projections`) that save data the
providers do not keep. The harvesters are the point. A laptop that sleeps leaves
gaps in those records that cannot be refetched, so the Pi is a second, independent
source. The laptop's crontab stays as it is.

Everything is built on the laptop; the Pi needs only a JRE.

## One-time setup

Needs a 64-bit Raspberry Pi OS with ssh. A Pi 4 or 5 with 2 GB or more is
comfortable; with 1 GB the server plus an hourly harvest is tight.

```bash
ssh pi@raspberrypi.local 'TZ=America/Chicago LAN=192.168.1.0/24 bash -s' \
  < scripts/pi-setup.sh
```

`TZ` sets the Pi's time zone, which the timers use; match the laptop's. `LAN`
limits port 8080 to your network with `ufw`. Both are optional.

## Deploy (and redeploy)

```bash
scripts/deploy-pi.sh                 # PI=pi@raspberrypi.local by default
SEED_DATA=1 scripts/deploy-pi.sh     # the first time: also copy data/trends and data/projections
```

It builds the SPA and the uberjar, copies the jar and `deploy/*` units, and
restarts the server. `SKIP_BUILD=1` re-ships the last jar. Then open
`http://raspberrypi.local:8080`.

## What runs

| Unit | When |
| --- | --- |
| `draft-day.service` | always; the app on :8080 |
| `harvest-trends.timer` | every hour at :30 |
| `harvest-projections.timer` | Wed 08:05, Thu 16:35, Sun 10:20 |

The times sit a few minutes off the laptop's crontab on purpose: when both are up
you get two vintages, and when the laptop sleeps the Pi still has one. `Persistent=true`
runs a job missed while the Pi was off at the next boot.

```bash
systemctl list-timers 'harvest-*'
journalctl -u harvest-trends -u harvest-projections -n 50
sudo systemctl start harvest-trends        # run one now
```

## Bringing the snapshots home

```bash
scripts/pull-harvest.sh
lein run -m draft-day.tools.projections --report
lein run -m draft-day.tools.projections --report --dir data/trends
```

The pull copies only files the laptop lacks (names are UTC stamps), never
overwrites and never deletes.

## What it does not do

- **Sensei stays on the laptop.** Its MCP endpoint and chat answer loopback only,
  so the Pi has no `DRAFTDAY_SENSEI*` or `DRAFTDAY_ESPN_*` settings. League
  credentials live in each browser's Settings and travel in the request.
- **Draft state is per browser and per URL.** The laptop, the phone, and
  `raspberrypi.local` versus the Pi's IP each hold their own picks. Use one device
  to run an auction, and always the same address.
- **No auth, plain HTTP.** An ESPN cookie entered on a device crosses the LAN
  unencrypted. Acceptable on a trusted home network; `LAN` above limits who can
  reach it.
- **Cache files are written in place.** The server and a harvest share `data/`; a
  read that lands mid-write sees a partial file. The same exposure already exists
  with the laptop's cron.
