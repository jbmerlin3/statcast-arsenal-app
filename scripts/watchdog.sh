#!/bin/bash
# watchdog.sh
# Runs hourly from launchd. If the live page is stale, trigger the refresh workflow
# and notify; otherwise log and exit. Runs on the laptop, so it catches the external
# trigger failing. Unload it in the off-season.

REPO="$(cd "$(dirname "$0")/.." && pwd)"
URL="https://jonathanmerlin.shinyapps.io/pitcher-arsenal/"
LOG="$REPO/logs/watchdog.log"
cd "$REPO" || exit 2
mkdir -p logs

say() { echo "$(date '+%Y-%m-%d %H:%M:%S')  $*" >> "$LOG"; }

notify() {
  /usr/bin/osascript -e "display notification \"$2\" with title \"Pitcher Arsenal\" sound name \"Basso\"" 2>/dev/null
  say "NOTIFIED: $1 / $2"
}

# Yesterday in Eastern is the freshest data that can exist
WANT=$(TZ=America/New_York date -v-1d +%Y-%m-%d 2>/dev/null || date -d yesterday +%Y-%m-%d)
LIVE=$(curl -sL --max-time 90 "$URL" | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}')

if [ -z "$LIVE" ]; then
  # Unreachable is reported, not acted on
  say "UNREACHABLE (want $WANT)"
  notify "App unreachable" "The live page did not answer. It may be booting."
  exit 2
fi

if [ "$LIVE" = "$WANT" ]; then
  say "ok, live=$LIVE"
  exit 0
fi

say "stale: live=$LIVE want=$WANT"

# Don't trigger if a run is already in progress
INFLIGHT=$(gh run list --workflow=refresh-and-deploy.yml --limit 5 \
           --json status --jq '[.[]|select(.status!="completed")]|length' 2>/dev/null)
if [ "${INFLIGHT:-0}" -gt 0 ]; then
  say "a run is already in progress"
  exit 0
fi

say "triggering the workflow"
if gh workflow run refresh-and-deploy.yml --ref main >/dev/null 2>&1; then
  notify "Link was stale, fixing it" "Live page showed $LIVE, wanted $WANT. Refresh started, ~10 min."
  say "workflow triggered"
  exit 1
fi

notify "Stale, and the refresh could not be triggered" "Live page shows $LIVE. Run morning_check.sh --fix"
say "trigger failed"
exit 2
