#!/bin/bash
# watchdog.sh
#
# Runs hourly from launchd. Asks one question: is the live page stale? If it is,
# fixes it and says so. If it is not, says nothing at all.
#
# WHY THIS EXISTS, and why it does not run on GitHub Actions.
#
# 2026-08-27: GitHub's scheduler missed three consecutive attempts, 06:15, 07:15
# and 08:15 Eastern, with zero event=schedule runs. The app froze on the previous
# day's data and nothing said so. A link that had been sent to an employer would
# have rotted silently, and the first person to notice would have been the
# recipient.
#
# A monitor that runs on the same scheduler as the thing it monitors cannot
# catch that scheduler failing. So this lives on the laptop, where launchd has a
# track record of actually firing, and it watches the LIVE PAGE rather than any
# internal state. The page is what a visitor sees; everything else is a proxy.
#
# It self-heals rather than only complaining. A notification that requires a
# human to act is still broken for the hours that human is asleep or busy.
# Triggering the workflow is idempotent: the deploy stamp means a run that finds
# nothing new deploys nothing.
#
# It does NOT close the laptop-shut case. Nothing local can. That needs the
# external trigger.

REPO="/Users/jonathanmerlin/statcast-arsenal-app"
URL="https://jonathanmerlin.shinyapps.io/pitcher-arsenal/"
LOG="$REPO/logs/watchdog.log"
cd "$REPO" || exit 2
mkdir -p logs

say() { echo "$(date '+%Y-%m-%d %H:%M:%S')  $*" >> "$LOG"; }

notify() {
  # osascript reaches Notification Center because this agent runs in the GUI
  # session. Fails harmlessly over ssh or a locked-out session.
  /usr/bin/osascript -e "display notification \"$2\" with title \"Pitcher Arsenal\" sound name \"Basso\"" 2>/dev/null
  say "NOTIFIED: $1 / $2"
}

# Yesterday in Eastern is the freshest data that can exist: Savant posts a day's
# games overnight.
WANT=$(TZ=America/New_York date -v-1d +%Y-%m-%d 2>/dev/null || date -d yesterday +%Y-%m-%d)
LIVE=$(curl -sL --max-time 90 "$URL" | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}')

if [ -z "$LIVE" ]; then
  # Unreachable is not the same as stale, and it is the worse one. Reported, but
  # not acted on: triggering a data refresh does not fix a down host, and a
  # booting free-tier instance looks identical to a dead one for ~30 seconds.
  say "UNREACHABLE (want $WANT)"
  notify "App unreachable" "The live page did not answer. It may be booting."
  exit 2
fi

if [ "$LIVE" = "$WANT" ]; then
  say "ok, live=$LIVE"
  exit 0
fi

say "STALE: live=$LIVE want=$WANT"

# Do not pile on. If a run is already going, it is probably about to fix this.
INFLIGHT=$(gh run list --workflow=refresh-and-deploy.yml --limit 5 \
           --json status --jq '[.[]|select(.status!="completed")]|length' 2>/dev/null)
if [ "${INFLIGHT:-0}" -gt 0 ]; then
  say "a run is already in flight, standing down"
  exit 0
fi

say "triggering the workflow"
if gh workflow run refresh-and-deploy.yml --ref main >/dev/null 2>&1; then
  notify "Link was stale, fixing it" "Live page showed $LIVE, wanted $WANT. Refresh started, ~10 min."
  say "workflow triggered"
  exit 1
fi

# Could not even trigger. This is the one a human has to see.
notify "STALE and could not self-fix" "Live page shows $LIVE. Run morning_check.sh --fix"
say "TRIGGER FAILED"
exit 2
