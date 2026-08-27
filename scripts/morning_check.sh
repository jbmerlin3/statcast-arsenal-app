#!/bin/bash
# morning_check.sh
#
#   ~/statcast-arsenal-app/scripts/morning_check.sh          just look
#   ~/statcast-arsenal-app/scripts/morning_check.sh --fix    look, and fix if stale
#
# Answers one question before you send the link to somebody: is the live page
# showing yesterday's baseball? Everything else here is in service of that.
#
# --fix triggers the GitHub workflow and waits for it, so a morning where cron
# did not fire costs about ten minutes instead of a day. Written so nobody has
# to remember the gh incantation under time pressure.

REPO="/Users/jonathanmerlin/statcast-arsenal-app"
URL="https://jonathanmerlin.shinyapps.io/pitcher-arsenal/"
cd "$REPO" || exit 2

# Yesterday in Eastern, which is the freshest data that can exist: Savant posts
# a day's games overnight, so "current" means yesterday, never today.
WANT=$(TZ=America/New_York date -v-1d +%Y-%m-%d 2>/dev/null || date -d "yesterday" +%Y-%m-%d)

live_date() {
  curl -sL --max-time 90 "$URL" | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}'
}

echo "Checking $URL"
LIVE=$(live_date)

if [ -z "$LIVE" ]; then
  echo "  COULD NOT READ THE PAGE. It may be booting, or down. Try again in a minute."
  exit 2
fi

echo "  live page says:  $LIVE"
echo "  freshest possible: $WANT"

if [ "$LIVE" = "$WANT" ]; then
  echo
  echo "  CURRENT. Safe to send."
  exit 0
fi

echo
echo "  BEHIND by at least a day."
echo "  The link still works and states its own date, so it reads as a day-old"
echo "  dashboard, not a broken one. But it is not the freshest it could be."

# Who was supposed to do this, and did they?
echo
echo "  last GitHub runs:"
gh run list --workflow=refresh-and-deploy.yml --limit 3 \
  --json event,conclusion,createdAt \
  -q '.[] | "    event=" + .event + "  " + (.conclusion // "running") + "  " + .createdAt' 2>/dev/null \
  || echo "    (could not reach GitHub)"

if [ "$1" != "--fix" ]; then
  echo
  echo "  To fix it now, about 10 minutes:"
  echo "    $REPO/scripts/morning_check.sh --fix"
  exit 1
fi

echo
echo "  --fix given. Triggering the workflow..."
gh workflow run refresh-and-deploy.yml --ref main || { echo "  could not trigger"; exit 2; }
sleep 10
RID=$(gh run list --workflow=refresh-and-deploy.yml --limit 1 --json databaseId -q '.[0].databaseId')
echo "  run $RID started, waiting..."

for i in $(seq 1 60); do
  sleep 20
  ST=$(gh run view "$RID" --json status,conclusion -q '.status + " " + (.conclusion // "")' 2>/dev/null)
  case "$ST" in
    "completed success"*) echo "  run finished OK"; break ;;
    "completed"*)         echo "  RUN FAILED: $ST"; echo "  see: gh run view $RID --log-failed"; exit 1 ;;
  esac
done

echo "  re-checking the page..."
LIVE=$(live_date)
echo "  live page now says: $LIVE"
if [ "$LIVE" = "$WANT" ]; then
  echo
  echo "  CURRENT. Safe to send."
  exit 0
fi
echo
echo "  still $LIVE. Savant may not have posted $WANT yet, which is not something"
echo "  this repo can fix. The link works and is one day behind."
exit 1
