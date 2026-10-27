#!/bin/bash
# morning_check.sh
# Is the live page showing yesterday's games? With --fix, trigger the workflow and wait.
#   scripts/morning_check.sh [--fix]

REPO="$(cd "$(dirname "$0")/.." && pwd)"
URL="https://jonathanmerlin.shinyapps.io/pitcher-arsenal/"
cd "$REPO" || exit 2

# Yesterday in Eastern is the freshest data that can exist
WANT=$(TZ=America/New_York date -v-1d +%Y-%m-%d 2>/dev/null || date -d "yesterday" +%Y-%m-%d)

live_date() {
  curl -sL --max-time 90 "$URL" | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}'
}

echo "Checking $URL"
LIVE=$(live_date)

if [ -z "$LIVE" ]; then
  echo "  Could not read the page. It may be booting, or down. Try again in a minute."
  exit 2
fi

echo "  live page says:  $LIVE"
echo "  freshest possible: $WANT"

if [ "$LIVE" = "$WANT" ]; then
  echo
  echo "  Current."
  exit 0
fi

echo
echo "  Behind by at least a day."
echo "  The link still works and states its own date, so it reads as a day-old"
echo "  dashboard, not a broken one. But it is not the freshest it could be."

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
    "completed"*)         echo "  Run failed: $ST"; echo "  see: gh run view $RID --log-failed"; exit 1 ;;
  esac
done

echo "  re-checking the page..."
LIVE=$(live_date)
echo "  live page now says: $LIVE"
if [ "$LIVE" = "$WANT" ]; then
  echo
  echo "  Current."
  exit 0
fi
echo
echo "  still $LIVE. Savant may not have posted $WANT yet, which is not something"
echo "  this repo can fix. The link works and is one day behind."
exit 1
