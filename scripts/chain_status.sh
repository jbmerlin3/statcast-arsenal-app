#!/bin/bash
# chain_status.sh
#
#   ~/statcast-arsenal-app/scripts/chain_status.sh
#
# Answers one question: did last night's chain run, and did it work?
#
# "No RUN line since yesterday" is a distinct answer from "RUN FAILED", and both
# are distinct from "RUN OK". A job that never fired because the laptop was off
# leaves an old RUN OK behind, which reads as success unless the age is checked.

REPO="/Users/jonathanmerlin/statcast-arsenal-app"
LOG="$REPO/logs/chain.log"

[ -f "$LOG" ] && [ -s "$LOG" ] || { echo "NO LOG at $LOG. The job has never produced a run."; exit 2; }

last=$(grep -E '^RUN (OK|FAILED)' "$LOG" | tail -1)
[ -n "$last" ] || { echo "Log exists but holds no finished run. A run may be in progress, or dying before it can report."; exit 2; }

echo "$last"

# Age, from the log's own timestamp rather than the file mtime, so a rotation
# or an editor touch cannot make a stale run look fresh.
ts=$(echo "$last" | grep -oE '[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}')
if [ -n "$ts" ]; then
  age_h=$(( ( $(date +%s) - $(date -j -f '%Y-%m-%d %H:%M:%S' "$ts" +%s 2>/dev/null || echo 0) ) / 3600 ))
  echo "  that run finished ${age_h}h ago"
  [ "$age_h" -gt 30 ] && echo "  STALE: more than 30h old, so last night's run did not happen."
fi

# The data's own dates, local to this machine.
LOCAL_AD=$("$(command -v Rscript)" -e '
  ad <- readRDS("'"$REPO"'/data/app_data.rds")
  cat(max(ad$game_date))
' 2>/dev/null)
LOCAL_GL=$("$(command -v Rscript)" -e '
  gl <- tryCatch(readRDS("'"$REPO"'/data/game_logs.rds"), error = function(e) NULL)
  if (!is.null(gl)) cat(max(gl$game_date))
' 2>/dev/null)
echo "  app_data through ${LOCAL_AD:-unknown}"
echo "  game_logs through ${LOCAL_GL:-unknown}"

problems=0

# What a VISITOR sees. This replaced a local stamp file, which only recorded
# deploys made from this laptop and therefore went quietly wrong the moment
# GitHub Actions became the thing that deploys. The app prints its own data date
# into the page's static UI, so the initial HTML carries it and one request
# answers the only question that matters: is the live page current.
#
# A cold free-tier instance takes a while to boot, hence the long timeout.
# "Could not reach" is reported as its own state and never collapsed into
# "stale": a link that is DOWN is a different emergency from one that is behind.
LIVE=$(curl -sL --max-time 90 https://jonathanmerlin.shinyapps.io/pitcher-arsenal/ \
       | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}')
if [ -z "$LIVE" ]; then
  echo "  LIVE PAGE: could not read a date from it. It may be booting, or down."
  problems=$((problems + 1))
else
  echo "  LIVE PAGE serves data through $LIVE"
  if [ -n "$LOCAL_AD" ] && [ "$LIVE" != "$LOCAL_AD" ]; then
    echo "  STALE LIVE APP: this machine holds $LOCAL_AD, visitors get $LIVE"
    problems=$((problems + 1))
  fi
fi

# The scheduled refresh now lives in GitHub Actions, so its health belongs here
# too. Fails soft: gh may be absent or logged out, and that is not a statement
# about the app.
if command -v gh >/dev/null 2>&1 && gh auth status >/dev/null 2>&1; then
  RUN=$(cd "$REPO" && gh run list --workflow=refresh-and-deploy.yml --limit 1 \
        --json conclusion,createdAt,event \
        -q '.[0] | (.conclusion // "running") + "  " + .createdAt + "  " + .event' 2>/dev/null)
  if [ -n "$RUN" ]; then
    echo "  GitHub Actions, last run: $RUN"
    case "$RUN" in
      failure*|cancelled*|timed_out*)
        echo "  ACTIONS FAILING: the morning refresh is not happening on its own."
        problems=$((problems + 1)) ;;
    esac
  else
    echo "  GitHub Actions: no runs found for refresh-and-deploy.yml"
  fi
else
  echo "  GitHub Actions: not checked, gh is missing or logged out"
fi

echo "$last" | grep -q FAILED && { echo "  --- last 15 log lines ---"; tail -15 "$LOG"; exit 1; }
[ "$problems" -gt 0 ] && exit 1
exit 0
