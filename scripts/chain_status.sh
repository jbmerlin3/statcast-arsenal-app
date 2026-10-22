#!/bin/bash
# chain_status.sh
# Status of the local launchd chain run, the live page's data date, and the last
# GitHub Actions run. Missing, failed and stale runs are reported separately.

REPO="$(cd "$(dirname "$0")/.." && pwd)"
LOG="$REPO/logs/chain.log"

[ -f "$LOG" ] && [ -s "$LOG" ] || { echo "No log at $LOG. The job has never produced a run."; exit 2; }

last=$(grep -E '^RUN (OK|FAILED)' "$LOG" | tail -1)
[ -n "$last" ] || { echo "Log exists but holds no finished run. A run may be in progress, or dying before it can report."; exit 2; }

echo "$last"

# Age from the log's own timestamp, not the file mtime
ts=$(echo "$last" | grep -oE '[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}')
if [ -n "$ts" ]; then
  age_h=$(( ( $(date +%s) - $(date -j -f '%Y-%m-%d %H:%M:%S' "$ts" +%s 2>/dev/null || echo 0) ) / 3600 ))
  echo "  that run finished ${age_h}h ago"
  [ "$age_h" -gt 30 ] && echo "  Stale: more than 30h old, so last night's run did not happen."
fi

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

# The live page prints its data date in static HTML
LIVE=$(curl -sL --max-time 90 https://jonathanmerlin.shinyapps.io/pitcher-arsenal/ \
       | grep -oE "Data through [0-9]{4}-[0-9]{2}-[0-9]{2}" | head -1 | awk '{print $3}')
if [ -z "$LIVE" ]; then
  echo "  Live page: could not read a date from it. It may be booting, or down."
  problems=$((problems + 1))
else
  echo "  Live page serves data through $LIVE"
  if [ -n "$LOCAL_AD" ] && [ "$LIVE" != "$LOCAL_AD" ]; then
    echo "  Live app is stale: this machine holds $LOCAL_AD, visitors get $LIVE"
    problems=$((problems + 1))
  fi
fi

# Optional: skipped if gh is missing or logged out
if command -v gh >/dev/null 2>&1 && gh auth status >/dev/null 2>&1; then
  RUN=$(cd "$REPO" && gh run list --workflow=refresh-and-deploy.yml --limit 1 \
        --json conclusion,createdAt,event \
        -q '.[0] | (.conclusion // "running") + "  " + .createdAt + "  " + .event' 2>/dev/null)
  if [ -n "$RUN" ]; then
    echo "  GitHub Actions, last run: $RUN"
    case "$RUN" in
      failure*|cancelled*|timed_out*)
        echo "  GitHub Actions is failing: the morning refresh is not running."
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
