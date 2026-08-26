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

# The data's own dates, and what the last deploy actually shipped. The second
# one is the point: every local check here can read current while the deployed
# link serves an older bundle, because the data rides inside the bundle. That
# gap is what made a stale live app survive weeks of green runs.
"$(command -v Rscript)" -e '
  ad <- readRDS("'"$REPO"'/data/app_data.rds")
  gl <- tryCatch(readRDS("'"$REPO"'/data/game_logs.rds"), error = function(e) NULL)
  cat("  app_data through", max(ad$game_date), "\n")
  if (!is.null(gl)) cat("  game_logs through", max(gl$game_date), "\n")
  stamp <- "'"$REPO"'/logs/deployed_through.txt"
  if (!file.exists(stamp)) {
    cat("  DEPLOYED: no stamp, the live app has never been deployed by the chain\n")
  } else {
    kv <- read.dcf(stamp)[1, ]
    cat("  deployed  app_data", kv[["app_data"]], "on", kv[["deployed"]], "\n")
    if (!identical(unname(kv[["app_data"]]), max(ad$game_date)))
      cat("  STALE LIVE APP: local data is through", max(ad$game_date),
          "but the deployed bundle carries", kv[["app_data"]], "\n")
  }
' 2>/dev/null

echo "$last" | grep -q FAILED && { echo "  --- last 15 log lines ---"; tail -15 "$LOG"; exit 1; }
exit 0
