# deploy.R
#
# Push the current data files to shinyapps.io.
#
#   Rscript scripts/deploy.R          run it by hand
#   deploy_app()                      what step 5 of the chain calls
#
# One manual step first, once per machine, and the one thing this script will
# not do: paste your token and secret from
# https://www.shinyapps.io/admin/#/tokens into
#
#   rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#
# ---- Why the data is bundled -------------------------------------------------
#
# The three rds files ship inside the bundle, about 25 MB, and the page prints
# "Data through <date>" read off app_data itself.
#
# The alternative, fetching the data at startup from a GitHub release asset, was
# rejected: it puts a 25 MB download in front of first paint on a portfolio link
# a stranger opens once, and it turns an honest staleness into an occasional
# blank error page. Decided 2026-08-21. Do not re-argue it, change it only if
# the reason changes.
#
# ---- Why the chain deploys now -----------------------------------------------
#
# This file used to say "a human runs this, the chain never does", and that is
# the bug. Because the data is bundled, refreshing data/*.rds does nothing for
# the deployed link until a redeploy happens. The chain ran green every morning
# and the live app sat wherever the last manual deploy left it. Found 2026-08-26:
# the chain had data through 08-25 and the live page read "Data through
# 2026-08-24", the visible symptom being a pitcher back from the IL whose IP
# was a start behind FanGraphs.
#
# Deciding not to bundle the data would have fixed it too, and that is the
# decision above, already made the other way. So the chain deploys.
#
# ---- Why appFiles is explicit ------------------------------------------------
#
# Two different things are at work here and they are easy to conflate.
#
# appFiles decides what is IN the bundle. scripts/ and tests/ are excluded, so
# the chain code that reaches out to Savant and StatsAPI never ships to a public
# server, and the bundle stays near 26 MB.
#
# renv.lock decides what the server INSTALLS. In an renv project rsconnect reads
# the lockfile rather than scanning the bundled files, so a narrower appFiles
# does not narrow the package list: measured 2026-08-21, the manifest is 83
# packages either way. That is first-deploy build time and nothing at runtime,
# since only loaded packages take memory. rsconnect itself is held out of the
# lock through renv settings, so the deploy tool is not installed on the thing
# it deploys to.

APP_NAME <- "pitcher-arsenal"

# Local record of what the last successful deploy carried. In logs/ rather than
# data/ so it cannot end up in the bundle, and so a stale one is obvious next to
# chain.log. Written only after deployApp() returns, so a failed deploy leaves
# the previous stamp and the next run tries again.
DEPLOY_STAMP <- "logs/deployed_through.txt"


#' The data dates a deploy would carry
#'
#' Both files, not just app_data. game_logs is what IP, ERA, WHIP and FIP come
#' from, and it can advance on a day app_data does not: a pitcher's line posts
#' to StatsAPI on a schedule of its own. Gating on app_data alone would have
#' skipped exactly the redeploy that fixes a wrong IP.
data_dates <- function() {
  gl <- tryCatch(max(readRDS("data/game_logs.rds")$game_date),
                 error = function(e) NA_character_)
  c(app_data = max(readRDS("data/app_data.rds")$game_date), game_logs = gl)
}


#' What the last successful deploy carried, or NULL
deployed_dates <- function(path = DEPLOY_STAMP) {
  if (!file.exists(path)) return(NULL)
  kv <- tryCatch(read.dcf(path)[1, ], error = function(e) NULL)
  if (is.null(kv) || !all(c("app_data", "game_logs") %in% names(kv))) return(NULL)
  kv[c("app_data", "game_logs")]
}


#' Bundle the app and its data and push it
#'
#' Returns TRUE if it deployed, FALSE if it skipped because the live bundle
#' already carries these dates. Errors if the deploy itself fails: the caller
#' decides what a failed deploy means, and in the chain it means the run is not
#' OK, because a stale live app is the one kind of staleness the page cannot
#' show you.
deploy_app <- function(force = FALSE) {

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect is not installed. renv::install(\"rsconnect\")", call. = FALSE)
  }

  # app_data and league_ref are the two the app cannot start without. game_logs
  # is the one it tolerates missing, so it is not required here either: a deploy
  # without it degrades the results panel rather than failing to boot.
  required <- c("data/app_data.rds", "data/league_ref.rds")
  missing  <- required[!file.exists(required)]
  if (length(missing)) {
    stop("Missing ", paste(missing, collapse = ", "),
         ". Build them with: Rscript scripts/update_data.R", call. = FALSE)
  }

  have <- data_dates()
  live <- deployed_dates()

  # Data dates only. This cannot see a change to app.R or R/, which is why the
  # hand-run path below passes force = TRUE rather than relying on it.
  if (!force && !is.null(live) && identical(unname(live), unname(have))) {
    message("Live bundle already carries app_data ", have[["app_data"]],
            " and game_logs ", have[["game_logs"]], ", no new data to deploy")
    return(invisible(FALSE))
  }

  data_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
  fg_files   <- list.files("fg_stuff", pattern = "\\.csv$", full.names = TRUE)
  if (!length(fg_files)) {
    warning("No FanGraphs export in fg_stuff/, the Stuff+ column will be blank.",
            call. = FALSE)
  }

  app_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
                 data_files, fg_files)

  mb <- sum(file.size(app_files)) / 1024^2
  message(sprintf("Bundling %d files, %.1f MB", length(app_files), mb))
  message("  app_data through ", have[["app_data"]],
          ", game_logs through ", have[["game_logs"]])
  message("  live bundle carries ",
          if (is.null(live)) "unknown, no stamp"
          else paste0("app_data ", live[["app_data"]], ", game_logs ", live[["game_logs"]]))

  rsconnect::deployApp(
    appDir      = ".",
    appName     = APP_NAME,
    appTitle    = "Pitcher Arsenal",
    appFiles    = app_files,
    # The app reads rds files and renders. Nothing in it needs to write anywhere,
    # so a redeploy is always a full replace and never a merge.
    forceUpdate = TRUE,
    launch.browser = FALSE
  )

  # Only after deployApp() returns. An interrupted deploy must not leave a stamp
  # claiming the live app is current, because nothing else on the page would
  # contradict it.
  dir.create("logs", showWarnings = FALSE)
  write.dcf(data.frame(app_data  = have[["app_data"]],
                       game_logs = have[["game_logs"]],
                       deployed  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                       stringsAsFactors = FALSE), DEPLOY_STAMP)
  message("Deployed, stamp written to ", DEPLOY_STAMP)
  invisible(TRUE)
}


# Same gate as update_data.R, for the same reason: sourcing this file from the
# chain must load the function without deploying, and must SAY so rather than
# doing it silently.
#
# force = TRUE on the hand-run path, and only there. The stamp check exists to
# stop the 06:15 job redeploying 27 MB on a morning when no game was played; it
# is a guard against a pointless AUTOMATIC deploy. A person typing this command
# has a reason, and the usual reason is a code change, which moves no data date
# and so would be refused with "nothing to deploy" by a gate that cannot see it.
# Caught while shipping the plot sizing fix, which is exactly that case.
if (sys.nframe() == 0L) {
  deploy_app(force = TRUE)
} else {
  message("deploy.R sourced: deploy_app() loaded, NOTHING WAS DEPLOYED.")
}
