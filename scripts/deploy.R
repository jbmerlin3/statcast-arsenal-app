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
#' `content` closes a hole the dates and the commit both miss. Savant BACKFILLS:
#' arm_angle is computed by a pose pipeline that lands days after the game, and a
#' re-pull that collects it changes tens of thousands of VALUES while changing no
#' date and no commit. Observed 2026-09-08: a repull_days=200 run refreshed
#' 40,000+ arm angles into the store, the gate compared 2026-09-07 to 2026-09-07
#' and b7379e8 to b7379e8, and skipped. The corrected data sat in the release
#' asset and never reached the live app.
#'
#' Same species as the 2026-08-27 incident this gate was widened for, one level
#' down: that was a code change invisible to a data-only stamp, this is a data
#' change invisible to a date-only stamp.
#'
#' A digest rather than a row count, because a backfill does not change row
#' count either. Hashing the frame is cheap next to the bundle upload it guards.
data_dates <- function() {
  gl <- tryCatch(max(readRDS("data/game_logs.rds")$game_date),
                 error = function(e) NA_character_)
  ad <- readRDS("data/app_data.rds")
  c(app_data = max(ad$game_date),
    game_logs = gl,
    code      = code_version(),
    content   = substr(digest_or_na(ad), 1, 12))
}


#' A digest of the data a deploy would carry, or "unknown"
#'
#' "unknown" compares unequal to anything and therefore deploys, which is the
#' right direction to fail: a redundant deploy costs one instance wake-up, a
#' skipped one ships nothing. digest is already in the lockfile via other
#' dependencies, but this degrades rather than erroring if it is ever absent.
digest_or_na <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) return("unknown")
  tryCatch(digest::digest(canon_frame(x), algo = "xxhash64"),
           error = function(e) "unknown")
}


#' A frame in canonical form, so equal data hashes equal
#'
#' The digest must answer "did any VALUE change", and it was answering "did the
#' file change", which is a different and much noisier question. Two things make
#' an unchanged frame hash differently.
#'
#' ROW ORDER. update_data.R folds with `bind_rows(new_clean, sc)`, new rows
#' first, and that ordering is load-bearing: it is the whole mechanism by which
#' a re-pulled revision beats the stored copy under distinct(). The side effect
#' is that every row inside the re-pull window is lifted to the front in
#' whatever order Savant returned it. When that response order shifts, tens of
#' thousands of rows move and nothing about the data changes.
#'
#' Measured 2026-09-09 on the run artifacts for 10:15 and 15:05, both carrying
#' app_data through 2026-09-08 at commit a518008. The frames are 641,065 x 33
#' and identical up to row order: sorted, zero cells differ in any column;
#' unsorted, 33 of 33 columns differ, peaking at 88,954 cells, about one 21-day
#' re-pull window of pitches. The gate read that as new data and redeployed
#' twice for a byte-identical dataset. That is the same cost the 2026-09-08
#' stamp bug caused, reintroduced by the fix for it.
#'
#' ROW NAMES. `[` carries the original positions through as a row.names
#' attribute and digest() hashes attributes, so sorting alone still produces two
#' different hashes for the same rows. Normalising to compact 1:n is what makes
#' the two runs above agree on 4c02f7dfd270.
#'
#' Ordered by every column rather than by a key, because app_data.rds has none:
#' game_pk, at_bat_number and pitch_number live in the store and are not in
#' APP_DATA_COLS. Costs 2.2s on 641k rows against a 39 MB bundle upload, so it
#' is free next to the deploy it guards.
#'
#' Not a fix for the row-order churn itself, which is still there in the store
#' and still invisible to everything else. This only stops the gate reacting to
#' it. Sorting at the source in update_data.R would fix both and would also
#' stabilise the rds bytes, but it touches the fold whose ordering the revision
#' path depends on, so it was kept out of this change deliberately.
canon_frame <- function(d) {
  if (!is.data.frame(d) || !nrow(d) || !ncol(d)) return(d)
  d <- d[do.call(order, unname(as.list(d))), , drop = FALSE]
  attr(d, "row.names") <- .set_row_names(nrow(d))
  d
}


#' The commit a deploy would carry
#'
#' The stamp used to record data dates ONLY, and that was a hole big enough to
#' drive a day through. A code change moves no data date, so the gate saw
#' "already current" and refused. Every fix shipped on 2026-08-27 built green in
#' CI and never reached the live app; the same bug kept being reported because
#' the running app was still the old one, and the deploy step said so plainly in
#' the log while nobody read it.
#'
#' Returns "unknown" outside a git checkout, which compares unequal to anything
#' and therefore deploys. Failing toward deploying is the right direction: a
#' redundant deploy costs one instance wake-up, a skipped one ships nothing.
code_version <- function() {
  sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL)
  if (is.null(sha) || !length(sha) || !nzchar(sha[1])) "unknown" else substr(sha[1], 1, 12)
}


#' What the last successful deploy carried, or NULL
deployed_dates <- function(path = DEPLOY_STAMP) {
  if (!file.exists(path)) return(NULL)
  kv <- tryCatch(read.dcf(path)[1, ], error = function(e) NULL)
  # A stamp written before `code` existed is unusable rather than a match, so
  # the first run after this change deploys instead of trusting a record that
  # could not have known what code was live.
  # A stamp written before `content` existed is unusable rather than a match, so
  # the first run after this change deploys instead of trusting a record that
  # could not have known what data was live.
  if (is.null(kv) || !all(c("app_data", "game_logs", "code", "content") %in% names(kv)))
    return(NULL)
  kv[c("app_data", "game_logs", "code", "content")]
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
  # Static season lookups, currently listed pitcher heights for the Context
  # tab's release model. Tracked in git and refreshed by hand, unlike data/,
  # so this ships whatever is committed. Without it load_pitcher_heights()
  # warns and the Context tab loses its expected-release column while every
  # other tab is unaffected: a degraded tab, not a failed boot.
  lookup_files <- list.files("lookups", pattern = "\\.csv$", full.names = TRUE)
  if (!length(lookup_files)) {
    warning("No lookups/*.csv, the Context tab will have no listed heights. ",
            "Build with: Rscript scripts/build_pitcher_heights.R", call. = FALSE)
  }
  if (!length(fg_files)) {
    warning("No FanGraphs export in fg_stuff/, the Stuff+ column will be blank.",
            call. = FALSE)
  }

  app_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
                 data_files, fg_files, lookup_files)

  mb <- sum(file.size(app_files)) / 1024^2
  message(sprintf("Bundling %d files, %.1f MB", length(app_files), mb))
  # `content` is printed too. It was the one field the gate compared and never
  # showed, which is why an order-sensitive digest redeployed for months of
  # unchanged data without leaving a trace in any log. A gate that does not say
  # which field disagreed cannot be debugged from a run log.
  message("  app_data through ", have[["app_data"]],
          ", game_logs through ", have[["game_logs"]],
          ", code ", have[["code"]],
          ", content ", have[["content"]])
  message("  live bundle carries ",
          if (is.null(live)) "unknown, no usable stamp"
          else paste0("app_data ", live[["app_data"]], ", game_logs ",
                      live[["game_logs"]], ", code ", live[["code"]],
                      ", content ", live[["content"]]))

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
  # Built FROM `have` by name rather than field by field. Listing the fields
  # here by hand is what broke the gate on 2026-09-08: data_dates() gained a
  # `content` digest and deployed_dates() started requiring it, but this write
  # still emitted four fields. Every stamp was therefore unusable, the gate
  # matched nothing, and all four scheduled runs a day deployed unconditionally
  # -- the exact cost the schedule comments in the workflow are written around.
  # The run reported success and the stamp looked plausible, which is why it
  # took reading the stamp for a missing field to catch it.
  #
  # as.list() so the frame has one column per element of `have` whatever
  # data_dates() returns, and a field added there can never again be silently
  # dropped here.
  stamp <- c(as.list(have), deployed = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  write.dcf(as.data.frame(stamp, stringsAsFactors = FALSE), DEPLOY_STAMP)

  # Read the stamp back and assert the gate would ACCEPT it. Structural
  # construction above prevents dropping a field, but only this proves the two
  # halves agree: deployed_dates() returning NULL is indistinguishable from "no
  # stamp yet", so a mismatch does not error anywhere, it just silently disables
  # the gate and deploys on every run forever. Cheap, and it fails here where
  # the cause is obvious rather than on a budget alert weeks later.
  back <- deployed_dates()
  if (is.null(back) || !identical(unname(back), unname(have))) {
    warning("Deploy stamp does not round-trip: the gate is disabled and every ",
            "run will redeploy. Written fields: ",
            paste(names(stamp), collapse = ", "),
            ". data_dates() produced: ", paste(names(have), collapse = ", "),
            call. = FALSE)
  }
  message("Deployed, stamp written to ", DEPLOY_STAMP)
  invisible(TRUE)
}


# Same gate as update_data.R, for the same reason: sourcing this file from the
# chain must load the function without deploying, and must SAY so rather than
# doing it silently.
#
# force = TRUE on the hand-run path, kept as a belt to the gate's braces.
#
# It used to be load-bearing, because the gate compared data dates only and a
# code change moves none of them. That patched the symptom on the ONE path a
# human types, and left CI silently refusing every code fix: on 2026-08-27 four
# separate fixes built green and never reached the live app. The gate now reads
# the commit too, so this force is redundant rather than essential, which is the
# correct relationship between a safety net and the thing it catches.
if (sys.nframe() == 0L) {
  deploy_app(force = TRUE)
} else {
  message("deploy.R sourced: deploy_app() loaded, NOTHING WAS DEPLOYED.")
}
