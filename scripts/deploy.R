# deploy.R
# Deploy the app and its data to shinyapps.io (step 6 of the chain)
#   Rscript scripts/deploy.R   deploy by hand (always deploys)
# Needs rsconnect::setAccountInfo() once per machine (scripts/ci_auth.R in CI).
# The data ships inside the bundle; scripts/ and tests/ don't.

APP_NAME <- "pitcher-arsenal"

# What the last successful deploy carried
DEPLOY_STAMP <- "logs/deployed_through.txt"


#' What a deploy would carry: data dates, commit, and a digest of app_data's
#' values (Savant backfills change values without changing dates).
data_dates <- function() {
  gl <- tryCatch(max(readRDS("data/game_logs.rds")$game_date),
                 error = function(e) NA_character_)
  ad <- readRDS("data/app_data.rds")
  c(app_data = max(ad$game_date),
    game_logs = gl,
    code      = code_version(),
    content   = substr(digest_or_na(ad), 1, 12))
}


#' Digest of a frame, or "unknown" (never matches, so it deploys)
digest_or_na <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) return("unknown")
  tryCatch(digest::digest(canon_frame(x), algo = "xxhash64"),
           error = function(e) "unknown")
}


#' Canonical row order and row names: identical data, identical hash
canon_frame <- function(d) {
  if (!is.data.frame(d) || !nrow(d) || !ncol(d)) return(d)
  d <- d[do.call(order, unname(as.list(d))), , drop = FALSE]
  attr(d, "row.names") <- .set_row_names(nrow(d))
  d
}


#' Current commit, or "unknown" outside a git checkout
code_version <- function() {
  sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL)
  if (is.null(sha) || !length(sha) || !nzchar(sha[1])) "unknown" else substr(sha[1], 1, 12)
}


#' The last deploy's stamp, or NULL (deploy)
deployed_dates <- function(path = DEPLOY_STAMP) {
  if (!file.exists(path)) return(NULL)
  kv <- tryCatch(read.dcf(path)[1, ], error = function(e) NULL)
  # A stamp missing any field is treated as no stamp
  if (is.null(kv) || !all(c("app_data", "game_logs", "code", "content") %in% names(kv)))
    return(NULL)
  kv[c("app_data", "game_logs", "code", "content")]
}


#' Deploy unless the live bundle already carries the same data, code and content.
#' TRUE if deployed, FALSE if skipped; errors if the deploy fails.
deploy_app <- function(force = FALSE) {

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect is not installed. renv::install(\"rsconnect\")", call. = FALSE)
  }

  # game_logs is optional; the app runs without it
  required <- c("data/app_data.rds", "data/league_ref.rds")
  missing  <- required[!file.exists(required)]
  if (length(missing)) {
    stop("Missing ", paste(missing, collapse = ", "),
         ". Build them with: Rscript scripts/update_data.R", call. = FALSE)
  }

  have <- data_dates()
  live <- deployed_dates()

  if (!force && !is.null(live) && identical(unname(live), unname(have))) {
    message("Live bundle already carries app_data ", have[["app_data"]],
            " and game_logs ", have[["game_logs"]], ", nothing changed since the last deploy")
    return(invisible(FALSE))
  }

  data_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
  # Tracked lookups ship with the data
  lookup_files <- list.files("lookups", pattern = "\\.csv$", full.names = TRUE)
  if (!length(lookup_files)) {
    warning("No lookups/*.csv, the Context tab will have no listed heights. ",
            "Build with: Rscript scripts/build_pitcher_heights.R", call. = FALSE)
  }
  if (!file.exists("lookups/stuff_v2_2026.csv")) {
    warning("No lookups/stuff_v2_2026.csv, the Stuff+ column will be blank.",
            call. = FALSE)
  }

  app_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
                 data_files, lookup_files)

  mb <- sum(file.size(app_files)) / 1024^2
  message(sprintf("Bundling %d files, %.1f MB", length(app_files), mb))
  # Log every gate field
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
    forceUpdate = TRUE,
    launch.browser = FALSE
  )

  # Stamp only after deployApp() returns, built from `have`
  dir.create("logs", showWarnings = FALSE)
  stamp <- c(as.list(have), deployed = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  write.dcf(as.data.frame(stamp, stringsAsFactors = FALSE), DEPLOY_STAMP)

  # Read the stamp back; an unreadable stamp would redeploy every run
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


# Deploy only when run as a script, never when sourced by the chain
if (sys.nframe() == 0L) {
  deploy_app(force = TRUE)
} else {
  message("deploy.R sourced: call deploy_app() to deploy.")
}
