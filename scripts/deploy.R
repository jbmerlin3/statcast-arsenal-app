# deploy.R
#
# Deploy to shinyapps.io. A human runs this, the chain never does.
#
#   Rscript scripts/deploy.R
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
# "Data through <date>" read off app_data itself. Redeploy when you want it
# current.
#
# The alternative, fetching the data at startup from a GitHub release asset, was
# rejected: it puts a 25 MB download in front of first paint on a portfolio link
# a stranger opens once, and it turns an honest staleness into an occasional
# blank error page. Decided 2026-08-21. Do not re-argue it, change it only if
# the reason changes.
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

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("rsconnect is not installed. renv::install(\"rsconnect\")", call. = FALSE)
}

APP_NAME <- "pitcher-arsenal"

data_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
fg_files   <- list.files("fg_stuff", pattern = "\\.csv$", full.names = TRUE)

# app_data and league_ref are the two the app cannot start without. game_logs is
# the one it tolerates missing, so it is not required here either: a deploy
# without it degrades the results panel rather than failing to boot.
required <- c("data/app_data.rds", "data/league_ref.rds")
missing  <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing ", paste(missing, collapse = ", "),
       ". Build them with: Rscript scripts/update_data.R", call. = FALSE)
}
if (!length(fg_files)) {
  warning("No FanGraphs export in fg_stuff/, the Stuff+ column will be blank.",
          call. = FALSE)
}

app_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
               data_files, fg_files)

mb <- sum(file.size(app_files)) / 1024^2
message(sprintf("Bundling %d files, %.1f MB", length(app_files), mb))
message("  data through ", max(readRDS("data/app_data.rds")$game_date))

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
