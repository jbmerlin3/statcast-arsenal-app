# chain_entry.R
# Runs the daily chain locally under launchd
# (scripts/com.jonathanmerlin.arsenal-chain.plist). Logs to logs/chain.log.
# The repo must live outside ~/Desktop, ~/Documents and ~/Downloads (blocked by
# macOS for launchd agents).

REPO <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
LOG  <- file.path(REPO, "logs", "chain.log")

banner <- function(con, text) {
  writeLines(c("", strrep("=", 62), text, strrep("=", 62)), con)
}

# A TCC denial looks like a missing directory
if (!dir.exists(REPO)) {
  dir.create("/tmp/arsenal-chain", showWarnings = FALSE)
  writeLines(c(
    paste("CHAIN FAILED", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Cannot reach ", REPO, "."),
    "If this path is under ~/Desktop, ~/Documents or ~/Downloads, that is why:",
    "macOS TCC blocks launchd agents from those three. Move it back under ~."),
    "/tmp/arsenal-chain/tcc-denied.log")
  quit(status = 1)
}

setwd(REPO)
dir.create("logs", showWarnings = FALSE)

con <- file(LOG, open = "at")
banner(con, paste("RUN START", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
close(con)

con <- file(LOG, open = "at")
sink(con, append = TRUE, split = FALSE)
sink(con, append = TRUE, type = "message")

status <- tryCatch({
  suppressMessages({
    library(dplyr); library(tidyr); library(purrr); library(forcats)
    library(ggplot2); library(gt); library(readr)
  })
  source("scripts/update_data.R")   # defines run_chain(), does not run it
  run_chain()
  0L
}, error = function(e) {
  message("ERROR: ", conditionMessage(e))
  1L
})

sink(type = "message"); sink(); close(con)

con <- file(LOG, open = "at")
# chain_status.sh greps for RUN OK / RUN FAILED
writeLines(if (status == 0L)
  paste("RUN OK    ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  else paste("RUN FAILED", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), con)
close(con)

# Keep about a month of runs
lines <- readLines(LOG, warn = FALSE)
if (length(lines) > 2000) writeLines(tail(lines, 2000), LOG)

quit(status = status)
