# chain_entry.R
#
# What launchd invokes:  Rscript scripts/chain_entry.R
#
# An R entry point rather than a bash wrapper, because macOS attributes Full
# Disk Access to the FIRST binary in the plist's ProgramArguments. With
# /bin/bash there, the grant would have to go to bash, which would hand every
# bash script on the machine access to the Desktop. With Rscript there, only R
# gets it.
#
# The repo lives directly under ~ and NOT under ~/Desktop, deliberately. TCC
# guards Desktop, Documents and Downloads; a launchd agent can read ~ and
# ~/Library but not ~/Desktop, verified 2026-08-20 by probe. Moving the repo
# sidesteps the permission entirely rather than granting Full Disk Access.
#
# Do not move it back under any of those three, and note that everything the
# chain READS has to stay out of them too, which is why the season store lives
# in ~/baseball-store rather than beside the source project.

REPO <- "/Users/jonathanmerlin/statcast-arsenal-app"
LOG  <- file.path(REPO, "logs", "chain.log")

banner <- function(con, text) {
  writeLines(c("", strrep("=", 62), text, strrep("=", 62)), con)
}

# TCC denial looks like a missing directory from in here, so say which it is.
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
# The word FAILED appears here and nowhere else in a healthy log, which is what
# the status command greps for.
writeLines(if (status == 0L)
  paste("RUN OK    ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  else paste("RUN FAILED", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), con)
close(con)

# Roughly a month of runs. Keeps the log findable rather than unbounded.
lines <- readLines(LOG, warn = FALSE)
if (length(lines) > 2000) writeLines(tail(lines, 2000), LOG)

quit(status = status)
