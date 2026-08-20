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
# The repo lives under ~/Desktop, which is TCC-protected: a launchd agent can
# read ~ and ~/Library but not ~/Desktop, verified 2026-08-20 by probe. Without
# the grant this exits 1 with a message naming the cause, rather than failing
# with a bare "Operation not permitted".

REPO <- "/Users/jonathanmerlin/Desktop/statcast-arsenal-app"
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
    "Almost certainly macOS Full Disk Access: a launchd agent cannot read",
    "~/Desktop without it. Grant it to Rscript in System Settings, Privacy",
    "and Security, Full Disk Access."), "/tmp/arsenal-chain/tcc-denied.log")
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
