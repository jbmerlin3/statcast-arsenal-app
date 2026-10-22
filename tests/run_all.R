# run_all.R
# Run every test file in its own R process from the repo root and report PASS, FAIL or SKIP.
#   Rscript tests/run_all.R
# Tests that read data/*.rds are skipped when the chain hasn't built them.

if (!dir.exists("R") || !dir.exists("tests")) stop("run this from the repo root", call. = FALSE)

files <- sort(list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE))
has_data <- all(file.exists(c("data/app_data.rds", "data/league_ref.rds")))
rscript <- file.path(R.home("bin"), "Rscript")

needs_data <- function(f) any(grepl("data/|load_app_data|load_league_ref|load_league_pool",
                                    readLines(f, warn = FALSE)))

status <- vapply(files, function(f) {
  name <- sub("\\.R$", "", basename(f))
  if (!has_data && needs_data(f)) {
    cat(sprintf("  SKIP  %-28s (no data/*.rds)\n", name))
    return("skip")
  }
  t0  <- Sys.time()
  out <- suppressWarnings(system2(rscript, f, stdout = TRUE, stderr = TRUE))
  rc  <- attr(out, "status"); if (is.null(rc)) rc <- 0L
  ok  <- rc == 0L && !any(grepl(": FAIL", out, fixed = TRUE))
  cat(sprintf("  %s  %-28s %5.1fs\n", if (ok) "PASS" else "FAIL", name,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (!ok) cat(paste0("        ", tail(out, 15)), sep = "\n")
  if (ok) "pass" else "fail"
}, character(1))

cat(sprintf("\n%d passed, %d failed, %d skipped\n",
            sum(status == "pass"), sum(status == "fail"), sum(status == "skip")))
quit(status = if (any(status == "fail")) 1 else 0)
