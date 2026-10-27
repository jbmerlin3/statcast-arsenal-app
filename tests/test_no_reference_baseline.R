# With no league context (ref missing or NULL), the Characteristics tables must render
# byte-for-byte as the committed baseline.
#   Rscript <this file>           compare against the baseline
#   Rscript <this file> --write   recapture it
# If it fails, diff the cells by column in both directions before recapturing.
suppressMessages({library(dplyr); library(tidyr); library(purrr); library(forcats)
                  library(ggplot2); library(gt); library(readr)})

REPO <- local({
  a <- grep("^--file=", commandArgs(), value = TRUE)
  root <- if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else normalizePath(".")
  if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".")
  root
})
invisible(lapply(sort(list.files(file.path(REPO, "R"), full.names = TRUE)), source))

BASELINE <- file.path(REPO, "tests/fixtures/chars_gt_null_baseline.rds")
FIXTURE  <- file.path(REPO, "tests/fixtures/pl_trim_702070.rds")
plt <- readRDS(FIXTURE)

# Synthetic Stuff+ inputs
stuff_empty <- tibble(pitch_type = character(), stuff_plus = numeric())
stuff_mixed <- tibble(pitch_type = c("FF", "SL"), stuff_plus = c(95.0, 102.7))

# Strip gt's random table id
render <- function(g) {
  h <- as.character(gt::as_raw_html(g))
  ids <- unique(regmatches(h, gregexpr('(?<=id=")[a-z]{10,}(?=")', h, perl = TRUE))[[1]])
  for (id in ids) h <- gsub(id, "GT_ID", h, fixed = TRUE)
  h
}

# Omit `ref` to test the default
current <- list(
  traits_R        = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R")),
  traits_L        = render(traits_gt(traits_tbl(arsenal_table(plt, "L", stuff_empty)), "L")),
  traits_footnote = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_mixed)), "R")),
  results_R       = render(results_gt(results_tbl(arsenal_table(plt, "R", stuff_empty)), "R")),
  results_L       = render(results_gt(results_tbl(arsenal_table(plt, "L", stuff_empty)), "L"))
)

# Explicit ref = NULL must equal the default
null_args <- list(
  traits_R  = render(traits_gt(traits_tbl(arsenal_table(plt, "R", stuff_empty)), "R", ref = NULL)),
  results_R = render(results_gt(results_tbl(arsenal_table(plt, "R", stuff_empty)), "R", ref = NULL))
)

if ("--write" %in% commandArgs(TRUE)) {
  saveRDS(current, BASELINE)
  cat("baseline written:", basename(BASELINE), "\n")
  for (nm in names(current)) cat(sprintf("  %-9s %6d chars\n", nm, nchar(current[[nm]])))
  quit(status = 0)
}

stopifnot("no baseline; run with --write at the commit you want to freeze" =
            file.exists(BASELINE))
base <- readRDS(BASELINE)

# A baseline missing a case can't fail on it
extra   <- setdiff(names(current), names(base))
missing <- setdiff(names(base), names(current))
moved   <- names(base)[vapply(names(base), function(nm)
             !identical(base[[nm]], current[[nm]]), logical(1))]

cat("\n", strrep("-", 58), "\n", sep = "")
for (nm in names(base)) {
  cat(sprintf("  %-16s %s\n", nm,
              if (identical(base[[nm]], current[[nm]])) "identical" else "MOVED"))
}
cat(strrep("-", 58), "\n", sep = "")

for (nm in moved) {
  b <- base[[nm]]; c2 <- current[[nm]]
  cat("\n", nm, ": ", nchar(b), " -> ", nchar(c2), " chars\n", sep = "")
  cb <- strsplit(b, "")[[1]]; cc <- strsplit(c2, "")[[1]]
  n <- min(length(cb), length(cc)); d <- which(cb[1:n] != cc[1:n])
  if (length(d)) {
    at <- d[1]
    cat("  first difference at char ", at, "\n", sep = "")
    cat("    baseline: ...", substr(b, max(1, at - 50), at + 50), "...\n", sep = "")
    cat("    current : ...", substr(c2, max(1, at - 50), at + 50), "...\n", sep = "")
  }
}
if (length(extra))   cat("\nbaseline has no entry for: ", paste(extra, collapse = ", "), "\n", sep = "")
if (length(missing)) cat("\ncurrent no longer renders: ", paste(missing, collapse = ", "), "\n", sep = "")

# Explicit ref = NULL must match the default path
null_ok <- TRUE
for (nm in names(null_args)) {
  this_ok <- identical(null_args[[nm]], base[[nm]])
  null_ok <- null_ok && this_ok
  cat("  ", nm, " ref=NULL  ", if (this_ok) "identical to its default render" else "MOVED",
      "\n", sep = "")
}

ok <- !length(moved) && !length(extra) && !length(missing) && null_ok
cat("\nNO-REFERENCE BASELINE: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
