# stuff.R
#
# FanGraphs Stuff+ grades and the code crosswalk that maps them onto Savant
# pitch types.
#
# This file owns where a grade comes from. arsenal_table() takes the result as
# its stuff_all argument and knows nothing about the source, which is what lets
# the Stuff+ v4 model replace this later without touching tables.R. Keep it that
# way: no FanGraphs read inside arsenal_table(), no `source` argument on it.
#
# The stuff_all contract is three columns: pitch_type, stuff_plus, fg_exact.

library(readr)
library(dplyr)
library(tidyr)
library(tibble)


#' FanGraphs pitch code to Savant pitch type
#'
#' FanGraphs and Savant do not use the same code set, and the mismatches are
#' resolved here rather than silently anywhere else. `exact` is FALSE where a
#' grade is filled from a non-matching label, which is what drives the automatic
#' footnote in traits_gt(). Do not re-flag those by hand.
#'
#' The many-to-many rows are deliberate. FanGraphs has no sweeper or slurve
#' column, so its single SL grade has to cover Savant SL, ST, and SV, and the
#' two curve labels cross-fill each other.
FG_TO_SAVANT <- tribble(
  ~fg_code, ~pitch_type, ~exact,
  "FA",     "FF",        TRUE,
  "SI",     "SI",        TRUE,
  "FC",     "FC",        TRUE,
  "CH",     "CH",        TRUE,
  "FS",     "FS",        TRUE,
  "CU",     "CU",        TRUE,
  "KC",     "KC",        TRUE,
  "SL",     "SL",        TRUE,
  "FO",     "FS",        FALSE,   # FanGraphs forkball, Savant buckets it as FS
  "CU",     "KC",        FALSE,   # curve graded under the other curve label
  "KC",     "CU",        FALSE,
  "SL",     "ST",        FALSE,   # FanGraphs folds sweepers and slurves into SL
  "SL",     "SV",        FALSE
)


#' Load Stuff+ grades for one pitcher from a FanGraphs export
#'
#' `path` is required and has no default. A default here has already caused
#' real confusion: a stale file produced "No FanGraphs row" warnings that read
#' like an MLBAM id mismatch. Make the caller name the export it means, and keep
#' the export's date window visible by passing it to traits_gt(fg_window =).
#'
#' Filter the export to the same date window as the Statcast pull. A half season
#' of Stuff+ next to full season rate stats is wrong in a way nothing on the
#' page will show.
#'
#' No matching row is not the same as NA on an existing row. It usually means
#' the export predates a callup rather than an id problem, so check nrow() on a
#' manual filter of the raw CSV before chasing the id.
#'
#' Returns the three-column stuff_all contract, zero rows if the pitcher is
#' absent, so arsenal_table() can left_join it either way.
load_fg_stuff <- function(mlb_id, path) {
  row <- read_csv(path, show_col_types = FALSE) |> filter(MLBAMID == mlb_id)
  if (nrow(row) == 0) {
    warning("No FanGraphs row for MLBAM ID ", mlb_id, ". Stuff+ column will be blank.")
    return(tibble(pitch_type = character(), stuff_plus = numeric(), fg_exact = logical()))
  }
  message("FanGraphs match: ", row$Name[1], " (", row$IP[1], " IP)")
  row |>
    select(starts_with("Stf+ ")) |>
    pivot_longer(everything(), names_to = "fg_code", values_to = "stuff_plus",
                 names_prefix = "Stf\\+ ") |>
    filter(!is.na(stuff_plus)) |>
    inner_join(FG_TO_SAVANT, by = "fg_code", relationship = "many-to-many") |>
    # Sort exact matches first, then keep one row per Savant type, so a real
    # grade always wins over one filled from a non-matching code.
    arrange(pitch_type, desc(exact)) |>
    distinct(pitch_type, .keep_all = TRUE) |>
    transmute(pitch_type, stuff_plus = round(stuff_plus, 1), fg_exact = exact)
}


# ---- Choosing which export to read -------------------------------------------

#' Index the exports in a directory
#'
#' Parses the date window out of the filename under the
#' `fg_stuff_<start>_<end>.csv` convention. Files that do not match are ignored
#' rather than guessed at.
fg_exports <- function(dir = "fg_stuff") {
  files <- list.files(dir, pattern = "^fg_stuff_\\d{4}-\\d{2}-\\d{2}_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                      full.names = TRUE)
  if (!length(files)) return(tibble(path = character(), start = as.Date(character()),
                                    end = as.Date(character()), season = integer()))
  m <- regmatches(basename(files), regexec("_(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})\\.csv$",
                                           basename(files)))
  tibble(
    path   = files,
    start  = as.Date(vapply(m, `[`, "", 2)),
    end    = as.Date(vapply(m, `[`, "", 3))
  ) |>
    mutate(season = as.integer(format(start, "%Y"))) |>
    arrange(desc(season), desc(end), start)
}


#' Pick the export to read
#'
#' Never by file mtime. The newest file by mtime in a working fg_stuff/ is
#' typically a half-season export, and pairing half-season Stuff+ against
#' full-season rate stats is wrong in a way nothing on the page would show.
#'
#' With no season given it takes the latest season present, then the widest
#' window ending latest within it. Selecting on season first matters: a prior
#' season's full-year export has an earlier start than the current season's, so
#' any rule reaching for "the earliest start" picks last year.
#'
#' Errors listing what is available rather than falling back to a near miss.
resolve_fg_export <- function(dir = "fg_stuff", season = NULL) {
  ex <- fg_exports(dir)
  if (!nrow(ex)) {
    stop("No exports matching fg_stuff_<start>_<end>.csv in ", dir, call. = FALSE)
  }
  if (is.null(season)) season <- max(ex$season)
  cand <- ex[ex$season == season, ]
  if (!nrow(cand)) {
    stop("No export for season ", season, ". Available:\n",
         paste0("  ", basename(ex$path), collapse = "\n"), call. = FALSE)
  }
  # Latest end, then widest window on a tie, so a full-season export beats a
  # half-season one that happens to share its end date.
  cand <- cand[order(-as.numeric(cand$end), as.numeric(cand$start)), ]
  row  <- cand[1, ]
  list(path   = row$path,
       start  = row$start,
       end    = row$end,
       season = row$season,
       label  = paste(row$start, "to", row$end))
}
