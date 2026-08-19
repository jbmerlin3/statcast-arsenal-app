# Phase 0 acceptance check (plan section 0.3).
# Proves the CSW definition on live data rather than by reading the source.
suppressMessages(library(dplyr))

STATCAST_RDS <- "~/Desktop/Baseball Questionnaires/03_ArsenalReports/statcast_clean_2026.rds"
MLB_ID <- 702070   # Cameron, the worked example in the original script

swing_only <- c("swinging_strike","swinging_strike_blocked","foul","foul_tip","hit_into_play")
whiff_desc <- c("swinging_strike","swinging_strike_blocked")

sc <- readRDS(path.expand(STATCAST_RDS))
cat("store rows:", format(nrow(sc), big.mark=","), " game_date max:", as.character(max(sc$game_date)), "\n\n")

# CLAUDE.md hard rule 4: verify string literals against the data before trusting a filter.
descs <- unique(sc$description)
cat("--- literal check against unique(description) ---\n")
for (lit in c("called_strike", whiff_desc, swing_only)) {
  cat(sprintf("  %-28s %s\n", lit, if (lit %in% descs) "PRESENT" else "*** ABSENT ***"))
}
cat("\n")

MIN_PITCH_COUNT <- 5
# Mirror build_pitch_level(): rare pitch types are dropped before any rate is
# computed, so the check must see the same rows arsenal_table() will.
one <- sc |>
  filter(pitcher == MLB_ID, !is.na(pitch_type), pitch_type != "") |>
  add_count(pitch_type, name = "pt_n") |>
  filter(pt_n >= MIN_PITCH_COUNT)

res <- one |>
  group_by(pitch_type) |>
  summarise(
    n         = n(),
    cstr      = sum(description == "called_strike"),
    whiffs    = sum(description %in% whiff_desc),
    swings    = sum(description %in% swing_only),
    # exactly as arsenal_table() computes it
    csw_pct   = round(mean(description %in% c("called_strike", whiff_desc)) * 100, 1),
    .groups   = "drop"
  ) |>
  mutate(
    csw_hand   = round((cstr + whiffs) / n * 100, 1),   # independent hand computation
    swstr_pct  = round(whiffs / n * 100, 1),
    whiff_pct  = round(whiffs / swings * 100, 1)
  ) |>
  arrange(desc(n))

print(as.data.frame(res), row.names = FALSE)

cat("\n--- acceptance ---\n")
a <- isTRUE(all.equal(res$csw_pct, res$csw_hand))
# csw_pct exceeds swstr_pct by exactly the called-strike share, so the
# inequality is strict only where called strikes exist. A pitch type with zero
# called strikes legitimately ties, and demanding strictness there would fail
# correct code.
b <- all(res$csw_pct >= res$swstr_pct) &&
     all(res$csw_pct[res$cstr > 0] > res$swstr_pct[res$cstr > 0])
cat("csw_pct == hand-computed (cstr+whiffs)/n :", a, "\n")
cat("csw_pct >= swstr_pct, strict where cstr>0:", b, "\n")
cat("\nPHASE 0 CHECK:", if (a && b) "PASS" else "FAIL", "\n")

cat("\n--- why PLAN.md's original check was wrong ---\n")
cat("csw_pct  range:", min(res$csw_pct), "to", max(res$csw_pct), " (denominator = all pitches)\n")
cat("whiff_pct range:", min(res$whiff_pct), "to", max(res$whiff_pct), " (denominator = swings)\n")
cat("csw_pct > whiff_pct for all pitch types  :", all(res$csw_pct > res$whiff_pct), "\n")
