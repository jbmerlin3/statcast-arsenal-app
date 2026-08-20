# CLAUDE.md

Context for Claude Code working in this repo. Read this before writing anything.

## What this is

A Shiny app that replaces a manual, pitcher-by-pitcher advance scouting workflow.
User types a pitcher name, picks a date range and a batter handedness, and gets
movement, velocity, usage, pitch characteristics, count usage, and heatmaps,
each with league context attached.

Two audiences. Jonathan on his laptop, and a deployed link he can send to
people to debug or to show as a portfolio piece. Everything must run from a
fresh clone with no credentials and no live Savant pull.

## Scope

In scope. The six chart and table outputs that already exist, wrapped in an app,
with league context added.

Out of scope for now. The user's Stuff+ v4 model. Stuff+ grades come from a
FanGraphs CSV export, exactly as they do today. See the Stuff+ section below for
the one thing that must stay true so the model can be swapped in later without a
refactor.

## How to work with Jonathan

He has never built a Shiny app. The point of this project is that he learns it,
so building the whole thing in one pass defeats the purpose.

- Plan mode first on anything structural. Get approval before writing files.
- One file, or one tab, at a time. Stop and let him run it.
- Before writing a reactive, say in two or three sentences what the reactive
  graph looks like and why. He wants the concept, not just the code.
- When he asks a "why does this work" question, answer it. Do not answer it by
  rewriting the code.
- He drives the prose in any writeup. Targeted fixes only.

## Repo layout

```
statcast-arsenal-app/
  app.R                     # ui + server, thin. Logic lives in R/.
  R/
    theme.R                 # pitch_colors, constants, gt theme
    data.R                  # load app_data.rds, league_ref.rds, player_index
    features.R              # build_pitch_level(), pl_trim column contract
    plots.R                 # plot_movement, plot_velo, plot_usage, plot_heatmap
    tables.R                # arsenal_table, arsenal_gt, count_usage_tbl, count_usage_gt
    stuff.R                 # load_fg_stuff() and the stuff_all contract
    league.R                # league_ref lookup + percentile helpers
    mod_*.R                 # Shiny modules, one per tab. NOT YET, see below
  scripts/
    update_data.R           # the daily chain, see below
    build_league_ref.R      # precomputes league context
  data/
    app_data.rds            # the only pitch data the app reads at startup
    league_ref.rds
  fg_stuff/                 # dated FanGraphs Stuff+ exports
  tests/
```

## Tabs are inline in app.R, not modules yet

A module buys three things: namespacing so two instances can coexist,
encapsulation of inputs a tab owns privately, and file separation. None of the
four tabs owns an input of its own; pitcher, dates and batter side are all
global and shared, and no tab is instantiated twice. So the first two buy
nothing here and the third is available from a plain function in `R/`. A module
with no inputs, one output and one instance is a function call with `NS()`
wrapped around it.

Two triggers to revisit, whichever comes first. The comparison pane in the
backlog renders the same table for two windows, which is exactly what
namespacing exists for. And the first tab that grows its own control, such as a
FanGraphs export selector on the characteristics tab.

## The daily chain

`scripts/update_data.R` runs these in order and nothing else touches the data.

1. Load `statcast_clean_2026.rds`, pull from `max(game_date) + 1` to today,
   filter, bind, dedup on `game_pk`/`at_bat_number`/`pitch_number`, save.
2. Rebuild `data/league_ref.rds`.
3. Write `data/app_data.rds`, the trimmed column set.

The app never pulls from Savant and never rebuilds the league reference. It reads
two rds files and renders.

Savant's endpoint uses `game_date_gt` / `game_date_lt`. Both are **inclusive**,
despite the names, confirmed by probing the endpoint on 2026-08-19:
`gt=2026-05-05&lt=2026-05-07` returns all three dates, and a single-day request
with `gt == lt` returns that day's rows rather than zero.

Start the daily pull at `last_date + 1`. Not because anything is strict, but
because `last_date` is already in the store and an inclusive lower bound would
re-fetch it. The chunking in `pull_season_statcast()`,
`ends = starts + chunk_days - 1`, is correct and non-overlapping under inclusive
bounds, so no day is being dropped.

## Data contracts

**app_data.rds.** One row per pitch, regular season 2026, all pitchers, trimmed
to the `pl_trim` column set. Add `release_pos_x`, `release_pos_z`, and `plate_z`
to that select list permanently, since release point is already an ad hoc part of
reports and the columns are needed later.

**league_ref.rds.** Keyed by `pitch_type` x `p_throws` x `stand` x
`count_bucket` x `metric`. Stores the mean plus quantiles of the per-pitcher
distribution at 1 point increments, so percentile lookup is `findInterval()` and
costs nothing at runtime.

**player_index.** Built from `distinct(pitcher, player_name)` on app_data, not
from `mlb_rosters()`. Guarantees every searchable name has data behind it.
Savant's `player_name` is "Last, First" and needs reformatting for display.

**stuff_all.** A frame with `pitch_type`, `stuff_plus`, `fg_exact`. Produced by
`load_fg_stuff()` and passed into `arsenal_table()` as an argument. See below.

**game_date is character, not Date.** It arrives that way from Savant and is
stored that way in `statcast_clean_2026.rds` and `app_data.rds`. Coerce
explicitly when comparing against a date, `as.character(the_date)`, rather than
comparing a character column to a `Date`.

Mixed comparison currently gives the right answer, because R renders the `Date`
back to `"YYYY-MM-DD"` and zero-padded ISO strings sort chronologically. The
existing pre-break and post-break report splits rely on that and are correct.
It is still worth not relying on: it holds only while every value stays
zero-padded ISO, and a single `"2026-7-5"` sorts before `"2026-07-17"` while
being later in time. Nothing errors when that happens.

**pt_n is not storable.** It counts a pitch type within the frame it is given,
so it is a function of the selected date window rather than a property of the
pitch. `app_data.rds` therefore carries `APP_DATA_COLS`, which is
`PL_TRIM_COLS` minus `pt_n`, and `shape_arsenal()` recomputes it after the date
filter on every query. The same applies to the usage-ordered `pitch_type`
factor. A stored season-wide `pt_n` would let a two-week window keep a pitch
type that has 200 pitches on the season and one in the window.

## Stuff+, and the one rule that keeps the swap cheap

`arsenal_table(df, hand, stuff_all)` takes grades as an argument rather than
computing them. Keep it that way. The app's data layer decides where `stuff_all`
comes from, and the table function stays ignorant of the source. When the v4
model gets wired in later, it produces the same three columns and nothing
downstream changes.

Do not inline a FanGraphs read inside `arsenal_table()`, and do not add a
`source` argument to it. The resolver lives in `R/stuff.R`.

Practical handling of the CSV, which is a manual export and goes stale silently.

- Exports live in `fg_stuff/` named with their pull date
- `load_fg_stuff()` takes an explicit `path`. Never rely on a default, a stale
  default has already produced "No FanGraphs row" warnings that looked like an ID
  mismatch
- Print the export date in the gt source note so staleness is visible on the page
- No player row is not the same as NA on an existing row. A missing row usually
  means the export predates the callup, not an ID problem. Check `nrow()` on a
  manual filter of the raw CSV before assuming
- `FG_TO_SAVANT` handles code mismatches. FanGraphs SL absorbs Savant ST and SV,
  FanGraphs FO maps to Savant FS. When `fg_exact == FALSE`, `arsenal_gt()`
  auto-adds the footnote. Do not manually re-flag these

## Hard rules

These each cause a silently wrong number rather than an error.

1. **League averages are pitcher-level first, then averaged across pitchers.**
   Never pool pitch-by-pitch across the league. Sample floor is n >= 50 for broad
   cuts and n >= 20 for narrow ones (pitch type crossed with count bucket).

2. **xwOBA is weighted by `woba_denom` and only PA-ending rows count.**
   `sum(estimated_woba_using_speedangle * woba_denom) / sum(woba_denom)`.
   Rows with `woba_denom == 0` are fouls and non-terminal pitches and will zero
   the sum if included.

3. **`bb_type` and `events` are empty string on non-applicable rows, not NA.**
   Filter `col != "" & !is.na(col)`.

4. **Verify string literals against `unique(df$description)` before trusting a
   filter.** A `"called strike"` typo (missing underscore) matched zero rows and
   put a wrong CSW% into a finished report once already.

5. **Report the n.** Thin samples get named in the UI, not smoothed over. Heatmap
   panels below `KDE_MIN_N` fall back to a white-dot scatter rather than a KDE
   built on nothing.

## Checks that did not discriminate

Every entry here is a check that ran green while the thing it existed to protect
was broken. They were recorded in three different files, which is why the same
species kept recurring; this is the one list.

The question to ask of any new check: **can it fail the real bug, and can it pass
correct code?** Both halves. Entry 4 is the second half failing.

1. **A fixture that does not sit on the threshold.** Nudging `KDE_MIN_N` from 15
   to 14 passes `phase1_check.R`, because no heatmap panel in the one fixture has
   exactly 14 pitches. One pitcher cannot sit on every threshold. Recorded at
   `scripts/phase1_check.R`.

2. **Comparing too narrow a slice of the artifact.** Plots were compared on built
   layer data only, so every purely thematic edit passed silently: panel colours,
   legend position, and the heatmap margin fix all reported "identical". Fixed by
   comparing themes as well. Recorded at `tests/phase1_artifacts.R`.

3. **A sanction coarser than the thing it sanctions.** `EXPECTED_DIFFS` is keyed
   by artifact name, so it records THAT an artifact may differ, not WHICH
   difference is allowed. Reverting a sanctioned change while another takes its
   place still passes. Still live. Recorded at `scripts/phase1_check.R`.

4. **A check that fails correct code.** Phase 0 originally compared CSW% against
   the whiff column expecting CSW% to be "meaningfully higher". The two use
   different denominators, whiff% off swings and CSW% off all pitches, so on 2026
   data whiff% legitimately exceeds CSW% for CH and SL. `swstr_pct` is the right
   comparison. Recorded in PLAN.md, Phase 0.

5. **A fixture that moves with the mutation.** Its own species, and the one that
   is hardest to see, because the check is correct and the FIXTURE is what fails.
   The floor-boundary cells in `tests/step2_states.R` were written as
   `swings = fl` and `swings = fl - 1`, reading the same `METRIC_SPEC$floor` the
   code reads. Nudge the floor from 50 to 49 and the cells slide with it: the
   `fl - 1` cell becomes a 48-swing cell, still below floor, still passing. The
   check tested the comparison operator and never the parameter.

   Measured both ways rather than argued, since the difference is invisible on
   reading: same mutation, `whiff_pct` floor 50 to 49.

   ```
   self-referential cells, swings = fl and fl - 1   -> PASS   (mutation survives)
   literal cells, swings = 50 and 49                -> FAIL   (5 assertions)
   ```

   **The rule: a fixture pinning a parameter must use literals, and must never
   read the constant under test.** The cost is that changing a floor deliberately
   now requires editing the test, which is the point. Floors come from the
   resampling study recorded in `R/theme.R`, not from taste.

   The same trap is waiting anywhere a fixture is written in terms of the thing
   it checks: `MIN_PITCH_COUNT`, `MIN_REF_PITCHERS`, `KDE_MIN_N`.

## Formula conventions

- whiff% is off swings, not off total pitches
- chase% is off out-of-zone pitches
- CSW% is `(called_strike + whiffs) / total pitches`
- in_zone is `plate_x` within +/- 0.8291 and `plate_z` between `sz_bot` and `sz_top`
- IVB is `pfx_z * 12`, HB is `-pfx_x * 12`. **Arm side reads positive for a RHP
  only.** HB is not handedness-normalized, so median HB has the opposite sign
  for lefties: SI is +15.5 for RHP and -15.6 for LHP, ST is -13.8 and +14.0.
  That is correct for the movement chart, where actual direction is the point,
  but it means any league comparison must keep `p_throws` in the grain. Pooling
  the hands gives a bimodal distribution with almost nothing at its own centre,
  and no sample-size floor catches it. See the fallback ladder in `R/league.R`,
  which never drops `p_throws` for this reason.
- IP is not derivable from pitch-level data. `events` is one row per PA, so a
  double play reads as one out. Pull IP from FanGraphs or `mlb_pitcher_game_logs()`.

## Count buckets

Two sets, deliberately different, easy to confuse.

Tables use six overlapping buckets. All Counts, Early Count, Pitcher Ahead,
Pitcher Behind, Pre Two Strikes, Two Strikes. They overlap by design, so they are
situational views and not a partition. Do not sum across them expecting 100.

Heatmaps use three coarser ones. 0-0, Hitter Ahead, Two Strikes. Coarser because
a KDE needs a bigger per-panel sample than a usage table does.

## Pitch types and codes

Savant codes only. FF, SI, FC, SL, ST, CU, KC, CH, FS, SV, **KN**. Eleven, not
ten: a knuckleball maps to nothing else, and dropping it would remove 30% of the
one genuine knuckleballer's arsenal.

Savant emits more codes than these. `PITCH_CODE_RULES` in `R/theme.R` is the
single rule, applied by `reconcile_pitch_codes()` in `R/features.R`, which
`shape_arsenal()` calls and nothing else does. CS maps to CU and FO maps to FS,
both real pitches by real pitchers. EP, FA, PO and UN are dropped, being
position-player artifacts and non-pitches. Anything unrecognised is dropped too,
so a code Savant adds next season degrades to a visible note rather than
crashing `pitch_colors[[pt]]`.

Nothing is done silently: the reconciler attaches a report the app renders above
the tabs, "39 CS remapped to CU". Flag code mismatches between sources rather
than mapping silently still holds; this is that rule with the mapping written
down.

`pitch_colors` lives in `R/theme.R` and is the single source of truth. Copy it
from `Learning_to_pull_stats.R`, which is what generated the existing reports.

## Style

- R first, tidyverse, explicit `library()` calls at the top of each file
- Comment the why, not the what
- Anything over about 30 lines becomes a named function
- No em dashes anywhere, including in code comments and UI copy
- gt for tables, ggplot2 for plots, existing formatting preserved

## Known bugs in the source file

Verified against the source on 2026-08-19. The file is
`05_PlayerEval/scripts/Learning to pull stats.R`, with spaces in the name, not
`Learning_to_pull_stats.R`.

**Fixed already, do not re-fix.** Two entries listed here previously had been
corrected in the source at some point and the note was never updated. Both were
checked by grep, not by reading:

- The CSW line, L349, already reads
  `mean(description %in% c("called_strike", whiff_desc))`. There is no spaced
  `% in %` anywhere in the file and no `"called strike"` without the underscore.
- `arsenal_gt()`'s `cols_label()` already has `csw_pct = "CSW%"` at L365.

**Still real.** L529 builds `rico_tbl_L` by filtering `stand == "R"`, so that
table was mislabeled. It is a top-level analysis call, so the Phase 1 split
deletes it along with every other top-level call rather than fixing it in place.

The two fixed entries are kept here rather than deleted because the CSW typo is
the origin of hard rule 4 above, and that rule is still live.

## Deployment

Free-tier shinyapps.io has a memory ceiling and a monthly active-hours budget.
Check current limits before assuming, they change. `app_data.rds` must load
inside that ceiling, which is why the update chain trims columns rather than
shipping raw Statcast.

Gitignore any full-season raw pull. The repo ships code plus a trimmed
`app_data.rds`, or fetches it from a release asset on first run.
