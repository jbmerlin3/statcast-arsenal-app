# Build plan

Seven phases. Each one ends with something Jonathan can run and verify before the
next one starts. Do not start a phase until the previous phase's checks pass.

Stuff+ v4 is not in this plan. It goes in the backlog. Phase 1 keeps the seam
open so it can be added later without touching the app.

---

## Phase 0. Repo and bug fixes

Small, and it stops known-wrong numbers from getting frozen into the app.

- [ ] `git init`, create the folder structure from CLAUDE.md, add `.gitignore`
      covering `*.rds`, `.Rproj.user/`, `.Rhistory`
- [ ] Fix the CSW% line (`%in%`, `"called_strike"`)
- [ ] Add `csw_pct = "CSW%"` to `arsenal_gt()`'s `cols_label()`
- [ ] Verify against a known pitcher that CSW% is no longer equal to whiff rate

**Check.** `arsenal_gt(arsenal_table(pl_trim, "R", stuff))` renders with a CSW%
column whose values are meaningfully higher than the whiff column.

---

## Phase 1. Split the pipeline into sourceable files

No app yet. The goal is that a fresh R session plus five `source()` calls gives
every function, which permanently kills the re-paste bug class that has already
cost two silent wrong numbers.

- [ ] `R/theme.R`, `R/features.R`, `R/plots.R`, `R/tables.R`, `R/stuff.R`
- [ ] Move constants (`MIN_PITCH_COUNT`, `KDE_BW`, `KDE_MIN_N`, `pitch_colors`,
      `pitch_text_colors`, `swing_only`, `whiff_desc`) into `theme.R`
- [ ] Move `FG_TO_SAVANT` and `load_fg_stuff()` into `stuff.R`. Add a resolver
      that picks the newest export in `fg_stuff/` and returns the export date
      alongside the grades, so the source note can print it
- [ ] Add `release_pos_x`, `release_pos_z`, `plate_z` to the `pl_trim` select list
- [ ] Delete all top-level calls from the R files. Functions only.

`arsenal_table()` keeps its `stuff_all` argument unchanged. Do not inline the
FanGraphs read into it.

**Check.** Restart R, source the five files, run every plot and table on one
pitcher, confirm output is identical to the current reports.

---

## Phase 2. Minimal app

One tab. Resist adding a second.

- [ ] `app.R` with `selectizeInput` for pitcher (server-side, the index is large),
      `dateRangeInput`, and a single `plotOutput` for `plot_movement()`
- [ ] One reactive, `pitcher_data()`, that filters `app_data` to the selected
      pitcher and date range. Every downstream output depends on it and on
      nothing else.
- [ ] Load `app_data.rds` at global scope, outside `server`, so it is shared
      across sessions rather than reloaded per user. `league_ref.rds` does not
      exist yet and is not needed to render a movement chart. It joins the same
      global block in Phase 3
- [ ] `data/app_data.rds` must exist before the app can start, and no earlier
      phase creates it. Write it via step 3 of `scripts/update_data.R`, the
      trimmed `pl_trim` column set, per the daily chain in CLAUDE.md

**Check.** Change the pitcher, the plot changes. Change the date range, the plot
changes. Nothing else exists yet.

**This is the phase where Jonathan should be typing.** Claude Code explains the
reactive graph, Jonathan writes `app.R`.

---

## Phase 3. League reference

The precomputed context object. This is what makes the app a scouting tool rather
than a chart viewer, and it has to be precomputed because running the per-pitcher
league query inside a reactive will hang on every input change.

Ordering note. This trails the minimal app deliberately, because nothing in the app
needs `league_ref.rds` to render a movement chart and a working screen comes first.
Running later does not make it reactive. It is still built on disk by the script
below, for the reason in the paragraph above.

- [ ] `scripts/build_league_ref.R`
- [ ] Grain is `pitch_type` x `p_throws` x `stand` x `count_bucket` x `metric`
- [ ] Metrics to start. velocity, ivb, hb, spin, strike%, whiff%, csw%, zone%,
      chase%, xwOBA, usage%
- [ ] Per-pitcher first, then across pitchers. Floors of 50 broad and 20 narrow.
- [ ] Store `mean` plus `quantile(x, probs = seq(0, 1, 0.01))` so the app returns
      a percentile with `findInterval()` instead of rescanning the season
- [ ] `R/league.R` exposes `lg_mean(...)` and `lg_pctile(value, ...)`

**Check.** Pick a pitcher and a pitch, compute his whiff% by hand from
`app_data`, then confirm `lg_pctile()` places him where a Savant percentile would.
Sanity, not precision. If a 40% whiff slider comes back as the 30th percentile,
the grain is wrong.

---

## Phase 4. Remaining tabs, one at a time

Order matters. Each becomes a Shiny module in `R/mod_*.R`.

- [ ] Movement and velocity (refactor Phase 2 into a module)
- [ ] Usage. `plot_usage()` plus `count_usage_gt()` for both hands
- [ ] Pitch characteristics. `arsenal_gt()` with the FanGraphs grades and the
      export date in the source note
- [ ] Heatmaps. Slowest render, so this is where `bindCache()` earns its place

Handedness handling. `plot_usage`, `plot_movement`, and `plot_velo` ignore `stand`
entirely, so they take one call. `arsenal_table`, `count_usage_tbl`, and
`plot_heatmap` filter on `stand` internally and need the argument.

Half-season toggle. 1H and 2H are presets on the date input, not a second filter.
Wire the buttons to `updateDateRangeInput()` so there is one source of truth.

Titles. `arsenal_gt()` and `count_usage_gt()` currently hardcode their titles from
the hand argument and know nothing about a date split. Generalize the signature to
take a free-text label instead of assuming hand, since the app will be passing
date ranges.

**Check.** Every tab matches the corresponding section of an existing PDF report
for the same pitcher and date window.

---

## Phase 5. League context in the UI

- [ ] Percentile fill or a delta column on the arsenal table
- [ ] League average reference lines on the movement plot for that pitch type and
      pitcher hand
- [ ] Usage table shows the pitcher's rate against the league rate for the same
      count bucket
- [ ] Every context number carries the n it was built from

**Check.** A pitch that scouts as plus should read as plus. If the context says
otherwise, the grain or the floor is wrong, not the pitcher.

---

## Phase 6. Share and deploy

- [ ] Trim `app_data.rds` to the minimum column set, check the file size
- [ ] `renv::init()` so the environment reproduces on someone else's machine
- [ ] Deploy, confirm it loads inside the memory ceiling
- [ ] README with a screenshot and the methodology in three paragraphs

---

## Backlog, not scheduled

- **Stuff+ v4 integration.** Split `stuffv4.R` into a pure library, a build
  script, and a `score_pitches()` function. Score pitch-level `pred` in the daily
  chain, aggregate with the bundle's frozen `moments_arsenal` and `type_centers`,
  emit the same `stuff_all` contract `load_fg_stuff()` emits today. Keep the
  FanGraphs column available alongside it as the benchmark.
- Download button that renders the existing two-page PDF from current app state
- Sequencing and tunneling tab
- Multi-pitcher comparison
- Hitter-side version
