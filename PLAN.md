# Build plan

Seven phases. Each one ends with something Jonathan can run and verify before the
next one starts. Do not start a phase until the previous phase's checks pass.

Stuff+ v4 is not in this plan. It goes in the backlog. Phase 1 keeps the seam
open so it can be added later without touching the app.

---

## Phase 0. Repo and bug fixes

Small, and it stops known-wrong numbers from getting frozen into the app.

- [x] `git init`, create the folder structure from CLAUDE.md, add `.gitignore`
      covering `*.rds`, `.Rproj.user/`, `.Rhistory`. `data/` needs the two-line
      `data/*` plus `!data/.gitkeep` form, or the folder will not survive a clone
- [x] ~~Fix the CSW% line~~ and ~~add `csw_pct` to `cols_label()`~~. Both were
      already correct in the source. See the known bugs section of CLAUDE.md
- [x] Verify on a known pitcher that CSW% is computed as defined

**Check.** For one pitcher, with the `MIN_PITCH_COUNT` filter applied so the check
sees the same rows `arsenal_table()` does:

```
csw_pct == round((cstr + whiffs) / n * 100, 1)    hand-computed, must match
csw_pct >= swstr_pct                              strict wherever cstr > 0
```

The original check here compared CSW% against the whiff column, expecting CSW% to
be "meaningfully higher". That is wrong and would fail correct code. The two use
different denominators, whiff% off swings and CSW% off all pitches, and on 2026
data whiff% exceeds CSW% for CH (33.9 vs 28.4) and SL (33.8 vs 30.1). The bug this
guards against collapsed CSW% onto `swstr_pct`, so `swstr_pct` is the right
comparison.

The inequality is not strict for a pitch type with zero called strikes, which ties
at zero, and the `MIN_PITCH_COUNT` filter matters: without it a 1-pitch `EP` row
that the real pipeline drops fails the check.

Ran 2026-08-19, PASS. Driver kept at `tests/phase0_csw_check.R`.

---

## Phase 1. Split the pipeline into sourceable files

No app yet. The goal is that a fresh R session plus five `source()` calls gives
every function, which permanently kills the re-paste bug class that has already
cost two silent wrong numbers.

- [x] `R/theme.R`, `R/features.R`, `R/plots.R`, `R/tables.R`, `R/stuff.R`
- [x] Move constants (`MIN_PITCH_COUNT`, `KDE_BW`, `KDE_MIN_N`, `pitch_colors`,
      `pitch_text_colors`, `swing_only`, `whiff_desc`) into `theme.R`
- [x] Move `FG_TO_SAVANT` and `load_fg_stuff()` into `stuff.R`. The export
      resolver is **deferred to Phase 4**, since nothing before the pitch
      characteristics tab calls it. `load_fg_stuff()` lost its default path,
      and `arsenal_gt(tbl, hand, fg_window = NULL)` prints the window instead.
      When the resolver is written it must key on the date window in the
      filename, never file mtime: the newest file by mtime is a half-season
      export, which would put half-season Stuff+ beside full-season rates
- [x] `PL_TRIM_COLS` in `features.R` is the single contract, 38 columns,
      resolving the two disagreeing selects toward the wider L168 block, which
      already carried all three of the named columns
- [x] Delete all top-level calls from the R files. Functions only.
- [x] Scrape functions moved to `scripts/update_data.R`, functions only

`arsenal_table()` keeps its `stuff_all` argument unchanged. Do not inline the
FanGraphs read into it.

**Check.** `Rscript scripts/phase1_check.R`. Runs the original definitions and
the new `R/` files over one frozen fixture in separate subprocesses and compares
14 artifacts.

Not against the existing PDF reports. The Statcast store has been updated since
those rendered, so correct code produces different numbers and that check would
send you hunting a bug that does not exist. A pure refactor is tested old code
against new code on identical input.

Two differences are expected and named in the script: `plot_movement`, whose
`set.seed(3)` is a deliberate change from the original's 42, and `arsenal_gt`'s
source note. The check fails on any other difference, and equally if an expected
difference disappears, since that means an intended change was reverted.

Ran 2026-08-19, PASS.

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

Handedness handling. Three functions take one call and three take two, but not
for the same reason, so do not collapse the two cases.

- `plot_movement` and `plot_velo` genuinely ignore `stand`. One call, no argument.
- `plot_usage` reads `stand` and renders both sides at once as a diverging bar.
  One call, no argument, but it is not ignoring handedness. Do not "fix" it to
  take a `hand` argument, that would destroy the two-sided chart.
- `arsenal_table`, `count_usage_tbl`, and `plot_heatmap` filter to one side
  internally and need the argument. Two calls each.

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
