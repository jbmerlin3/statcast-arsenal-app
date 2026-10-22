# Validation

Two kinds of checks: tests that the code does what it says, and comparisons with outside sources that what it says is right. A test that compares the app with itself can't catch a number that has been wrong since the day it was written, so the external checks matter as much as the tests.

## Tests

`Rscript tests/run_all.R` runs every test file in its own R process from the repo root and prints PASS, FAIL or SKIP per file. Most tests need the built `data/*.rds` and are skipped without them. `test_vaa_adjustment.R` also runs a check against the season store (`STATCAST_STORE`) when it is present.

| File | What it checks |
|---|---|
| `test_metric_spec.R` | Every table column maps to a metric, and every direction is handled. |
| `test_cell_states.R` | The four cell states, their rendering, direction per pitch type, HB mirroring, floor boundaries, zone geometry, whiff and swing definitions. |
| `test_cell_render.R` | The same states in the rendered gt HTML, table widths, note order, plot visibility, the masked `margin()` guard, NaN handling. |
| `test_no_reference_baseline.R` | Characteristics tables with no league context match a committed byte-level baseline. |
| `test_hand_all.R` | "All" batter sides equals the sum of L and R. |
| `test_results_panel.R` | Results panel sources, IP in thirds, hard-hit denominator, league shading direction and floors. |
| `test_search.R` | Search grain, slider bounds, filters, sorting, traded pitchers, missing traits. |
| `test_deploy_digest.R` | The deploy digest ignores row order and row names but catches changed, swapped, added and NA values. |
| `test_release_context.R` | Release model residuals, low-support flags, HB mirroring, shape floors, percentiles against an independent recompute. |
| `test_vaa_adjustment.R` | VAA location adjustment removes the plate-height effect and leaves thin pitch types raw. |
| `test_times_through.R` | Times-through toggle rules, count table identities, and the server via `shiny::testServer`. |
| `test_rare_pitch_rule.R` | Rare pitch types are left off charts, kept in tables, and named. |
| `test_league_pool.R` | A precomputed pool equals the live computation and is refused when stale. |
| `test_season_freeze.R` | No Savant requests after the season, with the network stubbed. |
| `test_trends.R` | Trends series, titles, change flags, pooled usage, mix-by-outing chart. |
| `test_arm_angle.R` | Impossible arm angles are blanked; real slot changes survive. |
| `test_stuff_plus.R` | Stuff+ parity with the model's leaderboard, window, batter side and per-group scale. |

## External checks

Run from the repo root after the chain has built `data/`. The arsenal and traits checks also read the season store (`STATCAST_STORE`). Results below are from the final 2026 data (through 2026-09-27), run on 2026-10-22.

| Script | Compares | Result on 2026 data |
|---|---|---|
| `scripts/verify_arsenal_savant.R` | Pitches, whiff%, run value and RV/100 per pitcher and pitch type with Savant's pitch-arsenal leaderboard; the sign of run value against Savant's wOBA. | Over 1,256 cells: pitches r = 0.9998, whiff% r = 0.9999 (MAE 0.05), run value r = 0.998, RV/100 r = 0.999. RV/100 vs wOBA allowed r = -0.77. |
| `scripts/verify_savant.R` | Per-pitcher arm angle, whiff%, chase%, zone% and hard-hit% with Savant's custom leaderboard. | Over 141 pitchers: arm angle MAE 0.05 degrees; whiff% 0.031, HH% 0.041, chase% 0.064, zone% 0.098 points. |
| `scripts/verify_traits.R` | Every trait column recomputed from raw store columns; table values against an independent groupby; physical invariants for VAA and release; stale NA bands in arm angle. | Prints PASS or FAIL per check. |
| `scripts/reconcile.R` | Pitch mix per pitcher-game against a fresh Savant pull; TBF, SO and BB against MLB game logs; internal invariants. | Run on a sample of dates. |

See [methodology](methodology.md#metric-definitions) for the rate definitions these checks cover. Read the mean absolute error over many pitchers, never one match.

Coverage has limits. Savant publishes no VAA, and its custom leaderboard doesn't serve extension or release position at a usable grain, so those traits rely on the internal and physics checks. The custom leaderboard also returns an all-NA column for any field name it doesn't recognize, so a fetch that looks successful can compare nothing; the scripts check for that.

## Rules for writing a check

Each of these comes from a check that passed while the thing it guarded was broken.

- **It must be able to fail.** Before trusting a check, break the code it guards and confirm it fails. Then confirm it passes on correct code.
- **Pin parameters with literals.** A fixture written as `floor - 1` moves with the floor it is meant to pin. Write 49.
- **Use literal fixtures for boundaries.** Live data grows every night, so a check built on "the pitcher at exactly 95.0 today" stops testing the boundary the next time he pitches.
- **Test where the app stands.** `renderPlot()` needs a visibly returned plot; `print()` in a test doesn't. A long console session can mask `ggplot2::margin()`; a fresh `Rscript` can't. Checks assert visibility and run with a masking `margin()` defined.
- **Turn a crash into a named failure.** Catch errors inside a check so one dead call fails its own comparison and the rest of the file still runs.
- **Compare column sets in both directions.** A diff that only reports added columns reads "no change" on a table that lost three.
- **A sanction must be as narrow as the change.** Allowing "this artifact may differ" also allows a later, unrelated difference in it.

## Known traps

- **gt:** `fmt()` calls on one column don't compound; the last one wins and gets the raw values. The later `tab_style()` wins, so league fills go after the pitch-code colors. `cols_width()` formulas are evaluated lazily, so widths must be literals in the formula. gt stamps a random table id into its HTML; strip it before comparing renders.
- **R:** a column named `.d` inside `mutate()` partial-matches the `.data` argument. `quantile()` returns named values, which `all.equal()` compares. `mean(x, na.rm = TRUE)` of an all-NA vector is NaN, which prints as "NaN".
- **Data:** `bb_type` and `events` are the empty string, not NA, on rows where they don't apply. Joining `app_data` to the store on pitcher and date alone multiplies rows; use `game_pk`, `at_bat_number` and `pitch_number`.
