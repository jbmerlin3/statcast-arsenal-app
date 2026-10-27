# Changelog

Dated milestones, newest first. Numbers in parentheses are pull requests.

## 2026-10-22: Off-season

- Test runner (`tests/run_all.R`) and documentation in `docs/`.
- `scripts/verify_savant.R` also checks whiff%, chase%, zone% and hard-hit% per pitcher.
- Unused code removed, including the switched-off league shading for the Search and Usage tables.
- Rate metrics fall back to both pitcher hands when a one-hand league cell is too thin (e.g., left-handed curveball Z-Whiff%).
- Scheduled refresh turned off until the 2027 season.

## 2026-10-18 to 10-20: Stuff+ v2

- The Stuff+ column now comes from my own v2 model instead of a FanGraphs export (#25), scaled within each pitch group (#26).
- Usage tab: pitch mix by outing, with a hover tooltip, beside the season usage chart (#24).
- Clearer wording for small-sample cells (#23).

## 2026-10-05 to 10-10: Trends tab and audit fixes

- Stuff+ grades from the full-season FanGraphs export (#17), before the v2 model replaced them.
- Trends tab: per-game velocity, spin, movement, usage and arm angle for one pitch, with a "last 5 games vs average" flag list (#20).
- Reliever usage pooled over recent outings, and a filter for Savant arm angles that disagree with an unchanged release point (#22).
- Z-Whiff% on the results table (#19).
- Position players removed from the pitcher dropdown (#18). Movement chart legend matches the Usage tab (#21).

## 2026-09-24 to 09-28: Scale and season end

- Search and Context league tables are precomputed by the daily chain, which fixed out-of-memory crashes on the 1 GB instance (#12, #13).
- The refresh waits for Savant when it posts late instead of failing (#14).
- Heat maps use two count buckets, Pre-2K and 2K, that cover every count (#15).
- Season freeze: no Savant requests after the last regular-season day (#16).

## 2026-09-22 to 09-23: Context tab, times through the order, VAA

- Context tab: league percentile chart, release profile (slot, height against expectation, rarity) and arsenal summary (#9).
- Usage tab toggle for times through the order (#10).
- VAA is now location-adjusted, so it measures the pitch rather than where it was thrown (#10).
- Heat maps leave off stray pitch labels and label empty panels 0% (#11).

## 2026-09-08 to 09-09: Shading and deploy reliability

- Every date window is shaded against the league; small samples show their own denominator instead of turning grey (#3).
- GB% and run value added to the results table (#3). Release height added to the search (#2); slider labels simplified (#6).
- HB shows its raw sign everywhere and is mirrored only for ranking (#8).
- Deploy gate compares a digest of the data's values, so Savant backfills deploy and row-order churn doesn't (#5, #7).
- Re-pull window widened to 21 days so late arm angle backfills land (#4).

## 2026-08-31 to 09-01: Characteristics split

- One characteristics table became two: Pitch Traits (what the pitch is) and Pitch Results (what it did) (#1).
- VAA, extension and release point added; release height and side are shaded by distance from average in either direction.
- scripts/verify_traits.R added to check every trait column against the raw data.

## 2026-08-26 to 08-27: Automated refresh

- The daily chain moved from a laptop to GitHub Actions and deploys the app itself.
- GitHub's own scheduler never fired for this repo, so an external cron triggers the workflow; an hourly watchdog checks the live page.
- Re-pulled days now replace stored rows, so Savant's pitch type revisions land.
- scripts/reconcile.R compares the app's numbers with Savant and the MLB game logs at the pitcher-game level.

## 2026-08-21 to 08-24: Deployable, and definitions matched to Savant

- renv lockfile, deploy script and README.
- Hard-hit rate over batted balls, foul tips and missed bunts counted as whiffs, and the ball radius added to the vertical zone edge. Each definition was checked against Savant's leaderboard over 105 pitchers.
- Pitch trait search tab.
- Results panel graded against the league; IVB and HB shaded by inches from the league average.

## 2026-08-19 to 08-20: First version

- The original single-pitcher R script split into tested modules under R/.
- Shiny app with Movement, Usage, Characteristics and Heat Maps tabs.
- Pitcher-level league reference with sample floors, a fallback ladder and four cell states.
- Results panel from Statcast and MLB game logs, with IP parsed in thirds.
- Daily data chain: Savant pull, league reference, game logs, app data.
