# Session state, 2026-08-19

Working tree clean, 22 commits. Both gates pass:

```bash
Rscript scripts/phase1_check.R      # PASS
Rscript tests/phase4_hand_all.R     # PASS
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

## What landed

Phases 0 through 4 complete and verified by hand. The app runs: pitcher search
over 802 arms, date range with All/1H/2H presets on a derived break boundary,
three-way batter side, four tabs (movement, usage, characteristics with Stuff+,
heat maps). Daily chain runs all three steps with `Rscript scripts/update_data.R`.

Late additions after Phase 4 closed:

- Heatmap column labels unclipped, strip annotation enlarged.
- Regression now compares plot **themes** as well as built layer data. It did not
  before, so every purely thematic edit passed silently.
- Pitch code reconciliation. `CS -> CU`, `FO -> FS`, `KN` kept as an 11th code,
  `EP FA PO UN` dropped, all in `reconcile_pitch_codes()` and nowhere else.

## Where Part A and Part B stand

**Part A, pitch codes: DONE**, committed at `5bb2493`. Peralta renders on all
four tabs, `CU` reads 331. Nothing outstanding.

**Part B, league context in the arsenal table: PLANNED, NOT STARTED.** No code
written, no files touched. The plan is at
`~/.claude/plans/peaceful-painting-lampson.md`, Part B. Approved, with the
measurements already done:

- fallback is 126 cells across all 802 pitchers, so it needs a loud marker
- below floor is 50% of cells in a two-week window, so it must read calm
- xwOBA qualifies for 2 of 21 cells in a one-month window

## Mid-way through

Nothing. Part A finished and committed; Part B had not been started.

## First three things tomorrow

1. `METRIC_SPEC` in `R/theme.R` gains a `direction` column. This is the one that
   bites if skipped: without it a **bad xwOBA renders green**, since low xwOBA is
   good. `hb` is directionless *and* handedness-dependent, so it needs the
   neutral scale plus a footnote.
2. `pctile_fill()` in `R/league.R`, plus the helper resolving a value and its
   counts into one of the four states (exact, fallback, below floor, no
   reference). One question per cell, so `tables.R` does not reassemble the
   logic.
3. `arsenal_gt(ref = NULL)` in `R/tables.R`. **NULL must render byte-identical
   to today**, which is what keeps `EXPECTED_DIFFS` unchanged and the Phase 1
   regression green. Run it before touching `app.R`.

Build against **Shane Baz, KC, whiff% vs RHH**, the concrete fallback case: it
drops to `pitch_type x p_throws x count` on 22 reference pitchers.

## Two open items, neither blocking

- `plot_movement` errors with "figure margins too large" below about 600px of
  panel width. `aspect.ratio = 1` plus its margins. Pre-existing, fine at desktop
  widths, ugly on a phone.
  **CLOSED 2026-08-20 as misdiagnosed.** Never reproduced at any size. The
  real fault was plot_movement returning invisibly, so renderPlot drew a
  blank white device. See CLAUDE.md, closed as misdiagnosed.
- `EXPECTED_DIFFS` is keyed by artifact name, so it sanctions *that* an artifact
  may differ, not *which* difference. Reverting a sanctioned change while another
  takes its place still passes. Noted in `scripts/phase1_check.R`.

## Testing gotchas worth remembering

- Use `shinyAppDir(".")`, not `shinyAppFile("app.R")`. Only the directory form
  auto-sources `R/`.
- `updateDateRangeInput()` does not round-trip under `testServer`; the presets
  had to be verified by driving the running app.
- `testServer` expressions cannot see `R/` functions. Source them in the test
  script itself.
- Do not `git checkout` a file to undo a mutation test unless it is committed.
  That cost uncommitted work once already.

## Fallback is rare per cell, common per pitcher

Measured 2026-08-20 over 3,290 arsenal cells, 60 pitchers, vs RHH, full season:

| state | share |
|---|---|
| exact | 75.6% |
| below floor | 21.0% |
| fallback | 2.2% |
| no reference | 1.2% |

The 2.2% is the number that misleads. **35 of those 60 pitchers carry at least
one fallback cell**, and Baz carries six. The plan's "126 cells across all 802
pitchers, about 0.16 per pitcher" was measured on a different cut and badly
understates the vs-RHH view, which is the view the arsenal table is built
against.

"Rare per cell, common per pitcher" is the accurate description. It does not
change the design, since the loud-marker decision rests on per-cell rarity and
that holds. It does change what to expect on screen: most pitchers will show a
dagger somewhere, so the footnote is not an edge case a reader meets once.

One thing that does not multiply: **no pitcher produces more than one distinct
fallback grain.** Baz's several distinct note strings differed only in the
pitcher count behind them, 20 to 79 across the population, never in the grain.
That is what lets a single `†` line name one grain unambiguously. It is asserted
in `R/league.R` rather than assumed, and the assertion stops rather than picking
one silently.

## Part B step 3 landed, 2026-08-20

`arsenal_gt(ref = NULL)` renders byte-identical to `9694276`, verified against a
committed baseline in `tests/step3_null_identical.R`. Phase 5's first bullet is
done; the movement-plot reference lines and the usage-table league rates are
still open.

**`phase1_check.R` cannot guard that invariant** and never could. All three
`arsenal_gt` artifacts sit in `EXPECTED_DIFFS` for the source-note change, so
they report "differs (expected)" whatever happens inside the function. Breaking
the xwOBA format passes it at six diffs. Keep running it, it guards the rest of
`tables.R`, but the `ref = NULL` guard is the baseline test.

Two gt behaviours are written up in CLAUDE.md and both bite silently:
`fmt()` does not compound, and the later `tab_style()` wins.

## The reading-layer risk to watch

Named 2026-08-20, before it has caused anything, so it is on the record.

**A percentile is a rank against pitchers who throw that pitch, and the app
presents it as a rank against pitchers.** The reference is keyed by
`pitch_type`, so a sweeper's whiff% is ranked against sweepers only. That is the
right denominator for "is this a good sweeper" and the wrong one for "is this a
good pitch", and the table gives the reader no way to tell which question a
number answers.

Concrete: a 35th-percentile sinker whiff% and a 35th-percentile slider whiff%
render the same colour, and they are not the same pitch. Sinkers miss few bats
by design, so a 35th-percentile sinker is close to what a sinker is for, while
a 35th-percentile slider is a slider that is not doing its job. The number is
correct, the colour is consistent, and the reading is wrong.

Same class as hb not being arm-side normalised: correct arithmetic, a reference
whose meaning shifts under the reader, and nothing on the page announcing the
shift. It is harder than hb, because hb flips on one visible attribute, pitcher
hand, while this one varies continuously by pitch type and by what the pitch is
being asked to do. It is also worse in one way. hb is directionless and reads as
neutral violet, which is a hint that the cell does not grade. This one renders
on the diverging good-to-bad scale, so it actively asserts quality.

Not fixed, and not obviously fixable by a colour rule. The cheapest partial
answer is per-pitch-type expectation in the label rather than in the fill, so
the reader sees "35th among sinkers" and supplies the context themselves.

## Phase 5 closed, 2026-08-20

All three bullets landed: percentile fill on the arsenal table, league reference
marks on the movement plot, league rates on the usage table. Wired into the app
and verified by driving the running server.

**The usage floor was a bug, now fixed.** `usage_pct` is a share, so its
precision comes from the cut's total pitches, not the numerator. It floored on
the pitch type's own count, which greyed a well-measured 30% share because the
pitch itself was uncommon. `denom` is now `cut_pitches`, and `DENOM_COLS` in
theme.R keeps the reference builder and the app supplying the same names.

Cameron's usage table, vs RHH:

| window | before | after |
|---|---|---|
| full season | 24 exact, 4 fallback, 8 below floor | 36 exact |
| two weeks | 1 exact, 35 below floor | 36 exact |

`league_ref.rds` was rebuilt through the chain's own step 2 off the existing
`app_data.rds`, so no Savant pull was involved. 4,112 cells, 2,616 above the
pitcher floor.

**One pitcher CAN produce two fallback grains, structurally.** The earlier
measurement said one and was taken on the arsenal table, where every column
shares the `All Counts` bucket. The usage table varies the bucket by column, and
the ladder's second and third rungs differ by bucket, so different columns land
on different rungs. The assertion caught it the first time the usage table ran
full season. The dagger note now enumerates the cuts it used rather than naming
one, still as a single line, because two lines under one marker leave the reader
unable to tell which applies.

## Phase 6 closed, 2026-08-20

Results panel live: two rows, equal-weight headers, one over Statcast and one
over MLB game logs. Verified by driving the running app. Toggling All to vs RHH
moves the Statcast header and values and leaves the game-log row byte-identical.

`results_gamelog()` takes no `hand` argument at all. That is the structural
guarantee, not a convention, and `tests/phase6_results.R` asserts the formals
stay that way, so adding one fails the check rather than silently narrowing.

**Do not delete the absence branch as dead code.** Through the UI it is nearly
unreachable: every pitcher in app_data has a game log, and a window with no games
usually has no pitches either, at which point the app's existing "No pitches for
this pitcher in the selected window" guard replaces the whole panel first. The
branch exists for the case that actually matters, a **stale log file**: step 4
fails, app_data advances past the logs, and recent windows have pitches but no
lines. Verified directly against that path, absence message shown, no zeros, no
blanks, Statcast row intact, log date printed. Clicking around will not find it.

**Driving date inputs from a browser does not work.** `changeDate`, the
datepicker API and direct typing all either revert or leave the widget and the
server out of sync. Only the All/1H/2H presets, which go through
`updateDateRangeInput()`, move `input$dates` reliably. Same round-trip problem
already recorded above for the presets under testServer. Drive dates by preset,
or test the functions the server calls.

## Phase 7, renv and README, 2026-08-21

Six suites passed before anything was touched, and again after. Chain triggered
through launchd, not reasoned about: RUN OK in 3m24s, all four steps.

**renv is live.** `renv::init()` hydrated 83 packages out of the user library, so
nothing downloaded. `baseballr` is correctly absent: the chain calls Savant's CSV
endpoint and the StatsAPI `gameLog` endpoint directly, and nothing in the repo
loads baseballr any more.

The thing renv could break is the nightly chain, because the plist sets
`WorkingDirectory` to the repo and `.Rprofile` now decides which library the job
runs against. Triggered it rather than reading the plist. All four steps green,
and every chain package resolves through the renv cache under
`~/Library/Caches`, which is one of the two places a launchd agent can still
read.

**rsconnect is installed but not in the lock.** `renv::settings$ignored.packages`
holds it out. This matters more than it looks: in an renv project rsconnect
builds the deploy manifest from `renv.lock` and not from the bundled files, so
anything in the lock gets installed on the server. Snapshotting rsconnect put 10
packages, including packrat, onto the list of things shinyapps.io would install
in order to run an app that never loads them.

**appFiles narrows the bundle, not the manifest.** Measured both ways on
2026-08-21: 83 manifest packages whether appFiles is the four app directories or
everything. What appFiles does buy is that `scripts/` and `tests/` stay off a
public server, and a 25.8 MB bundle.

**Startup footprint, measured not estimated.** app_data 103.2 MB, league_ref
3.6 MB, game_logs 0.7 MB, player_index 0.1 MB, and 191 MB on the R heap after
the four globals load. The comment in app.R said 142 MB, which predated the
column trim, and now says 103 with the date of the measurement.

**Two things the screenshot pass turned up.**

`titlePanel()` defaults `windowTitle` to the title itself, so passing a tag put
raw HTML in the browser tab: the tab read `<div> Pitcher Arsenal <span style=
...`. Caught by reading the rendered page, not by looking at the code, and fixed
with an explicit `windowTitle`.

`annotate("label", ..., label.size = 0.4)` warns "Ignoring unknown parameters"
under ggplot2 4.0.3, twice on every movement render. The label borders are
therefore not being drawn. Left alone: it is cosmetic, it fires in a function
three artifacts in `EXPECTED_DIFFS` already sanction, and changing it moves the
frozen comparison. Worth doing deliberately, not as a drive-by.

**Not a bug, recorded so it is not re-found.** Resizing the headless viewport
mid-session produces `invalid quartz() device size` from `startPNG` in the
movement output. It is the container reporting a degenerate width while the
viewport is torn down, and the next render replaces it. A plain load has no
error and the plot comes back 1050x620, and a legitimate resize to 900px wide
produces no error either. This is NOT the closed "figure margins too large"
entry returning.

Deploy is written and unrun. `scripts/deploy.R` bundles app.R, R/, data/ and
fg_stuff/, prints the bundle size and the data date, and refuses to run without
`app_data.rds` and `league_ref.rds`. It stops at `rsconnect::setAccountInfo()`,
which needs a token off the shinyapps.io admin page and is Jonathan's to paste.

## The masked margin, 2026-08-22

Reported as "the movement plots and heatmaps broke". Both tabs showed
`argument "observed" is missing, with no default`, the other two tabs were fine,
and all six suites passed.

`randomForest::margin(x, observed, ...)` masks `ggplot2::margin` when it is
attached after ggplot2. `margin()` is called in exactly two places in the repo,
`plot_movement` and `plot_heatmap`, which is the whole of the symptom.
`library(ggplot2)` in app.R does not save you: on an already-attached package
`library()` is a no-op and does not move it back to the front of the search
path.

How it was found, since the guessing took longer than it should have. The
message named an argument that appears nowhere in the repo and in none of the
122 installed packages' exported functions, which ruled out the obvious places
and was still the wrong question. The right one was structural: which calls do
the two broken plots share that the two working ones do not. `margin()` is the
only one. `randomForest::margin` then reproduces the message exactly.

Fixed with `ggplot2::margin()` at all four call sites, guarded by a masked
`margin()` in `tests/step3_render.R`, verified by mutation both ways, and
verified where the app stands: the app run from a session with randomForest
attached now draws both tabs, 930x620 and 930x700, no error.

CLAUDE.md entry 9 records the class. The exposure was measured across every
installed package rather than guessed: the only ggplot2-side clash is this one,
the rest are dplyr-side, `plyr` over mutate, arrange, count, summarise, rename,
`MASS` over select, `stats` over filter.

## HH% was counting fouls, 2026-08-22

Reported: Kirby's HH% reads lower than FanGraphs on both sides. It did, and the
app was wrong.

`results_statcast()` took its hard-hit denominator as every row with a
`launch_speed`. **Statcast measures fouls**, and fouls are weak, so the rate came
out roughly halved with nothing to error on.

Measured on Kirby 2026, before and after:

| | app before | app after | FanGraphs |
|---|---|---|---|
| vs LHH | 28.3 over 435 | 45.4 over 229 | 45.4 over 229 events |
| vs RHH | 24.2 over 393 | 39.0 over 205 | 39.0 over 205 events |

The 206 extra rows vs LHH are fouls, every one of them, mean EV 78.1, 9.2% of
them hard hit. The event counts now match FanGraphs exactly on both sides.

Untracked batted balls stay in the denominator. FanGraphs counts them the same
way: 80 of 205 is 39.0, while dropping the two rows with no EV gives 39.4 over
203. Rejected because the panel exists to be cross-checked against a public
source, and the alternative buys two rows of precision at the cost of never
quite agreeing with anyone.

`type == "X"` rather than `bb_type`, which says the same thing and is not in
`app_data`: the Phase 7 trim dropped it as unread. That note in
`APP_DATA_UNREAD` is now slightly wrong in spirit, since `type` carries the
in-play flag the panel needs and is still there.

Guarded in `tests/phase6_results.R` with a literal seven-row fixture, not a
slice of app_data: the store grows nightly and a rate pinned off live data would
need re-pinning daily. It discriminates three ways, verified by mutation:

```
correct, batted balls incl. untracked   2 of 4    50.0   PASS
old bug, every tracked EV               4 of 5    80.0   FAIL, both assertions
untracked dropped                       2 of 3    66.7   FAIL
```

No other metric was affected. `METRIC_SPEC` has no hard-hit entry, and its
denominators are pitches, swings, out-of-zone and PA, so `launch_speed` backs
nothing in the league reference. HH% lived only in the results panel.

CLAUDE.md gains hard rule 6 and a line in the formula conventions.

## Denominator audit, 2026-08-22

Every rate the app renders, checked against its denominator after the HH% bug.
Magnitudes measured on the 2026 store, 563,401 pitches.

**Verified correct.** CSW% over all pitches. chase% over out-of-zone pitches,
and `in_zone` has zero NAs in 563,401 rows, so the missing `na.rm` on
`sum(in_zone == 0)` cannot silently NA a cell. zone% over all pitches. strike%
over all pitches, with `type == "B"` accounting for exactly ball +
blocked_ball + hit_by_pitch + pitchout, so HBP correctly is not a strike. usage%
over the hand-filtered frame. Count usage over the bucket's pitches. K-BB% over
TBF.

**xwOBA is right, and the reason is worth writing down.** Savant populates
`estimated_woba_using_speedangle` for non-batted-ball events: walk 0.698,
hit_by_pitch 0.729, strikeout 0. So `sum(est * denom) / sum(denom)` is not
quietly scoring walks as zero, which was the obvious way for it to be wrong.

**Fixed: NaN reached the page.** 0/0 is NaN in R and it travelled. A pitch type
nobody swung at rendered `NaN (0)` in the whiff column, and a pitch type that
ended no plate appearance rendered NaN xwOBA. 1.1% of full-season table rows
carry at least one empty denominator, 4.5% over a one-week window, which is the
window a scout picks. `pct_or_na()` in tables.R, guarded in
`tests/step3_render.R` with a literal 12-row fixture, mutation tested: the raw
division fails all three assertions.

**Open, measured, not fixed.**

*Bunts are not counted as swings.* `swing_only` omits foul_bunt 1,221,
missed_bunt 212, bunt_foul_tip 22 and swinging_pitchout 1, which is 0.53% of
offers. League whiff% 22.93 against 22.89 with bunts included, chase% 32.54
against 32.73. Worst pitcher-level whiff shift 0.9 points on three bunt rows.
Structurally it is the HH% bug 20 times smaller: an out-of-zone bunt attempt
sits in the chase denominator and cannot reach the numerator. Not fixed because
it moves every whiff and chase number in the app and needs a league_ref rebuild,
for under a tenth of a point at league level.

*catcher_interf sits in the xwOBA denominator with no numerator.* 73 PA across
66 pitchers, `woba_denom` 1 and `estimated_woba_using_speedangle` NA. Median
shift 0.0011, worst 0.0126, one pitcher moving .4405 to .4531. wOBA's own
denominator is AB + BB - IBB + SF + HBP, which excludes catcher's interference,
so the fix is defensible and one line, in both tables.R and build_league_ref.R
plus a chain rerun.

*truncated_pa counts as a batter faced.* 267 rows. Median K-BB% shift 0.000,
worst 0.38 points. Leave it.

*An empty count bucket renders 0.0% for every pitch type* rather than saying the
count never happened. Two Strikes is empty for 12 pitchers on the season,
Pitcher Behind for 5, Pitcher Ahead for 2.

**Not a defect.** 400 batted balls carry no `launch_speed` and no `woba_denom`,
so Savant drops them from xwOBA while FanGraphs keeps them in the HH%
denominator. The app now matches each source on its own terms.

The league reference is immune to all of this: `summarise_cells()` filters
`is.finite(value)` before taking quantiles, so a NaN pitcher-cell was never
reaching a reference number.

## Percentile scope, and two definitions that do not match Savant, 2026-08-22

Question asked: is ranking within pitch type a problem, and what do FanGraphs and
Savant do. Answering it meant pulling both sources, which turned up two live
bugs.

**What Savant does.** Read off Kirby's player page. The "MLB Percentile
Rankings" panel is pitcher-level only: run value, xERA, xBA, fastball velo, exit
velo, chase%, whiff%, K%, BB%, barrel%, hard-hit%, GB%, extension. One
percentile per stat against qualified pitchers, and **no pitch-type percentile
anywhere**. The closest they come is grouping run value into fastball, breaking
and offspeed. Their pitch-type comparison lives on the movement profile as an
MLB AVG marker beside the pitcher's own, not as a rank and not on a colour
scale.

**What FanGraphs does.** Measured off Jonathan's own export, pitchers with 20+
IP. Stuff+ is NOT centred within pitch type: FC 92.5, CH 93.3, FA 97.4, SI 98.3,
FS 101.7, CU 102.7, KC 103.1, SL 107.0. One common scale, 100 is an average
pitch overall, and cross-pitch comparison is the intended reading. The price
they pay is that a good cutter still prints below 100.

**So the app's choice is the right one, and the note is honest.** Pooling is not
an option. From our own league_ref, RHP, all counts:

```
whiff%   p10    p50    p90
SI       6.1   10.7   17.1
SL      19.9   32.3   41.9
```

The top decile of sinkers does not reach the bottom decile of sliders. The
median slider sits above the sinker maximum and the median sinker below the
slider minimum, so a pooled percentile would paint every sinker red and every
slider green and carry no information about the pitcher.

What is left is a rendering problem, not a ranking problem: a 90th-percentile
sinker and a 90th-percentile slider get the same green while missing bats at 17%
and 42%. Savant avoids it by never ranking within pitch type; FanGraphs avoids
it by keeping one scale. Still open, still deferred.

### Two definitions that do not match Savant

Validated against Savant's custom leaderboard, 105 pitchers with 100+ PA,
comparing our store to theirs on the same dates. MAE is mean absolute error per
pitcher, in points.

```
whiff%   misses / swings                    MAE 2.10   bias -2.10
whiff%   (misses + foul_tip) / swings       MAE 0.09   bias +0.05   68 of 105 exact to 0.1
HH%      denominator type == "X"            MAE 0.04                  (the fix from earlier today)
zone%    our zone                           MAE 4.65   bias -4.79
zone%    vertical pad 0.125 ft              MAE 0.14
chase%   our zone                           MAE 2.25
chase%   vertical pad 0.125 ft              MAE 0.24
```

**Foul tips are whiffs to Savant.** Ours understates whiff% by 2.10 points on
average and by 3.4 at worst. Foul tips are 2.1% of swings. `whiff_desc` also
feeds `csw_pct`, so both move together if this changes.

**Our strike zone is missing the ball radius vertically.** The horizontal bound
already has it: 0.8291 ft is the 8.5 inch plate half-width plus a 1.45 inch ball
radius. The vertical bound is bare `sz_bot` to `sz_top`. A grid search over half
width and vertical pad against Savant lands on 0.825 and 0.125 ft, which is that
same ball radius, and drops zone% error from 4.79 points to 0.14. Our zone% is
systematically 4.8 points too low, which moves zone%, chase% on both ends of its
ratio, the heatmap IZ% strip, and both metrics in league_ref.

Neither is fixed. Both change every affected number in the app and need a
league_ref rebuild, so they are one deliberate change, not a drive-by.

## Both definitions fixed and the chain rerun, 2026-08-23

Approved and done: foul tips count as whiffs, and the strike zone carries the
ball radius vertically. Chain rerun so the store, `league_ref.rds` and
`app_data.rds` all sit on the new definitions.

**Against Savant, 105 pitchers with 100+ PA, before and after.**

```
             before   after
whiff%   MAE   2.10   0.087      bias +0.050, worst 0.39
chase%   MAE   2.19   0.192      bias -0.175, worst 0.62
zone%    MAE   4.65   0.105      bias +0.098, worst 0.29
HH%      MAE   0.04   0.037      (fixed the day before)
```

`ZONE_HALF_FT` is now written as `PLATE_HALF_FT + BALL_RADIUS_FT` so the two
axes share one constant and cannot drift apart again. The vertical pad is the
same 0.1208 ft, not a fitted number: a grid search over half width and pad
independently lands on it.

**The chain had a second bug, found by this change.** `refresh_store()`
recomputed `in_zone` only on the path where new rows were bound in. Both
early returns, "nothing to pull" and "pull returned no rows", handed back the
store untouched, so a formula change reached the file on the next day a game was
played and not before. On a quiet day the chain finished green with the old
geometry still in place. `refresh_derived()` now runs on every path out.

**phase1_check now sanctions at COLUMN granularity.** `whiff_pct` and `csw_pct`
in `arsenal_table_R` and `arsenal_table_L` are the deliberate divergence from
the original script; every other column in those frames is still compared byte
for byte, and each sanctioned column is asserted to ACTUALLY differ. Reverting
`whiff_desc` now FAILS the check with "sanctioned as changed but matches the
original", verified by mutation. That closes species 3 in CLAUDE.md for these
two artifacts: the sanction records which difference is allowed, not merely that
one is.

The fixture rebuild inside `phase1_check.R` compares everything except
`in_zone`, since the original script cannot agree with a zone it does not have,
and then checks the divergence for direction: the new zone must CONTAIN the old
one and be strictly wider. It reports "in_zone widened by 111 pitches as
intended".

**The frozen fixtures are now tracked, and this is the important part.** Both
were gitignored under `*.rds`, so they regenerated on every clone from a season
store that grows nightly: the fixture went 2,261 pitches on 2026-08-19 to 2,346
on 2026-08-23. A byte-identity baseline captured against one fixture and
compared against another is not a check, and nothing would have said so. 217 KB
committed. Frozen files are only frozen if they are committed.

`arsenal_gt_null_baseline.rds` was recaptured twice, deliberately, and the first
time only after diffing by column: 10 of 84 body cells moved, 5 `whiff_pct` and
5 `csw_pct`, one per pitch type, nothing else. The second recapture was for the
rebuilt fixture. Recapturing is the move that hides regressions, so the rule is
in the test header now: diff the cells by column BEFORE reaching for `--write`.

Guards added to `tests/step2_states.R`, both mutation tested. Zone geometry is
pinned with literal coordinates, 0.82 in and 0.84 out horizontally, 0.10 in and
0.15 out past both vertical edges, never through `ZONE_HALF_FT`. The foul-tip
fixture is 8 swings with 2 misses and 1 foul tip, so the right answer is 37.5,
the old code gives 25.0, and dropping the foul tip from the denominator would
give 28.6.

## Bunts are swings, and the report engine is aligned, 2026-08-23

Asked to make the app, the report engine and Savant agree. Doing it turned up
that my own recommendation from earlier today was wrong.

**The bunt measurement was confounded.** The audit tested "count bunts as
swings" while `foul_tip` was still missing from the whiff numerator. The two
errors run in opposite directions, they partly cancelled, the result looked like
a wash and I recommended leaving it. With the numerator correct:

```
swing set                                  whiff MAE   chase MAE
no bunts anywhere (this morning's commit)      0.087       0.192
bunts are swings, missed_bunt a whiff          0.036       0.060
+ bunt_foul_tip a whiff too                    0.032       0.060
```

One change at a time against the source. A confounded measurement is worse than
no measurement, because it talks you out of a real fix with a number attached.

Final state, app against Savant over 105 pitchers: whiff 0.032, chase 0.060,
zone 0.105, HH 0.037, every bias inside 0.10 point.

**The report engine had the same HH% bug the app did.** `04_arsenal_pdf.R` took
`bbe = sum(!is.na(launch_speed))`, so every hard-hit rate in every PDF was
computed over fouls as well as batted balls, the same roughly-halving error.
Now `sum(type == "X")`.

**The engine spelled the swing set out nine times across three files, three
different ways inside `04_arsenal_pdf.R` alone**: the whiff denominator carried
foul_bunt and missed_bunt, the chase numerator carried foul_bunt only, and the
splits block carried neither. All nine now match `R/theme.R`, verified by
parsing every `c("swinging_strike", ...)` literal out of the three files and
comparing the sets rather than by reading them.

Aligned: `engine/statcast API.R`, `engine/04_arsenal_pdf.R`,
`engine/advance-scouting-report/scripts/arsenal_report.R`. NOT touched:
`reports/lodolo.R` and `reports/David Peterson.R`, which carry their own inlined
copies. They are finished reports for specific pitchers, and silently changing
what they would re-render is not mine to decide.

Zone needed no work over there: all three engine files have carried the ball
radius on both axes all along. The app was the one that lost it in the Phase 1
split, which is worth remembering, since Savant was treated as the only
reference for a day when the answer was sitting in the next directory.

## The colour moved, then got the direction right, 2026-08-24

Three rounds of UI work, each one Jonathan's call and each one measured before
it was built.

**The colour moved off the arsenal table and onto the results panel, then came
back with different rules.** The table had been carrying two colour systems at
once, a pitch-colour row wash saying WHICH pitch and a percentile fill saying HOW
GOOD, and the eye cannot separate them. The wash is gone, the pitch code keeps
the colour, and the fill is the only thing that varies across a row.

**The results panel now grades seven of its eight numbers against the league
over the same window and batter side.** Window-matching was forced by IP: it is
a counting stat, and the league median IP over two weeks is 5.7 against 29.7
across the season, so a season reference paints every pitcher deep blue in any
short window. TBF takes no fill, being the sample rather than a result.

The floor scales with the window, `max(absolute_minimum, median denominator / 2)`,
because a fixed floor cannot serve both jobs it was being asked to do:
reliability wants an absolute count and participation wants a relative one. A
fixed 50 TBF left 16% of pitchers coloured over two weeks. The scaled version
keeps two thirds to five sixths at every window length. Measured:

```
window        TBF floor   clears
full season      65        66%
30 days          23        78%
14 days          12        86%
```

**HB is arm-side normalised on the comparison surfaces and raw on the movement
chart.** Once the cell carried a colour, the old convention stopped being
tenable: RHP sinkers averaged +14.9 and LHP sinkers -15.1, so two pitchers with
identical arm-side run rendered red and blue. `arsenal_table()` and
`build_league_ref.R` now both apply `arm_side_sign()`, `movement_ref()` converts
back so the league cross lands on a lefty's actual pitches, and `app_data` keeps
the raw signed value. The footnote says what the number IS rather than
apologising for what it is not.

**Direction turned out to belong to the pitch, not to the metric.** Grading IVB
globally high-is-good painted Logan Webb's changeup blue, when Savant paints that
same pitch red and calls it "9.0 MORE DROP". `PITCH_SHAPE_DIRECTION` now says
ride is good for FF and FC, drop for everything else, arm side for FF SI CH FS,
glove side for the rest. All eleven codes written out, no default: an unlisted
code stops the render. Webb now reads SI drop red, SI tail red, CH drop red, ST
sweep red, FF ride blue.

**Stuff+ shades itself off 100** with no league lookup, since 100 is average by
construction. Span 25, from the export: 2,202 grades at 20+ IP, 2nd and 98th
percentiles at 75.3 and 131.1, 94.2% inside the span.

**The neutral palette went from violet to symmetric grey**, sharing the diverging
scale's exact middle stop, so average is the same light grey everywhere. It lost
direction in exchange: dark now means unusual rather than high. `usage_pct` is
the only metric still on it.

### What the checks got wrong this round, twice

Both in tests written the same afternoon they cited the rules they broke.

`expect("hb neutral fill", hb$fill, pctile_fill(hb$pctile, "neutral"))` compared
a fill to the function that produced it, so it passed under any palette. And the
first Stuff+ block recomputed the ramp locally from `STUFF_PLUS_SPAN`, so
doubling the span moved the fixture with the constant, and centring the ramp on 0
instead of 100 passed because the check never touched `arsenal_gt`. Entry 5 and
entry 8 together. Both now read out of the rendered table against literal hexes.

Two assertions that pinned HB to the neutral scale were rewritten to assert the
opposite rather than deleted, because a check that stops describing the code
should not just go quiet.

`arsenal_gt_null_baseline.rds` was recaptured twice, each time only after
diffing what moved. The row-wash removal is the case worth remembering: it moved
zero cell text and 84 background declarations, so a text-level diff saw nothing
and the check had to be done on style.
