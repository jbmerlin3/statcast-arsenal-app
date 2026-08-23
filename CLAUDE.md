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


## game_logs.rds, and the IP trap

**game_logs.rds.** Step 4 of the daily chain. One row per pitcher per game date,
columns `pitcher`, `game_date`, `ip_outs`, `er`, `h`, `bb`, `so`, `tbf`, `hr`,
`hbp`. The only source for IP, and therefore for ERA, WHIP and FIP: `events` in
app_data is one row per plate appearance, so a double play reads as one out and
IP derived from it is wrong.

`baseballr` 2.0.0 has **no per-player game-log function**. `mlb_pitcher_game_logs()`
does not exist under that name, `mlb_player_game_stats()` takes one `game_pk` at
a time, and `mlb_stats()` has no `player_id` argument. `scripts/build_game_logs.R`
calls the StatsAPI `gameLog` endpoint directly, one request per pitcher. Measured
2026-08-20: 0.246 s each, 809 pitchers in 3m45s.

It is the only input the app tolerates missing. Steps 1 to 3 are pure transforms
of one store; this one depends on somebody else's uptime, so step 4 fails soft,
leaves the previous file in place and warns. The panel prints the log's own max
game date, the same way the arsenal table prints the FanGraphs export window, so
staleness is visible on the page rather than silent.

**IP is written in thirds, not decimals.** StatsAPI returns `"5.2"` meaning five
innings and two outs, NOT five and two tenths. Read as a decimal it understates
a `.1` and overstates a `.2`, and the error compounds across a season without
ever looking wrong. `ip_to_outs()` in `R/results.R` parses it and everything
downstream stores **outs**, so nothing else has to remember. `outs_to_ip()`
formats it back for display. One parser, used by both the chain and the app.

**cFIP is derived, not hardcoded.** FIP's constant is whatever makes league FIP
equal league ERA, so it comes from the same population the panel's FIPs are
computed against. `fip_constant()` computes it off the whole log file; on 2026
it lands at 3.088, against the 3.10 that gets quoted from memory.
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

6. **Hard hit is off BATTED BALLS, not off rows with an exit velocity.**
   Statcast measures fouls, and fouls are weak, so `!is.na(launch_speed)` as a
   denominator roughly halves the rate and nothing errors. Kirby 2026 vs LHH:
   435 rows carry an EV, 206 of them fouls at a mean 78.1 mph, and HH% read 28.3
   against FanGraphs' 45.4. On `type == "X"` it is 104 of 229, 45.4, and the
   event counts match FanGraphs exactly on both sides.

   Untracked batted balls stay in the denominator, which is what FanGraphs does:
   80 of 205 vs RHH reads 39.0, not the 39.4 you get over the 203 with an EV.

   `bb_type` says the same thing and is NOT in `app_data`, dropped by the Phase 7
   trim as unread. `type` is the in-play flag and is there.

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

   Recurred 2026-08-20, and not in a test file. A human verification
   instruction said to confirm `arsenal_gt(ref = NULL)` is byte-identical by
   running `phase1_check.R` at six diffs, but all three `arsenal_gt` artifacts
   were already sanctioned, so the check could not fail on that function at
   all. Breaking the xwOBA format passed. The failure mode is not confined to
   test code; it lands in instructions about tests just as easily.

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

- whiff% is off swings, not off total pitches. **A foul tip is a whiff, a bunt
  attempt is a swing, and a missed bunt is both.** Savant counts them that way
  and the numbers say so, over 105 pitchers: misses / swings ran 2.10 points
  under Savant's whiff%, foul_tip took it to 0.087, and the bunts took it to
  0.032 with chase% going 0.192 to 0.060. `whiff_desc` is also the whiff half of
  CSW%, so both move together. `swinging_pitchout` is left out: 1 row all season
- **Measure one change at a time against the source.** An earlier pass tested
  the bunts while foul_tip was still missing from the numerator, read the result
  as a wash, and recommended leaving it. The two errors ran in opposite
  directions and cancelled. A confounded measurement is worse than none: it
  talks you out of a real fix with a number attached
- hard hit% is off batted balls, `type == "X"`, not off rows carrying a
  launch_speed. Statcast tracks fouls. See hard rule 6
- chase% is off out-of-zone pitches
- CSW% is `(called_strike + whiffs) / total pitches`
- in_zone carries a ball radius on BOTH axes, because a pitch is a strike if any
  part of the ball crosses the zone. `plate_x` within +/- 0.8291, which is the
  8.5 inch plate half-width plus the 0.1208 ft ball radius, and `plate_z` within
  `sz_bot - 0.1208` to `sz_top + 0.1208`. The vertical radius was missing until
  2026-08-23, which put zone% 4.6 points under Savant's and chase% 2.2 over
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

## Validating a rate against Savant

Savant's custom leaderboard serves CSV, which is how the foul-tip and zone
findings were settled in one pass rather than argued. It is the same kind of
request the chain already makes.

```r
u <- paste0("https://baseballsavant.mlb.com/leaderboard/custom?year=2026&type=pitcher",
            "&filter=&min=100&selections=player_id,pa,whiff_percent,oz_swing_percent,",
            "hard_hit_percent,in_zone_percent,swing_percent&csv=true")
sv <- readr::read_csv(u)   # the player_id column arrives twice, hence player_id...2
```

Compare per pitcher and read the MEAN ABSOLUTE ERROR, not one player: a single
match can be luck. As of 2026-08-23, over 105 pitchers with 100+ PA:

```
HH%      denominator type == "X"           MAE 0.037
whiff%   foul tips and bunts counted       MAE 0.032
chase%   same swing set, same zone         MAE 0.060
zone%    ball radius on both axes          MAE 0.105
```

Anything above about 0.3 is a definition difference and not noise. The versions
that were live before this pass sat at 2.10, 4.65 and 2.19.

`03_ArsenalReports/engine` carries the same four definitions and was aligned on
2026-08-23. Its three files spelled the swing set out nine times between them,
three different ways inside one file. If you change a definition in one repo,
change it in the other: they produce numbers that get compared side by side.

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


6. **An explicit constraint with no test behind it.** "Every reference line
   carries the n it was built from" was written into the plan and into the code,
   and deleting the `geom_text` layer that carried it passed every check in
   `tests/step3_render.R`. Found by mutation, not by review.

   The tell is that the constraint lived in prose and in one line of drawing
   code, with nothing in between. A stated requirement that no assertion names is
   a comment, whatever it is written on. When a constraint is worth stating, ask
   what would fail if it were quietly removed; if the answer is nothing, that is
   the missing test.

7. **A check caught by a crash rather than by an assertion.** Two mutations,
   `below_floor` set to filled and `count_usage_gt`'s early return removed, were
   "caught" by errors inside `gt::cell_fill(color = NA)` and `nrow(NULL)`. That
   proves those functions are intolerant of bad input, not that anything was
   guarding the behaviour, and both would go quiet the moment the downstream code
   got more permissive.

   Fixed by asserting the invariant each crash stood in for: every resolved fill
   is a real hex, and every render returns a `gt_tbl`.

   **Some invariants can only be violated by an error, and that is fine. The
   check's job there is to OWN the error rather than die on it.** Removing a
   function's early return leaves it holding a NULL where it needs a list; no
   assertion can stop that from raising. What was wrong was not the error, it
   was that the check reported an uncaught stack trace instead of a named
   failure, and stopped the file before the remaining checks ran. So the rule is
   not "make it not crash", it is "decide, in the check, what the crash means".
   Any helper that renders, builds, or evaluates inside a check should catch and
   return a marker value, so a dead call fails the comparison it belongs to and
   everything after it still runs.

8. **Testing where the app does not stand.** `plot_movement()` ended on an
   assignment, so it returned INVISIBLY, and `renderPlot()` relies on
   auto-printing. Shiny drew a blank white device with no error anywhere: not in
   the log, not in the browser console, not in any of six suites. Live for four
   commits.

   Every test consumed the returned object directly, `ggplot_build(p)` or
   `print(p)`, and **both work perfectly on an invisible value**. The one
   consumer that depended on visibility was Shiny, and no test stood where Shiny
   stands. The checks were not weak, they were in the wrong place.

   Ask what the real consumer does differently from the test harness. Here it
   was auto-printing; elsewhere it could be lazy evaluation, a different
   environment, or a device. `tests/step3_render.R` now asserts visibility for
   all four plot functions, verified by mutation.

9. **A clean session is not where the app stands.** All six suites passed while
   the movement chart and the heat maps were dead in Jonathan's RStudio session,
   with `argument "observed" is missing, with no default` on those two tabs and
   nothing wrong on the other two.

   `randomForest` exports `margin(x, observed, ...)`. Attached after ggplot2 in
   a console that has been open for days, it masks `ggplot2::margin`, and the
   only two plots that set a plot margin are the movement chart and the heat
   maps. `library(ggplot2)` at the top of app.R does NOT fix the order: on an
   already-attached package `library()` is a no-op and leaves the later
   attachment in front.

   Every suite runs under a fresh `Rscript`, where the search path holds exactly
   what the file attaches, so no suite could ever see this. Same species as
   entry 8, one level up: entry 8 was a consumer difference, auto-printing, and
   this is an environment difference, the search path.

   Fixed by writing `ggplot2::margin()` out in full, and guarded in
   `tests/step3_render.R` by defining a masking `margin()` and asserting both
   plots still draw. Verified by mutation: bare `margin()` fails it, qualified
   passes. The general exposure was measured rather than guessed. The only
   ggplot2-side clash across every installed package is this one; the rest are
   dplyr-side, `plyr` over mutate, arrange, count, summarise and rename, `MASS`
   over select, `stats` over filter.

## gt mechanics that are not obvious

Both of these were found by probing gt, not by reading it, and both produce a
wrong page rather than an error.

**`fmt()` does not compound. The last call on a column wins, and it receives the
raw values.** Stacking a marker `fmt` on top of `arsenal_gt()`'s xwOBA `fmt`
silently drops the leading-zero rule and renders `0.32` where the table has
always shown `.320`. A column needing both a format and a suffix needs them in
one `fns`, not two calls.

**The later `tab_style()` wins.** `arsenal_gt()` ends by folding one pitch-colour
fill per pitch type across every body cell, so percentile fills must be applied
**after** that reduce. Applied before, the pitch colour silently overwrites them
and the context strip renders as the identity block.

**gt stamps one random table id** into rendered HTML, used as `id="..."` and in
every CSS selector, so two renders of the same object never compare equal.
Strip that one token and the render is deterministic. That is what
`gt_spec()` in `tests/phase1_artifacts.R` does, and why it compares the rendered
page rather than the internal spec.


## The percentile answers a narrower question than it looks like

`league_ref` is keyed by `pitch_type`, so every percentile in the arsenal table
is a rank **against that pitch type**, not against pitches. A 35th-percentile
sinker whiff% and a 35th-percentile slider whiff% render the same colour and do
not mean the same thing: sinkers miss few bats by design, sliders are supposed
to. The arithmetic is right, the fill is consistent, and the reading is wrong.

Same class as hb not being arm-side normalised, and harder, because hb flips on
one visible attribute while this varies continuously by pitch type and by role.
Worse in one way too: hb is neutral violet, which hints the cell does not grade,
while this renders on the diverging good-to-bad scale and asserts quality.

Open. See SESSION_STATE.md.
## Style

- R first, tidyverse, explicit `library()` calls at the top of each file
- Comment the why, not the what
- Anything over about 30 lines becomes a named function
- No em dashes anywhere, including in code comments and UI copy
- gt for tables, ggplot2 for plots, existing formatting preserved


## Closed as misdiagnosed, do not re-open from an old doc

**"plot_movement errors with figure margins too large below about 600px of panel
width."** Recorded in an earlier SESSION_STATE and carried forward for several
sessions. Closed 2026-08-20. The history is kept here rather than deleted so it
is not re-added from a stale handoff.

It was never reproduced. Swept 120px to 900px wide, heights 140px to 1240px, at
res 96 and 192, square and short, on the same device Shiny renders through. The
error never fired once.

What was actually wrong was that **`plot_movement()` returned invisibly**. The
Phase 5 restructure left it ending on `g <- g + theme(...)`, an assignment, and
a function ending in an assignment returns invisibly. `renderPlot()` relies on
auto-printing, so Shiny drew a blank white device with nothing in the log and
nothing in the browser console. Blank at every width, desktop included, which is
also why "below 600px" never held up.

The lesson is in the check list above, entry 8: every test consumed the returned
object with `ggplot_build()` or `print()`, both of which work fine on an
invisible value. Shiny was the only consumer that depended on visibility and no
test stood where Shiny stands. `tests/step3_render.R` now asserts visibility for
all four plot functions.
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
