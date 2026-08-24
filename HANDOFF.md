# Handoff, 2026-08-24

For the next Claude Code agent. Read `CLAUDE.md` first, then this. Where the two
disagree, CLAUDE.md wins on conventions and this file wins on current state.

This supersedes the 2026-08-21 handoff entirely. That one described Phase 7
before renv, before the search tab, and before a week of metric fixes.

---

## Read these, in this order

1. **`CLAUDE.md`.** Conventions, data contracts, hard rules, and two lists that
   exist because the same mistakes recurred: **"Checks that did not
   discriminate"** (nine entries now) and **"gt mechanics that are not
   obvious"**. Read both properly. Entry 9 was added this week and it is the one
   that will bite you: a clean `Rscript` session is not where the app stands.
2. **`PLAN.md`.** Phases 0 to 6 done. Phase 7 is one item from finished.
3. **`SESSION_STATE.md`.** Running log. The last six entries are this week.
4. This file.

## The tree is clean, and what just landed

Everything is committed as of `06e89bf`. The last four commits are one arc, and
if you are picking this up cold they are the fastest way to understand the
current shape of the app:

```
06e89bf  Colour IVB and HB by distance from the league, not by rank
8204b8e  Rewrite the handoff for the next agent
ff13c17  Grade the panel against the league, and the arsenal table by pitch shape
a684ca6  Add a pitch trait search tab
```

In one line each: the results panel grades seven of its eight numbers against
the league over the same window and batter side; the Characteristics table went
white-rowed with the pitch code holding the only pitch colour; HB is arm-side
normalised on the comparison surfaces and raw on the movement chart; IVB and HB
grade by pitch type and colour by inches off the league rather than by rank; the
search table has no fills.

## Run this before you touch anything

Seven suites now, not six. `tests/step7_search.R` is the new one.

```bash
cd ~/statcast-arsenal-app
for t in scripts/phase1_check.R tests/step3_null_identical.R tests/step3_render.R \
         tests/step2_states.R tests/phase4_hand_all.R tests/phase6_results.R \
         tests/step7_search.R; do
  printf "%-28s " "$(basename $t)"; Rscript "$t" 2>&1 | grep -E 'PASS|FAIL' | tail -1
done
```

All seven pass as of 2026-08-24. If one fails before you have changed anything,
find out why before building on it.

## Where the project is

Phase 7 is done except the deploy.

| Item | State |
|---|---|
| Trim `app_data.rds` | Done. 28 cols, 23.6 MB |
| `renv::init()` | Done. 83 packages, R 4.5.2 |
| Schedule the daily chain | Done. launchd 06:15, verified by triggering |
| README with a screenshot | Done. Two shots in `docs/` |
| **Deploy** | **Not done. Credentials ARE registered** |

`rsconnect::accounts()` returns `jonathanmerlin / shinyapps.io`, so
`setAccountInfo()` has already been run on this machine. `scripts/deploy.R` is
written and unrun. It bundles app.R, R/, data/ and fg_stuff/, about 26 MB.

```bash
cd ~/statcast-arsenal-app && Rscript scripts/deploy.R
```

**Confirm with Jonathan before running it.** The free tier URL is public.

## How to work with Jonathan

`CLAUDE.md` has the standing version. What this week added:

- **Measure, then decide, and put the number in the message.** Every design call
  here was settled by a probe: 44.7 s versus 5.5 s on a render, 16% versus 86%
  of pitchers cleared by a floor, 0.032 MAE against Savant. He responds to
  numbers and ignores adjectives.
- **One change at a time against the source.** A confounded measurement talked
  me out of a real fix: I tested bunts while `foul_tip` was still missing from
  the whiff numerator, the two errors cancelled, and I recommended leaving a bug
  in place. Recorded in CLAUDE.md's formula conventions.
- **He iterates on the UI fast and reverses himself.** The percentile fill was
  removed from the arsenal table and then put back two turns later with
  different rules. Do not argue the previous version back; build what he asked
  and name the one consequence he may not have seen.
- **Name the trade, then do the thing.** He wanted HB coloured by raw sign;
  flagging that a righty and a lefty with identical shape would render opposite
  colours got the better design out of him in one turn.
- **Plan mode for anything structural.** Two features this week went through it
  and both plans were approved with the recommended option.
- Shorter reports. Verdict first, no em dashes, name the judgment call.

## Rules this week established the hard way

**Verify where the app stands, not in the harness.** Four separate bugs this
week were invisible to every suite and obvious in the browser within a minute: a
masked `margin()` blanking two tabs, a 44.7 s render, a cap note describing the
wrong sort, and slider ticks colliding. Start the app and drive it.

**A fixture that reads the constant under test pins nothing.** Twice this week,
in checks I wrote the same afternoon I cited the rule. `STUFF_PLUS_SPAN` in the
expectation slid when the constant did; `pctile_fill(hb$pctile, "neutral")`
compared a fill to the function that made it. Write literals.

**Own the crash.** Three mutations were "caught" by stack traces rather than
assertions, which stops the file and hides everything after it. Gate on
preconditions, or `tryCatch` into a marker value. CLAUDE.md entry 7.

**A check that stops describing the code has to move, not go quiet.** Two
assertions pinned HB to the neutral scale; when it went directional they were
rewritten to assert the opposite rather than deleted.

**Recapturing a baseline is the move that hides regressions.** Diff by column
first, and if the change is style-only, diff by style: the row-wash removal
moved zero cell text and 84 backgrounds. The rule is in
`tests/step3_null_identical.R`'s header.

## Things that will surprise you

**`phase1_check.R` sanctions at COLUMN granularity now.** `EXPECTED_COL_DIFFS`
holds `whiff_pct`, `csw_pct`, `chase_pct` and `hb` on both arsenal-table
artifacts. Everything else in those frames is still compared byte for byte, and
each sanctioned column is asserted to ACTUALLY differ, so reverting a change
fails the check instead of quietly satisfying it. Adding an entry to make
something pass is still the regression decaying; adding one for a deliberate,
measured change is what it is for. Say which in the message.

**The fixtures are tracked now.** `tests/fixtures/*.rds` used to be gitignored,
so they regenerated on every clone from a season store that grows nightly: the
fixture went 2,261 pitches on 08-19 to 2,346 on 08-23. A byte-identity baseline
compared against a different fixture is not a check. 217 KB, committed, with a
`!tests/fixtures/*.rds` negation in `.gitignore`.

**Three surfaces disagree about HB on purpose.** `arsenal_table()` and
`league_ref` store it arm-side normalised, positive means arm side for both
hands. `movement_ref()` converts back, because a lefty's slider really does
sweep the other way and the league cross has to land on his pitches.
`app_data` keeps the raw signed value. One helper, `arm_side_sign()`, does the
conversion in all three places.

**Direction is a property of the pitch, not the metric.** `PITCH_SHAPE_DIRECTION`
in `theme.R` says ride is good for FF and FC, drop is good for everything else,
arm side is good for FF SI CH FS, glove side for the rest. All eleven codes are
written out with no default: an unlisted code stops the render. Never read
`METRIC_SPEC$direction` for `ivb` or `hb`; use `metric_direction()`.

**IVB and HB colour by MAGNITUDE, everything else by rank.** Those two are the
distance from the league average for that pitch type, in inches, saturating at
`SHAPE_DELTA_SPAN` of 6. A rank is not comparable across rows, which was the
complaint: 90th percentile is one inch in a cutter's IVB spread and four in a
curveball's. `pctile` still reports the true rank, so a cell can read 90th and
look pale. Do not "fix" that.

**The neutral palette is symmetric grey, not a rank ramp.** Average is `#F0F0F0`,
the same stop the diverging scale uses, and both ends darken. `usage_pct` is the
only metric still on it.

## Validating a rate against a public source

This is the technique that found four wrong denominators in two days. Savant's
custom leaderboard serves CSV.

```r
u <- paste0("https://baseballsavant.mlb.com/leaderboard/custom?year=2026&type=pitcher",
            "&filter=&min=100&selections=player_id,pa,whiff_percent,oz_swing_percent,",
            "hard_hit_percent,in_zone_percent&csv=true")
sv <- readr::read_csv(u)   # player_id arrives twice, hence player_id...2
```

Compare per pitcher and read the MEAN ABSOLUTE ERROR, never one player. Current
state, 105 pitchers with 100+ PA: whiff 0.032, chase 0.060, zone 0.105, HH 0.037.
Anything above about 0.3 is a definition difference and not noise.

## Known-open, deliberately

- **The percentile answers a narrower question than it looks like.** Ranks are
  within pitch type and hand. Now that the fill also encodes what each pitch
  WANTS, this is less wrong than it was, but a 35th-percentile sinker and a
  35th-percentile slider still render alike.
- **catcher_interf sits in the xwOBA denominator with no numerator.** 73 PA, 66
  pitchers, median shift 0.001, worst 0.013.
- **`truncated_pa` counts as a batter faced.** 267 rows, median K-BB% shift 0.000.
- **An empty count bucket renders 0.0%** rather than saying the count never
  happened. Two Strikes is empty for 12 pitchers on the season.
- **Bunts, foul tips and the zone are settled**, do not reopen: all three were
  measured against Savant and the numbers are in CLAUDE.md.
- **`03_ArsenalReports` was aligned on 2026-08-23 and is UNCOMMITTED in the
  Questionnaires repo.** Three engine files. Jonathan then said to focus on this
  app only, so leave that repo alone unless he raises it.

## Closed, do not re-open from an old doc

**"plot_movement errors with figure margins too large below 600px."** Closed
2026-08-21 as misdiagnosed, twice over. It was an invisible return, and the
`invalid quartz() device size` you may see in a headless browser is the viewport
being torn down mid-resize, not a bug. CLAUDE.md keeps the history.

## Paths, and why they are where they are

**The repo is `~/statcast-arsenal-app`.** It was moved off `~/Desktop` on
2026-08-21 because macOS TCC blocks launchd agents from Desktop, Documents and
Downloads. Do not move it back. Everything the chain READS must also stay out of
those three, which is why the season store lives at
`~/baseball-store/statcast_clean_2026.rds`.

## The daily chain

`Rscript scripts/update_data.R`. **Never `source()` it**, the chain block is
gated on `sys.nframe() == 0L`.

Four steps: season store, `league_ref.rds`, `game_logs.rds` (step 4, fails
soft), `app_data.rds`. **`refresh_store()` now recomputes `in_zone` and
`pitch_team` on every path out**, including the two that pull nothing, because a
derived column that only refreshes when new rows arrive reaches the file on the
next game day and not before.

Rerun the chain after any change to a stored derived column or to
`build_league_ref.R`. Takes about five minutes, most of it the game logs.

```bash
~/statcast-arsenal-app/scripts/chain_status.sh
```

Last run 2026-08-24 06:19, data through 2026-08-23.
