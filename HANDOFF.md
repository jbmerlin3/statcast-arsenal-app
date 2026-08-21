# Handoff, 2026-08-21

For the next Claude Code agent. Read `CLAUDE.md` first, then this. Where the two
disagree, CLAUDE.md wins on conventions and this file wins on current state.

---

## Read these, in this order

1. **`CLAUDE.md`** — conventions, data contracts, hard rules, and two lists that
   exist because the same mistakes recurred: **"Checks that did not
   discriminate"** (8 entries) and **"gt mechanics that are not obvious"**.
   Read both properly. Six of the eight entries were written after a check
   passed while the thing it guarded was broken.
2. **`PLAN.md`** — eight phases, 0 through 6 complete and ticked. Phase 7 is
   partly done, see below. The backlog at the bottom holds Stuff+ v4 and VAA.
3. **`SESSION_STATE.md`** — running log of what landed and what bit.
4. This file.

## Where the project is

Phases 0 through 6 are done and verified. **Phase 7, deploy, is the current
phase and is partly done.**

| Phase 7 item | State |
|---|---|
| Trim `app_data.rds` | Done. 37 to 27 cols, 49.2 to 22.4 MB |
| Fix the movement plot | Done, and it was not the bug that was reported |
| Schedule the daily chain | Done. launchd, verified by triggering |
| `renv::init()` | **Not started** |
| Deploy to shinyapps.io | **Not started.** Currency decision made, see below |
| README with a screenshot | **Not started** |

**Do not start Phase 8. There is no Phase 8.** After the deploy and README, the
next work is the backlog, and Jonathan chooses what.

## The one decision already made, do not re-litigate

**Deploy currency: bundle the data, print its date on the page.** Ship the three
rds files in the bundle, render "Data through YYYY-MM-DD" in the header from the
data itself, redeploy with `rsconnect::deployApp()` when you want it current.
25 MB, under a minute.

Rejected: fetching the data at startup from a GitHub release asset. It puts a
25 MB download in front of first paint on a portfolio link a stranger opens
once, and turns an honest staleness into an occasional blank error page.

Approved by Jonathan. Build it, do not re-argue it.

## How to work with Jonathan

`CLAUDE.md` has the standing version. What this session added:

- **He wants the measurement, not the reasoning.** "Report the before and after
  on both numbers." "Time it and tell me the number." If you find yourself
  writing "this should be roughly", stop and measure it.
- **Verify by doing, not by reading.** He asked for the launchd job to be
  "verified by triggering it and reading the log, not by reasoning about it",
  and it failed on the first trigger for a reason no amount of reading the plist
  would have found.
- **Report what passed, not only what failed.** Every mutation test round is
  expected to name the mutations that did NOT get caught. Two of the eight
  CLAUDE.md check-list entries came from mutations that passed.
- **Name the line you edited.** After two mis-targeted mutations, he asked for
  the file and line of every mutation, so a mis-fire is visible in the report
  rather than three runs later.
- **One recommendation, argued, plus the one alternative rejected and why.** Not
  a list of options. He says this explicitly and means it.
- **He drives the prose.** Targeted fixes to his writing only.
- Shorter reports. He has asked for this more than once.

## Rules this session established the hard way

**Commit before mutating.** Non-negotiable. `git checkout` to undo a mutation
will also silently revert an uncommitted fix, which happened once.

**A fixture pinning a parameter must use literals, never the constant under
test.** `swings = fl - 1` slides when you change the floor and the check passes.
`swings = 49` does not. CLAUDE.md entry 5, with the counterfactual.

**Test where the app stands, not where the harness stands.** `plot_movement()`
returned invisibly for four commits and Shiny drew a blank white page. Six
suites passed because every one consumed the object with `ggplot_build()` or
`print()`, which work fine on an invisible value. CLAUDE.md entry 8.

**`scripts/phase1_check.R` cannot guard `arsenal_gt` or `plot_movement.`** Both
sit in `EXPECTED_DIFFS`, so they report "differs (expected)" whatever happens
inside them. Breaking the xwOBA format passes it at six diffs. The real guards
are `tests/step3_null_identical.R` (a committed baseline snapshot) and
`tests/step3_render.R`. Keep running phase1_check, it guards everything else.

**`EXPECTED_DIFFS` is six entries.** Not four. An old handoff said four; it
predates the two heatmap entries. If you want to add a seventh to make something
pass, that is the regression decaying, and it is a thing to report, not to do.

## Run this before you touch anything

```bash
cd ~/statcast-arsenal-app
for t in scripts/phase1_check.R tests/step3_null_identical.R tests/step3_render.R \
         tests/step2_states.R tests/phase4_hand_all.R tests/phase6_results.R; do
  printf "%-28s " "$(basename $t)"; Rscript "$t" 2>&1 | grep -E 'PASS|FAIL' | tail -1
done
```

All six pass as of 2026-08-21. If one fails before you have changed anything,
find out why before building on it.

## Paths, and why they are where they are

**The repo is `~/statcast-arsenal-app`. It was under `~/Desktop` and was moved
on 2026-08-21. Do not move it back.** macOS TCC blocks launchd agents from
Desktop, Documents and Downloads. Probed and confirmed: an agent reads `~` and
`~/Library` fine and gets `Operation not permitted` on `~/Desktop`.

**Everything the chain READS must also stay out of those three.** The season
store therefore lives at `~/baseball-store/statcast_clean_2026.rds`, not beside
the source project and not inside the repo, because `data/` would drag 95 MB
into a deploy bundle.

`scripts/phase1_check.R`'s `ORIGINAL` still points at `~/Desktop/Baseball
Questionnaires/...`. That is correct and not an oversight: it is only read to
rebuild the frozen fixture, which is a Terminal task, never something launchd
does.

## The daily chain

`Rscript scripts/update_data.R`. **Never `source()` it.** The chain block is
gated on `sys.nframe() == 0L`, which is TRUE only under Rscript; under `source()`
it is 4 and every step is skipped. That used to fail silently and leave a stale
store behind a clean exit. It now prints "THE CHAIN DID NOT RUN".

Four steps: season store, `league_ref.rds`, `game_logs.rds` (step 4, fails
soft), `app_data.rds`. Step 4 runs before step 3 in the source; that is
deliberate, `ad` is computed early and written last.

**Scheduled**: launchd, `com.jonathanmerlin.arsenal-chain`, daily 06:15, plist
tracked at `scripts/com.jonathanmerlin.arsenal-chain.plist` and installed in
`~/Library/LaunchAgents/`. It invokes `Rscript scripts/chain_entry.R`, not bash,
so macOS attributes any future Full Disk Access grant to R alone.

**Check last night's run with one command:**

```bash
~/statcast-arsenal-app/scripts/chain_status.sh
```

It separates three outcomes that look alike: `RUN OK`, `RUN FAILED` (dumps the
last 15 log lines, exits 1), and a stale `RUN OK` left by a job that never fired
because the laptop was off, flagged past 30 hours.

**Zero rows against finished games is an error, with one exception.** Savant
lags the schedule by hours, so `schedule_final_games()` counts only dates
**before today**. Without that the evening run errors nightly: six games were
final on 2026-08-20 and Savant had posted none of them, which is normal. Do not
"fix" this by counting today.

## Traps that have already cost time

- **`fmt()` does not compound.** The last call on a column wins and receives raw
  values. A marker `fmt` stacked on the xwOBA `fmt` renders `0.32` instead of
  `.320`. Use `text_transform()`, which receives formatted text.
- **The later `tab_style()` wins.** Percentile fills must be applied AFTER the
  pitch-colour reduce or the row fill silently overwrites them.
- **gt stamps a random table id.** Two renders never compare equal. Strip the one
  token; `gt_spec()` in `tests/phase1_artifacts.R` does.
- **IP is thirds, not decimals.** StatsAPI `"5.2"` is five and two outs.
  `ip_to_outs()` parses it, everything stores outs.
- **`game_date` is character everywhere.** Compare with `as.character(the_date)`.
- **Driving date inputs from a browser does not work.** `changeDate`, the
  datepicker API and typing all revert or desync. Only the All/1H/2H presets,
  which go through `updateDateRangeInput()`, move `input$dates`. Drive dates by
  preset or test the functions the server calls.
- **`baseballr` 2.0.0 has no per-player game-log function.**
  `mlb_pitcher_game_logs()` does not exist. `scripts/build_game_logs.R` calls the
  StatsAPI `gameLog` endpoint directly.
- **The filesystem stalls occasionally.** Reads on one file time out for a few
  seconds while R can still read it. It is transient, not corruption; a `.git`
  read failed in the same window once. Retry before diagnosing.

## Known-open, deliberately

- **The percentile answers a narrower question than it looks like.** Percentiles
  rank a pitch against its own `pitch_type`, and the fill asserts quality on a
  diverging scale. A 35th-percentile sinker whiff% and a 35th-percentile slider
  whiff% render identically and do not mean the same thing. A source note now
  says percentiles rank within pitch type and hand, which stops the table
  implying a cross-pitch ranking but does not solve the reading. Per-cell "35th
  among sinkers" labels are deferred. See CLAUDE.md.
- **The results panel wraps awkwardly at mobile width**, stranding the fourth
  stat below a gap. Cosmetic, untouched.
- **The absence branch in `results_panel()` is nearly unreachable by clicking.**
  Every pitcher has a game log, and an empty window usually trips the "No
  pitches" guard first. It exists for the stale-log case. **Do not delete it as
  dead code.**

## Closed, do not re-open from an old doc

**"plot_movement errors with figure margins too large below 600px."** Closed
2026-08-21 as misdiagnosed. Never reproduced at any size. The real fault was the
invisible return. CLAUDE.md keeps the history so it is not re-added from a stale
handoff.
