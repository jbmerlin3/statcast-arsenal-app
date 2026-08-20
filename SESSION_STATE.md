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
