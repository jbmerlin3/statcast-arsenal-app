# Methodology

How the app turns 2026 Statcast pitches into the numbers and colors on the page. Each rule names the code that implements it.

## League context

Every shaded cell compares one pitcher's pitch with the league's pitchers throwing the same pitch type with the same hand.

- **Pitcher first, then across pitchers.** Each pitcher contributes one value per cell, and the reference is the distribution of those values. Pooling pitches would let one 3,000-pitch starter outweigh thirty relievers. (`scripts/build_league_ref.R`)
- **Precomputed.** `league_ref.rds` stores the mean and 101 quantiles for each pitch type × pitcher hand × batter side × count bucket × metric. A percentile at runtime is one `findInterval()`.
- **Pitcher hand is never pooled for shape metrics.** Raw horizontal break has opposite signs for righties and lefties, so a pooled reference centers where nobody throws, and no sample floor would catch that.
- **Fallback ladder.** A cell needs at least 20 contributing pitchers. Below that the lookup drops batter side, then count bucket. Rate metrics (whiff%, Z-Whiff%, chase% and the like) have one more step: the same pitch type from both hands, since a whiff means the same thing from either side. Thin cells like left-handed curveballs still get a comparison that way. (`LADDER` in `R/league.R`)
- **One season-long reference.** The league distribution barely moves within a season (splitting at mid-June moves whiff% and CSW% medians by about 1% of their interquartile range), so the full-season reference is valid for any window the user picks.

## Sample floors and cell states

A floor applies to the denominator a metric actually divides by, not to pitch count.

| Metric | Denominator | Floor |
|---|---|---|
| Velocity, IVB, HB, VAA, spin, extension, release height and side | pitches | 25 |
| Strike%, CSW%, zone% | pitches | 50 |
| Usage | the pitcher's pitches in that cut | 50 |
| Whiff% | swings | 50 |
| Z-Whiff% | in-zone swings | 60 |
| Chase% | out-of-zone pitches | 50 |
| xwOBA | PA-ending pitches (`woba_denom`) | 50 |
| GB% | balls in play | 25 |

The floors come from an offline resampling study (not included in this repo): draw windows of a given size from a pitcher's pitches of one type and compare the window percentile with his full-season percentile. At 5 to 10 swings a whiff% percentile misses by a median of 27 points; at 51 to 75 swings, 12. Velocity at 5 to 10 pitches misses by 4.9. A mean of a physical quantity settles an order of magnitude faster than a rate, so one floor for every metric would be wrong either way. Z-Whiff% needs 60 in-zone swings to match whiff%'s accuracy at 50.

Every cell resolves to one of four states, decided in one place (`resolve_cell()`):

| State | Meaning | Rendering |
|---|---|---|
| exact | League cell at the full grain | shaded |
| fallback | A coarser league cell answered | shaded, like exact |
| below floor | The pitcher's own sample is under the floor | shaded, with its denominator in parentheses |
| no reference | No league cell at any grain | unshaded, grey italic, double dagger |

Short windows are shaded, not withheld. A scout asking about one start wants to know what happened that day against what the league does over a season, and a small sample answers that exactly. The `(n)` beside a below-floor value is the only thing qualifying it, so it is never dropped. A value that can't be computed at all (a pitch nobody swung at) stays white.

Exact and fallback cells look the same: a fallback still compares against at least 20 pitchers. The other two states are marked by fill, text or a printed marker, never by hue alone, so they survive a greyscale screenshot.

## Metric definitions

| Metric | Definition |
|---|---|
| Whiff% | Whiffs over swings. Foul tips and missed bunts are whiffs; bunt attempts are swings. |
| CSW% | Called strikes plus whiffs, over all pitches. |
| Chase% | Swings at out-of-zone pitches, over out-of-zone pitches. |
| Zone% | In-zone pitches over all pitches. |
| Z-Whiff% | Whiffs on in-zone swings, over in-zone swings. |
| Strike% | Pitches with `type` S or X, over all pitches. |
| xwOBA | `sum(estimated_woba_using_speedangle * woba_denom) / sum(woba_denom)`. Only PA-ending rows have a nonzero `woba_denom`. |
| HH% | Batted balls (`type == "X"`) at 95+ mph, over all batted balls. Untracked batted balls stay in the denominator, as on FanGraphs. Statcast also measures exit velocity on fouls, so "rows with an exit velocity" roughly halves the rate. |
| GB% | Ground balls over balls in play. `bb_type` is the empty string, not NA, on other rows. |
| RV | Sum of `delta_pitcher_run_exp`: runs saved, positive is good for the pitcher. A counting stat, so it takes no percentile. |

**The strike zone** includes the ball's radius on both axes, since a pitch is a strike if any part of the ball crosses: `|plate_x| <= 0.8291` ft (half the 17 in plate plus 0.1208 ft) and `sz_bot - 0.1208 <= plate_z <= sz_top + 0.1208`.

These definitions are checked against Savant's custom leaderboard by `scripts/verify_savant.R`. On the final 2026 data (141 pitchers with 415+ PA), mean absolute error in percentage points: whiff% 0.031, HH% 0.041, chase% 0.064, zone% 0.098. Anything above about 0.3 points would mean a definition difference, not noise.

## Movement and release

- **IVB** is `pfx_z * 12` and **HB** is `-pfx_x * 12`, in inches. HB reads arm side positive for a right-hander only.
- **HB is displayed raw and ranked mirrored.** The tables and charts show the true direction. Before ranking, a left-hander's HB is multiplied by -1 so the league reference (stored arm side positive) means the same thing for both hands. (`arm_side_sign()`)
- **Release side** is `-release_pos_x`: positive toward third base, so a righty reads positive and a lefty negative. It is not mirrored, because release side never crosses the midline and its sign is just handedness. Percentiles are within hand anyway.
- **Direction depends on the pitch for shapes.** Ride is good on a four-seam and bad on a sinker; arm-side run is what a changeup wants and the opposite of what a sweeper wants. IVB, HB and VAA take their direction per pitch type from `PITCH_SHAPE_DIRECTION`. A knuckleball is neutral.
- **IVB and HB are shaded by inches from the league average** for that pitch type, saturating at 6 inches (about the 95th percentile of differences), instead of by rank. A rank means different things in a cutter's tight IVB spread and a curveball's wide one. The percentile is still reported.
- **Release height and side are shaded by distance from the median in either direction** (`2 * |p - 50|`). An unusual slot is what plays, whichever way it is unusual. Because the fill loses direction, the value is always printed beside it. Extension is not treated this way: more is simply better.

## VAA

Savant publishes no approach angle, so the app derives it from the trajectory fit at the front of the plate (17/12 ft):

```
t   = (-vy0 - sqrt(vy0^2 - 2 * ay * (50 - 17/12))) / ay
VAA = atan((vz0 + az * t) / |vy0 + ay * t|) * 180 / pi
```

Raw VAA depends heavily on where the pitch ends up: the same fastball crosses flatter at the top of the zone than at the knees. Its within-pitcher spread was 1.5 times the spread between pitchers, and a percentile needed 148 pitches to settle. The shipped VAA is location-adjusted: per pitch type, regress VAA on a quadratic in `plate_z`, take the residual, and add back the fit at that type's mean plate height so it still reads in degrees. The adjusted value correlates 0.99 with raw at the pitcher level and settles in 19 pitches. Pitch types with fewer than 2,000 pitches keep the raw angle. (`adjust_vaa_for_location()`)

## Pitch codes

The app charts ten pitch types: FF, SI, FC, SL, ST, CU, CH, FS, SV and KN. Other Savant codes are mapped or dropped by `PITCH_CODE_RULES`, and the note above the tabs says what happened ("39 CS remapped to CU").

- CS (slow curve) and KC (knuckle curve) map to CU. Savant sometimes labels one pitcher's single curveball both ways.
- FO (forkball) maps to FS.
- EP, FA, PO and UN are dropped: almost all EP and FA throwers are position players, and the others aren't pitch types.
- An unknown future code is dropped and reported rather than crashing a chart.

A pitch type under 1% of the window and under 10 pitches is left off the charts but kept in every table, and the note names it. Both conditions are required: share alone would hide a real 15-pitch cutter over a season, and count alone would hide 3 sliders out of 19 in one start.

## Count buckets

Tables use six overlapping situational buckets (All Counts, Early Count, Pitcher Ahead, Pitcher Behind, Pre Two Strikes, Two Strikes), so a row does not sum to 100. Heat maps use two buckets that partition every count (Pre-2K and 2K), because a density estimate needs more pitches per panel than a usage percentage.

## Results panel

The panel has two rows from two sources, kept separate because they support different cuts.

- **Statcast row** (TBF, K-BB%, HH%, xwOBA) follows the date window and the batter side.
- **Game-log row** (IP, ERA, WHIP, FIP) comes from MLB StatsAPI game logs and follows the dates only. A game log has no platoon split, so a vs-RHH ERA doesn't exist.
- **IP is not derivable from pitch data.** A double play is one row in `events`. Innings come from the game logs, where "5.2" means five innings and two outs. Everything stores outs. (`ip_to_outs()`)
- **The FIP constant is derived** from the same game logs (league FIP equals league ERA), not taken from a published value.
- **Batters faced** excludes `truncated_pa` (an inning that ended mid plate appearance). Intentional walks throw no pitch, so TBF reads one low per IBB; the panel notes it when the window has one.
- **League shading** is computed over the same window and batter side as the panel, because IP is a counting stat: a two-week IP can't be ranked against season totals. The floor is half the median denominator among pitchers in the window.

## Stuff+

Stuff+ grades come from a separate model (v2), built in its own repository: how often a pitch's shape misses bats when swung at, validated on the held-out 2026 season. 100 is the average pitch within its group (fastball: FF, SI, FC; offspeed: CH, FS; hard breaker: SL; soft breaker: ST, SV, CU), and 10 points is one standard deviation within the group. It reads velocity, movement, release point, the gap to the pitcher's hardest fastball, batter side and the gap between movement and spin axis; never the pitch label, count or location.

The 2026 grades ship as daily sums in `lookups/stuff_v2_2026.csv`, so a grade follows the date window and the batter side selector. "All" weights both batter sides 50/50. A grade compares a pitch with its own group: a 110 sinker and a 110 sweeper are each well above their group, not equally nasty.

## What the percentiles don't claim

Percentiles rank a pitch within its own pitch type, which is narrower than the color scale suggests. A 35th-percentile sinker whiff% and a 35th-percentile slider whiff% render the same color and mean different things, because sinkers miss few bats by design. The table's note says so, but the shared color scale still invites the comparison.

## Decisions and rejected alternatives

| Decision | Reason |
|---|---|
| No "All" option in the Search pitch type | Every fill is a within-pitch-type percentile; mixing types would make the colors assert cross-pitch quality. |
| Search filters on traits only, never outcomes | A filter on whiff% finds pitches that already worked; filtering on shape and then reading the outcome is the useful question. |
| Team filter narrows rows but never groups | 121 pitchers threw for more than one club in 2026. Grouping would split each into thin rows; a traded pitcher reads "NYM/TB". |
| No RV/100 | Its split-half reliability is 0.02 to 0.10 at every sample size (one home run is -2.7 runs), so its color would be noise. RV alone states what happened. |
| Short windows shaded, not greyed | Lowering floors prints noise in color; shrinkage changes calibration but not ranking and splits the number shown from the number colored; a window-matched reference would multiply `league_ref` by every window size. |
| No fill on the Search table | Fifty rows of one pitch type reads as a wall of color, and the sorted column is the comparison the user asked for. |
| Tabs inline in `app.R`, not Shiny modules | No tab is instantiated twice, and the Search tab writes to the global pitcher selector, so a module would add indirection without isolation. Revisit if a comparison view needs the same tab twice. |
| Data bundled with the app | Fetching about 45 MB at startup would slow first paint and turn staleness into an error page. The cost is that the chain must redeploy whenever the data changes. |
