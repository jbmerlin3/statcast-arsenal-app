# Data pipeline

The app reads precomputed files and never calls Savant while someone is on the page. One script, `scripts/update_data.R`, builds those files and deploys them. During the season it ran every morning in GitHub Actions. The 2026 regular season is complete, so the scheduled refresh is off until 2027; the workflow can still be started by hand, for example to deploy a code change.

## The chain

`Rscript scripts/update_data.R` runs these steps in order:

1. **Season store.** Load `statcast_clean_2026.rds`, pull new days from Savant plus a re-pull window, keep regular-season pitches with a pitch type, velocity and movement, and dedupe on `game_pk` / `at_bat_number` / `pitch_number`. Freshly pulled rows go first, so a revised pitch replaces the stored one.
2. **League reference.** Rebuild `data/league_ref.rds` from the season (`scripts/build_league_ref.R`).
3. **Game logs.** Rebuild `data/game_logs.rds` from the MLB StatsAPI, one request per pitcher (`scripts/build_game_logs.R`).
4. **App data.** Write `data/app_data.rds`, the trimmed column set the app reads.
5. **League pool.** Precompute the Search and Context league tables for the preset windows (`data/league_pool.rds`).
6. **Deploy.** Bundle the app with its data and push to shinyapps.io (`scripts/deploy.R`). Set `SKIP_DEPLOY=1` to stop before this step.

Steps 3 and 5 fail soft: the previous game logs stay in place (the panel prints their last date), and without the pool the app computes those tables live. Any other failure fails the run. A failed deploy in particular must fail loudly, since the live page would otherwise keep showing old data with nothing to say so.

### Savant details

- `game_date_gt` and `game_date_lt` are both inclusive, despite the names. The pull starts at the day after the store's last date.
- Savant revises pitch classifications for days after a game and fills in arm angle later still, sometimes weeks. The workflow re-pulls the last 21 days (`REPULL_DAYS`; a local run defaults to 7). A one-off `repull_days=200` from the workflow's manual trigger re-pulls the whole season.
- An empty pull on a day with finished games is a failure, not a no-op. The chain checks the MLB schedule first, counting only final games before today in Eastern time (the runner is on UTC). When Savant hasn't posted yet, the workflow retries every 20 minutes for up to two hours.
- After the last regular-season day (`SEASON_LAST_DAY`, 2026-09-27) the chain makes no Savant requests.

## Data files

All under `data/`, gitignored and rebuilt by the chain. At the end of the 2026 season:

| File | Contents |
|---|---|
| `app_data.rds` | One row per regular-season pitch, trimmed to `APP_DATA_COLS`: 714,111 pitches by 868 pitchers, 34 columns, 38 MB. |
| `league_ref.rds` | Mean and 101 quantiles per pitch type × pitcher hand × batter side × count bucket × metric, with the pitcher count behind each. |
| `game_logs.rds` | One row per pitcher per game: outs, ER, H, BB, SO, TBF, HR, HBP, IBB. |
| `league_pool.rds` | Search and Context league tables for the full season and each half, stamped with the app data and code that built them. The app ignores a pool built for other data or code. |

Data contracts worth knowing:

- `game_date` is character (`"YYYY-MM-DD"`), not Date. Compare against `as.character(date)`.
- `pt_n` (pitches of a type) and the usage ordering depend on the selected window, so they are computed per query and never stored.
- VAA is derived from seven trajectory columns that stay in the season store; only VAA ships.

Tracked lookups under `lookups/` change rarely and are refreshed by hand, outside the chain:

| File | Built by |
|---|---|
| `pitcher_heights_2026.csv` | `scripts/build_pitcher_heights.R` |
| `position_players_2026.csv` | `scripts/build_position_players.R` |
| `stuff_v2_2026.csv`, `stuff_v2_scale.csv` | exported from the [Stuff+ model repository](https://github.com/jbmerlin3/stuff-plus) |

## Deploy

The data ships inside the app bundle (about 50 MB), so new data reaches visitors only through a redeploy. `scripts/deploy.R` bundles `app.R`, `R/`, `data/` and `lookups/`; `scripts/` and `tests/` never ship.

Each deploy writes a stamp with the data dates, the commit and a digest of the app data's values. The next run deploys only if one of those changed. The digest is taken over the data in a canonical row order, because the store's row order shifts with Savant's response order while the values don't. This matters on the free tier: each deploy keeps an instance awake for about 15 minutes, 1% of the 25 active hours a month.

`rsconnect/` records the shinyapps.io app id and account name. It holds no credentials; CI authenticates from repository secrets (`scripts/ci_auth.R`).

## Scheduling

The workflow is `.github/workflows/refresh-and-deploy.yml`. GitHub's built-in `schedule` trigger was unreliable for this repository: it didn't fire at all at first and later ran hours late. So during the season the run was started externally:

- **cron-job.org** called the workflow_dispatch API each morning at 06:15 Eastern, when Savant reliably has the previous night's games. The call needs a `User-Agent` header and a fine-grained token limited to Actions read/write on this repository:

  ```
  POST https://api.github.com/repos/jbmerlin3/statcast-arsenal-app/actions/workflows/refresh-and-deploy.yml/dispatches
  Authorization: Bearer <TOKEN>
  Accept: application/vnd.github+json
  User-Agent: statcast-arsenal-cron
  {"ref":"main"}
  ```

- The fine-grained token expires. When it does, runs stop silently, which is what the watchdog below catches.

For 2027, set `SEASON_LAST_DAY` and the season year, then resume the external trigger.

The season store (about 115 MB) lives as an asset on the `season-store` release, not in git. Each run downloads it with the last deploy stamp, and uploads both back.

## Local monitoring

Optional macOS tools, run under launchd. The scripts find the repo from their own location; the two plists in `scripts/` hard-code absolute paths, so edit those before copying them to `~/Library/LaunchAgents`. The repo can't live under `~/Desktop`, `~/Documents` or `~/Downloads`, which macOS blocks for launchd agents. Unload the agents in the off-season.

| Script | What it does |
|---|---|
| `scripts/watchdog.sh` | Hourly. Reads the live page's "Data through" date and triggers the workflow if it is stale. |
| `scripts/morning_check.sh [--fix]` | One-off check of the live page; `--fix` triggers the workflow and waits. |
| `scripts/chain_status.sh` | Last local chain run, live page date and last Actions run. |
| `scripts/chain_entry.R` | Runs the whole chain locally, logging to `logs/chain.log`. |

The season store for local runs lives at `~/baseball-store/statcast_clean_2026.rds` (override with `STATCAST_STORE`).
