# What actually runs this every morning

## The root problem

The app bundles its own data, so fresh data reaches visitors only when something
*deploys*. That means a scheduler is unavoidable. The question was never "how do
we avoid one", it was "which one actually fires".

Three were tried:

| Scheduler | Fires? | Independent of the laptop? |
|---|---|---|
| GitHub Actions `schedule` | **never** — 6 slots on 2026-08-27, zero runs | yes |
| launchd on the MacBook | yes, reliably | **no** |
| External cron -> `workflow_dispatch` API | yes, 10/10 | yes |

GitHub's scheduler was the free zero-dependency option and it does not work for
this repository. The YAML is clean, the workflow is `active`, Actions are
enabled, the cron lines are on the default branch, and the same file's
`workflow_dispatch` trigger works every time. Visibility was tested too: it
failed identically private and public. That is GitHub's infrastructure, not
something this repo can fix.

So the daily trigger is external. It calls the one path with a perfect record.

## The call

Validated 2026-08-27, returns HTTP 204:

```
POST https://api.github.com/repos/jbmerlin3/statcast-arsenal-app/actions/workflows/refresh-and-deploy.yml/dispatches

Accept:                application/vnd.github+json
Authorization:         Bearer <TOKEN>
X-GitHub-Api-Version:  2022-11-28
Content-Type:          application/json
User-Agent:            statcast-arsenal-cron

{"ref":"main"}
```

`User-Agent` is not optional. GitHub's API rejects requests without one, and
some schedulers do not send a default.

## Setting it up

**1. Make a token.** https://github.com/settings/personal-access-tokens/new

- Resource owner: `jbmerlin3`
- Repository access: **Only select repositories** -> `statcast-arsenal-app`
- Permissions -> Repository permissions -> **Actions: Read and write**
- Nothing else. This token can start a workflow in one repo and do nothing else.
- Expiration: pick the longest offered, and note the date. **A fine-grained
  token expires, and when it does this trigger stops silently.** The watchdog is
  what catches that: the live page goes stale and it notifies. Renew it then.

**2. Point a scheduler at the call above.** cron-job.org is free and does this;
so does any service that can POST with custom headers.

- URL, method, headers and body exactly as above
- Schedule: **10:15 UTC** (06:15 Eastern). Add **12:15 UTC** for a second shot.
- Savant posts a day's games overnight, so anything after about 05:00 Eastern
  works. Earlier is not better.

**3. Confirm it fired.** After the first scheduled run:

```bash
gh run list --workflow=refresh-and-deploy.yml --limit 3 --json event,conclusion,createdAt
```

A run started this way shows `event=workflow_dispatch`. It will not say
`schedule`; that word is reserved for GitHub's own timer, which is exactly the
thing that does not work here.

## What covers you if it stops

Three independent layers, none of which need the others:

1. **External cron** -> GitHub -> deploy. The primary. Laptop-independent.
2. **launchd chain agent**, 06:15 / 09:00 / 13:00 / 18:00 whenever the Mac is
   awake. Runs the same chain and reaches the same result.
3. **launchd watchdog**, hourly. Reads the LIVE PAGE, and if it is behind it
   notifies and triggers a refresh itself. This is the one that catches a
   silently expired token.

And by hand, any time: `scripts/morning_check.sh --fix`.
