# Deploying this bundle

## Posit Connect Cloud from GitHub (free plan)

1. Create a **public** GitHub repository (the free plan publishes from public repositories only).
2. Push this directory to it (see the commands printed by the build script).
3. Sign in at https://connect.posit.cloud with your GitHub account.
4. Publish new content, pick this repository and branch, and choose `app.R` as the entry point.
5. Connect Cloud installs the packages listed in `manifest.json` and serves the app at a
   public https address you can share.

Redeploying: push a new commit and republish. Rebuild the bundle first with
`Rscript tools/build_deploy.R` in the development project so the engine here stays in step.

## What is switched off in this bundle

* **RANDOM.ORG** is unavailable unless the server sets `RANDOMORG_API_KEY`. Leave it unset on a
  public deployment: strangers would spend your quota. The other four sources are unaffected.
* **Background workers** start only when RANDOM.ORG is configured, so the app stays light.
* The `PUBLIC_DEMO` marker file displays the demonstration banner. Delete it (or rebuild with
  `nodemo`) for an institutional deployment behind a login.

## Institutional deployment

The same bundle runs behind Posit Connect, ShinyProxy, or Shiny Server with an authenticating
HTTPS reverse proxy. Use that for lists intended for real enrolment, remove `PUBLIC_DEMO`, and
set `RANDOMORG_API_KEY` there if you want that source.
