# Release instructions

## Before a release

- Make sure that [release.yml](.github/workflows/release.yml) is on the default branch of `grisp/grisp_tools`.
- Use a GitHub CLI account with write access to `grisp/grisp_tools`.

The GitHub `release` environment supplies the `RELEASE_SIGNING_KEY` secret and the `RELEASE_SIGNING_EMAIL` variable.
Hex Workload Identity uses the `release.yml` workflow and the `release` environment.

## Send a release request

1. Replace `X.Y.Z` in the command below with the version that the user gives.
2. Enter the command.

   ```sh
   gh api --method POST repos/grisp/grisp_tools/dispatches --input - <<'JSON'
   {"event_type":"release","client_payload":{"version":"X.Y.Z"}}
   JSON
   ```

3. Open the new run of the **Release** workflow in [GitHub Actions](https://github.com/grisp/grisp_tools/actions/workflows/release.yml).
4. Get the link to the Hex approval page from the **Publish to Hex** step log.
5. Give the link to the person who can approve the release on Hex.
6. Wait for that person to approve the release on Hex.
7. Make sure that GitHub shows the release for this version.

The person who approves the release must complete two-factor authentication on Hex.
The GitHub release follows the check for the package and documentation on Hex.
If the run stops at its time limit, wait for the person to approve the release on Hex.
After approval, select **Re-run all jobs** on the page for the same run.
