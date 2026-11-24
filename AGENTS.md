# Release instructions

## Before a release

- Use a GitHub CLI account with write access to `grisp/releaser`.

The [Release workflow](https://github.com/grisp/releaser/blob/main/.github/workflows/release.yml) runs in `grisp/releaser`.
Hex Workload Identity for `grisp_tools` must use `grisp/releaser`, `release.yml`, and the `release` environment.

## Send a release request

1. Replace `X.Y.Z` in the command below with the version that the user gives.
2. Enter the command.

   ```sh
   gh api --method POST repos/grisp/releaser/dispatches --input - <<'JSON'
   {"event_type":"release","client_payload":{"repo":"grisp/grisp_tools","version":"X.Y.Z"}}
   JSON
   ```

3. Open the new run of the **Release** workflow in [GitHub Actions](https://github.com/grisp/releaser/actions/workflows/release.yml).
4. Get the link to the Hex approval page from the **Publish to Hex** step log.
5. Give the link to the person who can approve the release on Hex.
6. Wait for that person to approve the release on Hex.
7. Make sure that GitHub shows the release for this version.

The person who approves the release must complete two-factor authentication on Hex.
The GitHub release follows the check for the package and documentation on Hex.
If the run stops at its time limit, wait for the person to approve the release on Hex.
After approval, select **Re-run all jobs** on the page for the same run.
