# Release instructions

## Before a release

1. Make sure that [release.yml](.github/workflows/release.yml) is on the default branch.
2. Use a GitHub CLI account with write access to `grisp/grisp_tools`.
3. Make sure that CI passed for the commit on the default branch.
4. Make sure that Hex Workload Identity uses `grisp/grisp_tools`, `release.yml`, and the `release` environment.

The GitHub `release` environment supplies the `RELEASE_SIGNING_KEY` secret and the `RELEASE_SIGNING_EMAIL` variable.
The `RELEASE_GPG_KEY_PASSPHRASE` secret supplies the passphrase for a protected key.

## Send a release request

1. Replace `X.Y.Z` with the version that the user gives.
2. Run the following command.

   ```sh
   gh api --method POST repos/grisp/grisp_tools/dispatches --input - <<'JSON'
   {"event_type":"release","client_payload":{"version":"X.Y.Z"}}
   JSON
   ```

3. Open the new **Release** run in [GitHub Actions](https://github.com/grisp/grisp_tools/actions/workflows/release.yml).
4. Get the Hex approval link from the **Publish to Hex** step log. For a docs
   upload by itself, use the **Publish the Hex documentation** step log.
5. Give the link to the person who can approve the release on Hex.

The person who approves the release must complete two-factor authentication on Hex.
The workflow creates the GitHub release before staging the package and documentation on Hex.
The job ends after the upload succeeds. Hex approval can happen later.

## Continue a release

If Hex approval is pending after a successful run, use the link in the
completed run to approve it. Approval completes the release on Hex.

If the run fails after pushing the tag, correct the cause and select
**Re-run all jobs** on the original run. The workflow checks that the tag
belongs to that run.

## Use the workflow in a different repository

1. Copy `.github/workflows/release.yml` to the default branch of the target repository.
2. Copy `.github/cliff.toml` to the same branch.
3. Set up the GitHub `release` environment with the signing settings above.
4. Set up Hex Workload Identity for the target repository, `release.yml`, and the `release` environment.
5. Use the target repository name in the release command.

The workflow uses the repository name as the Hex package name.
If the names are different, set the `HEX_PACKAGE_NAME` repository variable to the Hex package name.
To change the tool versions, set the `RELEASE_OTP_VERSION` and `RELEASE_REBAR3_VERSION` repository variables.
Set `RELEASE_TIME_ZONE` to change the time zone for the release date.

The target repository must have one Rebar3 application and a Hex documentation provider.
The application version must come from the Git tag.
Use `X.Y.Z` tags and the same changelog format as in [CHANGELOG.md](CHANGELOG.md).
