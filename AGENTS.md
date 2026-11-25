# Release instructions

## Before a release

- Make sure that [release.yml](.github/workflows/release.yml) is on the default branch of `grisp/grisp_tools`.
- Use a GitHub CLI account with write access to `grisp/grisp_tools`.

The GitHub `release` environment supplies the `RELEASE_SIGNING_KEY` secret and the `RELEASE_SIGNING_EMAIL` variable.
The `RELEASE_GPG_KEY_PASSPHRASE` secret supplies the passphrase for a protected key.
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

The person who approves the release must complete two-factor authentication on Hex.
The workflow creates the GitHub release before staging the package and documentation on Hex.
The job ends after the upload succeeds; Hex approval can happen later.
A successful job confirms upload, not approval or public availability on Hex.

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
