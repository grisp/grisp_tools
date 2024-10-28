# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## Changes

- The release files are now copied recursively instead of calling the OS command
  `cp -r`. This is because we are now calculating a hash of all the deployed
  files to generate a software release unique identifier.
- The deploy command now generate a MANIFEST files that contains information
  about the deployed software in a term file that can be read with 
  file:consult/1.

## [2.7.1] - 2024-10-11

### Changed

- Insert key to normalize toolchain maps result in package command

## [2.7.0] - 2024-09-06

### Added

- Exposed previously private function `grisp_tools_util:read_file/2`: [#25](https://github.com/grisp/grisp_tools/pull/25)
- Utility functions `grisp_tools_util:make_relative/[1,2]` to convert a path to
  relative: [#25](https://github.com/grisp/grisp_tools/pull/25)
- Utility functions `grisp_tools_util:maybe_relative/[2,3]` to convert a path to
  relative with a maximum number of double-dots: [#26](https://github.com/grisp/grisp_tools/pull/26)
- Add iteration function to iterate over a list that behave nicely with `pipe`
  and `weave` functions.
- Add optional cleanup actions to `weave` and `pipe` functions that will be
  called if any exception is raised while performing the actions, and that will
  receive the last know state before the exception was raised. The cleanup
  actions will **NOT** be called if the actions do not raise any exception.
  [#25](https://github.com/grisp/grisp_tools/pull/25)
- Add a firmware command to generate GRiSP 2 binary firmwares: [#26](https://github.com/grisp/grisp_tools/pull/26)
- Add a pack command to generate a GRiSP 2 software update package: [#28](https://github.com/grisp/grisp_tools/pull/28)

### Changed

- The deploy command is now supporting multiple distribution specifications.
  as such, the `copy` argument is changed to `distribute`, and is now a list
  of tuple `{Tag, DistArgs}`. The distribution now supports bundling in
  tarball archives. The `scripts` argument is moved inside the distribution
  argument, as you can have different scripts for the different distributions.
  [#25](https://github.com/grisp/grisp_tools/pull/25)

## [2.6.1] - 2024-06-21
### Added
- Some prompts of the `configure` command have been improved: [#24](https://github.com/grisp/grisp_tools/pull/24)
- For some choices, the prompts are explained by hints for `configure`: [#24](https://github.com/grisp/grisp_tools/pull/24)
- Coloring is used to display the prompts using ANSI escape codes for `configure`: [#24](https://github.com/grisp/grisp_tools/pull/24)

### Fixed
- The token provided by the user is trimmed to avoid spaces and uncessary quotes: [#24](https://github.com/grisp/grisp_tools/pull/24)
- Unused function warning for function `say/2` in `grisp_tools_io.erl`: [#24](https://github.com/grisp/grisp_tools/pull/24)

## [2.6.0] - 2024-05-07
### Added
- Implementation of the `configure` command: [#21](https://github.com/grisp/grisp_tools/pull/21)

### Fixed
- CI update to only test OTP 24 and 25: [#22](https://github.com/grisp/grisp_tools/pull/22)

## [2.5.0] - 2023-04-21

### Added

- Implementation of the `report` command: [#74](https://github.com/grisp/rebar3_grisp/pull/74)
- Support for `docker` toolchain images [#20](https://github.com/grisp/grisp_tools/pull/20)

## [2.4.0] - 2022-07-18

### Added

- Support for OTP 24

### Changed

- Log Git remote listing command output in case of failure

### Fixed

- Do not attempt to download if using OTP package cache

## [2.3.0] - 2022-06-07

### Added

- Exact OTP version selection, e.g. '=23.3'
- Toolchain validation before building

### Changed

- The OTP Git repository is more thoroughly checked for consistency

### Fixed

- Deploy and build tasks now work offline
- Extracted packages are cached globally instead of per project

## [2.2.2] - 2022-03-01

- Fix processing of already downloaded OTP packages

## [2.2.1] - 2022-03-01

### Added

- `grisp_tools_package:list/1` can now return toolchain packages. In addition,
  it returns more meta data about packages than before.

### Fixed

- Git cloning now uses `--depth 1` to avoid downloading all of OTP history,
  effectively halving the clone time and making the source size six times
  smaller

## [2.2.0] - 2022-02-16

### Changed

- `grisp_tools_package:list/1` now returns a list of versions with hashes
- Deploying without custom build will resolve the OTP version from existing
  packages instead of Git versions

### Fixed

- Fixed a formatting issue when the correct OTP version is not found

## [2.1.0] - 2022-02-14

### Added

- `grisp_tools_package:list/1` that lists pre-built OTP package versions

### Fixed

- Template rendering no longer crashes when environment variables contain
  unicode

## [2.0.0] - 2022-02-01

### Added

- All files can now be templates ([fa5f09d](https://github.com/grisp/grisp_tools/commit/fa5f09dea255bb5dd6dae6427d2df5f00fd02105))

### Changed

- Path to toolchain no longer requires the `rtems/5` postfix ([3601c4a](https://github.com/grisp/grisp_tools/commit/3601c4a6de55f1a3b4d0bbbb6ee58b7a372db354))

### Fixed

- `configure` is forced if source is downloaded or cleaned ([de19f96](https://github.com/grisp/grisp_tools/commit/de19f96182b5dd58f8fdec2e75cabb6e64adf40d))
- File overlays create necessary directories if missing ([559f8c6](https://github.com/grisp/grisp_tools/commit/559f8c64887b90619cb9d5cc4d0433ec04905211))
- Files overlays are copied over the final release ([58a08c0](https://github.com/grisp/grisp_tools/commit/58a08c038134ee84535d801dbd8c10f34006b838))

## [0.2.6] - 2019-09-27

### Changed

- Remove deprecated maintainers section [\#7](https://github.com/grisp/grisp_tools/pull/7) ([nextl00p](https://github.com/nextl00p))

## [0.2.5] - 2019-09-27

### Added
- Add NIF support [\#3](https://github.com/grisp/grisp_tools/pull/3) ([nextl00p](https://github.com/nextl00p))

### Fixed

- Remove duplicate / [\#6](https://github.com/grisp/grisp_tools/pull/6) ([nextl00p](https://github.com/nextl00p))
- Remove call to uri\_string, which is not supported in OTP 20 [\#5](https://github.com/grisp/grisp_tools/pull/5) ([nextl00p](https://github.com/nextl00p))
- Preserve directory structure as on CDN [\#4](https://github.com/grisp/grisp_tools/pull/4) ([nextl00p](https://github.com/nextl00p))

- Set correct path of custom OTP [\#2](https://github.com/grisp/grisp_tools/pull/2) ([nextl00p](https://github.com/nextl00p))

## [0.2.4] - 2018-09-13

## [0.2.3] - 2018-09-12

## [0.2.2] - 2018-09-12

## [0.2.1] - 2018-09-11

### Fixed

- Use correct application name for getting environment variables [\#1](https://github.com/grisp/grisp_tools/issues/1)

## [0.2.0] - 2018-09-11

[Unreleased]: https://github.com/grisp/grisp_tools/compare/2.7.1...HEAD
[2.7.1]: https://github.com/grisp/grisp_tools/compare/2.7.0...2.7.1
[2.7.0]: https://github.com/grisp/grisp_tools/compare/2.6.1...2.7.0
[2.6.1]: https://github.com/grisp/grisp_tools/compare/2.6.0...2.6.1
[2.6.0]: https://github.com/grisp/grisp_tools/compare/2.5.0...2.6.0
[2.5.0]: https://github.com/grisp/grisp_tools/compare/2.4.0...2.5.0
[2.4.0]: https://github.com/grisp/grisp_tools/compare/2.3.0...2.4.0
[2.3.0]: https://github.com/grisp/grisp_tools/compare/2.2.2...2.3.0
[2.2.2]: https://github.com/grisp/grisp_tools/compare/2.2.1...2.2.2
[2.2.1]: https://github.com/grisp/grisp_tools/compare/2.2.0...2.2.1
[2.2.0]: https://github.com/grisp/grisp_tools/compare/2.1.0...2.2.0
[2.1.0]: https://github.com/grisp/grisp_tools/compare/2.0.0...2.1.0
[2.0.0]: https://github.com/grisp/grisp_tools/compare/0.2.6...2.0.0
[0.2.6]: https://github.com/grisp/grisp_tools/compare/0.2.6...0.2.6
[0.2.5]: https://github.com/grisp/grisp_tools/compare/0.2.4...0.2.5
[0.2.4]: https://github.com/grisp/grisp_tools/compare/0.2.3...0.2.4
[0.2.3]: https://github.com/grisp/grisp_tools/compare/0.2.2...0.2.3
[0.2.2]: https://github.com/grisp/grisp_tools/compare/0.2.1...0.2.2
[0.2.1]: https://github.com/grisp/grisp_tools/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/grisp/grisp_tools/compare/05cf2b8a58ef7decfbb0f043d25f5f20bb3c45c6...0.2.0
