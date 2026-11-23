import datetime
import os
from pathlib import Path
import re
import sys


def release_points(changelog):
    unreleased = re.search(r"^## \[Unreleased\]\n", changelog, re.MULTILINE)
    releases = list(re.finditer(r"^## \[([0-9]+\.[0-9]+\.[0-9]+)\] - ", changelog, re.MULTILINE))
    if not unreleased or not releases or unreleased.end() > releases[0].start():
        sys.exit("Check the release headings in CHANGELOG.md.")
    return unreleased, releases


def prepare(changelog, version, repository_url, previous_tag, generated):
    unreleased, releases = release_points(changelog)
    latest = releases[0]
    previous = latest[1]
    if tuple(map(int, version.split("."))) <= tuple(map(int, previous.split("."))):
        sys.exit(f"Use a version greater than {previous}.")

    if previous_tag != previous:
        sys.exit(f"The last tag is {previous_tag}. Check CHANGELOG.md.")
    notes = changelog[unreleased.end():latest.start()].strip() or generated.strip()

    compare = f"{repository_url}/compare"
    old_link = f"[Unreleased]: {compare}/{previous}...HEAD\n"
    if changelog.count(old_link) != 1:
        sys.exit("Check the Unreleased link in CHANGELOG.md.")
    new_links = f"[Unreleased]: {compare}/{version}...HEAD\n[{version}]: {compare}/{previous}...{version}\n"
    section = f"## [{version}] - {datetime.date.today()}\n\n"
    if notes:
        section += f"{notes}\n\n"
    updated = changelog[:unreleased.end()] + "\n" + section + changelog[latest.start():]
    return updated.replace(old_link, new_links, 1)


def write_notes(changelog, version, output_dir):
    _, releases = release_points(changelog)
    if len(releases) < 2 or releases[0][1] != version:
        sys.exit(f"Check the {version} section in CHANGELOG.md.")
    current, previous = releases[:2]
    body = changelog[changelog.index("\n", current.end()) + 1:previous.start()].strip("\n")
    (output_dir / "release-notes.md").write_text(body + "\n", encoding="utf-8")
    (output_dir / "previous-version").write_text(previous[1] + "\n", encoding="utf-8")


if __name__ == "__main__":
    changelog_file = Path("CHANGELOG.md")
    changelog = changelog_file.read_text(encoding="utf-8")
    command = sys.argv[1]
    version = os.environ["RELEASE_VERSION"]
    if command == "prepare":
        repository_url = f"{os.environ['GITHUB_SERVER_URL']}/{os.environ['GITHUB_REPOSITORY']}"
        generated = Path(sys.argv[2]).read_text(encoding="utf-8")
        updated = prepare(changelog, version, repository_url, os.environ["PREVIOUS_TAG"], generated)
        changelog_file.write_text(updated, encoding="utf-8")
    elif command == "notes":
        write_notes(changelog, version, Path(os.environ["RUNNER_TEMP"]))
