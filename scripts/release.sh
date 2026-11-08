#!/usr/bin/env bash
# Prepares a release: moves the Unreleased notes in CHANGELOG.md to a section for the version, commits that as
# "Release <version>", and tags the commit. Nothing is pushed: review the commit, then push it and the tag, and CI
# publishes the packages to nuget.org and creates the GitHub release from the version's notes.
#
# Usage: scripts/release.sh <version>
#   scripts/release.sh 0.8.0
#   git push origin main 0.8.0
set -euo pipefail

version="${1:?usage: $0 <version>, e.g. $0 0.8.0}"
root="$(cd "$(dirname "$0")/.." && pwd)"
changelog="$root/CHANGELOG.md"
cd "$root"

fail() {
    echo "$1" >&2
    exit 1
}

[[ "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "$version is not a version like 0.8.0"
[ "$(git branch --show-current)" = "main" ] || fail "Releases are made from main"
[ -z "$(git status --porcelain)" ] || fail "The working tree has uncommitted changes"
git rev-parse -q --verify "refs/tags/$version" > /dev/null && fail "The tag $version already exists"
"$root/scripts/changelog-section.sh" Unreleased "$changelog" > /dev/null || fail "There is nothing to release"

previous=$(git describe --tags --abbrev=0)
date=$(date +%Y-%m-%d)
repo="https://github.com/JohSand/Orsak"

# keeps the file's line endings: a line ending in a carriage return gets one on the lines added after it
awk -v version="$version" -v date="$date" -v previous="$previous" -v repo="$repo" '
    { cr = sub(/\r$/, "") ? "\r" : "" }
    $0 == "## [Unreleased]" {
        print $0 cr; print cr; print "## [" version "] - " date cr
        next
    }
    index($0, "[Unreleased]: ") == 1 {
        print "[Unreleased]: " repo "/compare/" version "...HEAD" cr
        print "[" version "]: " repo "/compare/" previous "..." version cr
        next
    }
    { print $0 cr }
' "$changelog" > "$changelog.tmp"
mv "$changelog.tmp" "$changelog"

git add "$changelog"
git commit -q -m "Release $version"
git tag -a "$version" -m "$version"

echo "Release $version is committed and tagged. The notes:"
echo
"$root/scripts/changelog-section.sh" "$version" "$changelog"
echo
echo "To publish it: git push origin main $version"

# the breaks they record are against the previous release, and the next release is validated against this one
suppressions=$(git ls-files 'src/*/CompatibilitySuppressions.xml')
if [ -n "$suppressions" ]; then
    echo
    echo "Once $version is published, delete the files recording its breaking changes, in a commit of their own:"
    echo "$suppressions" | sed 's/^/  /'
fi
