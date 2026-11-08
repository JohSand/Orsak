#!/usr/bin/env bash
# Prints the body of one section of CHANGELOG.md, without its heading: the release notes of a version.
# Fails if the section is missing or empty.
#
# Usage: scripts/changelog-section.sh <version|Unreleased> [changelog]
#   scripts/changelog-section.sh 0.8.0
#   scripts/changelog-section.sh Unreleased
set -euo pipefail

version="${1:?usage: $0 <version|Unreleased> [changelog]}"
changelog="${2:-$(dirname "$0")/../CHANGELOG.md}"

# the lines after "## [<version>]" up to the next section or the link references, without carriage returns or the
# blank lines around them
section=$(awk -v heading="## [$version]" '
    { sub(/\r$/, "") }
    index($0, heading) == 1 { found = 1; next }
    found && (/^## \[/ || /^\[[^]]+\]: /) { exit }
    found { lines[++n] = $0 }
    END {
        first = 1; while (first <= n && lines[first] == "") first++
        last = n; while (last >= first && lines[last] == "") last--
        for (i = first; i <= last; i++) print lines[i]
    }
' "$changelog")

if [ -z "$section" ]; then
    echo "CHANGELOG.md has no notes for [$version]" >&2
    exit 1
fi

printf '%s\n' "$section"
