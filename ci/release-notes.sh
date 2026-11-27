#!/bin/sh
# Print the notes for a release: how to install that version, then its section of
# CHANGELOG.md. A pre-release (3.2.0-rc1) has no section and gets the Unreleased one.
#   ci/release-notes.sh 3.1.3
set -eu
version=$1
cd "$(dirname "$0")/.."
section() {
  awk -v h="## $1" '
    index($0, h) == 1 && (length($0) == length(h) || substr($0, length(h) + 1, 1) == " ") { on = 1; next }
    on && /^## / { exit }
    on { print }' CHANGELOG.md
}
notes=$(section "$version")
[ -n "$notes" ] || notes=$(section Unreleased)
[ -n "$notes" ] || { echo "CHANGELOG.md has no section for $version" >&2; exit 1; }
cat <<END
Update the package to get this release (Doom: \`doom sync -u\`; straight.el:
\`M-x straight-pull-package\`; package-vc: \`M-x package-vc-upgrade\`). The tag is \`v$version\`.

Needs Emacs 29.1 or newer and org-roam 2.2 or newer. Coming from 1.x or 2.x:
[MIGRATION.md](https://github.com/dcruver/org-roam-second-brain/blob/main/MIGRATION.md).

## What changed
$notes
END
