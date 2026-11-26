#!/bin/bash
# Compares the tables of the existing system of a demonstration, compiled by itself
# (as a system in production is), with the tables that the migration context expects of it.
# Usage: layout.sh <example> [<ampersand binary>]     example: kurk, garden or tuin
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
example="${1:-garden}"
ampersand="${2:-$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand 2>/dev/null | head -1)}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp "$repo/demos/migration/$example/"*.adl "$work/"
cd "$work" || exit 1
"$ampersand" --version | head -1
"$ampersand" proto --no-frontend existing.adl --proto-dir alone --crud-defaults cRud > alone.txt 2>&1
echo "compiled by itself: exit $?"
"$ampersand" proto --no-frontend migration.adl --context old --all-concept-tables \
  --proto-dir asold --crud-defaults cRud > asold.txt 2>&1
echo "compiled as the migration context expects it: exit $?"
for d in alone asold; do
  echo "== $d"
  grep -E '^CREATE (TABLE|VIEW)' "$(find "$d" -name database.sql | head -1)" \
    | grep -v 'PrototypeContext\|delta_' | sort
done
