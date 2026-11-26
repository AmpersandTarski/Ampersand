#!/bin/bash
# Runs included_layout.sh over every script in a shouldSucceed directory of testing/,
# and writes one line per script: the exit code of included_layout.sh and the script.
# Usage: run_all.sh <result file> [<ampersand binary>]
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
out="$1"
details="${out%.*}-details.txt"
: > "$out"
: > "$details"
find "$repo/testing" -path '*shouldSucceed*' -name '*.adl' | sort | while read -r f; do
  result="$(bash "$here/included_layout.sh" "$f" ${2:+"$2"} 2>&1)"
  code=$?
  echo "$code ${f#"$repo"/}" >> "$out"
  if [ "$code" -ne 0 ]; then
    { echo "== $code ${f#"$repo"/}"; echo "$result"; } >> "$details"
  fi
done
awk '{n[$1]++} END {for (c in n) print "exit " c ": " n[c]}' "$out" | sort
