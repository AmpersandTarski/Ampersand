#!/bin/bash
# Compiles the migration context of a demonstration with --all-concept-tables, as `ampersand deploy` does,
# once with the released image of the compiler and once with the compiler of this working copy,
# and compares database.sql and the files in generics.
# Usage: flag_unchanged.sh <example> [<image>]     example: kurk, garden or tuin; image default ampersandtarski/ampersand:v5.15.0
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
example="$1"
image="${2:-ampersandtarski/ampersand:v5.15.0}"
ampersand="$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand | head -1)"
work="$(mktemp -d "${TMPDIR:-/tmp}/flag.XXXXXX")"
if [ -n "${KEEP:-}" ]; then echo "kept in $work"; else trap 'rm -rf "$work"' EXIT; fi
cp "$repo/demos/migration/$example/"*.adl "$work/"
cd "$work" || exit 2
options=(proto --no-frontend migration.adl --all-concept-tables --crud-defaults cRud)
docker run --rm -e LANG=C.UTF-8 -v "$work:/work" -w /work "$image" "${options[@]}" --proto-dir released > released.txt 2>&1 || { tail -3 released.txt; exit 3; }
"$ampersand" "${options[@]}" --proto-dir built > built.txt 2>&1 || { tail -3 built.txt; exit 3; }
normal() {
  case "$1" in
    *.json) jq -S 'walk(if type == "object" then del(.origin) else . end)' "$1" | sed -e "s#$work#/work#g" -e 's#/private/work#/work#g' ;;
    *) grep -v '^\*\*\*' "$1" ;;
  esac
}
status=0
for f in $(cd released && find . -name database.sql -o -path '*generics/*.json' | sort); do
  # settings.json records the version of the compiler, which differs by construction.
  case "$f" in *settings.json) continue ;; esac
  # The origins name the directory of the compilation, which is /work in the image; the header names the compiler.
  if ! diff -q <(normal "released/$f") <(normal "built/$f") > /dev/null; then
    echo "differs: $f"
    status=1
  fi
done
[ "$status" -eq 0 ] && echo "$example: no difference"
exit "$status"
