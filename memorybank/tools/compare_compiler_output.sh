#!/usr/bin/env bash
#
# Compare what two builds of the compiler generate for the same scripts.
#
#   memorybank/tools/compare_compiler_output.sh <old ampersand> <new ampersand> <script.adl>...
#
# For every script it runs `ampersand proto --no-frontend` with both binaries and compares
# the generated backend files. Files are compared byte for byte, apart from the two fields
# of settings.json that name the compiler itself (version and environment), the banner with
# the version in database.sql, and the file compiler-version.txt.
# The model hash is compared, so an equal result means an equal model.
# Output that differs only in printed terms (the comments inside generated SQL and the
# normalisation steps in interfaces.json) is counted separately.
# The exit code is the number of scripts whose output differs otherwise.
#
# Use it as a guard for a change that must leave existing scripts alone: a script that
# does not use the new feature has to give identical output.
#
set -uo pipefail

if [ $# -lt 3 ]; then
  sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'
  exit 64
fi

OLD=$1; NEW=$2; shift 2
WORK=$(mktemp -d "${TMPDIR:-/tmp}/compare-compiler.XXXXXX")
trap 'rm -r "$WORK"' EXIT

differing=0; same=0; comments=0; skipped=0
for script in "$@"; do
  name=$(basename "$script" .adl)
  dir=$(cd "$(dirname "$script")" && pwd)
  mkdir -p "$WORK/old/$name" "$WORK/new/$name"
  (cd "$dir" && "$OLD" proto --no-frontend --proto-dir "$WORK/old/$name" "$(basename "$script")" >"$WORK/old/$name.out" 2>&1); oldexit=$?
  (cd "$dir" && "$NEW" proto --no-frontend --proto-dir "$WORK/new/$name" "$(basename "$script")" >"$WORK/new/$name.out" 2>&1); newexit=$?
  if [ $oldexit -ne $newexit ]; then
    echo "DIFFERENT exit code ($oldexit versus $newexit): $script"
    differing=$((differing + 1))
    continue
  fi
  if [ $oldexit -ne 0 ]; then
    skipped=$((skipped + 1))   # both builds refuse the script in the same way
    continue
  fi
  for side in old new; do
    find "$WORK/$side/$name" -name settings.json -exec \
      sed -i.bak -E '/"compiler\.(version|env)"/d' {} \;
    # database.sql opens with a banner that states the version of the compiler
    find "$WORK/$side/$name" -name database.sql -exec \
      sed -i.bak -E '/^\*+$/d; /^\*\*\* Ampersand-v/d' {} \;
    find "$WORK/$side/$name" \( -name '*.bak' -o -name compiler-version.txt \) -delete
  done
  if diff -r "$WORK/old/$name" "$WORK/new/$name" >"$WORK/$name.diff" 2>&1; then
    same=$((same + 1))
    continue
  fi
  # The generated SQL carries the term it implements as a comment, and interfaces.json
  # carries the normalisation steps of a term as text. A change in how a term is printed
  # alters those texts and nothing that a database or the framework executes.
  for side in old new; do
    find "$WORK/$side/$name" -type f \( -name '*.json' -o -name '*.sql' \) -exec perl -0pi -e 's#/\*.*?\*/##gs; s#"NormalizationSteps": \[.*?\]#"NormalizationSteps": []#gs' {} \;
  done
  if diff -r "$WORK/old/$name" "$WORK/new/$name" >"$WORK/$name.diff" 2>&1; then
    comments=$((comments + 1))
  else
    echo "DIFFERENT output: $script"
    head -20 "$WORK/$name.diff" | cut -c1-200 | sed 's/^/    /'
    differing=$((differing + 1))
  fi
done

echo "identical: $same, identical apart from printed terms: $comments, different: $differing, refused by both builds: $skipped"
exit $differing
