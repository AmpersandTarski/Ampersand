#!/usr/bin/env bash
#
# Oracle for INCLUDE ... AS: a script with aliased includes means the same as the single
# context in which every name carries its prefix.
#
#   scripts/check-multicontext-flattening.sh [<ampersand binary>]
#
# The compiler compiles an aliased include by renaming the included context and merging it.
# `ampersand export` writes the result as one script without INCLUDE statements. For every
# script in testing/Travis/testcases/MultiContext/shouldSucceed this check verifies that
#
#   1. the exported script compiles (`ampersand check`);
#   2. the prototype generated from the exported script equals, file by file, the prototype
#      generated from the original script (the compiler's own version and environment,
#      the positions in source files and the free text of meanings and purposes excepted).
#
# Point 2 is the flattening theorem of proof-track claim PRF-11 as a test: the system of
# contexts and the flattened context are the same information system.
#
# Without an argument the script uses the binary that `stack` built.
#
set -uo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CASES="$ROOT/testing/Travis/testcases/MultiContext/shouldSucceed"
BIN=${1:-$(cd "$ROOT" && stack path --local-install-root)/bin/ampersand}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/flattening.XXXXXX")
trap 'rm -r "$WORK"' EXIT

normalise() { # $1 = directory with generated files
  find "$1" -name settings.json -exec sed -i.bak -E '/"compiler\.(version|env|modelHash)"/d' {} \;
  find "$1" \( -name '*.bak' -o -name compiler-version.txt \) -delete
  # Free text (meanings, messages, purposes) is left out of the comparison: `ampersand export`
  # renders it through the markup converter, which does not give back the text it was given.
  find "$1" -type f -name '*.json' -exec \
    perl -pi -e 's#^(\s*"(meaning|message|purpose|description)[A-Za-z]*": ).*$#$1"",#i' {} \;
  # Positions in source files differ by construction: the flattened script is another file.
  # The same holds for the name that the compiler gives to the rule of an IDENT statement,
  # which contains a hash of its position.
  find "$1" -type f \( -name '*.json' -o -name '*.sql' \) -exec \
    perl -0pi -e 's#"origin": "[^"]*"#"origin": ""#g; s#/[^ "]*\.adl:[0-9]+:[0-9]+#POSITION#g; s#[A-Za-z_]+\.adl:[0-9]+:[0-9]+#POSITION#g; s#identity[0-9]{6,}#identityN#g' {} \;
}

failed=0; passed=0
for script in "$CASES"/*.adl; do
  name=$(basename "$script" .adl)
  dir="$WORK/$name"; mkdir -p "$dir/a" "$dir/b" "$dir/flat"
  problem=""
  (cd "$CASES" && "$BIN" export --output-dir "$dir/flat" "$name.adl" >"$dir/export1.out" 2>&1) || problem="export failed"
  if [ -z "$problem" ]; then
    mv "$dir/flat/export.adl" "$dir/flat/$name.adl"
    (cd "$dir/flat" && "$BIN" check "$name.adl" >"$dir/check.out" 2>&1) || problem="the exported script does not compile"
  fi
  if [ -z "$problem" ]; then
    (cd "$CASES" && "$BIN" proto --no-frontend --proto-dir "$dir/a" "$name.adl" >"$dir/proto-a.out" 2>&1) || problem="proto of the original failed"
    (cd "$dir/flat" && "$BIN" proto --no-frontend --proto-dir "$dir/b" "$name.adl" >"$dir/proto-b.out" 2>&1) || problem="proto of the exported script failed"
  fi
  if [ -z "$problem" ]; then
    normalise "$dir/a"; normalise "$dir/b"
    diff -r "$dir/a" "$dir/b" >"$dir/proto.diff" 2>&1 || problem="the generated prototypes differ"
  fi
  if [ -z "$problem" ]; then
    passed=$((passed + 1))
  else
    failed=$((failed + 1))
    echo "FAIL $name: $problem"
    for f in "$dir/proto.diff" "$dir/check.out" "$dir/export1.out"; do
      [ -s "$f" ] && { head -12 "$f" | cut -c1-220 | sed 's/^/    /'; break; }
    done
  fi
done
echo "flattening oracle: $passed passed, $failed failed"
exit $failed
