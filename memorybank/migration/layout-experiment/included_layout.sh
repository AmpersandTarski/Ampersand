#!/bin/bash
# Compiles one script by itself and as a context that a migration context includes under the label old,
# and compares the two layouts with compare_layout.py.
# Usage: included_layout.sh <script.adl> [<ampersand binary>]
# Exit: 0 same layout, 1 a difference, 2 the script does not compile by itself, 3 not as an included context,
#       4 the script is a system of contexts itself, whose labels this wrapper would nest.
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
script="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
ampersand="${2:-$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand 2>/dev/null | head -1)}"
is_system() {
  grep -qE '^[[:space:]]*CONTEXT[[:space:]].*[[:space:]]INCLUDES[[:space:]]' "$1" && return 0
  local inc
  for inc in $(grep -oE '^[[:space:]]*INCLUDE[[:space:]]+"[^"]+\.adl"' "$1" | sed -E 's/.*"([^"]+)"/\1/'); do
    [ -f "$(dirname "$1")/$inc" ] && is_system "$(dirname "$1")/$inc" && return 0
  done
  return 1
}
is_system "$script" && exit 4
name="$(grep -m1 -oE '^[[:space:]]*CONTEXT[[:space:]]+[A-Za-z_][A-Za-z0-9_]*' "$script" | awk '{print $2}')"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cd "$(dirname "$script")" || exit 2
"$ampersand" proto --no-frontend "$script" --proto-dir "$work/alone" --crud-defaults cRud > "$work/alone.txt" 2>&1 || exit 2
printf 'CONTEXT Migration INCLUDES %s FROM "%s" AS old\nCONTEXT Migration IN ENGLISH\nENDCONTEXT\n' "$name" "$script" > "$work/wrapper.adl"
"$ampersand" proto --no-frontend "$work/wrapper.adl" --proto-dir "$work/incl" --crud-defaults cRud > "$work/incl.txt" 2>&1 || { tail -5 "$work/incl.txt"; exit 3; }
python3 "$here/compare_layout.py" "$work/alone/generics" "$work/incl/generics" old
