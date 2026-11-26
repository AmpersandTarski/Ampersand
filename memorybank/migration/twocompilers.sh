#!/bin/bash
# Compiles one script with two released compilers and keeps the generated files of each.
# Usage: twocompilers.sh <project dir> <main.adl> <out dir> <version>...
set -u
proj="$1"; main="$2"; out="$3"; shift 3
for v in "$@"; do
  mkdir -p "$out/$v"
  docker run --rm --platform linux/amd64 \
    -v "$proj":/p:ro -v "$out/$v":/out \
    --entrypoint ampersand "ampersandtarski/ampersand:$v" \
    proto --no-frontend "/p/$main" --proto-dir /out --crud-defaults cRud > "$out/$v.txt" 2>&1
  echo "$v: exit $?"
done
