#!/bin/bash
# Installs the migration system of the demonstration `garden` next to an existing and a desired system
# that are each compiled by themselves, as systems in production are, with the compiler of this working copy.
# Phase 1 of the migration plan observed this installation fail ("Table 'garden_1.gardener' doesn't exist").
# The backends are generated locally and copied into the image of the framework, so that the image needs no
# compiler of this working copy. The frontends are left out.
# Usage: install_by_itself.sh <out dir> [<framework image>]
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
out="$1"
export FRAMEWORK_IMAGE="${2:-ampersandtarski/prototype-framework:v2.25.0}"
ampersand="$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand | head -1)"
work="$out/work"
rm -rf "$out"; mkdir -p "$out"; cp -R "$repo/demos/migration/garden" "$work"
cd "$work" || exit 1
"$ampersand" --version | head -1 > "$out/result.txt"
# The existing and the desired system, each compiled by itself; the migration context without --all-concept-tables.
for pair in garden1:existing.adl garden2:desired.adl migration:migration.adl; do
  d="${pair%%:*}"; script="${pair##*:}"
  "$ampersand" proto --no-frontend "$script" --proto-dir "deploy/$d/backend" --crud-defaults cRud > "$out/compile-$d.txt" 2>&1
  echo "compile $d: exit $?" >> "$out/result.txt"
  printf 'ARG FRAMEWORK_IMAGE=ampersandtarski/prototype-framework:latest\nFROM ${FRAMEWORK_IMAGE}\nCOPY deploy/%s/backend/ /var/www/backend/\n' "$d" > "deploy/$d/Dockerfile"
done
export MIGRATION_PORT=8290 GARDEN1_PORT=8291 GARDEN2_PORT=8292
p="install-by-itself"
docker compose -p "$p" -f deploy/compose.yaml up -d --build > "$out/build.txt" 2>&1
echo "build: exit $?" >> "$out/result.txt"
for pair in garden1:$GARDEN1_PORT garden2:$GARDEN2_PORT migration:$MIGRATION_PORT; do
  name="${pair%%:*}"; port="${pair##*:}"
  for _ in $(seq 1 60); do curl -s -o /dev/null "http://localhost:$port/" && break; sleep 2; done
  code=$(curl -s -o "$out/install-$name.json" -w '%{http_code}' "http://localhost:$port/api/v1/admin/installer")
  echo "install $name: HTTP $code" >> "$out/result.txt"
done
q() { docker compose -p "$p" -f deploy/compose.yaml exec -T db mariadb -uroot -pampersand -N -e "$1" >> "$out/result.txt" 2>&1; }
echo "== tables and views" >> "$out/result.txt"
q "SELECT table_schema, table_name, table_type FROM information_schema.tables WHERE table_name NOT LIKE 'delta%' AND table_name NOT LIKE 'prototypecontext%' AND table_name NOT LIKE '\_\_%' AND table_schema NOT IN ('mysql','sys','information_schema','performance_schema') ORDER BY 1,2"
echo "== the existing system, Garden_1.Plot" >> "$out/result.txt"; q "SELECT Plot, keeper, name FROM Garden_1.Plot ORDER BY 1"
echo "== the desired system after the installation of the migration system, Garden_2.Plot" >> "$out/result.txt"; q "SELECT Plot, keeper, name FROM Garden_2.Plot ORDER BY 1"
echo "== the registers of copying" >> "$out/result.txt"; q "SELECT 'copyName', COUNT(*) FROM Migration.copyName UNION ALL SELECT 'copyKeeper', COUNT(*) FROM Migration.copyKeeper"
docker compose -p "$p" -f deploy/compose.yaml logs migration > "$out/migration-log.txt" 2>&1
docker compose -p "$p" -f deploy/compose.yaml down -v > /dev/null 2>&1
echo done >> "$out/result.txt"
