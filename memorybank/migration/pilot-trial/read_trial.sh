#!/bin/bash
# Phase 2 on a copy of a production database: predictions 1, 3 and 4 of preregistration.md.
# Usage: read_trial.sh <work dir> <dump.sql.gz> <database in the dump> <database label of old>
# <work dir> holds v141/project (the existing release) and migration.adl (the bare migration context).
# The database label of old is the placeholder in database.sql of the migration system, {{db:<label>}}.
# The dump holds personal data: the work dir stays outside every repository, and the container is removed at the end.
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
work="$1"; dump="$2"; dbname="$3"; label="$4"
ampersand="$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand | head -1)"
out="$work/read"; rm -rf "$out"; mkdir -p "$out"
cd "$work" || exit 1

# The migration system, with the compiler of this working copy, as the pilot compiles (proto, --crud-defaults cRud).
"$ampersand" --version | head -1 > "$out/result.txt"
rm -rf mig && "$ampersand" proto --no-frontend migration.adl --proto-dir mig --crud-defaults cRud > "$out/compile-mig.txt" 2>&1
echo "compile migration: exit $?" >> "$out/result.txt"
# The existing release by itself, with the released compiler that built it (v5.15.0).
rm -rf alone && docker run --rm -e LANG=C.UTF-8 -v "$work:/work" -w /work ampersandtarski/ampersand:v5.15.0 \
  proto --no-frontend v141/project/main.adl --proto-dir alone --crud-defaults cRud > "$out/compile-alone.txt" 2>&1
echo "compile alone (v5.15.0): exit $?" >> "$out/result.txt"

c=migproef-db
docker rm -f "$c" > /dev/null 2>&1
# The options of the database in production (deploy/compose.prod.yaml of the pilot): its table names are in lower case.
docker run -d --name "$c" -e MARIADB_ROOT_PASSWORD=proef mariadb:12.3 \
  --lower-case-table-names=1 --sql-mode=ANSI,TRADITIONAL > /dev/null
for _ in $(seq 1 60); do docker exec "$c" mariadb -uroot -pproef -e 'SELECT 1' > /dev/null 2>&1 && break; sleep 2; done
m() { docker exec -i "$c" mariadb -uroot -pproef "$@"; }
# Only the database of the application: the dump also holds the database mysql, with the users of production.
m -e "CREATE DATABASE \`$dbname\`"
gunzip -c "$dump" | m --one-database "$dbname" > "$out/restore.txt" 2>&1
echo "restore: exit $?" >> "$out/result.txt"

# Prediction 1: the views of the migration system on the database of old.
grep -E "^CREATE VIEW .*\{\{db:$label\}\}" mig/generics/database.sql \
  | sed "s/{{db:$label}}/$dbname/; s/\$/;/" > "$out/views.sql"
m -e 'CREATE DATABASE mig'
m --force --init-command="SET SESSION sql_mode=CONCAT(@@sql_mode,',ANSI_QUOTES')" mig < "$out/views.sql" > "$out/views-errors.txt" 2>&1
echo "views: $(wc -l < "$out/views.sql") written, $(grep -c ERROR "$out/views-errors.txt") refused" >> "$out/result.txt"

# Predictions 3 and 4.
python3 -I "$here/read_compare.py" sql mig/generics alone/generics "$out" >> "$out/result.txt"
m -N -B --force mig < "$out/mig.sql" > "$out/mig.tsv" 2> "$out/mig-errors.txt"
m -N -B --force "$dbname" < "$out/alone.sql" > "$out/alone.tsv" 2> "$out/alone-errors.txt"
echo "query errors: migration $(grep -c ERROR "$out/mig-errors.txt"), alone $(grep -c ERROR "$out/alone-errors.txt")" >> "$out/result.txt"
python3 -I "$here/read_compare.py" compare "$out" >> "$out/result.txt"

docker rm -f "$c" > /dev/null 2>&1
echo done >> "$out/result.txt"
cat "$out/result.txt"
