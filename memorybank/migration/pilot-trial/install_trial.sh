#!/bin/bash
# Prediction 2 of preregistration.md: the migration system installs next to a copy of a production database.
# Usage: install_trial.sh <work dir> <dump.sql.gz> <database in the dump> <context dbnames>
# <context dbnames> is AMPERSAND_CONTEXT_DBNAMES of the migration system, such as "Ctx_1=<db>;Ctx_2=<db>_2".
# <work dir> holds v142/project (the desired release) and mig/ (the backend of the migration system, from read_trial.sh).
# The desired release is compiled by itself with the compiler of this working copy, the compiler of today.
# Both images get the ExecEngine functions in PHP of the pilot, because the migration system includes its ExecEngine rules.
# A checksum of every table of the existing database, before and after, shows whether the migration system writes in it.
# The dump holds personal data: the work dir stays outside every repository, and the containers are removed at the end.
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../../.." && pwd)"
work="$1"; dump="$2"; old="$3"; new="${old}_2"; dbnames="$4"
ampersand="$(ls "$repo"/.stack-work/install/*/*/*/bin/ampersand | head -1)"
framework=ampersandtarski/prototype-framework:v2.25.0
out="$work/install"; rm -rf "$out"; mkdir -p "$out"
cd "$work" || exit 1
rm -rf newalone && "$ampersand" proto --no-frontend v142/project/main.adl --proto-dir newalone --crud-defaults cRud > "$out/compile-new.txt" 2>&1
echo "compile new by itself: exit $?" > "$out/result.txt"
for s in newalone mig; do
  printf 'FROM %s\nCOPY %s/ /var/www/backend/\nCOPY v142/project/backend/*.php /var/www/backend/bootstrap/files/\n' "$framework" "$s" > "Dockerfile.$s"
  docker build -q --platform linux/amd64 -f "Dockerfile.$s" -t "migproef-$s" . > "$out/build-$s.txt" 2>&1
  echo "build $s: exit $?" >> "$out/result.txt"
done

net=migproef-net; db=migproef-db
docker rm -f migproef-newalone migproef-mig "$db" > /dev/null 2>&1; docker network rm "$net" > /dev/null 2>&1
docker network create "$net" > /dev/null
docker run -d --name "$db" --network "$net" -e MARIADB_ROOT_PASSWORD=proef mariadb:12.3 \
  --lower-case-table-names=1 --sql-mode=ANSI,TRADITIONAL > /dev/null
for _ in $(seq 1 60); do docker exec "$db" mariadb -uroot -pproef -e 'SELECT 1' > /dev/null 2>&1 && break; sleep 2; done
m() { docker exec -i "$db" mariadb -uroot -pproef "$@"; }
m -e "CREATE USER 'ampersand'@'%' IDENTIFIED BY 'ampersand'; GRANT ALL ON *.* TO 'ampersand'@'%'; CREATE DATABASE \`$old\`"
gunzip -c "$dump" | m --one-database "$old" > "$out/restore.txt" 2>&1
echo "restore: exit $?" >> "$out/result.txt"
checksums() {
  tables=$(m -N -e "SELECT GROUP_CONCAT(CONCAT('\`',table_name,'\`') ORDER BY table_name) FROM information_schema.tables WHERE table_schema='$old'")
  m -N -e "CHECKSUM TABLE $tables" "$old" > "$1"
}
checksums "$out/old-before.txt"
echo "tables of old: $(wc -l < "$out/old-before.txt")" >> "$out/result.txt"

run() { # name port dbname [context dbnames]
  docker run -d --name "migproef-$1" --network "$net" -p "127.0.0.1:$2:80" \
    -e AMPERSAND_DEBUG_MODE=true -e AMPERSAND_DBHOST="$db" -e AMPERSAND_DBUSER=ampersand -e AMPERSAND_DBPASS=ampersand \
    -e AMPERSAND_DBNAME="$3" -e AMPERSAND_SERVER_URL="http://localhost:$2" -e AMPERSAND_CONTEXT_DBNAMES="${4:-}" \
    --platform linux/amd64 "migproef-$1" > /dev/null
  for _ in $(seq 1 90); do curl -s -o /dev/null "http://localhost:$2/" && break; sleep 2; done
  code=$(curl -s -m 1200 -o "$out/install-$1.json" -w '%{http_code}' "http://localhost:$2/api/v1/admin/installer")
  echo "install $1: HTTP $code" >> "$out/result.txt"
  docker logs "migproef-$1" > "$out/log-$1.txt" 2>&1
}
run newalone 8296 "$new"
run mig 8295 migration "$dbnames"

checksums "$out/old-after.txt"
echo "tables of old whose checksum changed: $(diff "$out/old-before.txt" "$out/old-after.txt" | grep -c '^>')" >> "$out/result.txt"
diff "$out/old-before.txt" "$out/old-after.txt" | grep '^>' >> "$out/result.txt"
docker rm -f migproef-newalone migproef-mig "$db" > /dev/null 2>&1; docker network rm "$net" > /dev/null 2>&1
echo done >> "$out/result.txt"
cat "$out/result.txt"
