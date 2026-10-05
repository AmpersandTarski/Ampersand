#!/usr/bin/env bash
# Leest de teamkaart één keer: de tabellijst met kolommen en een dump van alleen de gegevens.
# Schrijft niets in de teamdatabase. Het wachtwoord blijft in de container.
set -euo pipefail
cd "$(dirname "$0")"
C=artefactenkaart-prototype-db
sql() { docker exec -i "$C" sh -c 'mysql -N -uampersand -p"$MYSQL_PASSWORD" "$@"' sh "$@"; }

DB=$(sql -e "SELECT table_schema FROM information_schema.tables WHERE table_name='__conj_violation_cache__' LIMIT 1")
echo "database: $DB"
sql -e "SELECT table_name, column_name, column_type, column_key FROM information_schema.columns WHERE table_schema='$DB' ORDER BY 1, ordinal_position" > team-kolommen.tsv
sql -e "SELECT table_name, table_rows FROM information_schema.tables WHERE table_schema='$DB' ORDER BY 1" > team-tabellen.tsv
wc -l team-kolommen.tsv team-tabellen.tsv
docker exec "$C" sh -c 'mysqldump -uampersand -p"$MYSQL_PASSWORD" --single-transaction --no-create-info --complete-insert --skip-triggers --skip-add-locks "$1"' sh "$DB" > team-gegevens.sql
ls -la team-gegevens.sql
echo "$DB" > team-db.txt
