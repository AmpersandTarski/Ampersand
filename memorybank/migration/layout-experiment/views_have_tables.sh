#!/bin/bash
# Checks that every view that an including context creates on a database of an included context
# reads a table that the included context creates when it is compiled by itself.
# Usage: views_have_tables.sh <database.sql of the context alone> <database.sql of the including context> <database label>
# The database label is the name between {{db: and }}, such as Garden.
# Prints the tables that the views read and the database lacks, and exits with 1 if there is one.
# The .gitignore of the repository ignores *.sh: add this file with `git add -f`.
set -u
alone="$1"
including="$2"
db="$3"
created="$(grep -oE '^CREATE TABLE "[^"]+"' "$alone" | sed -E 's/^CREATE TABLE "([^"]+)"/\1/' | sort -u)"
read="$(grep -oE "FROM \"\{\{db:$db\}\}\"\.\"[^\"]+\"" "$including" | sed -E 's/.*\."([^"]+)"$/\1/' | sort -u)"
missing="$(comm -13 <(echo "$created") <(echo "$read"))"
echo "$(echo "$read" | grep -c .) tables read, $(echo "$missing" | grep -c .) missing"
[ -n "$missing" ] && { echo "$missing"; exit 1; }
exit 0
