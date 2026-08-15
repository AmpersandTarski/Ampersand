#!/usr/bin/env bash
#
# Run the full test suite locally, database included.
#
# The regression suite contains 'ampersand validate' tests that need a
# reachable MariaDB. This script makes the right thing the easy thing:
#   1. starts (or reuses) a dedicated MariaDB container on a port of its own,
#      so it never collides with databases of other projects;
#   2. waits until the server answers;
#   3. serializes suite runs between terminal sessions with a directory lock
#      (mkdir is atomic, and works on macOS and Linux alike);
#   4. runs `stack test` with MYSQL_HOST pointing at the container.
#
# Extra arguments are passed to `stack test`, e.g.:
#   scripts/test-local.sh --fast

set -euo pipefail

CONTAINER=ampersand-regression-db
PORT=3310
LOCK=/tmp/ampersand-regression-db.lock

if ! command -v docker >/dev/null; then
  echo "docker is not available; start a MariaDB yourself and run:" >&2
  echo "  MYSQL_HOST=<host:port> stack test" >&2
  exit 1
fi

if ! docker ps --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  if docker ps -a --format '{{.Names}}' | grep -qx "$CONTAINER"; then
    echo "Starting existing container $CONTAINER ..."
    docker start "$CONTAINER" >/dev/null
  else
    echo "Creating container $CONTAINER (MariaDB 10.4 on 127.0.0.1:$PORT) ..."
    docker run -d --name "$CONTAINER" \
      -e MYSQL_ALLOW_EMPTY_PASSWORD=yes \
      -p 127.0.0.1:$PORT:3306 mariadb:10.4 >/dev/null
  fi
fi

echo -n "Waiting for the database to answer "
for _ in $(seq 1 30); do
  if docker exec "$CONTAINER" mysqladmin ping --silent 2>/dev/null; then
    echo " ready."
    break
  fi
  echo -n "."
  sleep 1
done

if ! mkdir "$LOCK" 2>/dev/null; then
  echo "Another test run holds $LOCK — waiting for it to finish ..." >&2
  until mkdir "$LOCK" 2>/dev/null; do sleep 5; done
fi
trap 'rmdir "$LOCK"' EXIT

MYSQL_HOST=127.0.0.1:$PORT stack test "$@"
