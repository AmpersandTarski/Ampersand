#!/usr/bin/env bash
# Install the applications of the system, one per database.
# A database is installed after every database it reads, because it has views on their tables.
# Installing an application replaces its database, and no other.
set -euo pipefail
cd "$(dirname "$0")"
[ -f .env ] && set -a && . ./.env && set +a

install() {
  echo "Installing $1 on port $2 ..."
  for _ in $(seq 1 60); do
    curl -s -o /dev/null "http://localhost:$2/" && break
    sleep 2
  done
  curl -sS -f "http://localhost:$2/api/v1/admin/installer${INSTALLER_OPTIONS:-}" >/dev/null
}

install kurk1 "${KURK1_PORT:-8081}"
install kurk2 "${KURK2_PORT:-8082}"
install migration "${MIGRATION_PORT:-8080}"
