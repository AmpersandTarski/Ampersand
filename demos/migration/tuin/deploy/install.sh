#!/usr/bin/env bash
# Install the applications of the system, each on its own database.
# A context is installed after every context it reaches, because it reads their tables.
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

install old "${OLD_PORT:-8081}"
install new "${NEW_PORT:-8082}"
install migratie "${MIGRATIE_PORT:-8080}"
