#!/bin/bash
# Start/ensure Ampersand MariaDB database is accessible for testing.
# Handles:
#   - Fresh Docker container start
#   - Docker container already running (no-op)
#   - Another database already occupying port 3306
# Reads credentials from .env (copy .env.example to .env to get started).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="$SCRIPT_DIR/.env"
ENV_EXAMPLE="$SCRIPT_DIR/.env.example"

# --- Require .env ---
if [ ! -f "$ENV_FILE" ]; then
    echo "❌ No .env file found."
    if [ -f "$ENV_EXAMPLE" ]; then
        echo "   Copy the example file and adjust credentials:"
        echo "     cp .env.example .env"
    else
        echo "   Create a .env file with at least:"
        echo "     MYSQL_HOST=127.0.0.1"
        echo "     MYSQL_PORT=3306"
        echo "     MYSQL_USER=root"
        echo "     MYSQL_PASSWORD="
    fi
    exit 1
fi

# --- Source .env (without exporting secrets to sub-processes) ---
set -a
# shellcheck disable=SC1090
source "$ENV_FILE"
set +a

# Defaults
MYSQL_HOST="${MYSQL_HOST:-127.0.0.1}"
MYSQL_PORT="${MYSQL_PORT:-3306}"
MYSQL_USER="${MYSQL_USER:-root}"
MYSQL_PASSWORD="${MYSQL_PASSWORD:-}"

echo "Setting up Ampersand database (${MYSQL_USER}@${MYSQL_HOST}:${MYSQL_PORT})..."

# --- Helper: run a SQL command, preferring docker exec over the host mysql CLI.
# The host MySQL 9.x client dropped the mysql_native_password plugin, so it
# cannot connect to MariaDB. We use docker exec when possible.
mariadb_exec() {
    local sql="$1"
    if docker exec ampersand-mariadb mariadb -u root \
           -e "$sql" >/dev/null 2>&1; then
        return 0
    fi
    # Fallback to host mysql client (works if compatible)
    if [ -n "$MYSQL_PASSWORD" ]; then
        mysql -u "$MYSQL_USER" -p"$MYSQL_PASSWORD" \
              -h "$MYSQL_HOST" -P "$MYSQL_PORT" \
              --connect-timeout=5 -e "$sql" 2>/dev/null
    else
        mysql -u "$MYSQL_USER" \
              -h "$MYSQL_HOST" -P "$MYSQL_PORT" \
              --connect-timeout=5 -e "$sql" 2>/dev/null
    fi
}

# --- Helper to test connectivity (returns 0 on success) ---
check_connection() {
    if docker exec ampersand-mariadb mariadb -u root \
           -e "SELECT 'Connection OK'" 2>/dev/null | grep -q "Connection OK"; then
        return 0
    fi
    if [ -n "$MYSQL_PASSWORD" ]; then
        mysql -u "$MYSQL_USER" -p"$MYSQL_PASSWORD" \
              -h "$MYSQL_HOST" -P "$MYSQL_PORT" \
              --connect-timeout=5 \
              -e "SELECT 'Connection OK'" 2>/dev/null | grep -q "Connection OK"
    else
        mysql -u "$MYSQL_USER" \
              -h "$MYSQL_HOST" -P "$MYSQL_PORT" \
              --connect-timeout=5 \
              -e "SELECT 'Connection OK'" 2>/dev/null | grep -q "Connection OK"
    fi
}

# --- Step 1: Try to start the Docker container ---
DOCKER_CONTAINER_USED=false
echo ""
echo "Step 1: Starting Docker MariaDB container..."
if docker-compose up -d 2>/dev/null; then
    DOCKER_CONTAINER_USED=true
    echo "✓ Docker container started (or was already running)."

    # Wait for it to be ready
    echo "  Waiting for MariaDB to be ready..."
    timeout=30
    counter=0
    while [ $counter -lt $timeout ]; do
        if docker exec ampersand-mariadb mariadb -u root -e "SELECT 1" >/dev/null 2>&1; then
            echo "✓ MariaDB is ready."
            break
        fi
        sleep 1
        counter=$((counter + 1))
    done
    if [ $counter -eq $timeout ]; then
        echo "⚠ MariaDB readiness timed out — proceeding anyway."
    fi
else
    echo "ℹ Docker container could not start (port ${MYSQL_PORT} may be in use by another database)."
    echo "  Will try to configure the existing database at ${MYSQL_HOST}:${MYSQL_PORT}."
fi

# --- Step 2: Apply GRANT root@'%' (needed for Docker host-bridge IP connections) ---
# The PHP scripts Ampersand generates connect via TCP to 127.0.0.1, but Docker's
# network bridge makes MariaDB see the connection from 192.168.x.x, not localhost.
# Without this grant, access is denied.
echo ""
echo "Step 2: Ensuring root@'%' access..."
GRANT_SQL="GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY '' WITH GRANT OPTION; FLUSH PRIVILEGES;"
GRANT_OK=false

# Prefer docker exec (bypasses auth, most reliable for our container)
if [ "$DOCKER_CONTAINER_USED" = "true" ] && \
   docker exec ampersand-mariadb mariadb -u root \
       -e "$GRANT_SQL" >/dev/null 2>&1; then
    echo "✓ Grant applied via Docker exec."
    GRANT_OK=true
fi

# Fallback: use mariadb_exec (tries docker exec first, then host mysql CLI)
if [ "$GRANT_OK" = "false" ]; then
    if mariadb_exec "$GRANT_SQL"; then
        echo "✓ Grant applied."
        GRANT_OK=true
    else
        echo "⚠ Could not apply grant. If tests fail with 'Access denied', run manually:"
        echo "  docker exec ampersand-mariadb mariadb -u root \\"
        echo "    -e \"GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY '' WITH GRANT OPTION; FLUSH PRIVILEGES;\""
    fi
fi

# --- Step 3: Verify the connection ---
echo ""
echo "Step 3: Verifying database connection..."
if check_connection; then
    echo "✓ Database is accessible!"
else
    echo "❌ Cannot connect to database. Check credentials in .env."
    echo "   MYSQL_HOST=${MYSQL_HOST}  MYSQL_PORT=${MYSQL_PORT}  MYSQL_USER=${MYSQL_USER}"
    exit 1
fi

# --- Done ---
echo ""
echo "Database connection details:"
echo "  Host:     ${MYSQL_HOST}"
echo "  Port:     ${MYSQL_PORT}"
echo "  User:     ${MYSQL_USER}"
echo "  Password: ${MYSQL_PASSWORD:-(empty)}"
echo ""
echo "✓ You can now run: stack test"
echo ""
echo "Tip: Ampersand reads MYSQL_HOST / MYSQL_USER / MYSQL_PASSWORD from the environment."
echo "     If you use non-default credentials, export them before running stack test:"
echo "       export MYSQL_HOST=${MYSQL_HOST} MYSQL_USER=${MYSQL_USER} MYSQL_PASSWORD=${MYSQL_PASSWORD}"
