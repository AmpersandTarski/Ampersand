#!/usr/bin/env bash
# stacktest.sh — Zet alle voorwaarden klaar voor 'stack test' en voer het daarna uit.
#
# Wat dit script doet:
#   1. Controleert dat PHP beschikbaar is (met mysqli-extensie)
#   2. Start de Ampersand MariaDB-container als dat kan; anders gebruikt het een
#      al-draaiende database op poort 3306.
#   3. Past GRANT toe zodat root@'%' zonder wachtwoord kan verbinden
#      (de PHP-scripts van Ampersand verbinden zo).
#   4. Verifieert de PHP→DB verbinding.
#   5. Stelt MYSQL_HOST / MYSQL_USER / MYSQL_PASSWORD in voor stack test.
#   6. Voert 'stack test' uit.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ── Kleurtjes ──────────────────────────────────────────────────────────────────
GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; NC='\033[0m'
ok()   { echo -e "${GREEN}✓${NC} $*"; }
warn() { echo -e "${YELLOW}⚠${NC} $*"; }
fail() { echo -e "${RED}✗${NC} $*"; exit 1; }

echo ""
echo "════════════════════════════════════════════════════════"
echo "  stacktest.sh — Ampersand test environment setup"
echo "════════════════════════════════════════════════════════"

# ──────────────────────────────────────────────────────────────────────────────
# STAP 1: PHP beschikbaar?
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "Stap 1: PHP controleren..."
PHP_BIN="$(command -v php 2>/dev/null || true)"
[[ -z "$PHP_BIN" ]] && fail "php niet gevonden in PATH. Installeer PHP (bijv. 'brew install php')."
PHP_VERSION="$("$PHP_BIN" --version 2>&1 | head -1)"
ok "PHP aanwezig: $PHP_VERSION"

# Controleer mysqli-extensie
if ! "$PHP_BIN" -r "new mysqli;" 2>&1 | grep -q "mysqli"; then
    if "$PHP_BIN" -r "exit(extension_loaded('mysqli') ? 0 : 1);" 2>/dev/null; then
        ok "PHP mysqli-extensie beschikbaar."
    else
        fail "PHP mysqli-extensie niet geladen. Controleer php.ini of installeer de extensie."
    fi
fi
ok "PHP mysqli-extensie beschikbaar."

# ──────────────────────────────────────────────────────────────────────────────
# STAP 2: Database beschikbaar maken
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "Stap 2: Database starten / opsporen..."

MYSQL_HOST="127.0.0.1"
MYSQL_PORT="3306"
# We verbinden uiteindelijk als root zonder wachtwoord (dat is wat de PHP-scripts doen).
# Maar om de GRANT te kunnen uitvoeren hebben we soms een rootwachtwoord nodig.
DB_ROOT_PASS=""   # wachtwoord waarmee we de GRANT uitvoeren (leeg of uit .env)

# Lees wachtwoord uit .env als dat bestaat
ENV_FILE="$SCRIPT_DIR/.env"
if [[ -f "$ENV_FILE" ]]; then
    # shellcheck disable=SC2046
    export $(grep -v '^#' "$ENV_FILE" | grep -v '^[[:space:]]*$' | xargs) 2>/dev/null || true
    DB_ROOT_PASS="${MYSQL_ROOT_PASSWORD:-${MYSQL_PASSWORD:-}}"
fi

# --- Helper: probeer verbinding te maken met mariadb/mysql ---
# Argumenten: host port user pass
_try_connect() {
    local host="$1" port="$2" user="$3" pass="$4"
    if [[ -n "$pass" ]]; then
        docker exec ampersand-mariadb mariadb -u"$user" -p"$pass" \
            -e "SELECT 1" >/dev/null 2>&1 && return 0
        mysql -h"$host" -P"$port" -u"$user" -p"$pass" \
            --connect-timeout=5 -e "SELECT 1" >/dev/null 2>&1 && return 0
    fi
    docker exec ampersand-mariadb mariadb -u"$user" \
        -e "SELECT 1" >/dev/null 2>&1 && return 0
    mysql -h"$host" -P"$port" -u"$user" \
        --connect-timeout=5 -e "SELECT 1" >/dev/null 2>&1 && return 0
    return 1
}

# --- Helper: voer SQL uit via docker exec, met of zonder wachtwoord ---
_exec_sql() {
    local sql="$1"
    # Probeer via docker exec ampersand-mariadb (eigen container, geen wachtwoord nodig)
    if docker exec ampersand-mariadb mariadb -u root \
           -e "$sql" >/dev/null 2>&1; then
        return 0
    fi
    # Probeer via docker exec prototype-db met rootwachtwoord
    if docker exec prototype-db mariadb -u root -p"$DB_ROOT_PASS" \
           -e "$sql" >/dev/null 2>&1; then
        return 0
    fi
    # Probeer via lokale mysql-client
    if mysql -h"$MYSQL_HOST" -P"$MYSQL_PORT" -u root -p"$DB_ROOT_PASS" \
           --connect-timeout=5 -e "$sql" >/dev/null 2>&1; then
        return 0
    fi
    return 1
}

# Probeer de eigen Ampersand MariaDB-container te starten
DOCKER_USED=false
if docker inspect ampersand-mariadb >/dev/null 2>&1; then
    CONTAINER_STATUS="$(docker inspect -f '{{.State.Status}}' ampersand-mariadb 2>/dev/null || echo unknown)"
    if [[ "$CONTAINER_STATUS" == "running" ]]; then
        ok "ampersand-mariadb al actief."
        DOCKER_USED=true
    else
        echo "  ampersand-mariadb bestaat maar staat op '$CONTAINER_STATUS'. Probeer te starten..."
        if docker start ampersand-mariadb >/dev/null 2>&1; then
            ok "ampersand-mariadb gestart."
            DOCKER_USED=true
        else
            warn "ampersand-mariadb kon niet starten (poort al bezet?)."
        fi
    fi
elif [[ -f "$SCRIPT_DIR/docker-compose.yml" ]]; then
    echo "  Probeer docker-compose up voor ampersand-mariadb..."
    if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" up -d mariadb >/dev/null 2>&1; then
        ok "ampersand-mariadb gestart via docker-compose."
        DOCKER_USED=true
    else
        warn "docker-compose start mislukt (poort al bezet?)."
    fi
fi

# Wacht tot MariaDB reageert (max 30 seconden)
echo "  Wachten tot database reageert..."
DB_READY=false
for i in $(seq 1 30); do
    # Probeer eerst via eigen container, dan via prototype-db, dan via TCP
    if docker exec ampersand-mariadb mariadb -u root -e "SELECT 1" >/dev/null 2>&1; then
        DB_READY=true; break
    fi
    if docker exec prototype-db mariadb -u root -p"$DB_ROOT_PASS" \
           -e "SELECT 1" >/dev/null 2>&1; then
        DB_READY=true; break
    fi
    if mysql -h"$MYSQL_HOST" -P"$MYSQL_PORT" -u root -p"$DB_ROOT_PASS" \
           --connect-timeout=3 -e "SELECT 1" >/dev/null 2>&1; then
        DB_READY=true; break
    fi
    sleep 1
done
$DB_READY || fail "Database reageert niet na 30 seconden. Controleer Docker of MySQL-installatie."
ok "Database is bereikbaar."

# ──────────────────────────────────────────────────────────────────────────────
# STAP 3: GRANT root@'%' zonder wachtwoord toepassen
#         (PHP-scripts verbinden als root@127.0.0.1 zonder wachtwoord)
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "Stap 3: GRANT root@'%' toepassen..."
GRANT_SQL="GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY '' WITH GRANT OPTION; FLUSH PRIVILEGES;"

if _exec_sql "$GRANT_SQL"; then
    ok "GRANT toegepast."
else
    warn "GRANT kon niet worden toegepast. Tests kunnen mislukken met 'Access denied'."
    warn "Voer handmatig uit:"
    warn "  docker exec prototype-db mariadb -u root -p<wachtwoord> -e \"$GRANT_SQL\""
fi

# ──────────────────────────────────────────────────────────────────────────────
# STAP 4: Verifieer PHP→DB verbinding (zoals de Ampersand-tests dat doen)
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "Stap 4: PHP→DB verbinding testen..."
PHP_CHECK=$("$PHP_BIN" -r "
    mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);
    try {
        \$c = mysqli_connect('127.0.0.1', 'root', '');
        echo 'OK:' . mysqli_get_server_info(\$c);
        mysqli_close(\$c);
    } catch (Exception \$e) {
        echo 'FAIL:' . \$e->getMessage();
    }
" 2>&1)

if [[ "$PHP_CHECK" == OK:* ]]; then
    ok "PHP verbindt met DB: ${PHP_CHECK#OK:}"
else
    fail "PHP kan niet verbinden met DB: ${PHP_CHECK#FAIL:}"
fi

# ──────────────────────────────────────────────────────────────────────────────
# STAP 5: Environment instellen voor stack test
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "Stap 5: Environment instellen..."
export MYSQL_HOST="127.0.0.1"
export MYSQL_USER="root"
export MYSQL_PASSWORD=""
ok "MYSQL_HOST=$MYSQL_HOST  MYSQL_USER=$MYSQL_USER  MYSQL_PASSWORD=(leeg)"

# ──────────────────────────────────────────────────────────────────────────────
# STAP 6: stack test
# ──────────────────────────────────────────────────────────────────────────────
echo ""
echo "════════════════════════════════════════════════════════"
echo "  Alle voorwaarden OK. Start: stack test"
echo "════════════════════════════════════════════════════════"
echo ""

cd "$SCRIPT_DIR"
exec stack test
