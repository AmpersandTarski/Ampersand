#!/usr/bin/env bash
# Setup for the RAP benchmark stack of issue #1687.
# Generates the RAP backend with the delta-sql compiler, lays out the runtime
# directories under BENCH_ROOT, and starts the compose stack.
set -euo pipefail

BENCH_ROOT="${BENCH_ROOT:-/tmp/rap1687-bench}"
FRAMEWORK_DIR="${FRAMEWORK_DIR:-$HOME/git/PrototypeFramework-delta}"
RAP_DIR="${RAP_DIR:-$HOME/git/RAP-incremental-evaluation}"
AMPERSAND_BIN="${AMPERSAND_BIN:-$HOME/git/Ampersand-delta-sql/.stack-work/install/aarch64-osx/b14ad57f46b2fff7e07bcc323ad7c0cfde78bc0158a3d02afb1e5537ea51082e/9.6.6/bin/ampersand}"
HERE="$(cd "$(dirname "$0")" && pwd)"

mkdir -p "$BENCH_ROOT"/{off,on}/{config,data,log}

# 0. Local throwaway database passwords, generated once under BENCH_ROOT so no
#    credential lives in the repository. Sourced by lib.sh and the compose stack.
if [ ! -f "$BENCH_ROOT/db.env" ]; then
  cat > "$BENCH_ROOT/db.env" <<EOF
RAP_BENCH_PW=$(openssl rand -hex 12)
RAP_BENCH_ROOT_PW=$(openssl rand -hex 12)
EOF
fi
set -a; . "$BENCH_ROOT/db.env"; set +a

# 1. Generate the backend (generics incl. delta queries) once; both instances share it.
if [ ! -f "$BENCH_ROOT/backend-gen/generics/conjuncts.json" ]; then
  "$AMPERSAND_BIN" proto --no-frontend "$RAP_DIR/RAP4/RAP4.adl" \
    --proto-dir "$BENCH_ROOT/backend-gen" --crud-defaults cRud
fi
"$AMPERSAND_BIN" --version > "$BENCH_ROOT/VERSIONS.txt"
git -C "$FRAMEWORK_DIR" log -1 --format='framework: %h %s' >> "$BENCH_ROOT/VERSIONS.txt"
git -C "$RAP_DIR" log -1 --format='rap: %h %s' >> "$BENCH_ROOT/VERSIONS.txt"

# 2. Per-instance config: identical apart from the delta switch.
for mode in off on; do
  cp "$FRAMEWORK_DIR/backend/config/logging.php" "$BENCH_ROOT/$mode/config/"
  cat > "$BENCH_ROOT/$mode/config/project.yaml" <<EOF
# Benchmark instance '$mode' (issue #1687). Single deliberate difference
# between the instances: transactions.deltaConjunctMaintenance.
settings:
  global.scriptTimeout: 0
  session.loginEnabled: false
  transactions.deltaConjunctMaintenance: '$mode'
EOF
done

# 3. PHP limits (same values the FC5 shadow run used).
cat > "$BENCH_ROOT/php-overrides.ini" <<'EOF'
memory_limit = 2G
max_execution_time = 0
EOF

# 4. Start the stack (db.env is exported above, so compose sees the passwords).
BENCH_ROOT="$BENCH_ROOT" FRAMEWORK_DIR="$FRAMEWORK_DIR" \
  docker compose -f "$HERE/docker-compose.yml" up -d

echo "Wait for MariaDB, then install both instances:"
echo "  curl -s 'http://localhost:8191/api/v1/admin/installer?defaultPop=true'"
echo "  curl -s 'http://localhost:8192/api/v1/admin/installer?defaultPop=true'"
