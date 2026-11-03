#!/usr/bin/env bash
#
# The demonstrator of a migration with a system of contexts.
#
#   ./demo.sh up [kurk|garden]     build what is needed and start the demonstration
#   ./demo.sh down [kurk|garden]   remove every container, the network and the data of the demonstration
#   ./demo.sh build [kurk|garden]  build the images only
#
# After `up`, open http://localhost:8200 (set REGIE_PORT and APP_PORT for other ports).
# The images of the three systems are built from what `ampersand deploy` generated in <demo>/deploy.
# FRAMEWORK_IMAGE names the prototype framework to build on; it has to contain a compiler that knows systems of contexts.
set -euo pipefail
cd "$(dirname "$0")"

action="${1:-}"; demo="${2:-kurk}"
export FRAMEWORK_IMAGE="${FRAMEWORK_IMAGE:-ampersandtarski/prototype-framework:v2.18.0}"
REGIE_PORT="${REGIE_PORT:-8200}"; APP_PORT="${APP_PORT:-8201}"
label="migration-demo=$demo"
[ -f "$demo/demo.json" ] || { echo "unknown demonstration: $demo"; exit 64; }

build() {
  # The three applications, with the Dockerfiles and the compose file that the compiler generated.
  docker compose -p "$demo" -f "$demo/deploy/compose.yaml" build
  docker build -q --build-arg DEMO="$demo" -t "$demo-regie" -f regie/Dockerfile . >/dev/null
}

down() {
  local containers
  containers="$(docker ps -aq --filter "label=$label" --filter "name=$demo-")"
  [ -z "$containers" ] || docker rm -f $containers >/dev/null
  docker rm -f "$demo-regie" >/dev/null 2>&1 || true
  docker volume rm -f "$demo-db-data" >/dev/null 2>&1 || true
  docker network rm "$demo-net" >/dev/null 2>&1 || true
}

case "$action" in
  build) build ;;
  down) down ;;
  up)
    build
    down
    docker network create "$demo-net" >/dev/null
    docker run -d --name "$demo-regie" --network "$demo-net" -p "$REGIE_PORT:8000" \
      -e APP_PORT="$APP_PORT" -v /var/run/docker.sock:/var/run/docker.sock "$demo-regie" >/dev/null
    echo "The demonstration '$demo' starts. Open http://localhost:$REGIE_PORT"
    ;;
  *) sed -n '2,11p' "$0" | sed 's/^# \{0,1\}//'; exit 64 ;;
esac
