#!/usr/bin/env bash
#
# Walk through the demonstration as its presenter would, and check what the audience is told.
#
#   ./test.sh [kurk|garden]
#
# The script starts the demonstration afresh, presses the three buttons and "start again" through the director,
# and works in the applications through the flipper, as a browser does.
# It follows the phases of "Data Migration under a Changing Schema in Ampersand" (RAMiCS 2024):
# pre-deployment, the moment of transition, the repair by users, and the moment of completion.
# KEEP=1 leaves the demonstration running at the end.
set -uo pipefail
cd "$(dirname "$0")"

demo="${1:-kurk}"
REGIE_PORT="${REGIE_PORT:-8200}"; APP_PORT="${APP_PORT:-8201}"
regie="http://localhost:$REGIE_PORT"; app="http://localhost:$APP_PORT/api/v1"
ifc="$(jq -r .test.interface "$demo/demo.json")"
field="$(jq -r .test.field "$demo/demo.json")"
value="$(jq -r .test.value "$demo/demo.json")"
# Optional: the rows are a list inside the screen (.test.list).
list="$(jq -r '.test.list // ""' "$demo/demo.json")"
jar="$(mktemp)"; failures=0

log() { printf '%s\n' "$*"; }
expect() {
  if [ "$2" = "$3" ]; then log "  ok    $1"
  else log "  FAIL  $1"; log "        expected: $2"; log "        actual:   $3"; failures=$((failures + 1)); fi
}
state() { curl -s "$regie/state" | jq -r "$1"; }
press() { curl -s -X POST "$regie/action/$1" | jq -r '.refused | if . == "" then "accepted" else "refused" end'; }
# await <jq condition on the state> [seconds]
await() {
  local deadline=$((SECONDS + ${2:-420}))
  until [ "$(state "$1" 2>/dev/null)" = "true" ]; do
    [ $SECONDS -lt $deadline ] || { log "  FAIL  waited in vain for: $1"; log "        $(state '{phase, busy, ready, error}' | tr -d '\n')"; failures=$((failures + 1)); return 1; }
    sleep 2
  done
}
buttons() { state '[ (.busy == "" and .phase == "existing"), (.busy == "" and .phase == "prepared" and .ready), (.busy == "" and .phase == "migrating" and (.violations | length) == 0) ] | map(if . then "on" else "off" end) | join(" ")'; }
systems() { docker ps --filter "label=migration-demo=$demo" --format '{{.Names}}' | sed "s/^$demo-//" | sort | tr '\n' ' ' | sed 's/ $//'; }
routed() { curl -sI "http://localhost:$APP_PORT/" | tr -d '\r' | awk 'tolower($1) == "x-routed-to:" { print $2 }'; }
# What a user does in the application that the flipper shows. A new system means a new session.
session() { : >"$jar"; curl -s -c "$jar" -o /dev/null "$app/app/navbar"; }
screen() { curl -s -b "$jar" -c "$jar" "$app/resource/SESSION/1/$ifc"; }
rows() { screen | jq -c --arg l "$list" 'if $l == "" then . else .[$l] end'; }
patch() { curl -s -b "$jar" -c "$jar" -X PATCH -H 'Content-Type: application/json' -d "$2" "$app/$1"; }
path_of() { rows | jq -r --arg id "$1" '.[] | select(._id_ == $id) | ._path_'; }
count() { rows | jq -r 'length'; }
signals() { curl -s -b "$jar" -c "$jar" "$app/app/navbar" | jq -r '[.notifications.signals[].violations[]] | length'; }
without() { rows | jq -r --arg f "$field" '[.[] | select(.[$f] == null) | ._id_] | join(" ")'; }
with_value() { rows | jq -r --arg f "$field" '[.[] | select(.[$f] != null) | ._id_][0]'; }
outcome() { jq -r 'if .isCommitted == true then "committed" elif .isCommitted == false then "refused" else "error: " + ((.msg // "no answer") | tostring | .[0:80]) end'; }
# Some systems edit one row in a screen of its own, which saves a change as one transaction (.test.single).
# While such a system serves the users, s_concept names the concept of a row; otherwise it is empty.
s_concept=""
s_ifc="$(jq -r '.test.single.interface // ""' "$demo/demo.json")"
s_name="$(jq -r '.test.single.name // ""' "$demo/demo.json")"
one() { patch "resource/$s_concept/$1/$s_ifc" "$2" | outcome; }
# The texts of a change are put in variables first: bash 3.2 of macOS mangles braces with a comma inside nested quotes.
# A user adds a row without what the new rule asks: with the plus of the list, or in the screen for one row.
create() {
  if [ -z "$s_concept" ]; then
    curl -s -b "$jar" -c "$jar" -X POST -H 'Content-Type: application/json' -d '{}' "$app/resource/SESSION/1/$ifc" | outcome
  else
    # Every new row gets an identifier of its own; a counter would be lost, because the caller is a subshell.
    local id="new-$RANDOM$RANDOM"
    local change="[{\"op\":\"replace\",\"path\":\"/$s_name\",\"value\":\"Extra $id\"}]"
    one "$id" "$change"
  fi
}
# A user adds a row with what the new rule asks, in one transaction. Only the screen for one row can do that.
create_with() {
  local id="new-$RANDOM$RANDOM"
  local change="[{\"op\":\"replace\",\"path\":\"/$s_name\",\"value\":\"Extra $id\"},{\"op\":\"replace\",\"path\":\"/$field\",\"value\":\"$1\"}]"
  one "$id" "$change"
}
give() {
  local change="[{\"op\":\"replace\",\"path\":\"/$field\",\"value\":\"$2\"}]"
  if [ -z "$s_concept" ]; then patch "$(path_of "$1")" "$change" | outcome; else one "$1" "$change"; fi
}
take() {
  local current; current="$(rows | jq -r --arg f "$field" --arg id "$1" '.[] | select(._id_ == $id) | .[$f]')"
  local change="[{\"op\":\"remove\",\"path\":\"/$field\",\"value\":\"$current\"}]"
  if [ -z "$s_concept" ]; then patch "$(path_of "$1")" "$change" | outcome; else one "$1" "$change"; fi
}
# A change in the list itself, which a system with a screen for one row does not allow.
in_list() { local change="[{\"op\":\"replace\",\"path\":\"/$field\",\"value\":\"$2\"}]"; patch "$(path_of "$1")" "$change" | outcome | cut -c1-5; }

log "== $demo: the demonstration starts"
./demo.sh up "$demo" >/dev/null || { log "  the demonstration did not start"; exit 1; }
await '.phase == "existing" and .busy == ""' || exit 1

log "-- only the existing system runs"
expect "the containers are the existing system, its database server and the flipper" "database existing-system flipper" "$(systems)"
expect "the flipper sends the users to the existing system" "existing" "$(routed)"
expect "only 'start migration' can be pressed" "on off off" "$(buttons)"
session
n0="$(count)"; w0="$(without | wc -w | tr -d ' ')"
expect "the existing system holds rows that the new rule does not allow" "true" "$([ "$w0" -gt 0 ] && echo true)"
expect "a user of the existing system adds a row that the new rule does not allow" "committed" "$(create)"
expect "'switch over' is refused before the migration has started" "refused" "$(press switch)"

log "-- start migration (pre-deployment)"
expect "'start migration' is accepted" "accepted" "$(press start)"
await '.phase == "prepared" and .ready and .busy == ""' || exit 1
expect "two more containers run" "database desired-system existing-system flipper migration-system" "$(systems)"
expect "the users are still in the existing system" "existing" "$(routed)"
expect "only 'switch over' can be pressed" "off on off" "$(buttons)"
expect "the migration system knows the work that waits" "$((w0 + 1))" "$(state '.violations | length')"
expect "a user of the existing system adds one more row that the new rule does not allow" "committed" "$(create)"
await '.phase == "prepared" and .ready and (.violations | length) == '"$((w0 + 2))" 60
expect "the migration system has caught up with it" "$((w0 + 2))" "$(state '.violations | length')"
expect "'clean up migration' is refused before the switch" "refused" "$(press cleanup)"

log "-- switch over (the moment of transition)"
expect "'switch over' is accepted" "accepted" "$(press switch)"
await '.phase == "migrating" and .busy == ""' || exit 1
expect "the flipper sends the users to the migration system" "migration" "$(routed)"
expect "no button can be pressed while violations are left" "off off off" "$(buttons)"
session
expect "the users find all rows of the existing system" "$((n0 + 2))" "$(count)"
expect "and the rows that are left to repair" "$((w0 + 2))" "$(without | wc -w | tr -d ' ')"
expect "the application itself tells its user what is left to repair" "$((w0 + 2))" "$(signals)"
s_concept="$(jq -r '.test.single.migrationConcept // ""' "$demo/demo.json")"
kept="$(with_value)"
# A new row without what the rule asks is work, as in step 5 of the paper: it never satisfied the rule, so it is not fixed.
expect "a new row without what the rule asks is accepted, and becomes work" "committed" "$(create)"
extra=1
await '(.violations | length) == '"$((w0 + 3))" 30
expect "the work that is left grows by that row" "$((w0 + 3))" "$(state '.violations | length')"
if [ -n "$s_concept" ]; then
  # After the switch the users have the screens of the desired system.
  expect "the list cannot be edited, as in the desired system" "error" "$(in_list "$kept" "$value")"
  expect "a new row that is saved with what the rule asks, in one transaction, is accepted" "committed" "$(create_with "$value")"
  extra=2
  expect "and it is among the rows" "$((n0 + 4))" "$(count)"
fi
# Steps 3 and 4 of the paper: what satisfies the rule is fixed, and a fixed row cannot violate it again.
expect "a row that satisfies the rule cannot be made to violate it" "refused" "$(take "$kept")"
first="$(without | cut -d' ' -f1)"
expect "a user repairs a violation" "committed" "$(give "$first" "$value")"
await '(.violations | length) == '"$((w0 + 2))" 30
expect "the work that is left has shrunk by one" "$((w0 + 2))" "$(state '.violations | length')"
expect "the repaired violation cannot return" "refused" "$(take "$first")"
expect "'clean up migration' is refused while violations are left" "refused" "$(press cleanup)"
for id in $(without); do
  expect "a user repairs $id" "committed" "$(give "$id" "$value")"
done
await '(.violations | length) == 0' 30
expect "only 'clean up migration' can be pressed" "off off on" "$(buttons)"

log "-- clean up migration (the moment of completion)"
expect "'clean up migration' is accepted" "accepted" "$(press cleanup)"
await '.phase == "completed" and .busy == ""' || exit 1
expect "the flipper sends the users to the desired system" "desired" "$(routed)"
expect "the migration system and the existing system are gone" "database desired-system flipper" "$(systems)"
expect "and so are their databases" "$(jq -r --arg s "$(jq -r .desired "$demo/demo.json")" '.contexts[] | select(.service == $s) | .defaultDatabase' "$demo/deploy/system.json")" \
  "$(state '.databases | to_entries | map(.value) | join(" ")')"
expect "no button can be pressed" "off off off" "$(buttons)"
session
s_concept="$(jq -r '.test.single.concept // ""' "$demo/demo.json")"
expect "the desired system holds all rows" "$((n0 + 2 + extra))" "$(count)"
expect "all of them satisfy the new rule" "" "$(without)"
expect "the desired system refuses a new row that violates its invariant" "refused" "$(create)"
expect "and a row that would lose what the rule asks" "refused" "$(take "$kept")"
if [ -n "$s_concept" ]; then
  expect "the list of the desired system cannot be edited" "error" "$(in_list "$kept" "$value")"
  expect "a new row that is saved with what the rule asks, in one transaction, is accepted" "committed" "$(create_with "$value")"
  expect "and it is among the rows" "$((n0 + 3 + extra))" "$(count)"
fi

log "-- start again"
expect "'start again' is accepted" "accepted" "$(press reset)"
await '.route == "existing"' || exit 1
expect "the application answers as soon as the flipper points to the existing system" "200" \
  "$(curl -s -o /dev/null -w '%{http_code}' "$app/app/navbar")"
await '.phase == "existing" and .busy == ""' || exit 1
expect "only 'start migration' can be pressed" "on off off" "$(buttons)"
session
expect "the existing system holds its original rows again" "$n0" "$(count)"

rm -f "$jar"
[ "${KEEP:-}" = "1" ] || ./demo.sh down "$demo"
if [ "$failures" -eq 0 ]; then log "== $demo: all checks passed"; else log "== $demo: $failures checks failed"; fi
[ "$failures" -eq 0 ]
