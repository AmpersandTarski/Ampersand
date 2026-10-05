#!/usr/bin/env bash
# De meetronden van de kaartproef: per omvang eerst de wandkloktijd zonder tracing, dan de tijdverdeling met tracing.
set -uo pipefail
cd "$(dirname "$0")"
python3 meet.py draai 250 --otel
for n in 500 1000; do
  PROEF_REPS=10 PROEF_OPWARM=1 python3 meet.py zaai "$n"
  PROEF_REPS=10 PROEF_OPWARM=1 python3 meet.py draai "$n"
  PROEF_REPS=10 PROEF_OPWARM=1 python3 meet.py draai "$n" --otel
done
