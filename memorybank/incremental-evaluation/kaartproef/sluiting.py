#!/usr/bin/env python3
"""Meet op de proefdatabase waar de tijd van conj_182 (bereikt |- hangtAf+) blijft.

Drie queries, elk drie keer, server-side getimed met SHOW PROFILES:
  a. de query zoals de compiler haar genereert (left join van bereikt op de afgeleide sluiting);
  b. alleen de sluiting (de recursieve CTE), geteld;
  c. dezelfde vraag als a, geschreven met EXCEPT.
"""
import json
import subprocess
import sys
from pathlib import Path

HIER = Path(__file__).resolve().parent
DB = "ie-kaartproef-prototype-db"
conj = {c["id"]: c for c in json.loads((HIER / "gen598/generics/conjuncts.json").read_text())}
a = conj["conj_182"]["violationsSQL"]
start = a.index("with recursive")
eind = a.index(") as enforceAnsisql")
cte = a[start:eind]
b = f"select count(*) from ({cte}) as s"
c = f'select "SrcArtefact" as src, "TgtArtefact" as tgt from "bereikt" except select src, tgt from ({cte}) as s'
d = conj["conj_135"]["violationsSQL"]

script = "SET SESSION sql_mode='ANSI,TRADITIONAL'; SET profiling=1; SET profiling_history_size=50;\n"
for q in (a, b, c, d):
    for _ in range(3):
        script += f"SELECT COUNT(*) FROM ({q}) AS meting;\n" if q is not b else q + ";\n"
script += "SHOW PROFILES;\n"
naam = subprocess.run(["docker", "exec", DB, "sh", "-c",
                       'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" -e "SELECT table_schema FROM information_schema.tables WHERE table_name=\'__conj_violation_cache__\' LIMIT 1"'],
                      capture_output=True, text=True).stdout.strip()
r = subprocess.run(["docker", "exec", "-i", DB, "sh", "-c", 'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" "$1"', "sh", naam],
                   input=script, capture_output=True, text=True)
if r.returncode != 0:
    sys.exit(r.stderr[-800:])
regels = [l.split("\t") for l in r.stdout.splitlines()]
profielen = [l for l in regels if len(l) == 3 and l[0].isdigit()]
namen = ["a. gegenereerd (bereikt left join sluiting)"] * 3 + ["b. alleen de sluiting"] * 3 + \
        ["c. bereikt EXCEPT sluiting"] * 3 + ["d. conj_135 (sluiting left join bereikt)"] * 3
uitkomst = [l[0] for l in regels if len(l) == 1]
print("uitkomsten (aantal rijen):", uitkomst)
for n, p in zip(namen, profielen[-12:]):
    print(f"{n:45} {float(p[1]) * 1000:9.1f} ms")
