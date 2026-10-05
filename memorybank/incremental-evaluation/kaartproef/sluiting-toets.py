#!/usr/bin/env python3
"""Toetst dat conj_182 zoals gegenereerd en de EXCEPT-vorm hetzelfde antwoord geven, ook als het niet leeg is.

Zet in één databasetransactie twee paren in `bereikt` die niet in hangtAf+ zitten, vraagt beide
queries hun rijen, en rolt de transactie terug.
"""
import json
import subprocess
import sys
from pathlib import Path

HIER = Path(__file__).resolve().parent
DB = "ie-kaartproef-prototype-db"
conj = {c["id"]: c for c in json.loads((HIER / "gen598/generics/conjuncts.json").read_text())}
a = conj["conj_182"]["violationsSQL"]
cte = a[a.index("with recursive"):a.index(") as enforceAnsisql")]
c = f'select "SrcArtefact" as src, "TgtArtefact" as tgt from "bereikt" except select src, tgt from ({cte}) as s'
# laag 0 steunt nergens op, dus een paar met een bron uit laag 0 zit niet in de sluiting
vals = [("Proef.d0.00001", "Proef.d4.00002"), ("Proef.d0.00003", "Proef.d0.00004")]
script = "SET SESSION sql_mode='ANSI,TRADITIONAL'; START TRANSACTION;\n"
for s, t in vals:
    script += f"INSERT INTO \"bereikt\" (\"SrcArtefact\", \"TgtArtefact\") VALUES ('{s}', '{t}');\n"
script += f"SELECT 'A', src, tgt FROM ({a}) AS x ORDER BY 2, 3;\nSELECT 'C', src, tgt FROM ({c}) AS x ORDER BY 2, 3;\nROLLBACK;\n"
script += 'SELECT \'NA\', COUNT(*) FROM "bereikt";\n'
naam = subprocess.run(["docker", "exec", DB, "sh", "-c",
                       'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" -e "SELECT table_schema FROM information_schema.tables WHERE table_name=\'__conj_violation_cache__\' LIMIT 1"'],
                      capture_output=True, text=True).stdout.strip()
r = subprocess.run(["docker", "exec", "-i", DB, "sh", "-c", 'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" "$1"', "sh", naam],
                   input=script, capture_output=True, text=True)
if r.returncode != 0:
    sys.exit(r.stderr[-800:])
rijen = [l.split("\t") for l in r.stdout.splitlines()]
A = sorted(tuple(l[1:]) for l in rijen if l[0] == "A")
C = sorted(tuple(l[1:]) for l in rijen if l[0] == "C")
print("ingevoegd:", sorted(vals))
print("gegenereerd:", A)
print("EXCEPT:     ", C)
print("gelijk en precies de ingevoegde paren:", A == C == sorted(vals))
print("rijen in bereikt na terugrollen:", [l[1] for l in rijen if l[0] == "NA"])
