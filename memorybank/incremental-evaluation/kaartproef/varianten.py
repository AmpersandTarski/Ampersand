#!/usr/bin/env python3
"""Meet varianten van de anti-join `l - r` waarin r een berekende term is, op de proefdatabase.

Voor conj_182 (bereikt - hangtAf+) en conj_203 (moetEerst - (I \\/ bereikt);nietActueel):
  V0  zoals gegenereerd: l LEFT JOIN (r) ON src,tgt WHERE r.src IS NULL OR r.tgt IS NULL
  V1  als V0, met alleen r.src IS NULL
  V2  NOT EXISTS, gecorreleerd op (r)
  V3  l EXCEPT r
  V4  (src,tgt) NOT IN (r)
Elke query drie keer, server-side getimed met SHOW PROFILES; daarna EXPLAIN van V0 en V3.
Elke variant draait ook in een transactie met twee extra paren in l, om te zien dat zij dezelfde rijen geeft.
"""
import json
import statistics
import subprocess
import sys
from pathlib import Path

HIER = Path(__file__).resolve().parent
DB = "ie-kaartproef-prototype-db"
conj = {c["id"]: c for c in json.loads((HIER / "gen598/generics/conjuncts.json").read_text())}


def delen(cid):
    """Splitst de gegenereerde query in l (als t1) en r (als t2)."""
    q = conj[cid]["violationsSQL"]
    kop = "select distinct t1.src as src, t1.tgt as tgt from "
    assert q.startswith(kop), q[:80]
    rest = q[len(kop):]
    i = rest.index(" as t1 left join ")
    l = rest[:i]
    rest2 = rest[i + len(" as t1 left join "):]
    j = rest2.rindex(" as t2 on ")
    return q, l, rest2[:j]


def varianten(cid):
    v0, l, r = delen(cid)
    return {
        "V0 gegenereerd (left join, OR)": v0,
        "V1 left join, een IS NULL": f"select distinct t1.src as src, t1.tgt as tgt from {l} as t1 left join {r} as t2 on (t1.src = t2.src) and (t1.tgt = t2.tgt) where t2.src is null",
        "V2 NOT EXISTS": f"select distinct t1.src as src, t1.tgt as tgt from {l} as t1 where not exists (select 1 from {r} as t2 where t2.src = t1.src and t2.tgt = t1.tgt)",
        "V3 EXCEPT": f"select src, tgt from {l} as t1 except select src, tgt from {r} as t2",
        "V4 NOT IN": f"select distinct t1.src as src, t1.tgt as tgt from {l} as t1 where (t1.src, t1.tgt) not in (select src, tgt from {r} as t2)",
    }


def mysql(script):
    naam = subprocess.run(["docker", "exec", DB, "sh", "-c",
                           'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" -e "SELECT table_schema FROM information_schema.tables WHERE table_name=\'__conj_violation_cache__\' LIMIT 1"'],
                          capture_output=True, text=True).stdout.strip()
    r = subprocess.run(["docker", "exec", "-i", DB, "sh", "-c", 'mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" "$1"', "sh", naam],
                       input="SET SESSION sql_mode='ANSI,TRADITIONAL';\n" + script, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(r.stderr[-1500:])
    return [l.split("\t") for l in r.stdout.splitlines()]


print("versie:", mysql("SELECT VERSION();")[0][0])
EXTRA = {"conj_182": ('"bereikt"', '"SrcArtefact"', '"TgtArtefact"'), "conj_203": ('"moetEerst"', '"SrcArtefact"', '"TgtArtefact"')}
for cid in ("conj_182", "conj_203"):
    vs = varianten(cid)
    print(f"\n== {cid}")
    script = "SET profiling=1; SET profiling_history_size=100;\n"
    for q in vs.values():
        script += f"SELECT COUNT(*) FROM ({q}) AS meting;\n" * 3
    script += "SHOW PROFILES;\n"
    uit = mysql(script)
    prof = [float(l[1]) * 1000 for l in uit if len(l) == 3 and l[0].isdigit()][-3 * len(vs):]
    for i, naam in enumerate(vs):
        drie = prof[3 * i:3 * i + 3]
        print(f"  {naam:34} mediaan {statistics.median(drie):9.1f} ms   ({', '.join(f'{x:.0f}' for x in drie)})")
    # gelijkwaardigheid op een niet-leeg antwoord, in een transactie die wordt teruggerold
    tabel, s, t = EXTRA[cid]
    script = "START TRANSACTION;\n"
    for a, b in (("Proef.d0.00001", "Proef.d4.00002"), ("Proef.d0.00003", "Proef.d0.00004")):
        script += f"INSERT INTO {tabel} ({s}, {t}) VALUES ('{a}', '{b}');\n"
    for i, q in enumerate(vs.values()):
        script += f"SELECT 'V{i}', src, tgt FROM ({q}) AS x ORDER BY 2, 3;\n"
    script += "ROLLBACK;\n"
    uit = mysql(script)
    per = {}
    for l in uit:
        per.setdefault(l[0], []).append(tuple(l[1:]))
    print("  rijen per variant met twee extra paren in l:", {k: len(v) for k, v in per.items()},
          "| alle gelijk:", len({tuple(v) for v in per.values()}) == 1 and len(per) == len(vs))
    for naam in ("V0 gegenereerd (left join, OR)", "V3 EXCEPT"):
        print(f"  EXPLAIN {naam}:")
        for l in mysql("EXPLAIN " + vs[naam] + ";"):
            print("    ", " | ".join(x[:38] for x in l))
