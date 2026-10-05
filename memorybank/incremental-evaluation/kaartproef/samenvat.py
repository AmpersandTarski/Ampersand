#!/usr/bin/env python3
"""Vat data/<N>-<stand>[-otel].json samen: medianen per stand en soort, en de tijdverdeling uit de traces.

Gebruik: samenvat.py [data-map] [generics-map]
"""
import json
import os
import statistics
import sys
from collections import defaultdict
from pathlib import Path

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else Path(__file__).resolve().parent / "data")
GEN = Path(sys.argv[2]) if len(sys.argv) > 2 else None
STANDEN = ("off", "off+skip", "shadow", "on", "on+skip")
SOORTEN = ("stempel", "afhankelijkheid")
med = lambda xs: statistics.median(xs) if xs else float("nan")

profiel = {}
if GEN:
    for c in json.loads((GEN / "conjuncts.json").read_text()):
        regels = (c.get("invariantRuleNames") or []) + (c.get("signalRuleNames") or [])
        profiel[c["id"]] = ((c.get("costProfile") or {}).get("class"), bool(c.get("deltaQueries")), regels[:1])


def telt(d):
    """De metingen zonder de opwarmers: de eerste aanroepen van elke soort."""
    per = defaultdict(list)
    for m in d["metingen"]:
        per[m["soort"]].append(m)
    opwarm = d.get("opwarm", None)
    uit = {}
    for s, ms in per.items():
        k = opwarm if opwarm is not None else (2 if len(ms) >= 22 else 1)
        uit[s] = ms[k:]
    return uit


groottes = sorted({int(p.name.split("-")[0]) for p in DATA.glob("*-*.json") if p.name[0].isdigit()})
print("## Wandkloktijd per aanroep, zonder tracing (mediaan in ms)\n")
print("| N | soort | " + " | ".join(STANDEN) + " |")
print("|---|---|" + "---|" * len(STANDEN))
for n in groottes:
    for s in SOORTEN:
        rij = []
        for st in STANDEN:
            p = DATA / f"{n}-{st}.json"
            if not p.exists():
                rij.append("")
                continue
            ms = [m["ms"] for m in telt(json.loads(p.read_text())).get(s, []) if m["ok"]]
            rij.append(f"{med(ms):.0f} (n={len(ms)})")
        print(f"| {n} | {s} | " + " | ".join(rij) + " |")

print("\n## Schaduwstand: vergelijkingen van delta en volledig\n")
for n in groottes:
    for extra in ("", "-otel"):
        p = DATA / f"{n}-shadow{extra}.json"
        if p.exists():
            d = json.loads(p.read_text())
            print(f"- N={n}{' (met tracing)' if extra else ''}: {d['identiek']} identiek, {d['mismatch']} mismatch")

print("\n## Tijdverdeling per aanroep, met tracing (mediaan in ms)\n")
kol = ["totaal", "app init", "session init", "execengine run", "transaction close", "conj_in_ee", "conj_in_close",
       "conj_in_ee_n", "conj_in_close_n", "sql_n"]
print("| N | soort | stand | " + " | ".join(kol) + " |")
print("|---|---|---|" + "---|" * len(kol))
top = defaultdict(lambda: defaultdict(list))
for n in groottes:
    for st in STANDEN:
        p = DATA / f"{n}-{st}-otel.json"
        if not p.exists():
            continue
        d = json.loads(p.read_text())
        sporen = d.get("sporen", [])
        aanroepen = d["metingen"]
        if len(sporen) != len(aanroepen):
            print(f"| {n} | | {st} | {len(sporen)} sporen bij {len(aanroepen)} aanroepen: niet te koppelen |")
            continue
        helft = len(aanroepen) // 2
        k = 2 if helft >= 22 else 1
        for s in SOORTEN:
            sel = [sp for m, sp, i in zip(aanroepen, sporen, range(len(sporen)))
                   if m["soort"] == s and m["ok"] and (i % helft) >= k]
            if not sel:
                continue
            print(f"| {n} | {s} | {st} | " + " | ".join(f"{med([sp[c] for sp in sel]):.0f}" for c in kol) + " |")
            for sp in sel:
                for cid, ms in sp["top"]:
                    top[(n, s, st)][cid].append(ms)

print("\n## Duurste conjuncts per aanroep (mediaan in ms over de aanroepen waarin de conjunct in de top zes stond)\n")
for (n, s, st), per in sorted(top.items()):
    if st not in ("off", "on"):
        continue
    rij = sorted(per.items(), key=lambda kv: -med(kv[1]))[:5]
    print(f"- N={n}, {s}, {st}: " + "; ".join(
        f"{cid} {med(ms):.0f} ms ({len(ms)}x{', ' + str(profiel[cid][0]) + (', delta' if profiel[cid][1] else ', geen delta') + ', ' + str(profiel[cid][2]) if cid in profiel else ''})"
        for cid, ms in rij))
