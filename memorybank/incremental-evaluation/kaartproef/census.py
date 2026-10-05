#!/usr/bin/env python3
"""Telling over conjuncts.json: kandidaat-queries en kostenklasse per conjunct."""
import json
import sys
from collections import Counter
from pathlib import Path

gen = Path(sys.argv[1])
conj = json.loads((gen / "conjuncts.json").read_text())
rules = json.loads((gen / "rules.json").read_text())
rels = json.loads((gen / "relations.json").read_text())

print("sleutels conjunct:", sorted(conj[0].keys()))
print("sleutels rules.json:", list(rules.keys()) if isinstance(rules, dict) else type(rules))
print("conjuncts:", len(conj))
print("relaties:", len(rels), "met deltaTable:", sum(1 for r in rels if r.get("deltaTable")))

met = [c for c in conj if c.get("deltaQueries")]
print("met deltaQueries:", len(met), "zonder:", len(conj) - len(met))
print("kostenklasse:", Counter((c.get("costProfile") or {}).get("class") for c in conj))
print("klasse x delta:", Counter(((c.get("costProfile") or {}).get("class"), bool(c.get("deltaQueries"))) for c in conj))
soort = Counter()
for c in conj:
    inv, sig = bool(c.get("invariantRuleNames")), bool(c.get("signalRuleNames"))
    soort[("inv" if inv else "") + ("+sig" if sig else ""), bool(c.get("deltaQueries"))] += 1
print("soort x delta:", soort)
zonder = [c for c in conj if not c.get("deltaQueries")]
print("\nconjuncts zonder deltaQueries:")
for c in zonder:
    namen = (c.get("invariantRuleNames") or []) + (c.get("signalRuleNames") or [])
    print(" ", c["id"], (c.get("costProfile") or {}).get("class"), namen[:3], "sqllen", len(c.get("violationsSQL", "")))
print("\nrecursive:")
for c in conj:
    if (c.get("costProfile") or {}).get("class") == "recursive":
        namen = (c.get("invariantRuleNames") or []) + (c.get("signalRuleNames") or [])
        print(" ", c["id"], bool(c.get("deltaQueries")), namen[:3])
