#!/usr/bin/env python3
"""Toont van genoemde conjuncts de regel, het kostenprofiel en de SQL."""
import json
import sys
from pathlib import Path

gen = Path(sys.argv[1])
conj = {c["id"]: c for c in json.loads((gen / "conjuncts.json").read_text())}
rules = json.loads((gen / "rules.json").read_text())
alle = {r["name"]: r for soort in rules.values() for r in soort}
for cid in sys.argv[2:]:
    c = conj[cid]
    namen = (c.get("invariantRuleNames") or []) + (c.get("signalRuleNames") or [])
    print("==", cid, c.get("costProfile"), "delta:", [d["relation"] for d in c.get("deltaQueries") or []])
    for n in namen:
        r = alle.get(n, {})
        print("   regel", n, "|", r.get("formalExpression", r.get("ruleAdl", ""))[:200])
    print(c["violationsSQL"][:1400])
    print()
