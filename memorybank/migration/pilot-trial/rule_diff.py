"""Vergelijkt de regels van old en new in rules.json van het migratiesysteem.

Een regel van new die in old niet bestaat, of daar een andere term heeft, is een kandidaat voor
stap 3 tot 5 van het artikel (een invariant) of voor een signaal (een procesregel).
Eigenschapsregels worden vergeleken op hun relatie, zodat een nieuwe UNI/TOT ook meetelt.
"""
import json, re, sys

rules = json.load(open(sys.argv[1]))

def norm(term, side):
    # haal het prefix weg, zodat old.r en new.r gelijk vergeleken worden
    return re.sub(r'\b' + side + r'\.', '', term)

def table(kind):
    out = {"old": {}, "new": {}}
    for r in rules.get(kind, []):
        name = r["name"]
        for side in ("old", "new"):
            if name.startswith(side + "."):
                key = name[len(side) + 1:]
                if "PropertyRule" in r.get("origin", ""):
                    key = norm(r["label"], side)
                out[side][key] = norm(r.get("ruleAdl", ""), side)
    return out

for kind in ("invariants", "signals"):
    t = table(kind)
    new_only = sorted(set(t["new"]) - set(t["old"]))
    changed = sorted(k for k in set(t["new"]) & set(t["old"]) if t["new"][k] != t["old"][k])
    gone = sorted(set(t["old"]) - set(t["new"]))
    print(f"== {kind}: old {len(t['old'])}, new {len(t['new'])}")
    print(f"-- alleen in new ({len(new_only)})")
    for k in new_only:
        print("  ", k, "::", t["new"][k][:160])
    print(f"-- gewijzigd ({len(changed)})")
    for k in changed:
        print("  ", k)
        print("     old:", t["old"][k][:200])
        print("     new:", t["new"][k][:200])
    print(f"-- alleen in old ({len(gone)})")
    for k in gone:
        print("  ", k)
