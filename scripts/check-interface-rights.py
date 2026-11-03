#!/usr/bin/env python3
"""Checks what the compiler says in PrototypeContext about the interfaces of a script.

A script reads these relations in a signal rule, at runtime: the compiler checks invariants on the population of the
script, before the metapopulation exists, so an invariant on them can only fail. This script therefore compiles
testing/InterfaceRights/InterfaceRights.adl and compares the population it generates with what the interfaces declare.

Usage: scripts/check-interface-rights.py <ampersand> [--expect-wrong]
With --expect-wrong it compares with a wrong expectation and has to fail, which shows that this check can fail.
"""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
MODEL = REPO / "testing" / "InterfaceRights" / "InterfaceRights.adl"

# interface -> (the concept it opens on, the rights it has or a box inside it has)
EXPECTED = {
    "ReadOnly": ("Project", ""),
    "EditableInABox": ("Project", "U"),
    "CreatesAndDeletes": ("Project", "CD"),
    "AnApi": ("Project", ""),
    "OnASession": ("SESSION", ""),
}
RIGHTS = {"C": "PrototypeContext.ifcCanCreate", "U": "PrototypeContext.ifcCanUpdate", "D": "PrototypeContext.ifcCanDelete"}


def population(ampersand):
    with tempfile.TemporaryDirectory() as out:
        subprocess.run([ampersand, "proto", "--no-frontend", str(MODEL), "--proto-dir", out, "--crud-defaults", "cRud"],
                       check=True, stdout=subprocess.DEVNULL)
        return json.loads((Path(out) / "generics" / "populations.json").read_text())


def pairs(pop, relation):
    """The pairs of a relation in the generated population, as {source: target}."""
    return {link["src"]: link["tgt"] for block in pop["links"] if block["relation"].startswith(relation + "[")
            for link in block["links"]}


def main():
    pop = population(sys.argv[1])
    expected = dict(EXPECTED)
    if "--expect-wrong" in sys.argv:
        expected["ReadOnly"] = ("Project", "U")
    concepts = pairs(pop, "PrototypeContext.ifcConcept")
    failures = []
    for ifc, (concept, rights) in expected.items():
        if concepts.get(ifc) != concept:
            failures.append(f"{ifc} opens on {concepts.get(ifc)}, expected {concept}")
        for right, relation in RIGHTS.items():
            if (ifc in pairs(pop, relation)) != (right in rights):
                failures.append(f"{ifc}: {relation} is {ifc in pairs(pop, relation)}, expected {right in rights}")
    print("\n".join(failures) or f"PrototypeContext says the right things about {len(expected)} interfaces")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
