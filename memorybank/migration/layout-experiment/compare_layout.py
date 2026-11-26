#!/usr/bin/env python3
"""Compare the layout of a context compiled by itself with the layout that a
migration context expects of it, when it includes that context under a label.

Usage: compare_layout.py <generics of the context alone> <generics of the including context> <label>

Both arguments are the directory `generics` that `ampersand proto` writes.
For every relation and every concept of the context, the script compares the
table and the columns in which the context stores it with the table and the
columns that the including context reads under the label.
It prints one line per difference and exits with 1 if there is one.
The .gitignore of the repository ignores *.py: add this file with `git add -f`.
"""
import json
import sys
from pathlib import Path

SYSTEM = "PrototypeContext."


def load(directory, name):
    return json.loads((Path(directory) / name).read_text())


def columns(table):
    if table is None:
        return None
    return (table["name"], table["srcCol"]["name"], table["tgtCol"]["name"])


RESERVED = ("SESSION", "ONE")


def relation_key(rel, label=None):
    """A relation by its name and the names of its source and target, as the including context names them.
    SESSION, ONE and the concepts of PrototypeContext and FormalAmpersand belong to every application and carry no label."""
    def named(n):
        reserved = n in RESERVED or n.startswith(("PrototypeContext.", "FormalAmpersand."))
        return n if label is None or reserved else label + "." + n
    return (named(rel["name"]), named(rel["srcConceptName"]), named(rel["tgtConceptName"]))


def concept_table(cpt):
    table = cpt.get("conceptTable")
    if table is None:
        return None
    return (table["name"], tuple(table["cols"]))


def prefixed(row, label):
    """The including context knows a table of the context by the label and the name of the table;
    the columns keep their names, because the view on the table selects them as they are."""
    if row is None:
        return None
    name, *rest = row
    return (label + "." + name, *rest)


def main():
    alone_dir, incl_dir, label = sys.argv[1], sys.argv[2], sys.argv[3]
    differences = []

    alone_rels = {r["signature"]: r for r in load(alone_dir, "relations.json") if not r["name"].startswith(SYSTEM)}
    incl_rels = {relation_key(r): r for r in load(incl_dir, "relations.json")}
    for sig, rel in sorted(alone_rels.items()):
        here = incl_rels.get(relation_key(rel, label))
        if here is None:
            differences.append(f"relation {sig}: the including context does not have it")
            continue
        expected = prefixed(columns(rel["mysqlTable"]), label)
        found = columns(here["mysqlTable"])
        if rel["storage"] != here["storage"]:
            differences.append(f"relation {sig}: stored as {rel['storage']} alone, as {here['storage']} when included")
        elif expected != found:
            differences.append(f"relation {sig}: {expected} alone, {found} when included")

    # ONE and SESSION belong to every application and are not part of the included context.
    alone_cpts = {
        c["name"]: c
        for c in load(alone_dir, "concepts.json")
        if not c["name"].startswith(SYSTEM) and c["name"] not in ("ONE", "SESSION")
    }
    incl_cpts = {c["name"]: c for c in load(incl_dir, "concepts.json")}
    for name, cpt in sorted(alone_cpts.items()):
        here = incl_cpts.get(label + "." + name)
        if here is None:
            differences.append(f"concept {name}: the including context does not have it")
            continue
        expected = prefixed(concept_table(cpt), label)
        found = concept_table(here)
        if expected != found:
            differences.append(f"concept {name}: table {expected} alone, {found} when included")

    for d in differences:
        print(d)
    print(f"{len(alone_rels)} relations, {len(alone_cpts)} concepts, {len(differences)} differences")
    sys.exit(1 if differences else 0)


if __name__ == "__main__":
    main()
