"""Predictions 3 and 4 of preregistration.md: does the migration system read what the existing system reads?

Usage:
  read_compare.py sql <migration generics> <alone generics> <out dir>
      writes mig.sql (to run in the database with the views of the migration system)
      and alone.sql (to run in the restored database of the existing system)
  read_compare.py compare <out dir>
      compares mig.tsv with alone.tsv, the outputs of `mariadb -N -B` on those files

A relation is read from its table as the framework reads it: the pairs whose source and target column are both set.
A concept is read from its allAtomsQuery when it has one, else from its own column in its concept table.
The prefix `old.` is removed from names, so that old.r of the migration system meets r of the existing system.
"""
import json, os, re, sys

MARK = "@@@"

def q(name):
    return '"' + name.replace('"', '""') + '"'

def strip_old(s):
    return re.sub(r'\bold\.', '', s)

def relation_sql(rel):
    t = rel["mysqlTable"]
    src, tgt = t["srcCol"]["name"], t["tgtCol"]["name"]
    return (f"SELECT {q(src)}, {q(tgt)} FROM {q(t['name'])} "
            f"WHERE {q(src)} IS NOT NULL AND {q(tgt)} IS NOT NULL;")

def concept_sql(c):
    if c.get("allAtomsQuery"):
        return c["allAtomsQuery"].rstrip().rstrip(";") + ";"
    t = c.get("conceptTable")
    if not t:
        return None
    cols = t.get("cols") or []
    label = c.get("label") or c.get("id")
    col = next((x for x in cols if x == label), cols[0] if cols else None)
    if col is None:
        return None
    return f"SELECT DISTINCT {q(col)} FROM {q(t['name'])} WHERE {q(col)} IS NOT NULL;"

def write_side(generics, keep, path, report):
    rels = json.load(open(os.path.join(generics, "relations.json")))
    cons = json.load(open(os.path.join(generics, "concepts.json")))
    lines = ["SET SESSION sql_mode = CONCAT(@@sql_mode, ',ANSI_QUOTES');"]
    keys = {}
    n = 0
    for r in rels:
        if not keep(r["name"]):
            continue
        key = "R " + strip_old(r["signature"])
        if r.get("storage") != "table" or not r.get("mysqlTable"):
            report.append(f"{os.path.basename(path)}: {key} has storage {r.get('storage')}, not compared")
            continue
        lines += [f"SELECT '{MARK} {key}';", relation_sql(r)]
        keys[len(lines)] = key
        n += 1
    for c in cons:
        name = c.get("name") or c.get("id")
        if not keep(name):
            continue
        sql = concept_sql(c)
        key = "C " + strip_old(name)
        if sql is None:
            report.append(f"{os.path.basename(path)}: {key} has no table and no query, not compared")
            continue
        lines += [f"SELECT '{MARK} {key}';", sql]
        keys[len(lines)] = key
        n += 1
    open(path, "w").write("\n".join(lines) + "\n")
    # the line of each query, so that an error of MariaDB ("at line N") names the key it belongs to
    json.dump(keys, open(path[:-4] + "-lines.json", "w"))
    return n

def is_old(name):
    return name.startswith("old.")

def is_user(name):
    # the existing system compiled by itself: everything except the contexts of Ampersand itself
    return not name.startswith("PrototypeContext.") and not name.startswith("FormalAmpersand.")

def read_tsv(path):
    out, cur = {}, None
    for line in open(path, encoding="utf-8", errors="replace"):
        line = line.rstrip("\n")
        if line.startswith(MARK + " "):
            cur = line[len(MARK) + 1:]
            out[cur] = set()
        elif cur is not None:
            out[cur].add(line)
    return out

def failed(out, side):
    keys = json.load(open(os.path.join(out, side + "-lines.json")))
    bad = {}
    for line in open(os.path.join(out, side + "-errors.txt"), errors="replace"):
        m = re.search(r"ERROR \d+ \(\w+\) at line (\d+): (.*)", line)
        if m and m.group(1) in keys:
            bad[keys[m.group(1)]] = m.group(2)
    return bad

def main():
    if sys.argv[1] == "sql":
        mig, alone, out = sys.argv[2:5]
        report = []
        a = write_side(mig, is_old, os.path.join(out, "mig.sql"), report)
        b = write_side(alone, is_user, os.path.join(out, "alone.sql"), report)
        print(f"queries: migration {a}, alone {b}")
        print("\n".join(report))
        return
    out = sys.argv[2]
    mig = read_tsv(os.path.join(out, "mig.tsv"))
    alone = read_tsv(os.path.join(out, "alone.tsv"))
    bad_mig, bad_alone = failed(out, "mig"), failed(out, "alone")
    print(f"failed queries: migration {len(bad_mig)}, alone {len(bad_alone)}")
    for side, bad in (("migration", bad_mig), ("alone", bad_alone)):
        for k, msg in sorted(bad.items()):
            print(f"  {side}: {k}: {msg}")
    for k in list(bad_mig) + list(bad_alone):
        mig.pop(k, None); alone.pop(k, None)
    shared = sorted(set(mig) & set(alone))
    only_mig = sorted(set(mig) - set(alone))
    only_alone = sorted(k for k in set(alone) - set(mig) if k.startswith("R "))
    differ = [k for k in shared if mig[k] != alone[k]]
    rels = [k for k in shared if k.startswith("R ")]
    cons = [k for k in shared if k.startswith("C ")]
    print(f"compared: {len(rels)} relations, {len(cons)} concepts")
    print(f"pairs read: {sum(len(alone[k]) for k in rels)} (alone), {sum(len(mig[k]) for k in rels)} (migration)")
    print(f"atoms read: {sum(len(alone[k]) for k in cons)} (alone), {sum(len(mig[k]) for k in cons)} (migration)")
    print(f"differ: {len(differ)}")
    for k in differ:
        print(f"  {k}: alone {len(alone[k])}, migration {len(mig[k])}, "
              f"only alone {len(alone[k] - mig[k])}, only migration {len(mig[k] - alone[k])}")
    print(f"only in the migration system: {len(only_mig)}")
    # A concept without a table in the existing system has as atoms what its relations hold:
    # the sources of the relations from it and the targets of the relations to it.
    for k in only_mig:
        concept = k[2:]
        held = set()
        for rk, pairs in alone.items():
            m = re.match(r"R .*\[(.*)\*(.*)\]$", rk)
            if not m:
                continue
            for p in pairs:
                src, _, tgt = p.partition("\t")
                if m.group(1) == concept:
                    held.add(src)
                if m.group(2) == concept:
                    held.add(tgt)
        verdict = "same" if held == mig[k] else f"DIFFERENT (only in pairs {len(held - mig[k])}, only read {len(mig[k] - held)})"
        print(f"   {k}: read {len(mig[k])}, held by the pairs of the existing system {len(held)}: {verdict}")
    print(f"relations only in the existing system: {len(only_alone)}")
    for k in only_alone:
        print("  ", k)

main()
