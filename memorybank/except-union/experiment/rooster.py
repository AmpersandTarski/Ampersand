"""Rooster voor issue #1763: vul de relaties van grid.adl op grootte n en kolombreedte w,
zet de afgeleide relaties op hun juiste inhoud (dus nul overtredingen, zoals in productie),
en meet de violation queries van de twaalf ENFORCE-conjuncts in drie vormen van de compiler.
Gebruik: rooster.py <uit.tsv> <n,n,...> <w,w,...>"""
import json, random, re, statistics, subprocess, sys, time

out, ns, ws = sys.argv[1], [int(x) for x in sys.argv[2].split(",")], [int(x) for x in sys.argv[3].split(",")]
FORMS = ["base", "noexcept", "closure"]
import os
C = ["docker", "exec", "-i", os.environ.get("DBC", "amp1728fc5-db"), "mariadb", "-uroot", "-p" + os.environ.get("DBPW", "root"), "-N"]
PRE = "SET SESSION sql_mode='ANSI,TRADITIONAL';\n"


def sql(q, db=None):
    p = subprocess.run(C + ([db] if db else []), input=PRE + q, capture_output=True, text=True)
    return p.returncode, p.stdout, p.stderr


HERE = os.path.dirname(os.path.abspath(__file__))
conj = json.load(open(os.path.join(HERE, "violations.json")))


def label(cid):
    s = conj["noexcept"][cid]
    m = re.search(r"EDcD (big|small)[UIC]*\[", s)
    side = m.group(1)
    shape = "C" if "recursive" in s.lower() else ("IU" if "subIntersect" in s else "U")
    direction = "ins" if int(cid.split("_")[1]) < 61 else "del"
    return f"{side}-{shape}-{direction}"


ids = [f"conj_{i}" for i in list(range(6)) + list(range(61, 67))]
labels = {c: label(c) for c in ids}
ddl = open(os.path.join(HERE, "database.sql")).read()


def pairs_insert(t, ps):
    ps = list(ps)
    out = []
    for i in range(0, len(ps), 5000):
        out.append(f'INSERT IGNORE INTO "{t}" ("SrcX","TgtX") VALUES ' +
                   ",".join(f"('x{a}','x{b}')" for a, b in ps[i:i + 5000]) + ";")
    return "\n".join(out)


def build(db, n, w):
    rnd = random.Random(1763 + n)
    p = {(rnd.randrange(n), rnd.randrange(n)) for _ in range(n)}
    q = {(rnd.randrange(n), rnd.randrange(n)) for _ in range(n)}
    t = {(i, i // 2) for i in range(2, n)}
    pl, tl = sorted(p), sorted(t)
    big = {rnd.choice(pl) for _ in range(n // 2)} | {rnd.choice(tl)[::-1] for _ in range(n // 4)} | \
          {(rnd.randrange(n), rnd.randrange(n)) for _ in range(n // 4)}
    small = {rnd.choice(pl) for _ in range(3)} | {(rnd.randrange(n), rnd.randrange(n)) for _ in range(7)}
    d = ddl if w == 255 else ddl.replace("VARCHAR(255)", f"VARCHAR({w})")
    rc, _, err = sql(f"DROP DATABASE IF EXISTS {db}; CREATE DATABASE {db} CHARACTER SET utf8mb4 COLLATE utf8mb4_nopad_bin;")
    rc, _, err = sql(d, db)
    if rc:
        sys.exit("ddl: " + err[-500:])
    body = "\n".join(pairs_insert(r, s) for r, s in [("p", p), ("q", q), ("t", t), ("big", big), ("small", small)])
    rc, _, err = sql(body, db)
    if rc:
        sys.exit("vul: " + err[-500:])
    # afgeleide relaties: hun inhoud volgt uit de vorm 'closure' van de ins-richting zonder het aftrekken van de opgeslagen relatie
    derived = {"big": {"U": "bigU", "IU": "bigIU", "C": "bigC"}, "small": {"U": "smallU", "IU": "smallIU", "C": "smallC"}}
    for cid in ids:
        side, shape, direction = labels[cid].split("-")
        if direction != "ins":
            continue
        # met de opgeslagen relatie leeg is de ins-violation query precies de term zelf
        rc, o, err = sql(f'INSERT INTO "{derived[side][shape]}" ("SrcX","TgtX") SELECT q.src, q.tgt FROM ({conj["closure"][cid]}) AS q;', db)
        if rc:
            sys.exit("afgeleid: " + err[-500:])
    rc, o, _ = sql("SELECT " + ", ".join(f'(SELECT COUNT(*) FROM "{r}")' for r in
                   ["p", "q", "t", "big", "small", "bigU", "bigIU", "bigC", "smallU", "smallIU", "smallC"]) + ";", db)
    sql('ANALYZE TABLE "p","q","t","big","small","bigU","bigIU","bigC","smallU","smallIU","smallC";', db)
    return o.strip()


def run(q, db):
    s = (f"SET SESSION max_statement_time=60;\nSET @t0=NOW(6);\n"
         f"SELECT COUNT(*), COALESCE(BIT_XOR(CRC32(CONCAT(q.src,'|',q.tgt))),0) FROM ({q}) AS q;\n"
         "SELECT TIMESTAMPDIFF(MICROSECOND,@t0,NOW(6))/1e6;\n")
    rc, o, err = sql(s, db)
    lines = o.strip().splitlines()
    if len(lines) < 2:
        return None, "timeout" if "max_statement_time" in err else err.strip()[-60:]
    return float(lines[-1]), lines[-2].replace("\t", " ")


with open(out, "w") as f:
    f.write("n\tw\tcell\t" + "\t".join(f"{v}_s\t{v}_res" for v in FORMS) + "\n")
    for n in ns:
        for w in ws:
            db = f"grid_{n}_{w}"
            print(n, w, "aantallen p q t big small bigU bigIU bigC smallU smallIU smallC:", build(db, n, w), flush=True)
            for cid in ids:
                row = [str(n), str(w), labels[cid]]
                for v in FORMS:
                    t0, r0 = run(conj[v][cid], db)
                    if t0 is None or t0 > 20:
                        row += ["timeout" if t0 is None else f"{t0:.3f}", r0]
                        continue
                    ts, rs = [], {r0}
                    for _ in range(3):
                        t, r = run(conj[v][cid], db)
                        ts.append(t if t is not None else float("nan"))
                        rs.add(r)
                    row += [f"{statistics.median(ts):.3f}", " / ".join(sorted(rs))]
                f.write("\t".join(row) + "\n")
                f.flush()
print("klaar")
