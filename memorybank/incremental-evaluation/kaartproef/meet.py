#!/usr/bin/env python3
"""Meetharnas van de kaartproef: dezelfde vastleggingen onder off, shadow en on.

    meet.py zaai N            installeert, laadt een verzonnen populatie van N documenten, maakt een momentopname
    meet.py draai N [--otel]  zet de momentopname terug en speelt per stand dezelfde werklast af
    meet.py tel               tabel- en rijtellingen van de proefdatabase

De stack is die van compose.proef.yml (kaart op 8196, jaeger op 8198). Niets hier raakt een
andere stack. Uitvoer: data/<N>-<stand>[-otel].json met per aanroep de duur en, met --otel, de
verdeling van de tijd over de ExecEngine, de afsluiting en de conjuncts.
"""
import json
import os
import random
import statistics
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import defaultdict
from pathlib import Path

HIER = Path(__file__).resolve().parent
BASIS = "http://localhost:8196"
JAEGER = "http://localhost:8198"
WEB, DB = "ie-kaartproef-prototype", "ie-kaartproef-prototype-db"
DATA = HIER / "data"
DATA.mkdir(exist_ok=True)
LAGEN, K = 5, 2
REPS, OPWARM = int(os.environ.get("PROEF_REPS", 20)), int(os.environ.get("PROEF_OPWARM", 2))
q = lambda s: urllib.parse.quote(s, safe="")


def sh(*cmd, invoer=None, uit=None):
    r = subprocess.run(cmd, input=invoer, stdout=uit or subprocess.PIPE, stderr=subprocess.PIPE)
    if r.returncode != 0:
        sys.exit(f"{' '.join(cmd)[:120]} faalde: {r.stderr.decode(errors='replace')[-600:]}")
    return r.stdout.decode(errors="replace") if not uit else ""


def roep(methode, pad, lijf=None, timeout=3600):
    r = urllib.request.Request(BASIS + pad, data=json.dumps(lijf).encode() if lijf is not None else None,
                               method=methode, headers={"Content-Type": "application/json"})
    t0 = time.perf_counter()
    try:
        with urllib.request.urlopen(r, timeout=timeout) as a:
            st, tekst = a.status, a.read()
    except urllib.error.HTTPError as e:
        st, tekst = e.code, e.read()
    ms = (time.perf_counter() - t0) * 1000
    try:
        return st, json.loads(tekst or b"{}"), ms
    except ValueError:
        return st, {"ruw": tekst.decode(errors="replace")[:400]}, ms


def wacht_op_kaart():
    for _ in range(180):
        try:
            st, a, _ = roep("GET", "/api/v1/app/navbar", timeout=60)
            # vóór de eerste installatie antwoordt de kaart met 500 en een verwijzing naar de installer
            if st == 200 or a.get("navTo") == "/admin/installer":
                return
        except Exception:
            pass
        time.sleep(2)
    sys.exit("de proefkaart antwoordt niet")


def mysql(opdracht=None, invoer=None, uit=None):
    cmd = ["docker", "exec", "-i", DB, "sh", "-c", 'exec mysql -N -uroot -p"$MYSQL_ROOT_PASSWORD" "$@"', "sh"]
    if opdracht:
        cmd += ["-e", opdracht]
    return sh(*cmd, invoer=invoer, uit=uit)


def dbnaam():
    return mysql("SELECT table_schema FROM information_schema.tables WHERE table_name='__conj_violation_cache__' LIMIT 1").strip()


STANDEN = ("off", "off+skip", "shadow", "on", "on+skip")


def op():
    """Bouwt en start de hele proefstack; de kaart komt op het lokale framework-image."""
    if not (HIER / "project.proef.yaml").exists():
        (HIER / "project.proef.yaml").write_text("settings:\n  session.loginEnabled: false\n")
    r = subprocess.run(["docker", "compose", "-f", str(HIER / "compose.proef.yml"), "up", "-d", "--build"],
                       capture_output=True)
    (HIER / "bouw-kaart.txt").write_bytes(r.stdout + r.stderr)
    if r.returncode != 0:
        sys.exit("de proefstack start niet; zie bouw-kaart.txt")
    wacht_op_kaart()


def stand(naam, otel):
    """Zet de stand en start de kaart vers op. 'on+skip' is 'on' met skipCleanConjuncts."""
    mode = naam.split("+")[0]
    (HIER / "project.proef.yaml").write_text(
        "settings:\n  session.loginEnabled: false\n"
        f"  transactions.deltaConjunctMaintenance: '{mode}'\n"
        + ("  transactions.skipCleanConjuncts: true\n" if naam.endswith("+skip") else ""))
    env = dict(os.environ, PROEF_OTEL_DISABLED="false" if otel else "true")
    subprocess.run(["docker", "compose", "-f", str(HIER / "compose.proef.yml"), "up", "-d", "--force-recreate",
                    "--no-deps", "prototype"], env=env, check=True, capture_output=True)
    wacht_op_kaart()


def zaai(n):
    stand("off", False)
    st, a, ms = roep("GET", "/api/v1/admin/installer")
    print(f"installer: {st} in {ms / 1000:.0f}s")
    if st != 200:
        sys.exit(str(a)[:600])
    pop = sh(sys.executable, str(HIER / "zaai.py"), str(n), "--lagen", str(LAGEN), "--k", str(K))
    bestand = DATA / f"populatie-{n}.json"
    bestand.write_text(pop)
    t0 = time.time()
    uitvoer = sh("curl", "-s", "-w", "\n%{http_code}", "-F", f"file=@{bestand};type=application/json",
                 BASIS + "/api/v1/admin/import")
    print(f"import: status {uitvoer.strip().splitlines()[-1]} in {time.time() - t0:.0f}s")
    antwoord = uitvoer.rsplit("\n", 1)[0]
    try:
        j = json.loads(antwoord)
        print("  isCommitted:", j.get("isCommitted"), "| invarianten:",
              [i.get("ruleMessage", "")[:90] for i in (j.get("notifications", {}).get("invariants") or [])][:5])
        if j.get("isCommitted") is False or uitvoer.strip().splitlines()[-1] != "200":
            sys.exit("de import is niet vastgelegd: " + antwoord[:800])
    except ValueError:
        sys.exit("onleesbaar antwoord: " + antwoord[:800])
    db = dbnaam()
    with open(DATA / f"momentopname-{n}.sql", "wb") as f:
        sh("docker", "exec", DB, "sh", "-c",
           'exec mysqldump -uroot -p"$MYSQL_ROOT_PASSWORD" --single-transaction --add-drop-table "$1"', "sh", db, uit=f)
    print("momentopname:", (DATA / f"momentopname-{n}.sql").stat().st_size // 1024, "kB")
    tel()


def tel():
    db = dbnaam()
    rijen = mysql(f"SELECT table_name FROM information_schema.tables WHERE table_schema='{db}' AND table_type='BASE TABLE'").split()
    views = mysql(f"SELECT COUNT(*) FROM information_schema.views WHERE table_schema='{db}'").strip()
    tot, per = 0, {}
    for t in rijen:
        per[t] = int(mysql(f'SELECT COUNT(*) FROM `{db}`.`{t}`').strip())
        tot += per[t]
    print(f"tabellen: {len(rijen)}, rijen: {tot}, views: {views}")
    for t in sorted(per, key=per.get, reverse=True)[:12]:
        print(f"  {t}: {per[t]}")
    return per


def werklast(n):
    """Dezelfde reeks voor elke stand: REPS keer een nieuwe stempel met vastlegging, REPS keer een nieuwe afhankelijkheid."""
    rnd = random.Random(42)
    per_laag = n // LAGEN
    doc = lambda l, i: f"Proef.d{l}.{i:05d}"
    reeks = []
    for i in range(REPS + OPWARM):
        d = doc(rnd.randrange(0, LAGEN - 1), rnd.randrange(per_laag))
        vid = f"vl.proef.{i:03d}"
        pad = f"/Document/{d}"
        ops = [{"op": "replace", "path": f"{pad}/stempel", "value": f"s1.{i}.{d}"},
               {"op": "add", "path": f"{pad}/vastleggingen", "value": vid}]
        ops += [{"op": "replace", "path": f"{pad}/vastleggingen/{vid}/{v}", "value": w} for v, w in
                [("stempel", f"s1.{i}.{d}"), ("commit", "0123456789ab"), ("sessie", "proef"),
                 ("zin", "verzonnen vastlegging voor de proef"), ("op", "2026-10-05 12:00"), ("wat", "gewijzigd")]]
        reeks.append(("stempel", ops))
    for i in range(REPS + OPWARM):
        l = rnd.randrange(1, LAGEN)
        d, b = doc(l, rnd.randrange(per_laag)), doc(l - 1, rnd.randrange(per_laag))
        x = f"afh.proef.{i:03d}"
        pad = f"/Document/{d}"
        ops = [{"op": "add", "path": f"{pad}/steuntOp", "value": x},
               {"op": "replace", "path": f"{pad}/steuntOp/{x}/op", "value": b},
               {"op": "replace", "path": f"{pad}/steuntOp/{x}/reden", "value": "verzonnen voor de proef"}]
        reeks.append(("afhankelijkheid", ops))
    return reeks


def sporen(sinds_us):
    """De traces van de PATCH-aanroepen sinds het tijdstip, uit Jaeger."""
    time.sleep(8)  # de exporter levert na het verzoek
    url = f"{JAEGER}/api/traces?service=ampersand-prototype&limit=1500&start={sinds_us}&end={int(time.time() * 1e6)}"
    with urllib.request.urlopen(url, timeout=120) as a:
        data = json.loads(a.read())["data"]
    uit = []
    for tr in data:
        sp = tr["spans"]
        wortel = [s for s in sp if not s.get("references") and s["operationName"].startswith("PATCH")]
        if not wortel:
            continue
        ouder = {s["spanID"]: (s["references"][0]["spanID"] if s.get("references") else None) for s in sp}
        naam = {s["spanID"]: s["operationName"] for s in sp}

        def onder(sid, wat):
            while sid:
                if naam.get(sid) == wat:
                    return True
                sid = ouder.get(sid)
            return False

        r = {"start": wortel[0]["startTime"], "totaal": wortel[0]["duration"] / 1000}
        for fase in ("app init", "session init", "execengine run", "transaction close"):
            r[fase] = sum(s["duration"] for s in sp if s["operationName"] == fase) / 1000
        conj = [s for s in sp if s["operationName"].startswith("conjunct ")]
        r["conjuncts"] = len(conj)
        r["conjuncts_ms"] = sum(s["duration"] for s in conj) / 1000
        r["conj_in_ee"] = sum(s["duration"] for s in conj if onder(s["spanID"], "execengine run")) / 1000
        r["conj_in_close"] = sum(s["duration"] for s in conj if onder(s["spanID"], "transaction close")) / 1000
        r["conj_in_ee_n"] = sum(1 for s in conj if onder(s["spanID"], "execengine run"))
        r["conj_in_close_n"] = sum(1 for s in conj if onder(s["spanID"], "transaction close"))
        sql = [s for s in sp if "mysqli" in s["operationName"].lower()]
        r["sql_n"], r["sql_ms"] = len(sql), sum(s["duration"] for s in sql) / 1000
        r["sql_in_close_ms"] = sum(s["duration"] for s in sql if onder(s["spanID"], "transaction close")) / 1000
        r["sql_in_ee_ms"] = sum(s["duration"] for s in sql if onder(s["spanID"], "execengine run")) / 1000
        per = defaultdict(float)
        for s in conj:
            per[s["operationName"].split(" ", 1)[1]] += s["duration"] / 1000
        r["top"] = sorted(per.items(), key=lambda kv: -kv[1])[:6]
        uit.append(r)
    return sorted(uit, key=lambda r: r["start"])


def draai(n, otel):
    momentopname = DATA / f"momentopname-{n}.sql"
    reeks = werklast(n)
    for mode in ([a.split("=")[1] for a in sys.argv if a.startswith("--stand=")] or STANDEN):
        stand(mode, otel)
        db = dbnaam()
        sh("docker", "exec", "-i", DB, "sh", "-c", 'exec mysql -uroot -p"$MYSQL_ROOT_PASSWORD" "$1"', "sh", db,
           invoer=momentopname.read_bytes())
        stand(mode, otel)  # verse processen na het terugzetten
        sinds = int(time.time() * 1e6)
        metingen = []
        for i, (soort, ops) in enumerate(reeks):
            st, a, ms = roep("PATCH", f"/api/v1/resource/Project/Proef/VastleggenProject?depth=0", ops)
            ok = st == 200 and a.get("isCommitted") is not False
            if not ok and i < 3:
                print("  niet vastgelegd:", st, str(a)[:500])
            metingen.append({"soort": soort, "ms": ms, "ok": ok, "status": st,
                             "reden": None if ok else str(a.get("msg") or [i.get("ruleMessage") for i in a.get("notifications", {}).get("invariants", [])])[:300]})
        # de opwarmers zijn de eerste OPWARM van elke soort
        telt = [m for j, m in enumerate(metingen) if (j % (REPS + OPWARM)) >= OPWARM]
        uit = {"n": n, "mode": mode, "otel": otel, "metingen": metingen}
        if otel:
            uit["sporen"] = sporen(sinds)
        # de container is per stand vers, dus zijn log gaat alleen over deze stand
        log = subprocess.run(["docker", "logs", WEB], capture_output=True)
        mism = log.stdout.decode(errors="replace") + log.stderr.decode(errors="replace")
        uit["mismatch"] = mism.count("DELTA SHADOW MISMATCH")
        uit["identiek"] = mism.count("Delta shadow check for conjunct")
        (DATA / f"{n}-{mode}{'-otel' if otel else ''}.json").write_text(json.dumps(uit))
        for soort in ("stempel", "afhankelijkheid"):
            ms = [m["ms"] for m in telt if m["soort"] == soort and m["ok"]]
            nok = sum(1 for m in telt if m["soort"] == soort and not m["ok"])
            if ms:
                print(f"N={n} {mode:8} {soort:15} mediaan {statistics.median(ms):8.1f} ms  min {min(ms):8.1f}  max {max(ms):8.1f}  (n={len(ms)}, mislukt {nok})")
            else:
                print(f"N={n} {mode:6} {soort:15} geen geslaagde aanroep (mislukt {nok})")
        print(f"   schaduw: {uit['identiek']} identiek, {uit['mismatch']} mismatch")


if __name__ == "__main__":
    wat = sys.argv[1]
    if wat == "op":
        op()
    elif wat == "zaai":
        zaai(int(sys.argv[2]))
    elif wat == "draai":
        draai(int(sys.argv[2]), "--otel" in sys.argv)
    elif wat == "tel":
        tel()
