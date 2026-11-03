#!/usr/bin/env python3
"""Measurement for issue #1744, R6: derived relations as views on the FC5 population.

Two copies of the database `Register` are made on the same MariaDB server (container
<database-container>): `meting_before` holds the tables as the running prototype has them;
`meting_after` holds the same tables, without the link tables of the derived relations that
`--derived-views` turns into views, and with those views created from the generated
database.sql. Every query is timed server-side with SHOW PROFILES, three runs, median per query.

Groups:
  G1  the violation queries of the Compute rules (what admin/execengine/run evaluates for the
      derived relations): before = 78 conjuncts, after = the 32 that remain (the `>:` statements
      and the stored sessionAllowedRoles).
  G2  the violation queries of all conjuncts (what admin/ruleengine/evaluate/all evaluates).
  G3  one full read per derived relation: the stored relation before, the view after.
  G4  the field queries of INTERFACE Zoekeisen, with the placeholder replaced by a session atom.

Usage: python3 -I meting.py [--keep]   (writes meting-result.md and meting-raw.csv next to it)
"""
import json, os, re, subprocess, statistics, sys, csv

S = os.path.dirname(os.path.abspath(__file__))
BEFORE = os.path.join(S, 'fc5-before', 'generics')
AFTER = os.path.join(S, os.environ.get('AFTER_DIR', 'fc5-after'), 'generics')
TAG = os.environ.get('TAG', '')
CONTAINER = os.environ['FC5_DB_CONTAINER']  # the MariaDB container of the FC5 stack
USER, PW = 'ampersand', 'ampersand'
SRC_DB = 'Register'
RUNS = 3
BATCH = 90  # SHOW PROFILES keeps at most 100 entries

def mysql(db, sql, raw=False):
    """Run a SQL script in the container; return stdout."""
    args = ['docker', 'exec', '-i', CONTAINER, 'mysql', f'-u{USER}', f'-p{PW}', '-c', '--batch', '--raw', '--skip-column-names']
    if db:
        args.append(db)
    r = subprocess.run(args, input=sql.encode('utf-8'), capture_output=True)
    err = r.stderr.decode('utf-8', 'replace')
    err = '\n'.join(l for l in err.splitlines() if 'Using a password' not in l)
    if r.returncode != 0:
        raise RuntimeError(f'mysql failed in {db}:\n{err}\n--- first 500 chars of script:\n{sql[:500]}')
    return r.stdout.decode('utf-8', 'replace')

def q(name):
    return '"' + name.replace('"', '""') + '"'

def tables_of(db):
    out = mysql(None, f"SELECT table_name FROM information_schema.tables WHERE table_schema='{db}' AND table_type='BASE TABLE'")
    return [l for l in out.splitlines() if l]

def copy_database(dst):
    mysql(None, f'DROP DATABASE IF EXISTS {dst}; CREATE DATABASE {dst} DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN;')
    stmts = ["SET SESSION sql_mode='ANSI'; SET FOREIGN_KEY_CHECKS=0;"]
    for t in tables_of(SRC_DB):
        stmts.append(f'CREATE TABLE {dst}.{q(t)} LIKE {SRC_DB}.{q(t)};')
        stmts.append(f'INSERT INTO {dst}.{q(t)} SELECT * FROM {SRC_DB}.{q(t)};')
    mysql(None, '\n'.join(stmts))

def view_statements(database_sql):
    """The CREATE VIEW statements of a generated database.sql, each as one string."""
    text = open(database_sql, encoding='utf-8').read()
    # statements are separated by a ';' at the end of a line; comment lines end with '*/'
    stmts, cur = [], []
    for line in text.splitlines():
        cur.append(line)
        if line.rstrip().endswith(';'):
            stmts.append('\n'.join(cur))
            cur = []
    views = []
    for s in stmts:
        body = '\n'.join(l for l in s.splitlines() if not l.lstrip().startswith('/*') or 'CREATE VIEW' in l)
        if re.search(r'^\s*CREATE VIEW', body, re.M):
            views.append(s)
    return views

def view_names(stmts):
    names = []
    for s in stmts:
        m = re.search(r'CREATE VIEW "((?:[^"]|"")*)"', s)
        names.append(m.group(1).replace('""', '"'))
    return names

def rules_with_conjuncts(generics):
    rules = json.load(open(os.path.join(generics, 'rules.json'), encoding='utf-8'))
    out = []
    def walk(o):
        if isinstance(o, dict):
            if 'name' in o and 'conjunctIds' in o:
                out.append(o)
            for v in o.values(): walk(v)
        elif isinstance(o, list):
            for v in o: walk(v)
    walk(rules)
    return out

def conjunct_queries(generics, only_compute=False):
    conjs = json.load(open(os.path.join(generics, 'conjuncts.json'), encoding='utf-8'))
    if only_compute:
        ids = {c for r in rules_with_conjuncts(generics) if str(r['name']).startswith('Compute') for c in r['conjunctIds']}
        conjs = [c for c in conjs if c['id'] in ids]
    return [(c['id'], c['violationsSQL']) for c in conjs]

def interface_queries(generics, ifcname, session_atom):
    ifcs = json.load(open(os.path.join(generics, 'interfaces.json'), encoding='utf-8'))
    out = []
    def walk(o, path):
        if isinstance(o, dict):
            if 'expr' in o and isinstance(o['expr'], dict) and 'query' in o['expr']:
                nm = path + '/' + str(o.get('name', ''))
                out.append((nm, o['expr']['query'].replace('_SRCATOM', session_atom)))
            for k, v in o.items():
                walk(v, path + ('/' + str(o.get('name', '')) if k == 'subinterfaces' else ''))
        elif isinstance(o, list):
            for v in o: walk(v, path)
    for i in ifcs:
        if i['name'] == ifcname:
            walk(i, ifcname)
    return out

def relation_reads(generics, names):
    """A full read per derived relation, as the stored relation of this generics."""
    rels = {r['name']: r for r in json.load(open(os.path.join(generics, 'relations.json'), encoding='utf-8'))}
    out = []
    for n in names:
        r = rels[n]
        t = r['mysqlTable']
        out.append((n, f'SELECT COUNT(*) FROM {q(t["name"])} WHERE {q(t["srcCol"]["name"])} IS NOT NULL AND {q(t["tgtCol"]["name"])} IS NOT NULL'))
    return out

def time_queries(db, queries):
    """Server-side duration per query (median of RUNS), in milliseconds."""
    durations = {k: [] for k, _ in queries}
    rows = {}
    for run in range(RUNS):
        for i in range(0, len(queries), BATCH):
            batch = queries[i:i + BATCH]
            script = ["SET SESSION sql_mode='ANSI';", "SET SESSION profiling_history_size=100;", "SET profiling=1;"]
            for j, (k, sql) in enumerate(batch):
                # the statement itself, as the prototype runs it: a COUNT(*) wrapper would hand the
                # optimizer another statement (the lesson of issue #1690)
                script.append(f"/*Q{j}*/ {sql.rstrip().rstrip(';')};")
            script.append("SELECT 'PROFILES-START';")
            script.append("SHOW PROFILES;")
            out = mysql(db, '\n'.join(script))
            lines = out.splitlines()
            start = lines.index('PROFILES-START')
            for l in lines[start + 1:]:
                parts = l.split('\t')
                if len(parts) < 3:
                    continue
                m = re.match(r'/\*Q(\d+)\*/', parts[2])
                if m:
                    durations[batch[int(m.group(1))][0]].append(float(parts[1]) * 1000)
    return {k: statistics.median(v) for k, v in durations.items() if v}, rows

def main():
    keep = '--keep' in sys.argv
    views = view_statements(os.path.join(AFTER, 'database.sql'))
    names = view_names(views)
    print(f'{len(views)} views in the generated database.sql: {", ".join(names)}')
    print('copying Register -> meting_before, meting_after ...')
    copy_database('meting_before')
    copy_database('meting_after')
    existing = set(tables_of('meting_after'))
    drops = [f'DROP TABLE {q(n)};' for n in names if n in existing]
    print(f'{len(drops)} link tables of derived relations dropped in meting_after; creating views')
    mysql('meting_after', "SET SESSION sql_mode='ANSI';\n" + '\n'.join(drops) + '\n' + '\n'.join(views))
    session = mysql('meting_before', 'SELECT "SESSION" FROM "SESSION" LIMIT 1').strip() or '_SRCATOM'
    print(f'session atom for the interface queries: {session!r}')

    groups = [
        ('G1 Compute-rule conjuncts (execengine/run)', conjunct_queries(BEFORE, True), conjunct_queries(AFTER, True)),
        ('G2 all conjuncts (evaluate/all)', conjunct_queries(BEFORE), conjunct_queries(AFTER)),
        ('G3 one full read per derived relation', relation_reads(BEFORE, names), relation_reads(AFTER, names)),
        ('G4 field queries of INTERFACE Zoekeisen', interface_queries(BEFORE, 'Zoekeisen', session), interface_queries(AFTER, 'Zoekeisen', session)),
    ]
    raw = []
    lines = ['| group | before: queries | before: ms | after: queries | after: ms |', '| --- | ---: | ---: | ---: | ---: |']
    detail = []
    for title, qb, qa in groups:
        db_, rb = time_queries('meting_before', qb)
        da_, ra = time_queries('meting_after', qa)
        lines.append(f'| {title} | {len(qb)} | {sum(db_.values()):.1f} | {len(qa)} | {sum(da_.values()):.1f} |')
        for k, v in db_.items():
            raw.append((title, 'before', k, f'{v:.3f}', rb.get(k, '')))
        for k, v in da_.items():
            raw.append((title, 'after', k, f'{v:.3f}', ra.get(k, '')))
        if title.startswith('G3'):
            detail.append('\n| derived relation | stored, ms (rows) | view, ms (rows) |\n| --- | ---: | ---: |')
            for n in names:
                detail.append(f'| {n} | {db_.get(n, float("nan")):.2f} ({rb.get(n, "")}) | {da_.get(n, float("nan")):.2f} ({ra.get(n, "")}) |')
        if title.startswith('G4'):
            slow = sorted(((da_[k] - db_.get(k, 0), k) for k in da_), reverse=True)[:5]
            detail.append('\n| Zoekeisen field | before, ms | after, ms |\n| --- | ---: | ---: |')
            for _, k in slow:
                detail.append(f'| {k} | {db_.get(k, float("nan")):.2f} | {da_[k]:.2f} |')
    with open(os.path.join(S, 'meting-raw' + TAG + '.csv'), 'w', newline='') as f:
        w = csv.writer(f); w.writerow(['group', 'side', 'query', 'median_ms', 'rows']); w.writerows(raw)
    result = '\n'.join(lines + detail)
    open(os.path.join(S, 'meting-result' + TAG + '.md'), 'w').write(result + '\n')
    print(result)
    if not keep:
        mysql(None, 'DROP DATABASE meting_before; DROP DATABASE meting_after;')
        print('measurement databases dropped')

if __name__ == '__main__':
    main()
