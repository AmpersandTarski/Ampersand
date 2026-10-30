#!/usr/bin/env python3
"""Measurements H1-H5 of the falsification experiment of issue #1744 (preregistration.md).

Usage: python3 -I meet.py STORED_GENERICS VIEWS_GENERICS OUT_PREFIX [N ...]

For every size N two databases are made in the experiment container: `stored_N` from the
database.sql generated without --derived-views, its derived tables filled from their terms,
and `views_N` from the one generated with --derived-views. Both get the base population of
populatie.py with seed N. Then, per N:
  H1  every derived relation, pair for pair: stored table against view;
  H2  per base relation, the summed violation-query time of the conjuncts it affects;
  H3/H4  the interface field query of every derived relation for one atom of A;
  H5  whether EXPLAIN of that field query shows a derived table.
Every query is timed server-side with SHOW PROFILES: one warm-up run, then RUNS runs, median,
minimum and maximum. Two deviations from the preregistration, added after the first trial run
took more than 20 s for one conjunct at N = 1 000: a query stops after LIMIT seconds and is
recorded as LIMIT, and a query whose warm-up takes longer than SLOW seconds is run SLOW_RUNS
times. Writes OUT_PREFIX.csv (one row per query).

The container, user and password come from EXP_CONTAINER, EXP_USER and EXP_PASSWORD.
"""
import csv
import json
import os
import re
import statistics
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
CONTAINER = os.environ.get('EXP_CONTAINER', 'dv-exp-db')
USER = os.environ.get('EXP_USER', 'root')
PASSWORD = os.environ.get('EXP_PASSWORD', 'exp')
RUNS = 7
SLOW, SLOW_RUNS = 5.0, 2  # a query slower than SLOW seconds is measured SLOW_RUNS times
LIMIT = 60.0  # max_statement_time: a query that takes longer is recorded as LIMIT
BATCH = 90  # SHOW PROFILES keeps at most 100 entries
DERIVED = ['rs', 'ru', 'rd', 'ri', 'voorouder']
BASE = ['r', 't', 's', 'parent']
ATOM = 'a1'


def mysql(db, sql):
    args = ['docker', 'exec', '-i', CONTAINER, 'mariadb', f'-u{USER}', f'-p{PASSWORD}',
            '--batch', '--raw', '--skip-column-names']
    if db:
        args.append(db)
    r = subprocess.run(args, input=sql.encode('utf-8'), capture_output=True)
    if r.returncode != 0:
        raise RuntimeError(f'mariadb failed in {db}: {r.stderr.decode()[:2000]}')
    return r.stdout.decode('utf-8', 'replace')


def statements(path):
    """The statements of a generated database.sql: each ends with ';' at the end of a line."""
    out, cur = [], []
    for line in open(path, encoding='utf-8'):
        cur.append(line)
        if line.rstrip().endswith(';'):
            out.append(''.join(cur))
            cur = []
    return out


def create(db, generics, population):
    mysql(None, f'DROP DATABASE IF EXISTS {db}; CREATE DATABASE {db} '
                'DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN;')
    ddl = open(os.path.join(generics, 'database.sql'), encoding='utf-8').read()
    mysql(db, "SET SESSION sql_mode='ANSI_QUOTES';\n" + ddl)
    mysql(db, population)


def view_sql(generics):
    """name -> the CREATE VIEW statement of every derived relation in a generated database.sql."""
    views = {}
    for s in statements(os.path.join(generics, 'database.sql')):
        m = re.search(r'CREATE VIEW "([^"]+)" AS', s)
        if m and m.group(1) in DERIVED:
            views[m.group(1)] = s[m.start():]
    return views


def relations(generics):
    return {r['name']: r for r in json.load(open(os.path.join(generics, 'relations.json'), encoding='utf-8'))}


def fill_stored(db, stored_generics, views):
    """Fill the derived tables of the stored schema from the views of their terms."""
    rels = relations(stored_generics)
    script = ["SET SESSION sql_mode='ANSI_QUOTES';"]
    for name, stmt in views.items():
        tmp = f'tmp_{name}'
        script.append(stmt.replace(f'CREATE VIEW "{name}"', f'CREATE VIEW "{tmp}"', 1))
        t = rels[name]['mysqlTable']
        script.append(f'INSERT INTO "{t["name"]}" ("{t["srcCol"]["name"]}", "{t["tgtCol"]["name"]}") '
                      f'SELECT DISTINCT src, tgt FROM "{tmp}";')
        script.append(f'DROP VIEW "{tmp}";')
    mysql(db, '\n'.join(script))


def h1_differences(n, stored_generics):
    rels = relations(stored_generics)
    out = {}
    for name in DERIVED:
        t = rels[name]['mysqlTable']
        stored = (f'SELECT DISTINCT "{t["srcCol"]["name"]}" AS src, "{t["tgtCol"]["name"]}" AS tgt '
                  f'FROM stored_{n}."{t["name"]}"')
        view = f'SELECT DISTINCT src, tgt FROM views_{n}."{name}"'
        sql = ("SET SESSION sql_mode='ANSI_QUOTES';"
               f'SELECT (SELECT COUNT(*) FROM ({stored} EXCEPT {view}) x) '
               f'+ (SELECT COUNT(*) FROM ({view} EXCEPT {stored}) y), '
               f'(SELECT COUNT(*) FROM ({view}) z);')
        diff, size = mysql(None, sql).split()
        out[name] = (int(diff), int(size))
    return out


def profile(db, sqls):
    """Run the queries in one session and return their server-side durations in seconds.
    A query stopped by max_statement_time gets the duration LIMIT."""
    script = ["SET SESSION sql_mode='ANSI_QUOTES';", f'SET SESSION max_statement_time={LIMIT};',
              'SET profiling_history_size=100;', 'SET profiling=1;']
    script += [sql.rstrip().rstrip(';') + ';' for sql in sqls]
    script.append('SHOW PROFILES;')
    args = ['docker', 'exec', '-i', CONTAINER, 'mariadb', f'-u{USER}', f'-p{PASSWORD}',
            '--batch', '--raw', '--skip-column-names', '--force', db]
    r = subprocess.run(args, input='\n'.join(script).encode('utf-8'), capture_output=True)
    lines = r.stdout.decode('utf-8', 'replace').splitlines()
    out = [min(float(l.split('\t')[1]), LIMIT) for l in lines[-len(sqls):]]
    if 'max_statement_time' in r.stderr.decode():
        stopped = r.stderr.decode().count('max_statement_time')
        out = [LIMIT if t >= LIMIT * 0.99 else t for t in out]
        assert stopped == sum(1 for t in out if t >= LIMIT), r.stderr.decode()[:500]
    return out


def time_queries(db, queries):
    """queries: [(label, sql)] -> {label: [seconds per run]}. One warm-up run per query, then
    RUNS runs; a query whose warm-up takes longer than SLOW seconds gets SLOW_RUNS runs."""
    warm = profile(db, [sql for _, sql in queries])
    times = {}
    fast = [(label, sql) for (label, sql), w in zip(queries, warm) if w <= SLOW]
    for label, sql in [(label, sql) for (label, sql), w in zip(queries, warm) if w > SLOW]:
        times[label] = profile(db, [sql] * SLOW_RUNS)
    plan = [(label, sql) for label, sql in fast for _ in range(RUNS)]
    for label, _ in fast:
        times[label] = []
    for i in range(0, len(plan), BATCH):
        chunk = plan[i:i + BATCH]
        for (label, _), t in zip(chunk, profile(db, [sql for _, sql in chunk])):
            times[label].append(t)
    return {label: times[label] for label, _ in queries}


def explain_has_derived(db, sql):
    out = mysql(db, "SET SESSION sql_mode='ANSI_QUOTES'; EXPLAIN " + sql.rstrip().rstrip(';') + ';')
    return '<derived' in out


def field_queries(generics):
    ifcs = json.load(open(os.path.join(generics, 'interfaces.json'), encoding='utf-8'))
    lees = next(i for i in ifcs if i['name'] == 'Lees')
    return [(o['name'], o['expr']['query'].replace('_SRCATOM', ATOM))
            for o in lees['ifcObject']['subinterfaces']['ifcObjects']]


def affected(generics):
    """The conjuncts that a write to each base relation affects, and the query of each."""
    rels = relations(generics)
    conjs = {c['id']: c['violationsSQL'] for c in json.load(open(os.path.join(generics, 'conjuncts.json'), encoding='utf-8'))}
    by_base = {x: rels[x]['affectedConjuncts'] for x in BASE}
    ids = sorted({cid for cs in by_base.values() for cid in cs})
    return by_base, [(cid, conjs[cid]) for cid in ids]


def main():
    stored_g, views_g, prefix = sys.argv[1], sys.argv[2], sys.argv[3]
    sizes = [int(a) for a in sys.argv[4:]] or [1000, 4000, 16000, 64000]
    views = view_sql(views_g)
    rows = []
    for n in sizes:
        pop = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'populatie.py'), str(n), str(n)],
                             capture_output=True, check=True).stdout.decode()
        create(f'stored_{n}', stored_g, pop)
        fill_stored(f'stored_{n}', stored_g, views)
        create(f'views_{n}', views_g, pop)
        for name, (diff, size) in h1_differences(n, stored_g).items():
            rows.append(dict(N=n, test='H1', schema='both', label=name, median=diff, min=size, max='', derived=''))
        for schema, g in (('stored', stored_g), ('views', views_g)):
            db = f'{schema}_{n}'
            by_base, qs = affected(g)
            for cid, ts in time_queries(db, qs).items():
                rows.append(dict(N=n, test='H2', schema=schema, label=cid, median=statistics.median(ts),
                                 min=min(ts), max=max(ts),
                                 derived=' '.join(x for x, cs in by_base.items() if cid in cs)))
            fq = field_queries(g)
            for (label, ts), (_, sql) in zip(time_queries(db, fq).items(), fq):
                rows.append(dict(N=n, test='H3H4', schema=schema, label=label, median=statistics.median(ts),
                                 min=min(ts), max=max(ts), derived=explain_has_derived(db, sql)))
        print(f'N={n} done', file=sys.stderr)
    with open(prefix + '.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=['N', 'test', 'schema', 'label', 'median', 'min', 'max', 'derived'])
        w.writeheader()
        w.writerows(rows)


if __name__ == '__main__':
    main()
