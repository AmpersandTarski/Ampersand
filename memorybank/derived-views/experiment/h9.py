#!/usr/bin/env python3
"""Test H9 of the falsification experiment of issue #1744 (preregistration.md): the cost of a
foreign key per written pair, and whether it grows with the population.

Usage: python3 -I h9.py STORED_GENERICS FK_GENERICS OUT.csv [N ...]

Per N two databases get the base population of populatie.py (seed N) without `r`: one from
the database.sql generated without --foreign-keys, one with. Then the 2N pairs of `r` are
inserted in one transaction, in statements of 1 000 rows, as a bulk load writes them; the
link table `r` has a foreign key to `A` in the second database. Server-side time with
SHOW PROFILES, summed over the statements of the transaction; RUNS runs after one warm-up,
the table emptied before every run.
"""
import csv
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


def mysql(db, sql):
    args = ['docker', 'exec', '-i', CONTAINER, 'mariadb', f'-u{USER}', f'-p{PASSWORD}',
            '--batch', '--raw', '--skip-column-names', db]
    r = subprocess.run(args, input=sql.encode('utf-8'), capture_output=True)
    if r.returncode != 0:
        raise RuntimeError(r.stderr.decode()[:2000])
    return r.stdout.decode()


def main():
    stored_g, fk_g, out = sys.argv[1], sys.argv[2], sys.argv[3]
    sizes = [int(a) for a in sys.argv[4:]] or [1000, 4000, 16000, 64000]
    rows = []
    for n in sizes:
        pop = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'populatie.py'), str(n), str(n)],
                             capture_output=True, check=True).stdout.decode()
        statements = pop.split(';\n')
        r_inserts = [s + ';' for s in statements if s.startswith('INSERT INTO "r"')]
        base = ';\n'.join(s for s in statements if not s.startswith('INSERT INTO "r"'))
        for schema, g in (('without', stored_g), ('with', fk_g)):
            db = f'h9_{schema}'
            mysql('mysql', f'DROP DATABASE IF EXISTS {db}; CREATE DATABASE {db} '
                           'DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN;')
            mysql(db, "SET SESSION sql_mode='ANSI_QUOTES';\n" + open(os.path.join(g, 'database.sql'), encoding='utf-8').read())
            mysql(db, base)
            totals = []
            for run in range(RUNS + 1):
                script = ["SET SESSION sql_mode='ANSI_QUOTES';", 'DELETE FROM "r";',
                          'SET profiling_history_size=100;', 'SET profiling=1;', 'START TRANSACTION;']
                script += r_inserts + ['COMMIT;', 'SHOW PROFILES;']
                lines = mysql(db, '\n'.join(script)).splitlines()
                profiles = [l.split('\t') for l in lines if re.match(r'^\d+\t', l)]
                totals.append(sum(float(p[1]) for p in profiles if p[2].startswith(('INSERT', 'COMMIT'))))
            totals = totals[1:]
            pairs = sum(s.count('),\n(') + 1 for s in r_inserts)
            rows.append(dict(N=n, schema=schema, pairs=pairs, median=statistics.median(totals),
                             min=min(totals), max=max(totals)))
            print(rows[-1], file=sys.stderr)
    with open(out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=['N', 'schema', 'pairs', 'median', 'min', 'max'])
        w.writeheader()
        w.writerows(rows)


if __name__ == '__main__':
    main()
