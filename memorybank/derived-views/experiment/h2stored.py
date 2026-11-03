#!/usr/bin/env python3
"""Completes H2 for the stored schema: the conjuncts that a write to a base relation affects
through the derived relations that the ExecEngine then rewrites.

Usage: python3 -I h2stored.py STORED_GENERICS OUT.csv [N ...]

meet.py sums, per base relation x, the conjuncts that relations.json lists as affected by x.
In the stored schema a write to x also makes the ExecEngine rewrite every derived relation
whose term reads x, and that write affects the conjuncts of the derived relation in the same
transaction. This script times those conjuncts in the databases stored_N that meet.py left,
and writes them as H2 rows labelled with the base relations they belong to.
"""
import csv
import os
import statistics
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import meet  # noqa: E402

# the base relations that the term of each derived relation of Experiment.adl reads
READS = {'rs': ['r', 's'], 'ru': ['r', 't'], 'rd': ['r', 't'], 'ri': ['r', 't'], 'voorouder': ['parent']}


def main():
    generics, out = sys.argv[1], sys.argv[2]
    sizes = [int(a) for a in sys.argv[3:]] or [1000, 4000, 16000, 64000]
    rels = meet.relations(generics)
    direct, _ = meet.affected(generics)
    conjs = {c['id']: c['violationsSQL'] for c in meet.json.load(open(os.path.join(generics, 'conjuncts.json'), encoding='utf-8'))}
    extra = {}  # conjunct id -> base relations it belongs to through a derived relation
    for d, bases in READS.items():
        for cid in rels[d]['affectedConjuncts']:
            for x in bases:
                if cid not in direct[x]:
                    extra.setdefault(cid, set()).add(x)
    rows = []
    for n in sizes:
        times = meet.time_queries(f'stored_{n}', [(cid, conjs[cid]) for cid in sorted(extra)])
        for cid, ts in times.items():
            rows.append(dict(N=n, test='H2', schema='stored', label=cid, median=statistics.median(ts),
                             min=min(ts), max=max(ts), derived=' '.join(sorted(extra[cid]))))
        print(f'N={n} done', file=sys.stderr)
    with open(out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=['N', 'test', 'schema', 'label', 'median', 'min', 'max', 'derived'])
        w.writeheader()
        w.writerows(rows)


if __name__ == '__main__':
    main()
