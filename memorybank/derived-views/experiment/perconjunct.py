#!/usr/bin/env python3
"""Per conjunct of H2: the rule it belongs to, the views it reads, and its time per N.

Usage: python3 -I perconjunct.py SERIES.csv VIEWS_GENERICS STORED_GENERICS
"""
import csv
import json
import os
import sys
from collections import defaultdict

VIEWS = ['rs', 'ru', 'rd', 'ri', 'voorouder']


def conjuncts(generics):
    out = {}
    for c in json.load(open(os.path.join(generics, 'conjuncts.json'), encoding='utf-8')):
        sql = c['violationsSQL']
        reads = [v for v in VIEWS if f'from "{v}"' in sql or f'FROM "{v}"' in sql]
        out[c['id']] = (', '.join(c['invariantRuleNames'] + c['signalRuleNames']), reads)
    return out


def main():
    rows = list(csv.DictReader(open(sys.argv[1], newline='')))
    names = {'views': conjuncts(sys.argv[2]), 'stored': conjuncts(sys.argv[3])}
    t = defaultdict(dict)
    for r in rows:
        if r['test'] == 'H2':
            t[(r['schema'], r['label'])][int(r['N'])] = float(r['median']) * 1000
    ns = sorted({n for d in t.values() for n in d})
    print('| schema | conjunct | rule | tables or views it reads | ' + ' | '.join(f'N = {n} (ms)' for n in ns) + ' |')
    print('| --- | --- | --- | --- |' + ' ---: |' * len(ns))
    for (schema, cid), d in sorted(t.items()):
        rule, reads = names[schema][cid]
        cells = [f'{d[n]:,.1f}'.replace(',', ' ') for n in ns]
        print(f'| {schema} | {cid} | {rule} | {", ".join(reads) if schema == "views" else "stored tables"} | ' + ' | '.join(cells) + ' |')


if __name__ == '__main__':
    main()
