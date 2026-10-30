#!/usr/bin/env python3
"""Tables for results.md from the CSV files of meet.py and h9.py.

Usage: python3 -I analyse.py AS_GENERATED.csv CORRECTED.csv H9.csv

Times in milliseconds. A conjunct stopped at the time limit counts as the limit, and a sum
that contains one is marked with ≥.
"""
import csv
import sys
from collections import defaultdict

LIMIT_MS = 60000.0
BASE = ['r', 't', 's', 'parent']
# what the compiler reports per derived relation (`ampersand check --derived-views`)
REPORT = {
    'as generated': {'rs': 'merged', 'ru': 'full', 'rd': 'merged', 'ri': 'merged', 'voorouders': 'merged'},
    'corrected': {'rs': 'merged', 'ru': 'full', 'rd': 'merged', 'ri': 'merged', 'voorouders': 'full'},
}


def load(path):
    return list(csv.DictReader(open(path, newline='')))


def ms(x):
    return float(x) * 1000


def fmt(v, capped=False):
    s = f'{v:,.1f}' if v >= 10 else f'{v:.2f}' if v >= 0.1 else f'{v:.3f}'
    return ('≥ ' if capped else '') + s.replace(',', ' ')


def h1(rows):
    print('| N | ' + ' | '.join(r for r in ['rs', 'ru', 'rd', 'ri', 'voorouder']) + ' |')
    print('| ---: |' + ' ---: |' * 5)
    by = defaultdict(dict)
    for r in rows:
        if r['test'] == 'H1':
            by[int(r['N'])][r['label']] = (int(float(r['median'])), int(float(r['min'])))
    for n in sorted(by):
        cells = [f'{d} of {s}' for d, s in (by[n][x] for x in ['rs', 'ru', 'rd', 'ri', 'voorouder'])]
        print(f'| {n} | ' + ' | '.join(cells) + ' |')


def h2(rows, label):
    sums = defaultdict(lambda: [0.0, False])
    for r in rows:
        if r['test'] != 'H2':
            continue
        for x in r['derived'].split():
            key = (int(r['N']), r['schema'], x)
            sums[key][0] += ms(r['median'])
            sums[key][1] |= ms(r['median']) >= LIMIT_MS * 0.99
    ns = sorted({k[0] for k in sums})
    print('| write to | ' + ' | '.join(f'N = {n} stored | views' for n in ns) + ' |')
    print('| --- |' + ' ---: | ---: |' * len(ns))
    larger = defaultdict(int)
    for x in BASE:
        cells = []
        for n in ns:
            st, sc = sums[(n, 'stored', x)]
            vw, vc = sums[(n, 'views', x)]
            cells += [fmt(st, sc), fmt(vw, vc)]
            larger[n] += vw > st
        print(f'| `{x}` | ' + ' | '.join(cells) + ' |')
    print()
    print(f'{label}: base relations whose write costs more with views: ' +
          ', '.join(f'N = {n}: {larger[n]} of 4' for n in ns))


def h3h4(rows, label):
    t = {}
    for r in rows:
        if r['test'] == 'H3H4':
            t[(int(r['N']), r['schema'], r['label'])] = (ms(r['median']), r['derived'] == 'True')
    ns = sorted({k[0] for k in t})
    fields = ['rs', 'ru', 'rd', 'ri', 'voorouders']
    print('| field | report | ' + ' | '.join(f'N = {n} stored / views' for n in ns) +
          ' | growth of views, first → last N | ratio views/stored at last N | derived table in EXPLAIN |')
    print('| --- | --- |' + ' ---: |' * len(ns) + ' ---: | ---: | --- |')
    for f in fields:
        cells = [f'{fmt(t[(n, "stored", f)][0])} / {fmt(t[(n, "views", f)][0])}' for n in ns]
        growth = t[(ns[-1], 'views', f)][0] / t[(ns[0], 'views', f)][0]
        ratio = t[(ns[-1], 'views', f)][0] / t[(ns[-1], 'stored', f)][0]
        derived = t[(ns[-1], 'views', f)][1]
        print(f'| `{f}` | {REPORT[label][f]} | ' + ' | '.join(cells) +
              f' | {growth:.1f} | {ratio:.1f} | {"yes" if derived else "no"} |')


def h9(rows):
    print('| N | pairs | without FK (ms) | with FK (ms) | overhead | µs per pair with FK |')
    print('| ---: | ---: | ---: | ---: | ---: | ---: |')
    by = defaultdict(dict)
    for r in rows:
        by[int(r['N'])][r['schema']] = (ms(r['median']), int(r['pairs']))
    for n in sorted(by):
        (wo, pairs), (wi, _) = by[n]['without'], by[n]['with']
        print(f'| {n} | {pairs} | {fmt(wo)} | {fmt(wi)} | {100 * (wi - wo) / wo:+.0f}% | {1000 * wi / pairs:.2f} |')


def main():
    asgen, corrected, h9rows = load(sys.argv[1]), load(sys.argv[2]), load(sys.argv[3])
    print('## H1\n')
    print('As generated:\n')
    h1(asgen)
    print('\nCorrected:\n')
    h1(corrected)
    for label, rows in (('as generated', asgen), ('corrected', corrected)):
        print(f'\n## H2, {label}\n')
        h2(rows, label)
        print(f'\n## H3, H4, H5, {label}\n')
        h3h4(rows, label)
    print('\n## H9\n')
    h9(h9rows)


if __name__ == '__main__':
    main()
