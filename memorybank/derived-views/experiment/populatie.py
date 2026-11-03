#!/usr/bin/env python3
"""The population of Experiment.adl for a size N, as SQL inserts into the base tables.

Usage: python3 -I populatie.py N SEED > population.sql

N atoms of A, B and C; two r pairs and one t pair per A (half of the t pairs also in r);
two s pairs per B; parent a random forest over C in which every atom has at most one parent
and the depth is about log2 N; nummer one value per A. The tables are those that
`ampersand proto` generates for Experiment.adl, with or without --derived-views.
"""
import random
import sys


def rows(table, cols, values):
    head = f'INSERT INTO "{table}" (' + ', '.join(f'"{c}"' for c in cols) + ') VALUES\n'
    out = []
    for i in range(0, len(values), 1000):
        chunk = values[i:i + 1000]
        out.append(head + ',\n'.join('(' + ', '.join(f"'{v}'" for v in row) + ')' for row in chunk) + ';')
    return '\n'.join(out)


def main():
    n, seed = int(sys.argv[1]), int(sys.argv[2])
    rnd = random.Random(seed)
    a = [f'a{i}' for i in range(n)]
    b = [f'b{i}' for i in range(n)]
    c = [f'c{i}' for i in range(n)]
    first = {x: rnd.choice(b) for x in a}
    r = sorted({(x, first[x]) for x in a} | {(x, rnd.choice(b)) for x in a})
    t = {(x, first[x]) if i % 2 == 0 else (x, rnd.choice(b)) for i, x in enumerate(a)}
    s = sorted({(y, rnd.choice(c)) for y in b for _ in range(2)})
    # A forest: atom k gets a parent among the atoms before it, chosen so that depth stays logarithmic.
    parent = [(c[k], c[rnd.randrange(k // 2, k)]) for k in range(1, n) if rnd.random() < 0.9]
    print("SET SESSION sql_mode = 'ANSI_QUOTES';")
    print(rows('A', ['A', 'nummer'], [(x, f'n{i}') for i, x in enumerate(a)]))
    print(rows('Nummer', ['Nummer'], [(f'n{i}',) for i in range(n)]))
    print(rows('r', ['A', 'B'], r))
    print(rows('t', ['A', 'B'], sorted(t)))
    print(rows('s', ['B', 'C'], s))
    print(rows('parent', ['SrcC', 'TgtC'], parent))


if __name__ == '__main__':
    main()
