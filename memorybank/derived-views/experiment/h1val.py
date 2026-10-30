#!/usr/bin/env python3
"""The first half of test H1 of the falsification experiment of issue #1744 (preregistration.md):
`ampersand validate --derived-views` on 30 random populations of Experiment.adl.

Usage: python3 -I h1val.py AMPERSAND_BINARY WORK_DIR [SEEDS]

Per seed a script is written: Experiment.adl with a POPULATION of size 12 from a random
generator, so that compositions, differences and a closure over a few levels occur. Validate
installs the script in a temporary database, with the derived relations as views, and compares
every view and every conjunct query with the evaluator of the compiler. Prints the exit code
per seed; a non-zero code is a difference. The database comes from MYSQL_HOST, MYSQL_USER and
MYSQL_PASSWORD, as for any `ampersand validate`.
"""
import os
import random
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
N = 12


def population(seed):
    rnd = random.Random(seed)
    a, b, c = ([f'{p}{i}' for i in range(N)] for p in 'abc')
    pairs = lambda xs, ys, k: sorted({(rnd.choice(xs), rnd.choice(ys)) for _ in range(k)})
    rel = {
        'r': pairs(a, b, 2 * N),
        't': pairs(a, b, N),
        's': pairs(b, c, 2 * N),
        'parent': sorted({(c[k], c[rnd.randrange(0, k)]) for k in range(1, N) if rnd.random() < 0.8}),
    }
    rel['t'] += rnd.sample(rel['r'], N // 2)  # some pairs in both r and t
    lines = []
    for name, ps in rel.items():
        body = ', '.join(f'("{x}","{y}")' for x, y in sorted(set(ps)))
        lines.append(f'POPULATION {name} CONTAINS [ {body} ]')
    return '\n'.join(lines)


def main():
    binary, work = sys.argv[1], sys.argv[2]
    seeds = int(sys.argv[3]) if len(sys.argv) > 3 else 30
    os.makedirs(work, exist_ok=True)
    model = open(os.path.join(HERE, 'Experiment.adl'), encoding='utf-8').read()
    failures = 0
    for seed in range(1, seeds + 1):
        script = model.replace('ENDCONTEXT', population(seed) + '\nENDCONTEXT')
        path = os.path.join(work, f'H1_{seed}.adl')
        open(path, 'w', encoding='utf-8').write(script)
        r = subprocess.run([binary, 'validate', '--derived-views', path], capture_output=True)
        failures += r.returncode != 0
        tail = r.stdout.decode().strip().splitlines()[-1:] + r.stderr.decode().strip().splitlines()[-2:]
        print(f'seed {seed}\texit {r.returncode}\t{" | ".join(tail)[:200]}')
    print(f'{failures} of {seeds} seeds differ')


if __name__ == '__main__':
    main()
