#!/usr/bin/env python3
"""Tests H7 and H8 of the falsification experiment of issue #1744 (preregistration.md).

Usage: python3 -I h7h8.py EXPERIMENT_FK_GENERICS CONSTRAINTS_FK_GENERICS

Both generics are generated with --foreign-keys: the first from Experiment.adl, the second
from testing/derivedViews/constraints/constraints.adl, which stores relations in columns of
a wide table. Every write below comes from outside the framework, as direct SQL, except the
H8 transaction, which writes as the framework's addLink does. Each test states the MariaDB
error it expects; 0 means that the write is accepted. Prints one line per test.
"""
import os
import subprocess
import sys

CONTAINER = os.environ.get('EXP_CONTAINER', 'dv-exp-db')
USER = os.environ.get('EXP_USER', 'root')
PASSWORD = os.environ.get('EXP_PASSWORD', 'exp')


def run(db, sql):
    """Run a script and return the MariaDB error code of its first failing statement, or 0."""
    args = ['docker', 'exec', '-i', CONTAINER, 'mariadb', f'-u{USER}', f'-p{PASSWORD}', db]
    r = subprocess.run(args, input=("SET SESSION sql_mode='ANSI_QUOTES';\n" + sql).encode(), capture_output=True)
    err = r.stderr.decode()
    if r.returncode == 0:
        return 0, ''
    line = next((l for l in err.splitlines() if l.startswith('ERROR')), err)
    return int(line.split()[1]), line


def fresh(db, generics, population):
    run('mysql', f'DROP DATABASE IF EXISTS {db}; CREATE DATABASE {db} '
                 'DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN;')
    code, msg = run(db, open(os.path.join(generics, 'database.sql'), encoding='utf-8').read())
    assert code == 0, msg
    code, msg = run(db, population)
    assert code == 0, msg


EXPERIMENT_POP = """
INSERT INTO "A" ("A", "nummer") VALUES ('a1', 'n1'), ('a2', 'n2');
INSERT INTO "r" ("A", "B") VALUES ('a1', 'b1'), ('a2', 'b2');
"""
CONSTRAINTS_POP = """
INSERT INTO "Stad" ("Stad") VALUES ('Utrecht'), ('Zwolle');
INSERT INTO "Persoon" ("Persoon", "paspoort", "woontIn") VALUES ('p1', 'NL1', 'Utrecht'), ('p2', 'NL2', 'Utrecht');
INSERT INTO "kent" ("SrcPersoon", "TgtPersoon") VALUES ('p1', 'p2');
"""

# (hypothesis, description, schema, sql, expected error code)
TESTS = [
    ('H7', 'link table refers to an A that does not exist', 'e',
     """INSERT INTO "r" ("A", "B") VALUES ('zz', 'b1');""", 1452),
    ('H7', 'delete an A that a link table refers to', 'e',
     """DELETE FROM "A" WHERE "A" = 'a1';""", 1451),
    ('H7', 'rename an A that a link table refers to', 'e',
     """UPDATE "A" SET "A" = 'a9' WHERE "A" = 'a1';""", 1451),
    ('H7', 'second A with the nummer of a1 (INJ)', 'e',
     """UPDATE "A" SET "nummer" = 'n1' WHERE "A" = 'a2';""", 1062),
    ('H7', 'wide column refers to a Stad that does not exist', 'c',
     """INSERT INTO "Persoon" ("Persoon", "woontIn") VALUES ('p9', 'Atlantis');""", 1452),
    ('H7', 'delete a Stad that a person lives in', 'c',
     """DELETE FROM "Stad" WHERE "Stad" = 'Utrecht';""", 1451),
    ('H7', 'link table refers to a Persoon that does not exist', 'c',
     """INSERT INTO "kent" ("SrcPersoon", "TgtPersoon") VALUES ('p1', 'p9');""", 1452),
    ('H7', 'second Persoon with passport NL1 (INJ)', 'c',
     """UPDATE "Persoon" SET "paspoort" = 'NL1' WHERE "Persoon" = 'p2';""", 1062),
    ('boundary', 'link table refers to a B; B has no concept table, so no foreign key', 'e',
     """INSERT INTO "r" ("A", "B") VALUES ('a1', 'b-unknown');""", 0),
    ('boundary', 'nummer that no Nummer row has; a scalar concept table has no primary key', 'e',
     """UPDATE "A" SET "nummer" = 'n-unknown' WHERE "A" = 'a1';""", 0),
    ('H8', 'two A atoms exchange their nummer in one transaction, written as addLink writes', 'e',
     """START TRANSACTION;
UPDATE "A" SET "nummer" = 'n2' WHERE "A" = 'a1';
UPDATE "A" SET "nummer" = 'n1' WHERE "A" = 'a2';
COMMIT;""", 0),
    ('H8', 'two persons exchange their passport in one transaction, written as addLink writes', 'c',
     """START TRANSACTION;
UPDATE "Persoon" SET "paspoort" = 'NL2' WHERE "Persoon" = 'p1';
UPDATE "Persoon" SET "paspoort" = 'NL1' WHERE "Persoon" = 'p2';
COMMIT;""", 0),
]


def main():
    schemas = {'e': (sys.argv[1], EXPERIMENT_POP), 'c': (sys.argv[2], CONSTRAINTS_POP)}
    for hyp, what, schema, sql, expected in TESTS:
        generics, pop = schemas[schema]
        fresh('h7h8', generics, pop)
        code, msg = run('h7h8', sql)
        verdict = 'as predicted' if code == expected else 'REFUTES' if hyp in ('H7', 'H8') else 'differs'
        print(f'{hyp}\t{what}\texpected {expected}\tgot {code}\t{verdict}\t{msg}')


if __name__ == '__main__':
    main()
