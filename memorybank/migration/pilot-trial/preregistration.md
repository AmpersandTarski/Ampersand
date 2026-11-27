# Trial on a copy of a production database: preregistration

This note registers the predictions of the first trial in production before any data is read.
The trial is phase 2 of `../plan.md`: a migration context reads a database in production as it is.
The copying of phase 3 runs later on the same copy, and its predictions are registered here too.

## The migration

The pilot is the application that `../plan.md` names as the first system in production to migrate by this method.
Its release 1.42.0 went live on 26 November 2026 by the pilot's own procedure,
which reinstalls the database, lays down the fixed population of the script and imports an export of production.
The trial replays that change, from release 1.41.0 to release 1.42.0,
on the dump that the pilot's server made just before that deployment.

Three facts make this the change to start with.
The dump has a known layout: release 1.41.0 was built with framework v2.25.0, which contains compiler v5.15.0,
without `--all-concept-tables`, so the database is laid out by the compilation by itself that phase 2 reproduces.
The outcome of the pilot's own procedure is known: the production database after the deployment.
And the change is large: 132 new invariants, 26 new signals, one changed ExecEngine rule,
three new scripts with more than 7,000 lines of fixed population, and no relation removed.

## What the trial does not test

No new or changed invariant of release 1.42.0 can be violated by the data of release 1.41.0.
`rule_diff.py` in this directory, run on `rules.json` of the migration system, finds that all 132 new invariants,
114 of them rules of properties, are about concepts that release 1.42.0 introduces,
such as a request for plant material and its lines, and that no invariant changed.
So steps 3 to 5 of the article, the relaxation of a rule that the existing data violates, are not exercised.
The regression cases in `testing/Travis/testcases/Migration` and the demonstrations `garden`, `kurk` and `tuin` test them;
the next migration of the pilot that tightens a rule tests them in production.

## Predictions for phase 2

1. Every table and every column that the migration system reads in the database of `old` exists in the dump.
2. The installer of the migration system answers HTTP 200 with the dump restored as the database of `old`
   and the desired system installed by itself.
3. For every relation of release 1.41.0, the pairs that the migration system reads as `old.r`
   are the pairs that the application of release 1.41.0 gives over the same database.
   The comparison takes the population of each relation from both, pair by pair.
4. For every concept of release 1.41.0, the atoms that the migration system reads are the atoms that release 1.41.0 gives,
   also for a concept without a table of its own, whose atoms come from the columns of its relations.

## Predictions for phase 3

These depend on the generator, which writes the copying rules; they are registered now so that the outcome cannot shape them.

5. The migration system installs none of the population in the scripts of `old` and `new`:
   its `populations.json` holds only the population of `PrototypeContext` (observed on 27 November 2026, 9 KB).
   The fixed population of release 1.42.0 reaches the desired system only through its own installation.
6. After the copying has ended, every relation of the desired system holds the pairs that production held
   right after the deployment of 1.42.0, apart from the changes that users made between that deployment
   and the dump that records it.
   The pilot's own procedure takes the union of the fixed population and an export,
   and the method of the article takes the union of the fixed population and a copy, so the two agree.
7. In particular, the relation that names the crops holds 205 pairs and not 203:
   the two crops that the users removed come back from the fixed population of the desired script, as they did in production.
   The copying does not remove them, because it only adds what `old` holds.
   A repair of that belongs in the script of the pilot, not in the migration.
