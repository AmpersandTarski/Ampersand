# Layout of an included context: results

The claim and the predictions are in `preregistration.md`, written before the compiler was changed.
The measurements were taken on 26 November 2026 on the branch `migration-plan`,
with `run_all.sh` over the 146 scripts in a directory `shouldSucceed` of `testing/`.

## Before the change

With compiler v5.15.0, 112 of the 146 scripts showed a difference and 34 none.
Most differences were the ones predicted:
a concept without a table when the script is compiled by itself had one when the script was included,
148 times over all scripts, such as `Gardener` and `PlotName` of the demonstration `garden`.

Two kinds of difference were not predicted.

- `concepts.json` named the atom of a specialisation, such as `Lemon`, only in its own column
  when its context was included, and in the columns of `Citrus` and `Fruit` as well when it was compiled by itself.
  The cause was that every generalisation with the label of an included context counted as stored elsewhere,
  also when it lands on the same database.
  For the existing system that is harmless, because the migration system does not write it.
  For the desired system it is not: an atom that the migration system writes into it would lack the columns
  of its generalisations, so the desired system, running by itself after the completion,
  would not count it as a `Fruit`.
- A concept `Session` gets the table `Session_f7f1997` when its script is compiled by itself,
  because its name clashes with `SESSION` when the case is ignored.
  Included, its table was called `Session`, which the database in production does not have.

A third kind showed up in the script of the pilot (see below) and then in four scripts of `testing/`
(`Issue1413`, `Issue152`, `Issue257`, `Ticket445`), once the comparison no longer gave `SESSION` a label.
A relation of the included context on a concept of Ampersand itself, such as `SESSION` or `PrototypeContext.Role`,
is a column in the table of that concept when the context is compiled by itself.
Included, it was a link table of its own, which the database in production does not have.

18 scripts were systems of contexts themselves at the first run, and 7 more were recognised as such later
(`NAMED` before `INCLUDES`, or a system joined by a plain `INCLUDE`).
The wrapper of the experiment includes such a script under one label and nests the labels of its own inclusions,
so the comparison does not apply to them.

## After the change

| Outcome | Scripts |
| --- | --- |
| Same layout | 120 |
| A system of contexts itself, outside the reach of the wrapper | 26 |
| A difference in the notation of the comparison only | 1 |

The last run, with the final compiler, covered 147 scripts: the case `Migration/shouldSucceed/atoms_from_columns.adl`
was added during the work, and it is a system of contexts itself.

The one remaining script is `prototype/shouldSucceed/aliasTests.adl`,
whose cyclic classification makes `A`, `B` and `C` one concept with three names.
`relations.json` writes that concept differently in the two compilations (`AaliasABC` and `old.Aaliasold.Aold.Bold.C`),
so `compare_layout.py` does not find the relation.
Compared by hand, the relation is stored in the table `A` alone and in `old.A` included, with the columns `A` and `r` in both.

The claim survives on the 121 scripts that the experiment can compare.
It is not tested on an included context that includes contexts itself.

## The pilot

The script of the pilot, the first system in production that will migrate by this method,
has 201 relations and 49 concepts.
Before the fix of the relations on concepts of Ampersand itself, 26 relations differed:
24 with `SESSION` as source, one injective relation with `SESSION` as target, which is stored in the table of `SESSION`,
and one on `PrototypeContext.Role`; the comparison reported a 27th by a flaw of its own, since repaired.
After it, `compare_layout.py` reports no difference.
`views_have_tables.sh` checks the same at the level of the installation:
the migration context reads 58 tables of the database of the pilot, and the pilot compiled by itself creates all 58.
The migration context compiles the pilot twice now, once included and once by itself:
`ampersand proto` took 32 seconds for the pilot by itself and 53 seconds for the migration context,
measured once while the regression suite ran on the same machine.

## With `--all-concept-tables`

`ampersand deploy` compiles with `--all-concept-tables`, and so do the deployed demonstrations.
`flag_unchanged.sh` compiles the migration contexts of `kurk`, `garden` and `tuin` that way
with the released image of v5.15.0 and with the changed compiler, and compares `database.sql` and `generics`
apart from the version in the header and the paths in the origins.
At first the changed compiler added views on tables of `PrototypeContext` in the database of `old`
that hold no relation of `old`; such a table is now left out, and the three compile to the same files.

## The installation

`install_by_itself.sh` repeats the installation of phase 1, which failed with "Table 'garden_1.gardener' doesn't exist".
It compiles the existing and the desired system of `garden` each by itself, and the migration context without
`--all-concept-tables`, with the changed compiler, and installs the three in framework v2.25.0.
All three installers answer HTTP 200.
The migration system reads both databases through the views `old.Plot` and `new.Plot`, and has no view on a table that is not there.
During its installation the ExecEngine copied the five names and the three keepers into the desired system,
whose script has no population of its own, and the registers of copying hold 5 and 3 pairs,
so `old.r = copy_r` holds for both relations, the condition under which the article says the copying has ended.
The relaxed invariant `everyPlotHasKeeper` gives two signals, one for each plot without a keeper.

## Reading the atoms of a concept without a table

`Migration/shouldSucceed/atoms_from_own_relation.adl` gives the migration context a relation of its own on `old.Gardener`,
with an atom that the existing system does not have, and an interface on `old.Gardener`.
Two things went wrong at first, and `ampersand validate` showed the second.
The interface made `old.Gardener` a concept of objects, and a typology of objects always got a table,
so the view on the missing table `Gardener` came back; the compilation by itself now decides alone.
And the atoms were read from the relations of the existing system only, so the SQL found Ann and the evaluator Ann and Bob;
they are now read from every relation that holds them.

## What this does not show

Apart from the installation of `garden`, the experiment compares generated files.
The claim that a migration context reads the same pairs as the export of the production application gives
is tested on a copy of the production database of the pilot, which the session of that project makes (phase 2 of `../plan.md`).
