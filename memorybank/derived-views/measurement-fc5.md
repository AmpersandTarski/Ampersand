# Measurement: derived relations as views on FC5

Requirement R6 of [issue #1744](https://github.com/AmpersandTarski/Ampersand/issues/1744)
asks for a measurement of `--derived-views` on the model of FC5,
before and after. This note records what was measured on 27 October 2026, how, and what
came out. The script is `meting-fc5.py` next to this note.

## What was measured, and on what

**Model.** The model that the running prototype of the shared FC5 stack was built from,
copied from its container (`<prototype-container>`, `/usr/local/project`) on
27 October 2026, because that is the model its database population belongs to. Its
`register.adl`, `workflow.adl` and `Kenmerk.adl` are byte-equal to `innersource/main` at
`8e6380a1`; `Eistekst.adl`, `Samenvoegen.adl`, `api.ifc` and `interfaces.ifc` differ from
that commit, and no commit of the last 120 on `innersource/main` matches all files. The
model has 16 `ENFORCE r := term` statements, plus the two of `PrototypeContext`.

**Population.** 1 285 `Eis`, 17 159 `WetenschappelijkeNaam`, 6 620 pairs in `parent`,
9 297 in `productNaam`, 19 643 `Kenmerk`, 23 sessions; `orgParent` is empty. This is the
trial population (`POP=proef`) of one workplace, the same as in the research note attached
to the issue. The full corpus was not loaded.

**Database.** MariaDB 10.6.27 (the container `<database-container>`). The script copies
every table of `Register` with `CREATE TABLE ... LIKE` and `INSERT ... SELECT` into two
databases on the same server: `meting_before` as the prototype has it, and `meting_after`
without the six link tables of the derived relations that become views and with the
`CREATE VIEW` statements of the generated `database.sql` instead. Both are dropped
afterwards. Every query runs as the prototype runs it, without a `COUNT(*)` wrapper (the
lesson of issue #1690), timed server-side with `SHOW PROFILES`; three runs, median per query.

**Compiler.** Branch `derived-relations-as-views`, the commit of this note; generics
generated with `ampersand proto --no-frontend` and the same with `--derived-views`, and
twice more with `--materialize` for three and for four relations.

**Four groups of queries.**

- G1: the violation queries of the `Compute` rules, which is what `admin/execengine/run`
  evaluates to keep the derived relations equal to their terms.
- G2: the violation queries of all conjuncts, which is what `admin/ruleengine/evaluate/all`
  evaluates.
- G3: one full read of every derived relation: the stored table or column before, the
  view after.
- G4: the 38 field queries of `INTERFACE Zoekeisen`, with the placeholder replaced by one
  session atom.

## What the compiler generates

| | before | `--derived-views` | ... `--materialize` 4 relations |
| --- | ---: | ---: | ---: |
| rules | 533 | 451 | 469 |
| `Compute` rules | 78 | 32 | 42 |
| conjuncts | 590 | 499 | 519 |
| violation SQL (bytes) | 1 136 619 | 946 362 | 1 017 952 |
| share of the `Compute` conjuncts in that SQL | 18.6% | 8.3% | 13.4% |
| conjuncts with candidate queries | 577 | 455 | 474 |
| `CREATE TABLE` | 309 | 282 | 291 |
| `CREATE VIEW` | 0 | 17 | 13 |

The 82 rules that disappear with views are 46 `Compute` rules of the 17 view relations and
36 property rules (`UNI`, `INJ`, `SYM`, `ASY`) that the shape of a definition guarantees
(DC-11). One derived relation stays stored on its own account:
`PrototypeContext.sessionAllowedRoles`, because a second `ENFORCE` writes it (DC-2).

## Timings

Milliseconds, server-side, median of three runs, summed per group.

| group | before | `--derived-views` | ... `--materialize` 3 | ... `--materialize` 4 |
| --- | ---: | ---: | ---: | ---: |
| G1 `Compute` conjuncts (execengine/run) | 255 (78) | 7 (32) | 223 (40) | 224 (42) |
| G2 all conjuncts (evaluate/all) | 4 434 (590) | 12 732 (499) | 4 760 (517) | 4 281 (519) |
| G3 one full read per derived relation | 2.6 (17) | 1 411 (17) | 7.8 (14) | 5.9 (13) |
| G4 field queries of `Zoekeisen` | 2.6 (38) | 3.1 (38) | 3.0 (38) | 3.0 (38) |

In brackets the number of queries. The "before" column was measured four times, once per
run; it varied between 4 282 and 4 434 ms for G2 and between 253 and 258 ms for G1.
The three relations materialised in the third column are `eisHeeftTweeling`,
`eisFacetGelijk` and `kenmerkHeeftDrager`; the fourth adds `gebiedInHerkomst`.

### What each view costs to read in full (`--derived-views`, G3)

| derived relation | stored, ms | view, ms | the view's SQL |
| --- | ---: | ---: | --- |
| `eisHeeftTweeling` | 0.06 | 1 167 | `I[Eis] /\ eisFacetGelijk;eisFacetGelijk~`, a view on a view |
| `kenmerkHeeftDrager` | 1.3 | 154 | a union over 19 643 `Kenmerk` |
| `eisFacetGelijk` | 0.4 | 82 | six self-joins of `Eis` on its facets |
| `basiseis` | 0.05 | 2.2 | `I[Eis] - land;land~` |
| `gebiedInHerkomst` | 0.2 | 1.9 | two closures over `ligtIn` |
| `landeneis` | 0.1 | 1.6 | `I[Eis] /\ land;land~` |
| the other eleven | 0.03 to 0.12 each | 0.12 to 0.55 each | |

### Which rules change, per rule (G2)

With `--derived-views` alone, two rules carry the whole increase: `RuimWeesKenmerkOp`
goes from 11.5 to 8 257 ms (it joins `kenmerkHeeftDrager`, the union view, which the
database computes in full inside the join) and `BehoudtDeelverzameling` from 6 to 565 ms
(it joins `gebiedInHerkomst`, the recursive view). `OriginesSluitenElkaarUit` goes the
other way, from 161 to 6 ms, because the view of `gebiedInHerkomst` is read where the
stored table was read through a wider join. The 82 rules that are gone took 321 ms before.
`Ruimeissignatuurop` (2.0 s) and `Bepaaleissignatuur` (1.2 s) are as slow before as after
and read no view.

With the four relations materialised, no rule is more than 25 ms slower than before, and
G2 is 4 281 against 4 282 ms. `BehoudtDeelverzameling` is back at 6 ms, because
`gebiedInHerkomst` is a table again.

## What this says

The views of this measurement were generated by commit b841107f8, which wrote `select distinct`
in every view, also in those the report called merged (test H5 of `experiment/results.md`).
So no view was merged here, and the numbers are those of 17 views that the database computes in
full; the compiler after that experiment leaves `DISTINCT` out of a one-`SELECT` view.

1. The materialisation work of the ExecEngine leaves the write path: the conjuncts that
   `execengine/run` evaluates for the derived relations go from 255 to 7 ms per run, and
   from 78 to 32 queries, with every `:=` relation a view. The reads of the
   `Zoekeisen` interface do not change (2.6 against 3.1 ms for its 38 field queries).
2. A view that the database cannot merge into the reading query (a union, a recursion, a
   `DISTINCT`) is a temporary table without an index inside every rule that joins it: the
   block-nested-loop join of issue #1708. That is what the report line of the compiler
   says with "computed in full at every use", and what `--materialize` is for. On this
   model three such views feed rules that matter (`kenmerkHeeftDrager`, `gebiedInHerkomst`)
   or are expensive by themselves (`eisHeeftTweeling`, `eisFacetGelijk`); with those four
   stored, `evaluate/all` costs what it cost before and `execengine/run` 224 instead of
   258 ms, while 13 derived relations are views that can never lag behind.
3. The three expensive derivations are expensive as stored tables too: their `Compute`
   conjuncts are 224 of the 258 ms of G1 before. A view does not remove that cost, and a
   stored table pays it at every transaction that touches a facet. The incremental route of
   issue #1675 is the answer for those, not this change.

## The other two options on the same model (counts, not timings)

`ampersand proto --no-frontend --derived-views --foreign-keys --schema-views` on the same
model gives a `database.sql` with 282 tables and 1 011 views: 495 for the interface fields
(the number the research note counted in `interfaces.json`), 499 for the conjuncts and 17
for the derived relations. 90 view names were longer than 64 characters and were cut and
given a hash; no two names, tables included, coincide when case is ignored. The longest
name is `ifc_Eis_Kenmerken_32__38__32_registratie_Kenmerken_32__4_b68f0e0`. 169 foreign
keys and 47 `UNIQUE` constraints are generated; 84 of the 253 relation columns get no
foreign key, because their target is a concept whose table has no primary key (a value
such as `Userid`, `Emailadres` or `Naam`, issue #341) or no table at all. The installation
of this schema in a database was not tried on this model; the regression test
`testing/derivedViews/constraints` installs the same statements for a small model.

## What was not measured

The import time of the corpus, the wall-clock time of `admin/execengine/run` and
`admin/ruleengine/evaluate/all` through the prototype framework, and the navigation bar of
a running prototype with views: these need a framework image with the pull request
AmpersandTarski/prototype#514 and a compiler release that emits `storage`, which comes
after this release. The full corpus (`POP=totaal`) was not loaded; the research note's
`eisDektOrganisme` is not in this model, so the recursive case is `gebiedInHerkomst`.
