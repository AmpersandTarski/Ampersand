# Design choices — multi-context Ampersand

Register of design choices for systems of contexts (issue [#1509](https://github.com/AmpersandTarski/Ampersand/issues/1509), phase 2 of [#1307](https://github.com/AmpersandTarski/Ampersand/issues/1307)).
Numbers are stable and never reused; the current state stands here, the history lives in git.
The specification is `AmpersandData/FormalAmpersand/MultiContext.adl`,
and the mathematics is in the article *Multi-Context Information Systems in Ampersand* (2026).

Stef Joosten decided these choices: most of them on 6 October 2026,
and on 7 October 2026 he confirmed the ones that the AI agent had taken provisionally during the build.
Open questions sit at the bottom under "Still to decide".

## The language

**A context includes another context with a statement of its own: `CONTEXT A INCLUDES B`.
`INCLUDE "file"` keeps its meaning.**
*DC-1 · decided · 2026-10-06*

The statement stands outside every `CONTEXT` block:
`CONTEXT A, B INCLUDES C FROM "file" AS alias, D`.
`INCLUDE "file"` brings in the text of a file, which becomes part of the context in which it stands.

*Considerations:*

1. Two relations were easily confused under one keyword: a relation between files,
   which organises the text of a script, and a relation between contexts.
   FormalAmpersand itself consists of eleven files that `INCLUDE` joins into one context.
2. Every existing script relies on `INCLUDE` as a union.
   For the 207 scripts in `testing/Travis/testcases` that the compiler accepts, `memorybank/tools/compare_compiler_output.sh` reports "identical apart from printed terms" against the commit this branch started from, and no script as different. Printed terms are the comments in generated SQL and the normalisation steps in `interfaces.json` (DC-10).
3. The statement stands outside the block, so that a reader sees that it concerns more than one context.
4. Rejected: `INCLUDE "file" AS alias` inside a block, which the first build had.
   It made a file the unit of inclusion, where the unit is a context.

*Impact on the specification:* the pattern `Inclusion` of `MultiContext.adl`.

*Impact in production:* none for existing scripts.

**The words `INCLUDES`, `FROM` and `AS` are recognised by their position and are no keywords.**
*DC-2 · decided · 2026-10-07*

*Considerations:*

1. A new keyword would invalidate every script that uses the word as the name of a concept.
   The regression case `as_is_no_keyword.adl` uses all three.
2. After `CONTEXT` and a name, no context element can start with an identifier in upper case,
   so the position is unambiguous.

*Impact on the specification:* none.

*Impact in production:* none.

**The name of a context identifies the context.
Two versions of a context are two contexts.**
*DC-11 · decided · 2026-10-06*

Everything between `CONTEXT Foo` and `ENDCONTEXT` belongs to the context `Foo`.
A file can contain several contexts and several fragments of one context;
fragments with the same name are united.
The compiler identifies a context by its name together with the file after `FROM`,
which stands for the version.

*Considerations:*

1. The existing and the desired system in a migration are two versions of one context:
   both scripts say `CONTEXT Kurk`. They have a database each.
2. Ampersand makes no statement about version management.
   Which version runs on which database is decided at deployment (DC-13).
3. A file that `INCLUDE` brings in keeps contributing to the including context whatever its header says,
   as before. Two of the files of FormalAmpersand have the header `CONTEXT RAP`.

*Impact on the specification:* a context is determined by its name and its version.

*Impact in production:* none.

**The prefix of a name is the name of the included context. An alias is a second name.**
*DC-3 · decided · 2026-10-06*

Without `AS`, the things of an included context are referred to with its name as a prefix.
With `AS`, both the alias and the name can be used.
If a context includes two contexts with the same name, the compiler demands an alias for each,
and reports the name as ambiguous where a script uses it.
An alias cannot be a name space of Ampersand itself.

*Considerations:*

1. The developer reads `Registry.Person` as the person of the register.
   A freely chosen alias as the only prefix would hide that.
2. The requirement in general: every inclusion has a prefix that fits no other context that the same context includes.
   It is the condition under which every thing of an included context has a reference (proof-track claim PRF-11, `reference_complete`).

*Impact on the specification:* the rules `PrefixDefinition` and `EveryInclusionCanBeNamed`.

*Impact in production:* none.

**A prefix names a context that is included directly.**
*DC-5 · decided · 2026-10-06*

If `Registry` includes `Towns`, then `Permits` sees the towns without being able to name them.
To refer to `Towns.Town`, `Permits` states that it includes `Towns`.

*Considerations:*

1. Stating the inclusion costs one line and adds no database (DC-6).
2. Rejected: a prefix that follows a path of inclusions, such as `Registry.Towns.Town`.
   It adds no expressive power and makes a script depend on the inclusions in other scripts.

*Impact on the specification:* the rule `DenotesDefinition`.

*Impact in production:* none.

**A concept name with a prefix has to exist in the included context.**
*DC-4 · decided · 2026-10-07*

*Considerations:*

1. Ampersand declares a concept by using it.
   Without this check, `Registry.Persn` would silently become a new concept.
2. A misspelled relation needs no check of its own,
   since the type checker reports an undeclared relation.

*Impact on the specification:* one error message.

*Impact in production:* none.

**Inclusion has no cycles.**
*DC-12 · decided · 2026-10-06*

*Considerations:*

1. A context must be deployable without the contexts that include it.
   In a cycle, no context could be deployed before the others.
2. The closure of an acyclic relation is a partial order (PRF-11, `reach_antisymm`),
   which gives the order of compilation and installation.

*Impact on the specification:* the rule `InclusionIsAcyclic`.

*Impact in production:* none.

**Roles, the concept SESSION and the name spaces of Ampersand itself get no prefix.**
*DC-7 · decided · 2026-10-07*

*Considerations:*

1. A role is a function of a person in an organisation.
   The clerk of the permit system is the same clerk when a rule of the register asks for one.
2. SESSION and `PrototypeContext` belong to the application that serves the user.

*Impact on the specification:* a rule of an included context that is assigned to a role is assigned to the role with that name in the including application.

*Impact in production:* none.

**The interfaces of a context belong to its own application.**
*DC-14 · decided · 2026-10-07*

The compiler does not join the interfaces of an included context, so another context cannot refer to them.

*Considerations:*

1. An interface is the user interface of the application of its context.
   Joined into the including application, it would show up in a navigation menu where nobody asked for it.
2. Views and identities are joined, because they say how an atom of an included concept is shown.

*Impact on the specification:* none; `MultiContext.adl` does not cover interfaces.

*Impact in production:* none.

## Storage

**Every context has one database, and each fact is stored once.**
*DC-6 · decided · 2026-10-06*

A pair is stored in the database of the context that declares the relation,
and an atom in the database of the context that declares the concept.
A context that is reached along two paths is one context with one database.

*Considerations:*

1. A fact must be updated in one place only.
2. Rejected: a copy of the included context per inclusion, which the first build had.
   In a diamond of inclusions it yields two concepts where one is meant,
   so that a rule that composes relations of both sides is a type error.

*Impact on the specification:* `database` is a bijection; the patterns `OwnershipAndVisibility` and `ReadingAndWriting`.

*Impact in production:* one database per context.

**The tables of a context are a function of its own declarations.**
*DC-15 · decided · 2026-10-07*

In a system of contexts the compiler makes the tables per owner.
A relation is folded into the table of a concept only if both belong to the same context,
a classification that relates concepts of two contexts does not make them share a table,
and every concept gets a table.

*Considerations:*

1. The context that owns a table and a context that reads it have to agree on its layout.
   They are compiled separately, so the layout cannot depend on who reads it.
2. Since v5.9.4 a concept gets a table only if a query of its own context reads it.
   An including context may read a concept that its owner never reads.
   The option `--all-concept-tables` therefore gives every concept a table; `ampersand deploy` sets it.
3. Rejected: one table per relation and per concept everywhere.
   It is simpler, and gives up the wide tables inside a context.

*Impact on the specification:* none.

*Impact in production:* a context that others include is compiled with `--all-concept-tables`.

**A table of another context is a view, and the name of its database is filled in at deployment.**
*DC-13 · decided · 2026-10-07*

`database.sql` creates the tables of the compiled context.
For a table of another context it creates a view on that table,
with the placeholder `"{{db:<label>}}"` where the name of the database belongs.
The framework replaces it at installation, from `AMPERSAND_CONTEXT_DBNAMES`.

*Considerations:*

1. Ampersand makes no statement about version management, so the compiler cannot know the name of a database.
   The framework already takes the name of its own database from the environment.
2. With a view, every query, insert and delete that the compiler generates keeps working unchanged,
   and reads or writes the one table in the database of its owner.
3. MariaDB evaluates a query over several databases on one server, and a transaction over them is atomic.
   Databases on different servers are outside this design.
4. An unresolved placeholder is an unknown database, so a missing setting fails at installation with a message that names the context.

*Impact on the specification:* none; the specification says that a database belongs to one context.

*Impact in production:* the databases of a system share one database server.

**A classification that relates concepts of two contexts writes.**
*DC-8 · decided · 2026-10-06*

`CLASSIFY S ISA G` keeps a table for S and a table for G, each in the database of its owner.
The application of the context that states the classification stores every atom of S in the table of G as well.
Only the context that declares a concept can state a `REPRESENT` for it.

*Considerations:*

1. A migration needs to tell the type checker that a concept of the existing system and a concept of the desired system correspond: `CLASSIFY old.A ISA new.A`.
2. The application that owns S does not know of the classification.
   So the application that states it brings new atoms across before it evaluates its rules,
   as it restores any other transactional invariant.
3. Two contexts that disagree on the representation of one concept cannot both be right.

*Impact on the specification:* the rule `WritesDefinition`; the article gives the classification as a transactional invariant.

*Impact in production:* the framework restores such classifications at the start of every run of the ExecEngine.

**The compiled context contains the rules of every context it reaches.**
*DC-16 · decided · 2026-10-07*

*Considerations:*

1. A context that writes in the database of another context has to keep the invariants of that context.
   It can, because a rule has the same violations in every context that reaches it (PRF-11, `truth_is_imported`).
2. The framework evaluates a rule only when a transaction touches something it depends on,
   so the rules of a context that is only read cost nothing.
3. The initial population is that of the compiled context alone; every context installs its own.

*Impact on the specification:* none.

*Impact in production:* a signal of a rule of an included context shows up in the including application as well.

**A context relaxes an invariant of a context it includes with `ROLE … MAINTAINS`, and the compiler makes the relaxed invariant harden.**
*DC-9 · decided · 2026-10-07 · issue [#1714](https://github.com/AmpersandTarski/Ampersand/issues/1714)*

The desired system of a migration states its new invariant as an ordinary rule, `RULE totalR : I[A] |- r;r~`.
The migration context includes the desired system as `new` and writes `ROLE User MAINTAINS new.totalR`.
In the migration context the rule is then a business constraint, which signals what users have to repair.
In the desired system, where no role maintains it, the same rule is a blocking invariant.

For every invariant that a context relaxes in this way, the compiler adds to that context:
a relation `fixedTotalR` that registers what satisfies the rule,
an enforced rule `fixTotalR` that fills it,
and a blocking invariant `blockTotalR` that keeps what is registered from violating the rule again.

*Considerations:*

1. This is the method of *Data Migration under a Changing Schema in Ampersand* (RAMiCS 2024, section 4.1).
   Step 5 implements a new blocking invariant of the desired system as a business constraint.
   Steps 3 and 4 add the relation `fixed` and a blocking invariant with the violations of the rule that are registered in it.
   A violation that a user has repaired can then not return,
   and when the last violation is repaired the rule holds for all data it applied to.
2. The rule is written once, in the script of the desired system, and that script contains nothing that serves the migration.
   The migration context names the rule, and the compiler makes the additions from the rule as the type checker understands it.
   Written by hand, as the first build had them, the additions repeated the term of the rule twice,
   and nothing checked that the copies were the same rule.
3. The application of the desired system runs on one model, during the migration and after it.
   At the moment of completion nothing is compiled or installed again.
4. What satisfies the rule is, for a rule `l |- r`, the pairs in both `l` and `r`.
   The paper registers every pair that is no violation.
   For a rule over `I` that is the same; in general it would register a Cartesian product.
   The consequence: a pair that enters `l` for the first time may violate the rule once, and is then shown as work to do.
5. A script that states a rule with the name of the blocking invariant keeps its own version.
   That lets a migration engineer replace what the compiler adds, as the paper foresees, and it lets an exported script compile.
6. Rejected: a preprocessor variable that removes the rule from the desired system during the migration, with a copy of the rule in the migration context.
   The desired system had to know that it would be migrated to, the rule was written twice, and its application needed a second model at the moment of completion.
7. An invariant that the desired system states as a property of a relation, such as `[TOT]`, has no name that a `ROLE` statement can use.
   The desired system states it as a rule if a migration is to relax it.
8. Somebody still chooses by hand which invariants are new. Deriving them from the two scripts is the task of the generator of migration scripts.

*Impact on the specification:* the specification does not model roles yet, so it does not say that a rule is a business constraint in one context and an invariant in another.

*Impact in production:* the migration application has one relation more per relaxed invariant, in its own database.

## Deployment

**`ampersand deploy` generates a compose file and a Dockerfile per context.**
*DC-17 · decided · 2026-10-07*

The compose file has one MariaDB server and one service per context.
The name of a database comes from the environment, with a default of the context name and a version,
counted from 1 for contexts with the same name.
`install.sh` installs a context after every context it includes.

*Considerations:*

1. A context is compiled inside its image, together with the scripts of the contexts it reaches,
   with the options `--context` and `--all-concept-tables`.
2. A new name of a database means a new, empty database.
   So a new version runs next to the old one until its data has been migrated,
   which is the method of the 2024 paper.
3. All applications use one database user with all rights.
   Rights per context, derived from what a context reads and writes, are a next step.
4. Docker builds from the directory that contains both the scripts and the output.
   The command warns if that is another directory than the one with the scripts,
   which happens when the output directory lies outside it.

*Impact on the specification:* none.

*Impact in production:* one container per context.

## Inside the compiler

**The compiler joins the compiled context and the contexts it reaches into one context.**
*DC-18 · decided · 2026-10-07*

A thing of another context gets one prefix: the label that the compiled context has for that context,
which is the alias, or else the name.
The joined context carries, as `META` data, the label, the name and the file of every other context,
and how those contexts call one another.

*Considerations:*

1. The type checker, the normaliser and the SQL generator keep working on one context.
   This is the flattening theorem of the article (PRF-11, `flattening`).
2. A context that is reached along two paths gets one label, so its things are joined once.
3. The metadata lets the table generator know the owner of every thing without a change in the data structures of the compiler,
   and `ampersand export` shows it.

*Impact on the specification:* none.

*Impact in production:* none.

**The printer writes the name space of a relation and of a view,
unless it is a name space of Ampersand itself.**
*DC-10 · decided · 2026-10-07*

*Considerations:*

1. The printer dropped the name space of a relation and of a view reference,
   so an exported script with such names did not parse back to the same model.
2. The compiler prints terms into the files it generates.
   Every prototype contains rules of `PrototypeContext`,
   so printing that name space would change those texts in every generated prototype.

*Impact on the specification:* none.

*Impact in production:* none for scripts without name spaces of their own.

## Still to decide

1. `CONTEXT` or `MODULE` as the unit that gets a name space
   (the core team chose `MODULE` on 24 February 2023).
2. How an application notices a change that another application made in a database it reads.
   Rules are evaluated on the current data, and the stored signals of a rule over another database are refreshed when the application next evaluates that rule.
3. Which rights the database user of an application has on the databases of other contexts.
   [database-rights.md](database-rights.md) reports what was measured and compares five solutions,
   from one user with all rights to rights per table and column.
   Databases on different servers belong here too.
4. Whether an including context writes a database directly, as it does now, or through the API of its owner.
5. Error positions: the checks of DC-3, DC-4 and DC-8 report the first line of the context,
   because a name in the parse tree carries no position.
6. A warning for a context name that is covered by an alias and never used.
