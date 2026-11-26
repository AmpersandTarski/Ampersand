# Deploying a changed specification while the data stays: plan

Tracking issue: AmpersandTarski/Ampersand #1393, "Generate a data migration script",
the first issue of the migration epic of 2023.
The specification of this work is *Data Migration under a Changing Schema in Ampersand*
(Joosten and Joosten, RAMiCS 2024), called "the article" below.
Stand of 26 November 2026, compiler v5.15.0, prototype framework v2.25.0.

## Goal

Stef asked for this plan on 26 November 2026: "Ik wil het migratie-artikel implementeren,
zodat we voortaan gemakkelijk wijzigingen in een Ampersand specificatie kunnen deployen in productie zonder zorgen over het behoud van de bestaande gegevens."

A team that changes an Ampersand script then deploys the change in production,
while the data that users entered stays and the users keep working.
The plan is done when the following holds for a system in production.

- One command derives the migration script from the script in production and the changed script.
- Before the migration starts, the team reads which data comes along and which stays behind.
- The migration passes the three moments of the article with files
  and conditions that are generated.
- The next change is deployed in the same way, from the system that the previous migration left.

## Where things stand

The article has a migration pass three moments: the pre-deployment, the moment of transition,
at which the users move to the migration system, and the moment of completion,
at which they move to the desired system.
The table follows one release through those moments.

| Moment | What exists | What is still to come |
| --- | --- | --- |
| Before the migration | The language for a migration context: `INCLUDES`, `AS`, `NAMED` (v5.10.0 to v5.12.0). Three migration scripts written by hand in `demos/migration`, which follow steps 2 to 5 of the article. | The generator of the script (#1393). Reading the database of a system that was compiled by itself. A report of the data that stays behind. |
| Pre-deployment | `ampersand deploy` generates a compose file, a Dockerfile per context and an install script, for a system that starts from nothing. | Deploying the desired system and the migration system next to a system that is already running. A trigger that lets the migration system catch up with the existing system. Database rights that keep the migration system from writing the existing database (#1719). |
| Moment of transition | The director of the demonstration (`demos/migration/regie`) turns an nginx and compares checksums of the existing database. The screens of the desired system are written out by hand in the migration script. | The condition "copying is done", shown by the migration system itself. The screens of the desired system in the migration system, which come along with the inclusion once issue #1755 is built. An existing system that changes no more. |
| Moment of completion | The director counts violations in the database of the migration system. PRF-11 has the theorem `moment_of_completion`; PRF-14 is stated. | The condition as a signal for a role. A cleanup with a point to return to. A desired system from which the next migration can start. |

Two mechanisms exist for steps 3 and 4 of the article,
the relation `fixed` and the blocking invariant.
The three demonstration scripts write them out,
and the compiler adds them when a script leaves them out
(`hardenRelaxedInvariants` in `Ampersand.FSpec.ToFSpec.CreateFspec`).
The notes of the session of 16 October 2026 record that Stef wants the pattern in the migration script,
which is meant to be generated;
choice 10 below proposes what follows from that.
A third mechanism, `hardens` in the framework, is not on `origin/main` of the framework.

## A measurement that orders the plan

A migration context reads the database of the existing system through views on its tables.
On 26 November 2026 I compiled the existing system of two demonstrations in two ways: by itself,
as every system in production is compiled,
and as the migration context expects it (`--context old --all-concept-tables`, which `ampersand deploy` sets).

| Example | Tables when compiled by itself | Tables that the migration context expects |
| --- | --- | --- |
| `garden` | `Plot`, `SESSION` | `Plot`, `PlotName`, `Gardener`, `SESSION`, `ONE` |
| `kurk` | `A`, `SESSION` | `A`, `B`, `SESSION`, `ONE` |

The table `Plot` has the same columns in both compilations.
The migration context creates the views `old.PlotName`
and `old.Gardener` on tables that the system in production does not have.
That much is measured.
What I expect, and have not observed, is that the installation of the migration system then fails,
so that a migration context cannot read a system that runs in production today.
Phase 1 observes it, by installing a migration system against a database of a system compiled by itself.
The demonstrations do not show this, because `ampersand deploy` builds their existing system too.
The script of the measurement is `layout.sh`, next to this plan.
The compiler of the measurement was built from a local trial branch with switches in `SQL.hs`,
so the baseline of phase 1 repeats the measurement with a compiler built from `main`.

This is why reading a production database comes before the generator in the phases below.

## The issues this plan builds on

The migration was laid out as an epic in 2023,
before the article was finished and before a context could include another one.
Systems of contexts (v5.10.0) have since delivered the name spaces
that issue #1393 was waiting for (#1307, #1509).
The table places every issue on the subject in this plan.

| Issue | What it asks | Place in this plan |
| --- | --- | --- |
| #1393 Generate a data migration script | Compile the new system together with the existing one, and generate the migration script. | Phase 3. This plan is its elaboration. |
| #1401 Retrieve schema (compiler), prototype #111 Retrieve schema (back-end) | A system gives its schema, from its source and while it runs. | Phase 2: the migration takes what it knows of the existing system from that system, and the stored source of a deployment is that schema. |
| #1406 Isolate the schema | The schema as a data structure in the compiler. | Phase 3: the generator is a function of two schemas, as in the article. |
| #1405 Check whether a given deployment implements a given schema | Know whether a running system implements a schema. | Phase 2, the check. |
| #1402 Implement Cork rules, #1379 Switching between invariance and process rule | A rule that signals its existing violations and blocks new ones, until it works as an invariant. | The cork is what steps 3 to 5 of the article do in the script; choice 10 says where they live. Switching a rule at run time, which both issues ask as well, is outside the article and outside this plan. |
| #1404 Generate the cleanup service, prototype #112 Migration Orchestrator Service | Start the migration system, and remove it and the existing system afterwards. | Phase 4, and choice 11. |
| #1430 Make the Ampersand compiler read a dataset | Read the triples of an existing database, to fill the migrated one. | Since v5.10.0 the migration context reads the existing database itself, and step 2 copies. Proposal: narrow the issue to the other sources it names, such as spreadsheets. |
| #1429 Mapping named things | A mapping of names in an inclusion. | Choice 6, a renamed relation. |
| #1755 An application should offer the interfaces of every context it includes | Decided on 1 November 2026, and not built yet. | Choice 4: the screens of the desired system come along with the inclusion. |
| #1714 A migration should not need a preprocessor directive | Solved in v5.10.0 with `ROLE … MAINTAINS`. | Closes in phase 1. |
| #1720 Relaxing a property of a relation, such as `[TOT]` | A property has no name to relax it by. | Choice 1. |
| #1719 Every application can write every database on the server | Rights per context. | Phase 4. |
| #1722 `MultiContext.adl` is not part of FormalAmpersand | The specification that the compiler is built against. | The proof obligations: the specification of the generator belongs next to it. |
| prototype #495 and #494 | A signal over the data of another context; a deletion inside a transaction. | Phase 4. |

## Phases

### 1. Baseline

The demonstrations were last recorded on 18 October 2026: `test.sh` gave 45 of 45 for `kurk`,
51 of 51 for `garden` and 51 of 51 for `tuin`, on framework v2.18.0 with compiler v5.11.0.
Phase 1 runs the three again on the released images of today, v5.15.0 and v2.25.0,
so that every later phase can show that it left the demonstration as it was.
It repeats the measurement of the tables on `main`,
and it installs a migration system against a database of a system compiled by itself,
to observe what the measurement predicts.
Once the pilot has moved to today's compiler (see phase 2),
it compiles the script of the pilot (see phase 5) included twice, as `old` and as `new`,
because nobody has compiled a script of that size as an included context.
It states the proof obligations below in the register.
Issue #1714 is solved since v5.10.0 and is still open; it closes here,
with a reference to the release.

The cost is three runs of `test.sh`, which took about five minutes each in October,
two compilations and one installation.
What the compilation of the pilot brings is unknown; a failure there is a finding of its own.
The phase is done when the three counts are recorded with the versions,
the installation and the compilation of the pilot are reported, and #1393 links to this plan.

### 2. Reading a production database as it is

The measurement above is the observation.
The layout of a database also depends on the compiler that installed it:
v5.9.4 dropped concept tables that no query reads, v5.11.0 added `MULTITABLE`,
and v5.12.0 lays tables out per database.
So a migration context reads a production database
only if its compiler lays the script out as that database is laid out.

Stef decided on 26 November 2026 where that layout comes from.
The schema of a system is its Ampersand source,
and the layout is what the compiler of that deployment makes of that source.
So every running container holds the source it was built from,
together with the version of the compiler and the options of the build,
so that compiling that source again with that compiler reconstructs what the deployment has.

Part of this exists.
The generated file `settings.json` records the version of the compiler, its options and a hash of the model.
The generated file `relations.json` names the table and the columns of every relation,
and `concepts.json` those of every concept.
The image of the pilot holds its source, because its Dockerfile copies the project into it.
A Dockerfile decides that today, so another project may not have it.
I propose that the compiler writes the source files it read next to the files it generates,
so that every deployment has them whatever its Dockerfile does.

The article does not consider two compilers, and neither does this plan.
Today's compiler compiles the migration context, on the assumption
that it lays out the script of the existing system as the compiler of the deployment did.
Stef decided that on the same day: the plan takes no measure for the case in which the assumption fails,
and only detects that case.
A first check compiles the stored source twice, with the stored compiler and with today's,
and compares the generated files.
A difference is solved before the migration starts, in the way the project has for a new compiler.

The pilot is such a case, measured on 26 November 2026 with `memorybank/migration/twocompilers.sh`.
Compiler v5.11.0 built its deployment, in framework v2.18.1.
Between v5.11.0 and v5.15.0 ten commits touched the files of the table layout,
the last on 30 October 2026.
Compiler v5.11.0 compiles the script of the pilot, and v5.15.0 refuses it with 13 errors.
All 13 follow from issue #1727, in the compiler since v5.13.0:
a concept of integers or dates has no table, and a term cannot ask for all its atoms.
Six are rules of the script,
and seven are properties such as `[UNI]` and `[INJ]` of a relation on such a concept.
So the pilot first moves to today's compiler, with the export, reinstallation and import it uses today,
and its first migration by the method starts from that deployment.

Four directions give the migration context the layout of the existing system.

| Direction | What it asks | What it costs |
| --- | --- | --- |
| A. The compiler computes for the included context the layout that a compilation by itself gives. Where a concept has no table, the view reads its atoms from the columns that hold them. | A change in `ADL2Plug` and in the generation of views. Nothing in production changes before the migration. | The compiler computes two layouts in one compilation; unknown until tried. It gives the layout of today's compiler, so it serves only a database that today's compiler would lay out the same way, and the check below has to refuse the others. |
| D. The compiler takes the layout of the included context from the files that the compiler of the deployment generates from the stored source. | A way to hand those files to the compilation of the migration context, and views that follow them. Nothing in production changes before the migration. | A new input of the compiler; unknown until tried. It serves a database that an older compiler laid out. Issue #1401 and prototype issue #111 ask the same for the schema. |
| B. The production database gets the tables it lacks, once, and its application is rebuilt with `--all-concept-tables`. | A script per project, and a rebuild whose model hash differs from the one the database records. | A change in production before the first migration, which is what the method avoids. |
| C. Every concept gets a table in every prototype. | Reverting the choice of issue #1672. | More tables in every system, and the systems that are in production already still need B. |

A and D both leave the existing system as it is, which the article assumes,
and which the guide promises: being included changes nothing for the included context.
A follows from the decision above: it is the direction in which today's compiler computes the layout.
It holds for the desired system as well, which runs by itself after the completion.
D serves a deployment that another compiler built, and the plan takes no measure for that case.
B and C change the system in production before the migration, which the method avoids.
A changes design choice DC-15.

A second check is what issue #1405 asked for in 2023:
a command that compares the tables
and columns that a script expects with those of a running database,
and reports every difference before anything is deployed.
The Artefactenkaart has such a check as a script of its own, which this phase reads first.

The trial is an experiment that tries to refute the claim.
The claim, registered first: for every relation of the script in production,
the migration context reads the same pairs as the export of the production application gives.
The experiment runs on a copy of the production database of the pilot (see phase 5).
The copy is made by the session of that project, under the rules that project has for its data.
The phase is done when the claim survives on that copy and the check reports no difference.
It touches the table layout (`ADL2Plug`),
the generation of views (`FSpec2SQL`, `TableSpec`) and a new command.
How much work that is, the trial on the demonstrations has to show.

Building the generator first is rejected.
It would be tested on demonstrations only,
and its first use in production would fail at the installation of the migration system.

### 3. The generator

Today somebody writes the migration script by hand and chooses by hand
which invariants are new (DC-9, consideration 8).
The article defines the script as a function of the two schemas (section 4.1, steps 1 to 6),
and it ends with:
"our next step is to automate the generation of migration scripts as an extension to Ampersand."

The migration context includes the existing and the desired system:
`CONTEXT Migration INCLUDES … AS old, … AS new`.
The compiler then does step 1 of the article.
It joins the three contexts into one (`flattenSystem` in `Ampersand.Input.Qualify`).
In that context everything of the existing system has the prefix `old.`,
and everything of the desired system the prefix `new.`.
These are the two arrows with which the article relabels the two systems in section 4.1.
Stef pointed this out on 26 November 2026: the compiler does that work,
so the generator does not repeat it.

So the generator is a command of the compiler
that starts from a migration context with the inclusion statement alone.
It lets the compiler join the contexts, and it compares within the joined context:
a relation `old.r` with `new.r`, a rule `old.u` with `new.u`.
It writes what steps 2 to 5 add, as source text, together with a report.
The article asks for source text,
so that a migration engineer can replace a business constraint with a rule of her own.

| Step of the article | What the generator writes |
| --- | --- |
| 1, the disjoint union | Nothing but the inclusion statement; the compiler joins the contexts. One addition: `CLASSIFY old.C ISA new.C` for every concept that both systems have, because the compiler gives a concept the prefix of its context, where the article leaves concepts without one. |
| 2, copying | For every relation that both scripts have: a relation that registers what has been copied, and two `ENFORCE` rules. |
| 3 and 4, a repaired violation stays repaired | For every new blocking invariant: the relation `fixed`, the `ENFORCE` rule that fills it, and the blocking invariant. |
| 5, work for users | `ROLE … MAINTAINS` for every new blocking invariant. |
| Beyond the article | The two conditions of phase 4, as rules. The screens of the desired system come along with the inclusion (#1755). |

The report lists what a reader has to know before the migration starts.
It names the relations of the existing system that the desired system lacks,
whose pairs stay behind.
It names the relations that start empty, and the invariants that are treated as new.
And it names everything the generator could not decide,
with the place in the script where the engineer decides it.
That report is what "without worries" rests on: nothing stays behind unannounced.

With the generator in place, steps 3 and 4 can have one home, the generated script.
Choice 10 below proposes that, because it changes a decided design choice and released behaviour.

The generator is a new module next to `Ampersand.Commands.Deploy`.
It reuses the joining of contexts, the printer of scripts,
and the logic of `hardenRelaxedInvariants` for steps 3 and 4.
The demonstrations `garden` and `tuin` have screens,
so issue #1755 is built before this phase ends, or their screens stay written by hand until it is.
Choices 1, 2 and 8 below are the unknown part of the work:
each needs a design before it can be built.

The phase is done when three things hold.
The generator reproduces the three demonstration scripts closely enough
for `test.sh` to give the counts of the baseline.
A property in `stack test` generates arbitrary contexts
and states that migrating a script to itself yields a script that type checks and relaxes no rule;
a mutation of the generator makes it fail.
The guide `docs/guides/systems-of-contexts.md`,
the command reference and the release notes describe the command.

No change of the syntax is foreseen.
If a choice below brings one, the parser,
the printer and the generator of parse trees change together (issue #1736).

A compiler that builds the migration system without a script is rejected.
The engineer could then not tailor it, and the three demonstrations show that tailoring happens,
in the messages that users read.

### 4. The three moments in production

The director of the demonstration plays the migration engineer and the ingress.
It starts containers through Docker,
compares checksums of the existing database to see whether the copying is done,
and reads the violation cache of the migration system with SQL to see whether the migration can be completed.
A team in production has its own deployment street,
so each of these has to become something every project can use.

| What | Proposal |
| --- | --- |
| Deploying next to a running system | `ampersand deploy` generates the desired system and the migration system only, attached to the database server that the existing system uses. All databases of a system share one server (DC-13), which is a condition for a project. |
| Catching up | The framework re-evaluates rules over the data of another context on a trigger, which is the open choice of prototype issue #495. That is a facility for systems of contexts in general, so the framework learns nothing about migration. |
| "Copying is done" and "ready for completion" | Two kinds of rules in the generated script, assigned to the role of the migration engineer: one that signals a pair of the existing system that is not registered as copied, and the relaxed rules themselves. An empty list of signals is the condition. |
| Rights | The database user of the migration system reads the existing database and writes the other two (#1719). |
| The existing system after the transition | It receives no traffic and its database user loses the right to write, because the article requires that it changes no more. |
| Returning | Until the transition, removing the two new systems restores the old situation. After it, the changes of users are in the desired database, so the transition is the point of no return; the cleanup at completion makes a dump first (#1404). |
| The ingress | It stays with the project: Caddy, nginx, or the ingress of Kubernetes. The generated files name the two switches and the condition for each. |

The phase is done when the demonstration runs on generated files only,
the director works nothing but Docker and the proxy
and reads both conditions through the API of the migration system,
and `test.sh` gives the counts of the baseline.

The phase touches `Ampersand.Commands.Deploy`, the generator of phase 3,
the director of the demonstration, and the framework, through the two issues named in the table.
The work in the framework is the unknown part:
issue #495 has a diagnosis and no chosen solution yet.

Prototype issue #112 of 2023 asks for an orchestrator as a service of the back-end.
Choice 11 has ExecEngine rules in the migration system act when the condition of completion is reached.
The table above predates that choice and follows it once the questions it raises are answered.

### 5. A trial migration, and the second one

Stef chose the pilot on 26 November 2026.
It is an application that runs in production with data that volunteers entered by hand,
and whose model changes every few weeks.
It keeps its data today by exporting the population, reinstalling and importing.
That gives a comparison, and a way back if the trial disappoints.
Its repository is private, so this plan does not name it.
The work in that repository is done by a session there, from a task file that this plan delivers.
Phases 1 and 2 already need the pilot, for its script and for a copy of its data.
The cost of this phase lies in that project: four migrations,
and for each the time users take to repair violations.
The duration of the copying at production size is unknown, and step 1 measures it.

The trial has four steps, each on a copy of the production data until the last.

1. A migration of the script to itself.
   The claim: every relation has the same pairs before and after.
   The duration of the copying is measured at production size, with the claim registered first,
   because the copying runs through the ExecEngine and nobody has measured it beyond five plots.
2. A change of the model that the project has made before, replayed.
3. The next real change, in production.
4. The change after that, from the system that step 3 left.

Step 4 is what "voortaan" means: the cycle closes,
and the names of the databases and the layout allow a next migration.
The phase is done when step 4 has run and the guide describes the cycle for a project.

## Choices that the article leaves open

The article is the specification, so what it does not contain is a choice and not a step.
Stef went through the table on 26 November 2026 and rewrote rows 1, 2, 4, 5 and 11.
The choices are recorded in `memorybank/multi-context/DesignChoices.md`.

| # | What the article leaves open | Choice | Phase |
| --- | --- | --- | --- |
| 1 | A new invariant that is a property of a relation, such as `[TOT]`, has no name that `ROLE … MAINTAINS` can use (#1720). | The generator treats it as a new invariant. The script names the rule of a property by prefixing the property to the relation name and making it unique. | 3 |
| 2 | A new `[UNI]` or `[INJ]`. The desired system stores such a relation as a column, which holds one value, so the pairs that violate it cannot be stored there. The article does not address this situation. | The desired system has a data model with the one column, but the migration system must allow for multiple atoms as long as violations of the new rule exist. So, the migration system has relation of its own to stand in for the (too narrow) attribute in the desired system. Every time a violation is mended, the attribute of the desired system is used to store the remaining atom, so the migration system can safely be deleted after all violations are gone. The others are work in the migration system, shown from `old.r`, and a user chooses. | 3 |
| 3 | When a rule of the desired system is "the same" as one of the existing system. | The same name and the same term after renaming. Anything else is new. | 3 |
| 4 | The screens during the migration come along with the inclusion: Stef decided on 1 November 2026 this in issue#1755. This would imply that the interfaces of the old and the new system are both given to the user. This is wrong however, because the Migration article wants to see the interfaces of the new system, so the migration system mimics the new system as much as possible.  |This is resolved by renaming the roles of the new system (e.g. ROLE "new.Manager AS Manager) and exposing the renamed roles during the migration phase. This way, the user doesn't notice changed role names when the new system takes over. | 3 |
| 5 | The role that maintains the relaxed rules. | See 4. | 3 |
| 6 | A relation that was renamed, or whose signature changed. | The report lists it as staying behind, and the engineer writes the copy rule in the generated script. Issue #1429 proposes a mapping of names in the inclusion statement, which would let the generator do it. | 3 |
| 7 | A concept whose representation changes. | The report lists it; no classification is generated for it (DC-8, consideration 3). | 3 |
| 8 | A pair that is deleted in the existing system after it was copied. The article names this as a threat to data quality: a person who was removed would reappear. | A rule in the generated script that follows the deletion until the transition. Its design needs care, because a user may add the same pair again after the transition. | 3 |
| 9 | A relation on `SESSION` in an included context gets a table of its own, and the installation fails. | An issue for the compiler; until then the report names such relations. | 3 |
| 10 | Where steps 3 and 4 live. Design choice DC-9, decided on 7 October 2026 and released in v5.11.0, has the compiler add them when a script leaves them out. | The generated script writes them out, as the three demonstrations do. The compiler first warns when it adds them itself, and a later release stops adding them; a script that relies on the addition then needs three lines written out, which the warning shows. This changes DC-9. Issue #1402 proposed a third home in 2023, the violation cache of the framework; that was tried on a branch in October 2026 and not released. | 3 |
| 11 | Who conducts the three moments. Prototype issue #112 asks for an orchestrator as a service of the back-end. | These are Exec-Engine rules in the Migration system that fire once the condition of completion is reached. | 4 |

## Work per repository

A change lands in the repository it changes, made by a session in that repository.

| Repository | Work | How it gets there |
| --- | --- | --- |
| Ampersand | Phases 1 to 3, and the generated files of phase 4. | Branches from `origin/main`, a pull request each, with a release note. |
| Prototype framework | The trigger for rules over another context (#495), rights per context, a deletion inside a transaction (#494). | Issues there; #494 and #495 exist. |
| The pilot project | Moving to today's compiler before its first migration (phase 2), and phase 5. | A task file for its session, and an issue in its repository. |
| Ampersand, `demos/migration` | The director reduced to Docker and the proxy. | With phase 4. |

The branch `incremental-evaluation` is not involved.

## Proof obligations

The generator writes rules whose meaning the migration rests on,
so its claims are stated before it is built.
The register ends at PRF-22 today;
a new claim takes the next free number after a check of the git history.

- PRF-14, a repaired violation of a relaxed invariant does not return, is stated.
  It gets a proof in Lean.
- New: when the copying has ended and no user deleted in the desired system,
  every pair of a relation that both systems have is in the desired system.
- New: at the moment of transition the migration system violates none of its blocking invariants,
  given that the existing system satisfies its own.
  This is the second point of the validation in the article.
- New: at the moment of completion, a relation that only the migration system has
  (a register of copying, `fixed`, a stand-in of choice 2) holds nothing that the desired system needs,
  so the migration system can be removed.
- New: the script that the generator writes is the schema of the migration system in the article
  (its equation for the migration schema).
  Property tests bind this claim to the code.

PRF-11 already has `moment_of_completion` in an abstract model;
I read the statement in the source and did not run the build.
The Isabelle theory with the article (`Migration.thy` in the Publications repository)
has lemmas on the well-typedness of a disjoint union of typed graphs;
I read their names and did not read their proofs.

## Not in this plan

- Databases on different servers.
- A migration that changes the identity of atoms.
- The incremental evaluation.
- Kubernetes: the generated files of phase 4 are for Docker compose, as `ampersand deploy` is today.
