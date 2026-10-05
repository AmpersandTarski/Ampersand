# Design choices — multi-context Ampersand

Register of design choices for including a context in another context (issue [#1509](https://github.com/AmpersandTarski/Ampersand/issues/1509), phase 2 of [#1307](https://github.com/AmpersandTarski/Ampersand/issues/1307)).
Numbers are stable and never reused; the current state stands here, the history lives in git.
The plan of approach is [plan.md](plan.md);
its decisions D1 to D7 are the questions that these choices answer.

The choices below were made on 5 October 2026 by the AI agent that built the first phases,
while Stef Joosten was unavailable.
Each one is the option that the plan recommends, or a deviation that the build made necessary,
and each is marked *provisional* until Stef and the core team confirm it.
Open questions sit at the bottom under "Still to decide".

## Syntax and names

**A plain `INCLUDE` keeps its meaning;
`INCLUDE "file" AS alias` includes the file as a context of its own.**
*DC-1 · provisional · 2026-10-05 · origin: plan D1*

A plain `INCLUDE` adds the declarations of the file to the including context, as it always did.
With an alias, every name that the included file declares is available under the alias as a prefix,
and under that name only.

*Considerations:*

1. Every existing script and every regression case relies on the union.
   This choice changes none of them:
   the generated backend files of all 204 scripts in `testing/Travis/testcases` that the compiler accepts are byte-identical before and after, the model hash included (measured with `memorybank/tools/compare_compiler_output.sh`; the other 17 scripts are refused by both builds in the same way).
2. The brief of 5 October 2026 and the article give every `INCLUDE` the new meaning,
   with a keyword `QUALIFIED` to ask for prefixes only.
   That option would change the meaning of every script in
   which two files declare the same relation; how many scripts that touches has not been measured.
3. The literature study found that Haskell users ask for prefixes as the default,
   and that Haskell cannot change its default any more.
   With this choice the new form always has a prefix, so the question of the default does not arise.
4. A separate keyword for the new form was rejected:
   it adds a keyword and gives nothing that `AS` does not give.

*Impact on the specification:* one new form of the `INCLUDE` statement.
The article needs a revision of its section on syntax; its definitions and theorems are unaffected.

*Impact in production:* none for existing scripts.

**`AS` is recognised by its position and is no keyword.**
*DC-2 · provisional · 2026-10-05 · origin: build*

The parser reads `AS` as an identifier that directly follows the file name of an `INCLUDE` statement.

*Considerations:*

1. A new keyword would invalidate every script that uses `AS` as the name of a concept.
   The regression case `as_is_no_keyword.adl` does so.
2. No context element starts with an identifier in upper case, so the position is unambiguous.

*Impact on the specification:* none beyond DC-1.

*Impact in production:* none.

**An alias stands for one file within a context, differs from the name of that context,
and is no name space of Ampersand itself.**
*DC-3 · provisional · 2026-10-05 · origin: article, definition of a multi-context system*

The compiler reports the three cases as errors.

*Considerations:*

1. The first two are the conditions under
   which a qualified name denotes exactly one thing (proof-track claim PRF-11, theorem `qualified_names_suffice`).
2. The third one keeps the renaming injective:
   the names in `PrototypeContext` and `FormalAmpersand` get no prefix,
   so an alias with that name would let a renamed name coincide with one of them.
   The property suite found this.

*Impact on the specification:* three error messages.

*Impact in production:* none.

**A concept name that starts with an alias has to exist in the included context.**
*DC-4 · provisional · 2026-10-05 · origin: build*

*Considerations:*

1. Ampersand declares a concept by using it.
   Without this check, `Reg.Persn` would silently become a new concept.
2. A misspelled relation needs no check of its own,
   since the type checker reports an undeclared relation.

*Impact on the specification:* one error message.

*Impact in production:* none.

**A name can be reached through two aliases, as in `Reg.Geo.Town`.**
*DC-5 · provisional · 2026-10-05 · origin: build; deviates from the article*

If `registry.adl` includes `towns.adl` as `Geo`,
a script that includes `registry.adl` as `Reg` can write `Reg.Geo.Town`.

*Considerations:*

1. The article follows Haskell and gives the things of the inner context no name in the outer one.
2. With DC-6, the inner context is a private part of the included context,
   so naming it reveals nothing that the outer context could not already see.
3. Stef expected this form in 2023 (`old.gnu.foo`, comment on issue #1307),
   and the literature study found that Haskell users ask for it.
4. Forbidding it would need a rule to tell an alias from a name space that a script chose itself,
   which the syntax cannot tell apart.

*Impact on the specification:* the article says that names are not transitive;
the compiler allows it.

*Impact in production:* none.

## Instances and storage

**Every alias stands for a copy of the included context,
which the including application stores in its own database.**
*DC-6 · provisional · 2026-10-05 · origin: plan D3, D4 and D5*

Including one file under two aliases gives two copies with separate populations.
The compiler renames the included context and merges it into the including one,
so that the rest of the pipeline compiles a single context.

*Considerations:*

1. The plan distinguishes an instance that is private to the including application from an instance that several applications share.
   A private instance is touched by one application only,
   so storing it in the database of that application cannot be observed from outside.
2. This is the flattening theorem of the article put to work:
   a context with everything it reaches is one information system.
3. It needs no change in the prototype framework and no new deployment files,
   and it covers the migration case,
   in which the migration system serves the existing and the desired system.
4. A shared instance, such as one register for several applications, needs a database of its own,
   a deployment descriptor and generated grants.
   That is the next phase of the plan.
5. A context cannot include itself under an alias, directly or indirectly:
   it would have to contain a copy of itself.
   A cycle becomes possible with shared instances.

*Impact on the specification:* none; a script does not say where a context is stored.

*Impact in production:* one database per application, as today.

**Roles, the concept SESSION and the name spaces of Ampersand itself get no prefix.**
*DC-7 · provisional · 2026-10-05 · origin: build*

*Considerations:*

1. A role is a function of a person in an organisation.
   The clerk of the permit system is the same clerk when a rule of the register asks for one.
2. SESSION and `PrototypeContext` belong to the application that serves the user,
   of which there is one.

*Impact on the specification:* an included context that assigns a rule to a role assigns it to the role with that name in the including application.

*Impact in production:* none.

**Only the context that declares a concept can state a `REPRESENT` for it;
a `CLASSIFY` can relate concepts of different contexts.**
*DC-8 · provisional · 2026-10-05 · origin: plan D6; deviates from it for `CLASSIFY`*

*Considerations:*

1. The plan recommends a strict start: `CLASSIFY` within one owner, `REPRESENT`,
   `VIEW` and `IDENT` by the owner, no `POPULATION` across contexts.
2. The migration case cannot be written that way.
   The concepts `old.A` and `new.A` are different concepts,
   and the copy rule `new.r >: old.r - copyR` type-checks only if the script says how they correspond.
   `CLASSIFY old.A ISA new.A` says it.
3. The article carries instances across by a rule that enforces a concept.
   Ampersand has no such rule, and a classification does what that rule would do.
4. With DC-6, the including application is the only one that touches the included context,
   so `POPULATION`, `VIEW` and `IDENT` for its things do no harm.
   They have to be reconsidered for shared instances.
5. `REPRESENT` stays with the owner,
   because two contexts that disagree on the representation of one concept cannot both be right.

*Impact on the specification:* one error message, for a `REPRESENT` of a concept with an alias.

*Impact in production:* none.

**A relaxed version of an included context is selected with a preprocessor variable.**
*DC-9 · provisional · 2026-10-05 · origin: build, migration case*

The migration context includes the desired system as `INCLUDE "desired.adl" AS new --# [ "Migrating" ]`, and the desired system guards its new blocking invariant with `--#IFNOT Migrating`.

*Considerations:*

1. During a migration the desired system is deployed while its new invariants do not hold yet.
   The article calls such a context not guarded.
2. The preprocessor exists and already passes variables through an `INCLUDE` statement,
   so this needs no new language construct.
3. It asks the desired system to mark its new invariants.
   A generator of migration scripts, the stated next step of the 2024 paper, can do that marking.

*Impact on the specification:* none.

*Impact in production:* none.

## Printing

**The printer writes the name space of a relation and of a view,
unless it is a name space of Ampersand itself.**
*DC-10 · provisional · 2026-10-05 · origin: build*

*Considerations:*

1. The printer dropped the name space of a relation and of a view reference,
   so an exported script with such names did not parse back to the same model.
2. The export of a script with an aliased include is the flattened single context,
   which serves as the oracle for the meaning of the include.
   It has to be a script that the compiler accepts.
3. The compiler prints terms into the files it generates: as comments in the SQL,
   as normalisation steps in `interfaces.json` and as formal expressions in `rules.json`.
   Every prototype contains rules of `PrototypeContext`,
   so printing that name space would change those texts in every generated prototype.
   The printer therefore leaves the name spaces `PrototypeContext` and `FormalAmpersand` out,
   as it always did.
   With this exception, the generated files of the existing regression scripts are byte-identical before and after (see DC-1).
4. The exception is an inconsistency:
   an exported script that contains relations of `PrototypeContext` still loses their name space.
   It existed before this change.

*Impact on the specification:* none.

*Impact in production:* none for scripts without name spaces of their own.

## Still to decide

1. DC-1 against the brief: should a plain `INCLUDE` become the include relation after all?
   (plan D1)
2. `CONTEXT` or `MODULE` as the unit that gets a name space.
   (plan D2; the core team chose `MODULE` on 24 February 2023)
3. Shared instances: the deployment descriptor, a database per instance, generated grants,
   and the command `ampersand deploy`.
   (plan D3, D4, D5 and phases P4 to P6)
4. How an application notices a change in a shared instance.
   (plan D7)
5. Whether an including context writes a shared instance directly or through its interfaces.
   (literature study, question 6)
6. Error positions: the checks of DC-4 and DC-8 report the first line of the file,
   because a name in the parse tree carries no position.
