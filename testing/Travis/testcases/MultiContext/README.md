# Regression cases for systems of contexts

These cases test the statement `CONTEXT A INCLUDES B` (see [Systems of contexts](../../../../docs/reference-material/syntax-of-ampersand.md#systems-of-contexts)).
If a context includes another context, everything that the included context declares is available in the including one,
under the name of the included context or its alias as a prefix.

## Layout

| Directory | What it holds | What the suite does with it |
| --- | --- | --- |
| `contexts/` | The contexts that the cases include. | Nothing: the directory has no `testinfo.yaml`. |
| `shouldSucceed/` | Scripts that are correct. | `ampersand check` (exit 0) and `ampersand validate` (exit 0), which compares the generated SQL with the compiler's own evaluation in a real database. |
| `shouldFail/` | Scripts with a mistake in an inclusion or in the use of a prefix. | `ampersand check` has to refuse each of them (exit 10). |
| `invariantViolations/` | Scripts whose population violates an invariant of an included context. | `ampersand check` has to refuse each of them (exit 10). |

## What each case guards

| Case | What it guards |
| --- | --- |
| `shouldSucceed/permits.adl` | The example of the documentation: two contexts with a relation `address` and a concept `Address` that mean different things, and a rule of the including context over data of the included one. |
| `shouldSucceed/diamond.adl` | Two included contexts include the same context. It remains one context, so a rule can compose relations of both sides. |
| `shouldSucceed/one_context_two_names.adl` | One context under two aliases is one context: the alias and the name are references to the same things. |
| `shouldSucceed/chain.adl` | A chain of three contexts; the first one includes the third one itself, to refer to its things. |
| `shouldSucceed/visible_without_inclusion.adl` | A concept of a context two steps away is visible as the type of a relation, without an inclusion. |
| `shouldSucceed/system_in_one_file.adl` | Several contexts, two fragments of one context and the inclusions between them, in one file; the comma form of the statement. |
| `shouldSucceed/graph_in_main_file.adl` | The main file states an inclusion of another context, which the file of that context does not state. |
| `shouldSucceed/plain_and_alias.adl` | `INCLUDE` and `INCLUDES` side by side: the text of a file, and the context in that file. |
| `shouldSucceed/alias_of_two_files.adl` | The included context consists of two files that `INCLUDE` joins. |
| `shouldSucceed/every_kind_of_name.adl` | Every kind of name gets the prefix: pattern, rule, enforcement, identity, view, classification, representation, purpose. |
| `shouldSucceed/as_is_no_keyword.adl` | `INCLUDES`, `FROM` and `AS` remain ordinary identifiers outside the statement. |
| `shouldSucceed/alias_with_variables.adl` | Preprocessor variables after the file name, for the included file `contexts/with_variable.adl`. |
| `shouldSucceed/kurk_desired_alone.adl` | The desired system of the migration case compiles by itself, with its blocking invariant. |
| `shouldSucceed/kurk_migration.adl` | The migration system of the RAMiCS 2024 paper as a context that includes two versions of the context Kurk, as `old` and `new`. |
| `shouldSucceed/relaxed_invariants.adl` | A context relaxes two invariants of an included context: one over the identity and one between two relations. |
| `shouldSucceed/kurk_completed.adl` | The moment of completion: with all violations repaired, the desired system is included with its blocking invariant. |
| `invariantViolations/kurk_too_early.adl` | The same inclusion before the violations are repaired is refused. |
| `invariantViolations/invariant_of_included_context.adl` | A rule of an included context holds in the including context. |
| `shouldFail/same_name_needs_alias.adl` | Two included contexts with the same name need an alias each. |
| `shouldFail/ambiguous_prefix.adl` | With aliases, the shared name is still ambiguous where a script uses it. |
| `shouldFail/alias_for_two_files.adl` | One alias for two contexts. |
| `shouldFail/alias_is_reserved.adl` | An alias that is a name space of Ampersand itself. |
| `shouldFail/alias_missing.adl` | `AS` without an alias. |
| `shouldFail/cycle_self.adl`, `shouldFail/cycle_a.adl` | A context that includes itself, directly and by way of another context. |
| `shouldFail/included_file_missing.adl`, `shouldFail/context_not_in_file.adl`, `shouldFail/from_missing.adl` | The file does not exist; the file has no context with that name; the statement does not say where the context is. |
| `shouldFail/prefix_follows_no_path.adl` | A prefix names a context that is included directly; `Reg.Geo.Town` is refused. |
| `shouldFail/include_takes_no_alias.adl` | `INCLUDE "file" AS alias` is no statement. |
| `shouldFail/interface_of_included_context.adl` | The interfaces of an included context belong to its own application. |
| `shouldFail/misspelled_concept.adl` | `Reg.Persn` is reported; it does not silently become a new concept. |
| `shouldFail/misspelled_relation.adl`, `shouldFail/unknown_alias.adl` | A relation that the included context does not have; a prefix that names no included context. |
| `shouldFail/name_without_prefix.adl` | A name of the included context is available with its prefix only. |
| `shouldFail/represent_of_included_concept.adl` | Only the context that declares a concept states its representation. |
| `shouldFail/type_error_across_contexts.adl` | `Address` and `Reg.Address` are different concepts. |

## The migration case

The three files `contexts/kurk_existing.adl`,
`contexts/kurk_desired.adl` and `shouldSucceed/kurk_migration.adl` are the proof of concept of *Data Migration under a Changing Schema in Ampersand* (RAMiCS 2024), which was one script with the prefixes `old_` and `new_` typed in by hand.
Here the existing and the desired system are scripts of their own, both with the context name `Kurk`,
and the migration context includes them under two aliases.
Three things in the migration script are worth knowing.

- The concepts of the two systems are different concepts.
  `CLASSIFY old.A ISA new.A` states that every atom of the existing system is an atom of the desired one.
- The desired system has a new blocking invariant, the rule `totalR`, which the data of the existing system violates.
  The migration context writes `ROLE User MAINTAINS new.totalR`.
  In the migration context the rule is then a business constraint that signals what users have to repair,
  and the compiler marks it in `rules.json` as a rule whose violations can only disappear (`"hardens": true`).
  The prototype framework then refuses a transaction that adds a violation, so that a repaired violation cannot return.
  That is a matter of running applications, which the scenario `migration` of the framework tests on three databases.
- `kurk_completed.adl` and `kurk_too_early.adl` include the desired system without assigning the rule to a role,
  after and before the violations are repaired. So the rule is an invariant there, and the second script is refused.

## Other checks

- These cases run in one database, because `ampersand validate` builds one.
  The scenarios in `test/multi-context` of the prototype framework run a system with one database per context:
  a diamond of four contexts, and this migration on three databases up to the moment of completion.
- The property suite `Ampersand.Test.MultiContext.QualifyProperties` runs in `stack test`.
  It states for arbitrary contexts that an included context contributes its names with the prefix and nothing else,
  and that a context that is reached along two paths contributes its names once.
- `memorybank/multi-context/spec-checks` holds populations of the specification `AmpersandData/FormalAmpersand/MultiContext.adl`.
