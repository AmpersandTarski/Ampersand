# Regression cases for `INCLUDE "file" AS alias`

These cases test the inclusion of a context under an alias (see [the INCLUDE statement](../../../../docs/reference-material/syntax-of-ampersand.md#the-include-statement)).
With an alias, the included file is a context of its own,
and its names are available in the including context under the alias as a prefix.

## Layout

| Directory | What it holds | What the suite does with it |
| --- | --- | --- |
| `contexts/` | The files that the cases include. | Nothing: the directory has no `testinfo.yaml`. |
| `shouldSucceed/` | Scripts that are correct. | `ampersand check` (exit 0) and `ampersand validate` (exit 0), which compares the generated SQL with the compiler's own evaluation in a real database. |
| `shouldFail/` | Scripts with a mistake in the use of an alias. | `ampersand check` has to refuse each of them (exit 10). |
| `invariantViolations/` | Scripts whose population violates an invariant of an included context. | `ampersand check` has to refuse each of them (exit 10). |

## What each case guards

| Case | What it guards |
| --- | --- |
| `shouldSucceed/permits.adl` | The example of the documentation: two contexts with a relation `address` and a concept `Address` that mean different things, and a rule of the including context over data of the included one. |
| `shouldSucceed/nested.adl` | A chain of three contexts; a name through two aliases (`Reg.Geo.Town`). |
| `shouldSucceed/two_copies.adl` | One file under two aliases gives two copies with separate populations. |
| `shouldSucceed/diamond.adl` | Two included contexts that include the same file each get a copy of it. |
| `shouldSucceed/plain_and_alias.adl` | The same file with and without an alias: a union and a disjoint union side by side. |
| `shouldSucceed/alias_of_two_files.adl` | The alias applies to a context that consists of several files. |
| `shouldSucceed/every_kind_of_name.adl` | Every kind of name gets the prefix: pattern, rule, enforcement, identity, view, interface, classification, representation, purpose. |
| `shouldSucceed/as_is_no_keyword.adl` | `AS` remains an ordinary identifier outside the `INCLUDE` statement. |
| `shouldSucceed/alias_with_variables.adl` | Preprocessor variables after an alias. |
| `shouldSucceed/kurk_desired_alone.adl` | The desired system of the migration case compiles by itself, with its blocking invariant. |
| `shouldSucceed/kurk_migration.adl` | The migration system of the RAMiCS 2024 paper as a context that includes the existing system as `old` and the desired system as `new`. |
| `shouldSucceed/kurk_completed.adl` | The moment of completion: with all violations repaired, the desired system is included with its blocking invariant. |
| `invariantViolations/kurk_too_early.adl` | The same inclusion before the violations are repaired is refused. |
| `invariantViolations/invariant_of_included_context.adl` | A rule of an included context holds in the including context. |
| `shouldFail/alias_for_two_files.adl` | One alias for two files. |
| `shouldFail/alias_is_own_name.adl` | An alias that equals the name of the including context. |
| `shouldFail/alias_is_reserved.adl` | An alias that is a name space of Ampersand itself. |
| `shouldFail/alias_missing.adl` | `AS` without an alias. |
| `shouldFail/cycle_self.adl`, `shouldFail/cycle_a.adl` | A context that would include itself, directly and by way of another context. |
| `shouldFail/included_file_missing.adl` | An aliased include of a file that does not exist. |
| `shouldFail/misspelled_concept.adl` | `Reg.Persn` is reported; it does not silently become a new concept. |
| `shouldFail/misspelled_relation.adl`, `shouldFail/unknown_alias.adl` | A relation that the included context does not have; a prefix that is no alias. |
| `shouldFail/name_without_prefix.adl` | A name of the included context is available with its prefix only. |
| `shouldFail/represent_of_included_concept.adl` | Only the context that declares a concept states its representation. |
| `shouldFail/type_error_across_contexts.adl` | `Address` and `Reg.Address` are different concepts. |

## The migration case

The three files `contexts/kurk_existing.adl`,
`contexts/kurk_desired.adl` and `shouldSucceed/kurk_migration.adl` are the proof of concept of *Data Migration under a Changing Schema in Ampersand* (RAMiCS 2024), which was one script with the prefixes `old_` and `new_` typed in by hand.
Here the existing and the desired system are scripts of their own,
and the migration context includes them.
Three things in the migration script are worth knowing.

- The concepts of the two systems are different concepts.
  `CLASSIFY old.A ISA new.A` states that every atom of the existing system is an atom of the desired one.
- The desired system has a new blocking invariant, which does not hold during the migration.
  The migration context includes the desired system with the preprocessor variable `Migrating`,
  which leaves that invariant out, and states the relaxed version itself.
- `kurk_completed.adl` and `kurk_too_early.adl` include the desired system without that variable,
  after and before the violations are repaired.

## Two more checks

- `scripts/check-multicontext-flattening.sh` is the oracle for the meaning of an aliased include.
  It exports every case in `shouldSucceed/` to a single script in
  which every name carries its prefix,
  and verifies that this script compiles and yields the same prototype as the original.
- The property suite `Ampersand.Test.MultiContext.QualifyProperties` runs in `stack test`
  and states for arbitrary contexts that the renaming prefixes every name,
  identifies no two names and yields disjoint names for two aliases.
