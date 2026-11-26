# Regression cases for data migration

These cases test the method of *Data Migration under a Changing Schema in Ampersand*
(Joosten and Joosten, RAMiCS 2024), section 4.1, which the migration plan
(`memorybank/migration/plan.md`) takes as the specification of every migration.
They were written before the generator of migration scripts,
so that the generator is built against them.

## How a case tests the article

The existing system (`contexts/library_existing.adl`) and the desired system (`contexts/library_desired.adl`)
differ in each kind of item that section 4.1 distinguishes.
The migration system (`scripts/library_migration.adl`) is written by hand, step by step as the article prescribes.
A case joins that script by a plain `INCLUDE`, adds the population of one moment of the migration,
and states as a `RULE` what the article promises for that moment.
`ampersand check` then accepts or refuses the population.
The script itself holds no assertions, so a generated script can later take its place unchanged.

`ampersand check` does not execute an `ENFORCE` rule, so a case describes a state and not a transition.
That the ExecEngine copies the data, that the copying ends, and that a removed pair does not return
are transitions; the running prototype in `demos/migration` tests those (`test.sh`).

## Layout

| Directory | What it holds | What the suite does with it |
| --- | --- | --- |
| `contexts/` | The existing and the desired system. | Nothing: the directory has no `testinfo.yaml`. |
| `scripts/` | The migration system, written by hand. | Nothing; the cases include it. |
| `shouldSucceed/` | States that the article promises. | `ampersand check` (exit 0) and `ampersand validate` (exit 0). |
| `invariantViolations/` | The counterpart of each state, which the article excludes. | `ampersand check` has to refuse each of them (exit 10). |
| `shouldFail/` | A migration script with a type error. | `ampersand check` has to refuse it (exit 10). |

## What each case guards

The labels in the second column are the labels of the equations in the source of the article.

| Case | Article | What it guards |
| --- | --- | --- |
| `shouldSucceed/copy_done.adl` | step 2, eqn:copyingTerminates, eqn:satisfaction | After copying, both transactional invariants of step 2 are satisfied and `old.title = copyTitle`. The new invariant is violated, and the migration system accepts that at the moment of transition. |
| `invariantViolations/copy_not_done.adl` | step 2, eqn:copyingTerminates | Before copying, the copying has not terminated. |
| `shouldSucceed/copied_pair_removed.adl` | step 2 | A user removed a copied pair from `new.title`; `copyTitle` still registers it, so both copying rules hold and the pair is not copied again. |
| `invariantViolations/removed_pair_unregistered.adl` | step 2 | Without that registration the copying rule is violated: the pair would be copied again. |
| `shouldFail/copy_across_signatures.adl` | eqn:copy relations | `loan[Book*Person]` and `loan[Book*Member]` are two relations, so copying the one into the other is a type error. |
| `invariantViolations/kept_invariant_blocks.adl` | eqn:schema migrsys | The property `[UNI]` of `title` is in both schemas and stays blocking in the migration system. |
| `shouldSucceed/dropped_invariant_lapses.adl` | eqn:schema migrsys | The rule `titled` of the existing system is not in the desired one; a new book without a title is accepted. |
| `shouldSucceed/violation_repaired.adl` | step 3, step 4, eqn:enforceForRules, eqn:Bfix | A repaired book is registered in `fixedAuthored`; a book that is not yet repaired is a signal and no refusal. |
| `invariantViolations/repair_unregistered.adl` | step 4, eqn:enforceForRules | A repaired book that `fixedAuthored` does not register violates the transactional invariant of step 4. |
| `invariantViolations/repaired_violation_returns.adl` | step 3, eqn:blockRule | A registered book without an author is refused by `blockAuthored`, which the compiler adds (DC-9). |
| `shouldSucceed/ready_for_completion.adl` | eqn:readyForMoC | With every book repaired and registered, the condition for the moment of completion holds. |
| `invariantViolations/not_ready_for_completion.adl` | eqn:readyForMoC | With one book left, it does not. |
| `shouldSucceed/completed.adl` | section 4.2, the third and fourth point | The desired system includes the data with its blocking invariant `authored`, and the data satisfies it. |
| `invariantViolations/completed_too_early.adl` | section 4.2 | The same before every book is repaired is refused. |

## What these cases do not cover yet

- According to eqn:schema migrsys, the transactional invariants and business constraints of the existing system
  are not part of the migration system. `INCLUDES` brings them along under the prefix `old`.
  Whether that changes the behaviour shows in a running system, so this belongs to the tests of `demos/migration`.
- The choices of the migration plan that the article leaves open (the stand-in for a new `[UNI]`,
  `ALIAS ROLE`, the propagation of deletions, the generated helper relations of DC-9)
  get cases of their own with the phase that builds them.
