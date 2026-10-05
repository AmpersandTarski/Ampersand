# Machine-checked semantics of including a context under an alias

This Lean 4 session backs the statement `INCLUDE "file" AS alias` in Ampersand.
The statement makes the included file a context of its own,
whose names are available in the including context under the alias as a prefix.
The theory is that of the article *Multi-Context Information Systems in Ampersand* (2026),
which builds on the definitions of *Data Migration under a Changing Schema in Ampersand* (RAMiCS 2024).
The register entry is claim PRF-11 in [`docs/proofs/README.md`](../../docs/proofs/README.md).

## Contents

`MultiContext.lean` models a system of contexts.
A fragment is a set of facts, and a fact is a triple of a relation or an instance pair of a concept.
Every relation and every concept has one owner.
The view of a context is the union of the fragments of the contexts it reaches by include statements.

| Theorem in Lean | What it says | Where the compiler relies on it |
| --- | --- | --- |
| `qualified_names_suffice` | An alias with a local name denotes exactly one thing. | Name resolution of `alias.name` (`Ampersand.Input.Qualify`). |
| `restricted_view`, `view_restr_reach` | A view restricted to what an included context reaches is the view of that context. | Used by the theorems below. |
| `truth_is_imported` | A rule of an included context has the same violations in the including context. | The rules of an included context are kept as they are. |
| `closure_has_concepts`, `wellTyped_view_iff`, `flattening` | A context with everything it reaches is one information system precisely when each reached context is consistent. | The compiler renames and merges, and then compiles one context. |
| `reach_extension`, `view_extension` | Adding an including context changes nothing for the included one. | A file compiles to the same result whether or not another file includes it. |
| `frame`, `non_interference`, `not_affected`, `preservation` | A change affects only the contexts that reach what it writes. | Not used yet: it describes contexts on separate databases. |
| `moment_of_completion`, `moment_of_completion_typed` | When no violation of the new invariants is left, the desired system is consistent on its own data. | The regression case `kurk_completed.adl`. |

## Building

```bash
cd proofs/multicontext
lake build
lake env lean Axioms.lean
```

The session uses core Lean 4 (version 4.34.1, see `lean-toolchain`) without mathlib,
so the build needs no downloads.
`lake-build.txt` holds the output of the build as it was observed on 5 October 2026,
with the list of axioms per theorem.

## What is not proved

The model treats the violations of a rule,
the rules of a context and the include statements as given functions and predicates.
That the type checker and the SQL generator of the compiler respect this model is covered by tests:
the property suite `Ampersand.Test.MultiContext.QualifyProperties`
and the cases in `testing/Travis/testcases/MultiContext/`.
Finiteness of populations plays no part in the proofs and is left out of the model.

Two things in the compiler go beyond the model.
The compiler also names the things of a context that an included context includes, as in `Reg.Geo.Town`;
the theorem `qualified_names_suffice` covers one alias, and the property suite covers the composition of two.
And where the article carries the atoms of a concept to another context by a rule,
a script uses `CLASSIFY old.A ISA new.A`.

The article keeps a copy of this session with its text; the copy in this repository is the authoritative one.
