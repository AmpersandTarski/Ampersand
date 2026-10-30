---
title: "Trail: the Kleene operators"
---

# The Kleene operators

*This trail visits claims [PRF-3](./README.md#prf-3), [PRF-4](./README.md#prf-4) and [PRF-5](./README.md#prf-5) of the [proof track](./README.md).*

## The question

Transitive closure sits awkwardly in a relational language. The operators `r+` and `r*` are indispensable for modelling reachability, hierarchy and precedence, yet they escape first-order relation algebra: no finite term of the other operators expresses them, and an implementation must therefore treat them specially — in the normaliser, in the generated SQL, and in any maintained closure relation. Each special treatment is an opportunity for a law that sounds right and is not. This trail answers the question:

> What does Ampersand guarantee about the Kleene operators — the laws it rewrites with, the transitive reduction it computes, and the closure relations it maintains?

The guarantees are three machine-checked claims, and each of them began as a defect or a doubt. The practical side of working with closures in a model is the guide [Computing transitive closures](../guides/transitive-closure.md); the design discussion is in issues [#1651](https://github.com/AmpersandTarski/Ampersand/issues/1651) and [#1635](https://github.com/AmpersandTarski/Ampersand/issues/1635).

## PRF-5 — the law that flooded a database

*Proof track: [PRF-5 — the singleton laws, corrected](./README.md#prf-5), machine-checked.*

The compiler's normaliser simplifies terms before code generation, guided by algebraic properties it computes for each subterm. One of those computations judged a singleton relation `"a"[C]` — the relation `{(a,a)}` on concept `C` — to be both total and surjective. Neither holds when `C` has more than one atom, and the consequence was not hypothetical: from surjectivity the normaliser concluded `V;"a" = V`, silently discarding the restriction `;"a"` from any term containing it. The defect surfaced in production as the unbounded growth of a database table.

`SingletonSurjective.thy` closes the episode the way a proof should: it refutes the assumed properties with explicit two-atom witnesses, proves `V;"a" ≠ V` for every concept with more than one atom, and records the properties a singleton *does* enjoy — univalence, injectivity, symmetry, antisymmetry, transitivity — so that the corrected property computation rests on proved ground. The theorem covers the algebra; the corrected Haskell (`isTotSur` and `isTot` in `Ampersand.Classes.Relational`) is pinned by the regression suite.

## PRF-3 — sound unfoldings, the true reduction, and a fixpoint

*Proof track: [PRF-3 — the Kleene laws are sound](./README.md#prf-3), machine-checked.*

The same discipline was then applied to the rewrite laws for the closures themselves. `KleeneReduction.thy` establishes three groups of results.

**The unfoldings.** The normaliser once rewrote with `r* = r;r*`, a law that reads plausibly and fails already on the empty relation, since `r*` contains the identity and `r;r*` need not. The theory refutes that law (`rStar_naive_law_unsound`) and proves the unfoldings that replaced it and its three siblings in `NormalForms.hs`: `r+ = r ∪ r;r+` and its right-handed twin, `r* = I ∪ r;r*` likewise, and `r* = I ∪ r+`.

**The transitive reduction.** Ampersand's `r%` is defined by the desugaring `r% = r − (r;r+)`: the pairs of `r` that no composite path re-derives. For finite acyclic `r` the theory proves this is *the* transitive reduction in the strong sense — it preserves the closure (`red_trancl_eq`) and it is minimum among all closure-preserving subrelations (`red_minimal`). It also proves that `%` commutes with converse, which is the fact the parser's treatment of `flp` relies on. On cyclic input `r%` remains well-defined, but minimality is proved only for the acyclic case; that boundary is part of the claim.

**The fixpoint.** A model may maintain a closure with an `ENFORCE` rule of the shape `c ⊇ r ∪ r;c`. By Knaster–Tarski this inclusion has `r+` as its least fixpoint, and the theory proves that the insert-only maintenance converges to it (`rPlus_is_lfp`). Convergence is what rules out oscillation for this rule shape: each step only adds pairs, and the pairs it adds are exactly the missing ones.

## PRF-4 — deletion is the hard direction

*Proof track: [PRF-4 — incremental deletion from a stored closure is exact](./README.md#prf-4), machine-checked.*

The fixpoint result covers growth. Shrinkage is harder: deleting a pair from `r` may or may not invalidate derived pairs of the stored closure, because other derivation paths may survive. The theory first proves that the tempting shortcut — subtract the deleted pairs from the stored closure — is unsound (`naive_delete_unsound`), and then proves the procedure that is exact: confine re-derivation to the affected region `r* ; del ; r*`, reuse the stored closure verbatim outside it, and recombine. The correctness theorem `(r − del)+ = (safe ∪ (r − del))+` holds for `r+`, `r*` and `r%` alike, cycles included. This claim is shared with the [incremental-evaluation trail](./incremental-evaluation.md), where it marks the road ahead: the compiler does not yet maintain closures incrementally, and the theorem is in place before the code that will need it.

## Reproducing the results

The session builds headless on Isabelle 2025-2, with no `sorry` and no `quick_and_dirty`:

```bash
isabelle build -D proofs/kleene
```

The lemma inventory is kept next to the sources, in [`proofs/kleene/README.md`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/README.md).
