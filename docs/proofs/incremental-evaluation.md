---
title: "Trail: incremental evaluation"
---

# Incremental evaluation

*This trail visits claims [PRF-2](./README.md#prf-2) and [PRF-4](./README.md#prf-4) of the [proof track](./README.md).*

## The question

An Ampersand system keeps its rules satisfied by re-evaluating, at every commit, the violation queries of the rules a transaction may have affected. That strategy is correct, and [Part III of *From rules to running code*](../reference-material/from-rules-to-running-code.md#part-iii--do-the-compiler-and-the-back-end-agree) proves that the selection of rules is exact. Its cost, however, follows the size of the database rather than the size of the change: inserting one pair re-runs queries over the whole population.

Incremental evaluation replaces the re-computation by a *delta computation*: from the change in the population it derives the change in each violation set directly, in time proportional to the change. The question a careful reader must ask is the one this trail answers:

> Can a system that maintains its violation sets incrementally ever disagree with the definition — that is, with a full evaluation from scratch?

For the evaluator that now lives in the Ampersand compiler, the answer is no, and the answer is a theorem.

## The setting

The theory behind the evaluator is DBSP, a compile-time transformation that turns any relational-algebra query into its incremental form (Budiu et al., *DBSP: Automatic Incremental View Maintenance for Rich Query Languages*, VLDB 2023, [arXiv 2203.16684](https://arxiv.org/abs/2203.16684)). Ampersand's rule terms are relational algebra over binary relations, so the theory applies without translation loss. The mechanism — Z-sets, circuits, and the delta rules for each operator — is described for contributors in [Part IV of *From rules to running code*](../reference-material/from-rules-to-running-code.md#part-iv--paying-for-the-change-not-the-database); this page concerns what is proved about it.

Three facts about the construction locate the proof burden. A **Z-set** assigns an integer weight to each pair, and a change is itself a small Z-set: `+1` for an inserted pair, `−1` for a deleted one. Most operators are *linear* or *bilinear* in these weights, which is what makes their deltas cheap. Set semantics is restored by a `distinct` step that clips positive weights to one — and this clip is where the danger lives, because it is the one non-linear step: a pair enters the set only when its weight crosses zero, and a proof must show that no other weight movement is ever visible.

## PRF-2 — the evaluator equals the semantics

*Proof track: [PRF-2 — the incremental evaluator is exact](./README.md#prf-2), machine-checked.*

The Isabelle/HOL session `Incremental_Delta` ([`proofs/incremental/`](https://github.com/AmpersandTarski/Ampersand/tree/main/proofs/incremental); six theories, parent `HOL`, no axioms beyond HOL, no `sorry`) establishes the claim in four layers, each consumed by the next.

**The algebra of Z-sets** (`ZSet.thy`, `Delta.thy`). Z-sets under pointwise addition form an abelian group, and the operators of the core language respect it: converse, union and difference are linear; composition, intersection and cartesian product are bilinear, with the delta expansion `Δ(a⊗b) = Δa⊗b + a'⊗Δb`; and the `distinct` step obeys the zero-crossing rule, with the work bounded by the support of the incoming delta.

**The desugaring identities** (`Desugar.thy`). Ampersand's surface operators — the residuals `l/r` and `l\r`, the diamond, the relative addition, the typed complement — are defined away into the core language before any circuit is built. The theory proves each identity at the level of set semantics, under the typing premises that every relation lies inside the universal relation of its signature.

**The circuit induction** (`Circuit.thy`). A rule term becomes a circuit: one node per subterm, each holding exactly the state its delta rule needs. The theory is a deep embedding of the compiler's circuit language, one constructor per node kind, and proves an invariant-preservation theorem by structural induction over all of them. Its main corollary is the statement a user cares about: *every state reachable from the empty database by a sequence of transactions — the loading of the initial population included — yields exactly the set semantics of every term.* Constructs without a proven delta rule (the Kleene closures among them) enter the induction as specification nodes that recompute from their maintained input; the theorem therefore covers every circuit the compiler builds, with the fallback route slower but never different.

**The population mirror** (`Population.thy`). Atom creation and deletion are changes like any other; the theory proves that the concept-population bookkeeping is linear in the transaction and that its carried sets mirror the evaluator's own, which is how the specialisation order of Part III joins the calculus.

The obligation-to-lemma map — twenty-six obligations, each naming its lemma and theory — is kept next to the sources, in [`proofs/incremental/README.md`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/incremental/README.md).

**The bridge to the code.** A theorem about a model constrains the model; the repository binds the model to the implementation in two ways. The QuickCheck suite (`Ampersand.Test.Incremental.Properties`, run by `stack test`) states the Isabelle lemmas as properties over the actual Haskell functions, so every build re-checks the correspondence. And the command `ampersand incremental-bench --verify` compares the maintained violation set of every conjunct against a fresh full evaluation after every transaction of a random stream. Working out the induction's base case is also how two engine defects were found that testing had not reached — a reminder that the value of a formalisation lies as much in the questions it forces as in the certificate it yields.

**What is not proved.** The shortcuts by which the implementation skips recomputation when nothing changed, the Kleene nodes' reading of their child, and the Haskell code itself lie outside the theorems; the oracle and the property suite cover them. Verified extraction from the proofs was considered and rejected, so this division of labour is a settled design choice, not an omission.

## PRF-4 — deletion, the hard direction

*Proof track: [PRF-4 — incremental deletion from a stored closure is exact](./README.md#prf-4), machine-checked.*

The circuits above treat transitive closure by recomputation, because closure is where naive incrementality genuinely fails. Insertion is benign: a stored closure can only grow, and the growth is monotone. Deletion is not — a deleted pair may or may not sever the derived pairs that once depended on it, since other derivation paths may remain. Subtracting the deleted pairs from the stored closure is therefore wrong, and `IncrementalDelete.thy` proves it wrong before proving what is right.

The sound procedure confines re-derivation to the *affected region* `r* ; del ; r*` — the pairs with a derivation path through a deleted edge. Everything outside that region survives verbatim (`reuse_outside_affected`), and the recombination theorem `(r − del)+ = (safe ∪ (r − del))+` shows that re-deriving within the region recovers the closure exactly. The result holds for `r+`, `r*` and the transitive reduction `r%` alike, and it assumes nothing about cycles.

This claim runs ahead of the code: the compiler does not yet maintain closures incrementally. That order — the theorem first, the implementation against it — is the working method this proof track exists to record.

## Reproducing the results

Both sessions build headless, in a few seconds each, on Isabelle 2025-2:

```bash
isabelle build -D proofs/incremental
isabelle build -D proofs/kleene
```

The property bridge runs with the ordinary test suite (`stack test`), and the oracle comparison with `stack exec ampersand -- incremental-bench --verify`. The benchmark measurements that motivate the whole construction — an incremental step whose cost follows the change while full re-evaluation follows the database — are reported with their method in the repository, under `memorybank/incremental-evaluation/`.

The road from these in-compiler results to the generated SQL and the running system is the subject of the trail [Correctness of the incremental SQL queries](./incremental-sql.md).
