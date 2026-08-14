---
title: The proof track
---

# The proof track

Ampersand generates information systems from relation-algebraic specifications. That method carries a promise: the generated system behaves as the specification prescribes. Parts of that promise have been proved, several of them with a proof assistant. This section of the documentation discloses those proofs.

The proof track is organised around two notions. A **claim** is a single proposition, stated precisely, together with the artifact that establishes it — an Isabelle/HOL or Lean session, or a written proof. A **trail** is a narrative that answers one reader's question by visiting the claims that settle it. The two are related many-to-many: one claim may serve several trails, and one trail usually visits several claims. Both are recorded in the register below, under identifiers that remain stable for the life of the repository.

The formal sources themselves live in the [`proofs/`](https://github.com/AmpersandTarski/Ampersand/tree/main/proofs) directory of the Ampersand repository, next to the code they speak about. The pages here tell the stories; the repository holds the single authoritative copy of every proof.

## How to read the register

Each claim carries one of four statuses.

- **machine-checked** — a proof assistant has verified the proof from first principles. The entry names the session and states its trust base.
- **paper proof** — the proof is written out in full and has been reviewed by people, but has not yet been formalised.
- **stated** — the proposition is formulated precisely; its proof is still outstanding.
- **in progress** — formalisation is under way.

Two habits keep these statuses honest. A status is recorded only after the build of the proof artifact has actually been observed, never on the strength of a report. And every machine-checked entry states what is *not* proved — the assumptions, the boundary of the model, the parts covered by testing rather than by the theorem. A proof is a precise instrument, and precision includes knowing where it ends.

## Claims

| ID | Claim | Status | Artifact | Trails |
| --- | --- | --- | --- | --- |
| [PRF-1](#prf-1) | The runtime re-checks exactly the conjuncts a transaction can have changed | paper proof | [From rules to running code, Part III](../reference-material/from-rules-to-running-code.md#part-iii--do-the-compiler-and-the-back-end-agree) | TRAIL-1, TRAIL-4 |
| [PRF-2](#prf-2) | The incremental evaluator computes exactly the set semantics of every rule term | machine-checked | [`proofs/incremental/`](https://github.com/AmpersandTarski/Ampersand/tree/main/proofs/incremental), session `Incremental_Delta` | TRAIL-1, TRAIL-2, TRAIL-4 |
| [PRF-3](#prf-3) | The Kleene rewrite laws are sound, `r%` is the transitive reduction, and insert-only closure maintenance converges | machine-checked | [`proofs/kleene/KleeneReduction.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/KleeneReduction.thy) | TRAIL-3 |
| [PRF-4](#prf-4) | Incremental deletion from a stored closure is exact, cycles included | machine-checked | [`proofs/kleene/IncrementalDelete.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/IncrementalDelete.thy) | TRAIL-2, TRAIL-3 |
| [PRF-5](#prf-5) | A singleton relation is neither total nor surjective in general; the law that assumed so is refuted | machine-checked | [`proofs/kleene/SingletonSurjective.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/SingletonSurjective.thy) | TRAIL-3 |
| [PRF-6](#prf-6) | The generated delta SQL and its maintenance protocol keep the violation records equal to full re-evaluation | stated | obligation stated in [Correctness of the incremental SQL queries](./incremental-sql.md); formalisation planned in Lean 4 | TRAIL-4 |

### PRF-1 {#prf-1}

**The runtime re-checks exactly the conjuncts a transaction can have changed.** The compiler ships, per concept and per relation, the list of conjuncts a change can affect; the back-end unions those lists at commit. The correspondence theorem states that the retrieved set equals the ground truth — every conjunct whose violation set can have changed is re-checked, and no other. The proof observes that both sides denote the same predicate, so equality is definitional; the only order fact used is the reflexivity of the specialisation order.

*Artifact:* the written proof in [From rules to running code, Part III](../reference-material/from-rules-to-running-code.md#part-iii--do-the-compiler-and-the-back-end-agree). *Not yet formalised:* the theorem is a natural first candidate for a Lean formalisation; its statement is small and its ingredients (a partial order, two set comprehensions) are standard.

### PRF-2 {#prf-2}

**The incremental evaluator computes exactly the set semantics of every rule term.** The Isabelle session `Incremental_Delta` (six theories, parent `HOL`, no axioms beyond HOL, no `sorry`) proves the Z-set algebra, the desugaring identities for residuals and complements, the delta rules for the linear, bilinear and `distinct` operators, and a whole-circuit induction: every state reachable from the empty database by transactions — the loading of the initial population included — yields exactly the set semantics of every term. A QuickCheck suite in `stack test` states the same lemmas as properties over the actual Haskell functions, so the code is bound to the proofs on every build.

*Artifact:* [`proofs/incremental/`](https://github.com/AmpersandTarski/Ampersand/tree/main/proofs/incremental); the obligation-to-lemma map is in its `README.md`. *Not proved:* the shortcuts by which the implementation skips recomputation when nothing changed, the Kleene nodes' reading of their child, and the Haskell code itself — these are covered by the per-transaction oracle and the property suite, not by the theorems. The trail [Incremental evaluation](./incremental-evaluation.md) walks this claim in full.

### PRF-3 {#prf-3}

**The Kleene rewrite laws are sound, `r%` is the transitive reduction, and insert-only closure maintenance converges.** `KleeneReduction.thy` proves the unfoldings of `r+` and `r*` that replaced four earlier rewrite laws, and refutes the law `r* = r;r*` that they replaced. It further proves that `r% = r − (r;r+)` is, for finite acyclic `r`, *the* transitive reduction — closure-preserving and minimum — and that the `ENFORCE` fixpoint `c ⊇ r ∪ r;c` converges to `r+` by Knaster–Tarski, so insert-only maintenance cannot oscillate.

*Artifact:* [`proofs/kleene/KleeneReduction.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/KleeneReduction.thy). *Boundary:* minimality of `r%` is proved for finite acyclic relations; on cyclic input `r%` is still well-defined but no longer minimum.

### PRF-4 {#prf-4}

**Incremental deletion from a stored closure is exact, cycles included.** Subtracting the deleted pairs from a stored transitive closure is wrong, and the theory proves it wrong (`naive_delete_unsound`). The sound procedure confines re-derivation to the affected region `r* ; del ; r*`: outside it the old closure is reused verbatim, and the recombination `(r − del)+ = (safe ∪ (r − del))+` is proved exact for `r+`, `r*` and `r%` alike, with no acyclicity assumption.

*Artifact:* [`proofs/kleene/IncrementalDelete.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/IncrementalDelete.thy). *Status in the code:* the compiler does not yet apply this procedure; the claim precedes its implementation, which is the intended order.

### PRF-5 {#prf-5}

**A singleton relation is neither total nor surjective in general; the law that assumed so is refuted.** The compiler once judged the singleton `"a"[C]` both total and surjective, from which the normaliser concluded `V;"a" = V` and silently discarded the restriction — a defect that surfaced as unbounded growth of a production database. The theory refutes the assumed properties with explicit witnesses, proves `V;"a" ≠ V` whenever `C` has more than one atom, and records the properties a singleton does have (univalent, injective, symmetric, antisymmetric, transitive).

*Artifact:* [`proofs/kleene/SingletonSurjective.thy`](https://github.com/AmpersandTarski/Ampersand/blob/main/proofs/kleene/SingletonSurjective.thy). *Scope:* the refutation motivated the corrected `isTotSur`/`isTot` in `Ampersand.Classes.Relational`; the correctness of that Haskell code is covered by the regression suite, not by the theorem.

### PRF-6 {#prf-6}

**The generated delta SQL and its maintenance protocol keep the violation records equal to full re-evaluation.** The compiler derives, per conjunct and affected relation, a delta term — an ordinary relation-algebra expression over the base relations and the transaction's changed pairs — and compiles it with the same term-to-SQL translation as the full violation queries; the runtime applies the resulting delta queries to a materialized violation table through SQL statements that touch only the affected rows, with full re-evaluation as fallback outside the supported class. The claim is that this maintained table equals, after every commit, the result of the full violation queries. The obligation has two halves: a correspondence between the generated SQL operations and the proven Z-set operations of [PRF-2](#prf-2), and a protocol statement that the commit-time updates keep the table equal to full re-evaluation, fallback and repair iterations included.

*Status:* stated — registered before its proof, as the working method requires. *Evidence to date:* a harness in the style of `ampersand validate` over the regression suite, and a divergence-free shadow run of 1142 transactions on a production-scale application, documented in the trail [Correctness of the incremental SQL queries](./incremental-sql.md). *Planned vehicle:* Lean 4, with [Chajed's Lean formalisation of DBSP](https://github.com/tchajed/database-stream-processing-theory) as a lemma-by-lemma roadmap.

## Trails

| ID | Trail | The question it answers | Narrative | Claims visited |
| --- | --- | --- | --- | --- |
| TRAIL-1 | From rules to running code | When I write a rule, where does its code end up, and is the enforcement machinery correct? | [From rules to running code](../reference-material/from-rules-to-running-code.md) | PRF-1, PRF-2 |
| TRAIL-2 | Incremental evaluation | Can a system that maintains rule violations incrementally ever disagree with full re-evaluation? | [Incremental evaluation](./incremental-evaluation.md) | PRF-2, PRF-4 |
| TRAIL-3 | The Kleene operators | What does Ampersand guarantee about transitive closure — its laws, its reduction, its maintenance? | [The Kleene operators](./kleene-operators.md) | PRF-3, PRF-4, PRF-5 |
| TRAIL-4 | Correctness of the incremental SQL queries | When the runtime maintains its violation records by increments, what guarantees they never drift from the full queries that define them? | [Correctness of the incremental SQL queries](./incremental-sql.md) | PRF-1, PRF-2, PRF-6 |

A trail's narrative need not live on this site. TRAIL-1 is anchored in the reference chapter that contributors already read; a future trail may be anchored in a published article, with the register linking the article to the claims it rests on. What the register guarantees is the connection: from any claim to every story that uses it, and from any story to every claim it stands on.

## Tools and trust base

The machine-checked claims are Isabelle/HOL sessions (Isabelle 2025-2), built on the plain `HOL` heap with no additional axioms and no `sorry`. Each session builds headless with `isabelle build -D proofs/<session>`. Where a claim speaks about running code, a bridge binds the two: the QuickCheck properties of `stack test` state the Isabelle lemmas over the actual Haskell functions, so every build re-checks the correspondence.

For new formalisations the project prefers **Lean 4** with the mathlib toolchain. Lean's blueprint culture matches the shape of this proof track — a human-readable narrative in which every statement links to its formalisation, with an explicit status per statement and a dependency graph generated from the sources. The existing Isabelle sessions remain authoritative for their claims; a claim migrates to Lean only when there is a reason beyond the change of tool, and the register records such a migration as a new artifact under the same claim number.

Three further habits are borrowed deliberately from neighbouring cultures: the assumptions discipline of seL4 (every verification statement names its boundary), the per-pass provenance tables of CompCert (each claim entry names its artifact and its bridge to the code), and the permanent tags of the Stacks Project (identifiers below are never renumbered and never reused).

## Extending the proof track

The track grows claim by claim and trail by trail. The conventions that keep it coherent:

1. **Identifiers are permanent.** A claim number (`PRF-n`) or trail number (`TRAIL-n`) is assigned once and never renumbered or reused, even if the claim is superseded — a superseded entry keeps its number and points to its successor. Before assigning a number, check the git history as well as the current file.
2. **Every proof session appears in the register.** A directory under `proofs/` that contains an Isabelle `ROOT` (or a Lean `lakefile`) has at least one claim entry here. The CI script `scripts/check-proof-track.js` enforces this, together with the uniqueness of identifiers and the validity of statuses.
3. **Ordinary documentation pages refer to a claim with a single line.** The fixed form is one italic sentence, for example: *Proof track: [PRF-2 — the incremental evaluator is exact](../proofs/README.md#prf-2).* No formal notation, no proof sketches, and no further apparatus appear outside this section; readers who have no use for proofs should meet at most that one quiet line.
4. **A proof obligation arises with the work, not after it.** A change to semantics-bearing code — rewrite laws, the delta calculus, the SQL generation of terms, the typology logic — states its claim in this register in the same change, with status *stated* until the proof exists. The claim precedes the proof, and the proof precedes the confidence.
5. **A trail page follows one template.** It opens with the reader's question, answers it in prose, then visits its claims in order — for each: the statement in words, the idea of the proof, and the boundary of what is proved. It closes with instructions to reproduce the builds. The prose is written for publication: unhurried, complete sentences, claims no stronger than their artifacts.
