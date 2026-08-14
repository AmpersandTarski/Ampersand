---
title: "Trail: correctness of the incremental SQL queries"
---

# Correctness of the incremental SQL queries

*This trail visits claims [PRF-1](./README.md#prf-1), [PRF-2](./README.md#prf-2), [PRF-6](./README.md#prf-6) and [PRF-7](./README.md#prf-7) of the [proof track](./README.md).*

## The question

An Ampersand application keeps a set of business rules satisfied at every transaction boundary. The machinery that achieves this is generated: from a script written in relation algebra, the compiler produces the SQL that finds rule violations, and a generic runtime executes that SQL at every commit. The incremental-evaluation work replaces those commit-time re-runs by *delta queries*: SQL queries that compute how the violation set changes, from how the population changed. This paper answers the following question:

> The full violation queries define what a violation is. When the runtime maintains its violation records by increments instead, what guarantees that the maintained records never drift from that definition?

This trail answers by walking the whole chain — from the script to the running system — and then we focus on the incremental route: how the delta queries are generated, which parts of their correctness are already theorems, which parts rest on other evidence, and which claim remains to be proved. The claim at the end of the chain, [PRF-6](./README.md#prf-6), is deliberately registered *before* its proof exists; stating the obligation precisely is the first half of discharging it. Its mathematical core has since been discharged as a theorem of its own, [PRF-7](./README.md#prf-7).

## The chain

An Ampersand **script** declares concepts, relations, and rules. A rule is a term of heterogeneous relation algebra — composition, converse, union, complement, residuals, closures over binary relations [[8]](#ref-8), [[9]](#ref-9) — and the intended semantics is that the term denotes the *violations*: an empty denotation means the rule is satisfied. The compiler parses and type-checks the script and normalises every rule into **conjuncts**, the smallest queryable units of its violation condition.

Each conjunct is compiled to SQL once, at compile time. The translation (`selectExpr` in `Ampersand/FSpec/SQL.hs`) maps every operator of the algebra onto a SQL construct — joins for composition, antijoins (a pair passes only when no witnessing row exists) for the complements and residuals, and recursive common table expressions for the closures — and the result ships as *data*: the file `generics/conjuncts.json` carries one violation query per conjunct. The database schema is likewise generated (wide tables per concept kernel, two-column tables per non-functional relation), and it carries data only: no triggers, no stored procedures. All interpretation is left to a generic PHP runtime that is identical for every application.

At run time, a transaction records which relations and concepts it touched. At commit, the runtime re-checks exactly the conjuncts those changes can have affected, using per-relation and per-concept lists the compiler shipped. That this selection is *exact* — no violation can slip through unchecked, no conjunct is checked in vain — is the correspondence theorem of this proof track:

*Proof track: [PRF-1 — the runtime re-checks exactly the affected conjuncts](./README.md#prf-1).*

The violation sets themselves are materialized: the runtime keeps one violation record per conjunct in a cache table, refreshed at each commit by deleting the conjunct's rows and re-running its full query. Invariant rules read this machinery one way (a non-empty result rolls the transaction back), signal rules another (the violations are presented to a responsible role as work to do). The full account of this chain, at source-level detail, is the chapter [From rules to running code](../reference-material/from-rules-to-running-code.md); the theory of information systems it realises is described in [A theory of information systems](../conceptual/theory.md), and the publications behind it are collected on the [research page](../research.md).

For the present question, one property of this chain matters most: **the full violation query is the definition**. Everything the incremental route produces will be judged against it.

## Zooming in: from full queries to delta queries

Re-running a violation query is correct and simple, but its cost follows the size of the database, not the size of the change: one inserted pair re-runs joins over the whole population. The mismatch is the classical problem of incremental view maintenance, studied since the counting algorithm of Gupta, Mumick and Subrahmanian [[2]](#ref-2) and the duplicate-aware algebra of Griffin and Libkin [[3]](#ref-3), given an algebraic footing by Koch's ring of databases [[4]](#ref-4), and brought to closed form by DBSP [[1]](#ref-1) — whose general theory also carries a machine-checked Lean 4 formalisation [[5]](#ref-5): a compile-time transformation that turns any relational-algebra query into its incremental counterpart. Ampersand's rule terms are relational algebra over binary relations, so the theory applies without translation loss.

The construction replaces sets by **Z-sets** — maps from pairs to integer weights — so that a change is itself a small Z-set: `+1` for an insertion, `−1` for a deletion. A weight counts the ways a pair can be derived; a `distinct` step clips every positive weight back to 1, so the result is a set again, and a pair enters or leaves that set only when its weight crosses zero. Every operator of the core language then has a **delta rule**: linear operators pass deltas through, bilinear operators (composition, intersection, products) update by an expansion that touches only the changed rows, and `distinct` emits exactly the zero-crossings. A rule term becomes a circuit of such nodes, and a transaction propagates through it in time proportional to the change.

That this calculus is *exact* — that every circuit state reachable from the empty database — the loading of the initial population included — yields precisely the set semantics of its term — is machine-checked in Isabelle/HOL [[6]](#ref-6), and bound to the compiler's Haskell implementation by a QuickCheck [[7]](#ref-7) property per lemma on every build:

*Proof track: [PRF-2 — the incremental evaluator is exact](./README.md#prf-2).*

The trail [Incremental evaluation](./incremental-evaluation.md) walks that proof in full. Here it serves as the semantic anchor for the step that follows: pushing the calculus out of the compiler's memory and into the generated SQL.

## Generating the incremental SQL

The delta transformation is implemented **inside the compiler, at the term level, before SQL generation**, and it is candidate-based. For a conjunct `K` and a touched relation `r`, the compiler derives *candidate terms*: ordinary relation-algebra expressions over the base relations and one new leaf, `Δr`, a fabricated relation that denotes the transaction's changed pairs. A candidate term names the pairs whose membership in the violation set may have changed — deliberately a superset, because the runtime settles every candidate pair by re-running the conjunct's own violation predicate on it. A candidate set that is too large costs time; only a candidate set that misses a changed pair costs correctness. The calculus therefore owes exactly one property, *candidate completeness*, and that property is a theorem:

*Proof track: [PRF-7 — the candidate calculus is complete](./README.md#prf-7).*

Because a candidate term is just a term, it needs no new SQL machinery: **the same `selectExpr` that compiles the full violation queries compiles the delta queries** — the per-relation candidate queries the runtime consumes. The schema gains one delta table per relation (the table `Δr` denotes: filled during the transaction, empty after commit) and a materialized violation table per conjunct; `conjuncts.json` grows an optional per-relation delta-query field, which older runtimes ignore, so the contract between compiler and runtime stays backward compatible.

At commit, the runtime evaluates the candidate queries of the touched relations and updates the conjunct's violation records by two SQL statements scoped to the candidate pairs — one removes the candidates that no longer violate, one inserts the candidates that now do — instead of deleting and refilling the whole table. The route is deliberately *conservative*: a conjunct falls back to full re-evaluation whenever the transaction leaves the supported class (a touched concept that the conjunct mentions, a bulk mutation, a construct without a proven delta rule). Fallback is the general discipline of this whole design: the incremental route may be locally slower, never different.

## The correctness argument, layer by layer

The trust in the delta-maintained violation records decomposes into three layers, and the layers carry unequal kinds of evidence today.

**Layer 1 — the candidate terms name every pair that can change.** This is [PRF-7](./README.md#prf-7), a theorem. The envelopes that bound a term's old and new denotation, one completeness lemma per operator of the supported class, the whole-term statement, and the per-relation decomposition — the shape of query the compiler actually generates — are machine-checked, with the stated boundary: the Haskell functions that mirror the calculus and the contract that the delta tables record every change lie outside the theorems, covered by the harness and by the protocol layer below. Next to it stands [PRF-2](./README.md#prf-2), the machine-checked exactness of the in-memory delta calculus, proven in the same Isabelle session; it anchors the compiler's independent evaluator, the oracle every harness compares against.

**Layer 2 — the SQL says what the terms mean.** Delta terms are compiled by the *same* term-to-SQL translation as the full queries, so the incremental route introduces no second compiler to trust. What it inherits is the existing question — does `selectExpr` translate terms faithfully? — which the project guards operationally: `ampersand validate` holds the generated SQL against the compiler's independent in-memory evaluator, relation by relation and rule by rule, on a live database. The incremental route is held to the same standard by construction, and sharpens it: because the delta queries and the full queries are generated from *different* terms that must agree about every change, a translation defect that shifts the two routes differently becomes observable as a divergence.

**Layer 3 — the maintenance protocol keeps the cache equal to the definition.** Between the proven calculus and the running system stands the runtime protocol: delta tables filled and cleared per transaction, the two update statements per (conjunct, relation), the conservative fallback classification, and the interleaving with the repair engine — the runtime's loop that executes automated repair rules within the same commit. The evidence here is operational and layered: a harness in the style of `ampersand validate` holds delta-maintained violation tables equal to full-query results across the regression suite; and a **shadow run** on a production-scale application — both routes live, the full route in charge, every difference logged — replayed 1142 transactions through the complete request pipeline, repair engine included, with *zero* divergences, and with identical commit decisions in all 1043 paired runs of the delta-only mode. On the replayed mix, 61 % of the (transaction, conjunct) instances took the delta path; the remainder exercised exactly the fallback that keeps the route conservative.

A shadow run is evidence, not proof: it establishes agreement on the transactions that occurred, under the workload that occurred. What is *proved* covers the mathematics of the route — layer 1 — while layer 2 rides on the shared translator and its operational guard. The remaining gap is layer 3, and closing it is a stated claim of this proof track:

*Proof track: [PRF-6 — the delta SQL maintains the violation records exactly](./README.md#prf-6).*

With the candidate calculus discharged by [PRF-7](./README.md#prf-7), what remains of the obligation is a protocol statement: that the delta tables record exactly the transaction's changes, that the two candidate-scoped update statements, applied to a cache that equals the full-query result, leave it equal again, and that the fallback classification and the repair-engine iterations preserve this — the SQL-level counterpart of the circuit induction of [PRF-2](./README.md#prf-2). The intended vehicle is Isabelle/HOL, extending the candidate theory of PRF-7 within the same session.

## What this trail asserts today

The reader deserves the summary in one place. The *selection* of what to re-check is proved exact (PRF-1, on paper). The *calculus* of change is proved exact and is bound to the compiler's implementation on every build (PRF-2, machine-checked). The *candidate sets* the SQL route rechecks are proved complete — no pair can change its violation status without being named for recheck (PRF-7, machine-checked). The *generated SQL and the maintenance protocol* are generated from the proven terms by the same translator as the definition they must agree with, and they have agreed with it on every transaction of a divergence-free shadow run on a production-scale application — but the protocol's correctness is a stated claim (PRF-6), not yet a theorem. Until it is one, the runtime keeps the full queries as fallback and as periodic self-check, and the switch that enables the delta route stands per application, default off. The proof obligation is on the register; this trail will be updated when its status changes.

## Reproducing the results

The Isabelle sessions build headless on Isabelle 2025-2 (`isabelle build -D proofs/incremental`), the property bridge runs with the ordinary test suite (`stack test`), and the in-compiler oracle with `stack exec ampersand -- incremental-bench --verify`. The delta-SQL generation and the runtime maintenance live on feature branches of the compiler and the prototype framework until PRF-6's evidence warrants promotion; the design record, the measurement method and the shadow-run protocol are kept in the repository under `memorybank/incremental-evaluation/`, with progress tracked in issues [#1682](https://github.com/AmpersandTarski/Ampersand/issues/1682) and [#1683](https://github.com/AmpersandTarski/Ampersand/issues/1683).

## References

1. <a id="ref-1"></a>M. Budiu, T. Chajed, F. McSherry, L. Ryzhyk, V. Tannen. *DBSP: Automatic Incremental View Maintenance for Rich Query Languages.* Proc. VLDB Endow. 16(7), 2023. [arXiv:2203.16684](https://arxiv.org/abs/2203.16684)
2. <a id="ref-2"></a>A. Gupta, I. S. Mumick, V. S. Subrahmanian. *Maintaining Views Incrementally.* SIGMOD 1993. [doi:10.1145/170035.170066](https://doi.org/10.1145/170035.170066)
3. <a id="ref-3"></a>T. Griffin, L. Libkin. *Incremental Maintenance of Views with Duplicates.* SIGMOD 1995. [doi:10.1145/223784.223849](https://doi.org/10.1145/223784.223849)
4. <a id="ref-4"></a>C. Koch. *Incremental Query Evaluation in a Ring of Databases.* PODS 2010. [doi:10.1145/1807085.1807100](https://doi.org/10.1145/1807085.1807100)
5. <a id="ref-5"></a>T. Chajed. *Database Stream Processing Theory* — a Lean 4 formalisation of DBSP. [github.com/tchajed/database-stream-processing-theory](https://github.com/tchajed/database-stream-processing-theory)
6. <a id="ref-6"></a>T. Nipkow, L. C. Paulson, M. Wenzel. *Isabelle/HOL — A Proof Assistant for Higher-Order Logic.* LNCS 2283, Springer, 2002. [doi:10.1007/3-540-45949-9](https://doi.org/10.1007/3-540-45949-9)
7. <a id="ref-7"></a>K. Claessen, J. Hughes. *QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs.* ICFP 2000. [doi:10.1145/351240.351266](https://doi.org/10.1145/351240.351266)
8. <a id="ref-8"></a>A. Tarski. *On the Calculus of Relations.* Journal of Symbolic Logic 6(3), 1941. [doi:10.2307/2268577](https://doi.org/10.2307/2268577)
9. <a id="ref-9"></a>S. Joosten. *Relation Algebra as Programming Language Using the Ampersand Compiler.* Journal of Logical and Algebraic Methods in Programming 100, 2018. [doi:10.1016/j.jlamp.2018.04.002](https://doi.org/10.1016/j.jlamp.2018.04.002) — further Ampersand publications are collected on the [research page](../research.md).
