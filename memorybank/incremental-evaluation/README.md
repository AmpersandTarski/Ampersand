# Incremental evaluation — research folder

Working notes for the "Incremental evaluation" feature: DBSP-style incremental
maintenance of rule-violation queries in Ampersand prototypes, inspired by
"DBSP: Automatic Incremental View Maintenance for Rich Query Languages"
(arXiv 2203.16684, VLDB 2023) and the [Developer Voices episode with Lalith
Suresh](https://www.youtube.com/watch?v=CyvnH8OUCUA).

Contents:

- [plan.md](plan.md) — the plan of approach: chosen direction, phases with
  exit criteria, proof and paper tracks, risks. **Start here.**
- [DesignChoices.md](DesignChoices.md) — the design-choice register (OK-1 …),
  each choice with its considerations and rejected alternatives.
- [delta-calculus.md](delta-calculus.md) — Phase 1: the desugaring and delta
  rules the implementation and the Isabelle proofs follow.
- [bench/](bench/) — the benchmark model and measurement results of
  `ampersand incremental-bench`.
- [dbsp-paper-study.md](dbsp-paper-study.md) — study note on the DBSP paper:
  Z-sets, the stream calculus, the incrementalization algorithm, and the mapping
  to Ampersand's relation algebra.
- [ampersand-architecture-map.md](ampersand-architecture-map.md) — the compiler
  pipeline with verified file:line references and the candidate intervention
  points for this feature.
- [ecosystem-and-video.md](ecosystem-and-video.md) — the podcast episode's content
  and links, Feldera and alternative engines, annotated IVM literature.

Keep this folder the single place for results of this research line; update
plan.md when a phase finishes or an insight changes the route.
