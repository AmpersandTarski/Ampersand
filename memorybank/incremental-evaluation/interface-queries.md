# Interface queries as a second application of the delta calculus

Status: research line opened as issue
[#1687](https://github.com/AmpersandTarski/Ampersand/issues/1687)
(2026-08-14). This note records the assessment behind that issue, so the
line of thought does not interrupt the phase-4 work on rules.

## The question

Rules and interfaces both compile to SQL over the same term language, and the
prototype's interfaces together generate exactly the change stream that DBSP
takes as input. Does incremental evaluation with Z-sets pay off for interface
queries the way it does for violation queries?

## Why the economics differ from the rule track

A conjunct query is global. It produces the full violation set, is read on
every transaction close (invariants block the commit), and is already
materialized in `__conj_violation_cache__`. Many reads, whole-set reads:
maintenance pays.

An interface query is a parameterized point query. The compiler generates one
query per interface object with a placeholder for the source atom
(`broadQueryWithPlaceholder`, `src/Ampersand/FSpec/SQL.hs:35`, used from
`src/Ampersand/Output/ToJSON/Interfaces.hs:166`); the PHP runtime substitutes
the atom of the opened resource. The query runs when a human opens a page, and
for most fields — stored relations — it is an index lookup. Incremental view
maintenance pays when read frequency times read cost exceeds the maintenance
cost per write; for interfaces, reads are rare and already cheap.
Materializing every interface expression buys write amplification on every
transaction for views nobody has open.

## Where it does pay

1. **Expensive derived expressions** — long compositions, closures — that are
   recomputed integrally on every page open. This win is real but bounded by
   the share of such queries in an actual model; measure before building.
2. **Shared materialization with conjuncts.** When an interface field
   coincides with (a subterm of) a conjunct table the rule track already
   maintains, the interface reads that table at no extra maintenance cost.
   This needs only recognition of the overlap at generation time.

## The qualitatively new capability: push

The input side already exists once phase 3/4 lands: the delta tables are the
change stream all interfaces jointly produce. Feeding that stream through the
delta version of an interface expression yields not "the same page faster"
but *the patch on an open page*: when user A commits, the delta of expression
E, filtered to the atoms user B has open, is exactly what to push to B over a
websocket. That is the subscription model of Materialize and Feldera, and it
would make prototypes reactive across users. Within one request the same idea
applies: after an ExecEngine run the frontend currently refetches the whole
interface tree; an outgoing delta lets the response carry only the change.

Three complications make this its own line of thought, separate from the rule
track:

- **SESSION dependence.** Interfaces are SESSION-rooted, so the "view" differs
  per session. The session atom must be treated as a parameter; the fields
  below the root are usually session-free, so this looks manageable, but it
  belongs in the design.
- **Subscription administration.** The framework must know which client has
  which atom open in which interface, to filter and route deltas. That is new
  runtime state.
- **The frontend contract changes.** Push over websockets and patch-based
  updates touch the Angular frontend and the API, not only the query layer.

The push track is out of scope of #1687, which covers the performance
question only.

## Validation vehicle: RAP

RAP is the largest Ampersand application in production and runs on the
standard prototype framework. The plan of approach stands in #1687 as five
steps with checkboxes: (1) deploy RAP and build it with the current compiler,
(2) profile the baseline under a volume-scaling load harness, (3) design
compiler + framework changes with design choices and proof obligations, (4)
implement and deploy side by side on another port, (5) run the same harness
against both and test the hypothesis that latency stays near-constant as the
population grows.

Two obstacles are known going into step 1, both observed earlier and to be
re-verified: RAP's ADL sources use older syntax the current compiler rejects,
and RAP pins prototype-framework `^1.8.5` while current work targets v2.6.x.
Step 4 requires both resolved.
