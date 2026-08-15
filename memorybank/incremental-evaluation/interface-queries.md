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

## Step 1 log (2026-08-15)

The starting position turned out better than the issue assumed, because the
RAP branch `feature/interactive-editor` had already modernized the sources.
Findings, each verified first-hand on this date:

1. **Both known obstacles are already resolved on RAP branch
   `feature/interactive-editor`.** That branch carries `RAP4/Dockerfile.v2`,
   which builds RAP on prototype-framework v2 with compiler v5.6.0, and a
   local `docker-compose.yml`; a container built from it (framework
   v2.1.0-local, compiler v5.6.0) has been serving RAP on `localhost:8088`
   since seven weeks. The framework pin `^1.8.5` lives in the repo-root
   `composer.json`, which the v2 Dockerfile does not use.
2. **RAP `origin/main` (the production sources) still trips the current
   compiler**, exactly as the issue predicted: `ampersand proto` v5.9.7 stops
   at `src/RAP4.adl:196:16 — UnexpectedChar '_'` (relation names such as
   `pf_ifcRoles`; the names-and-labels change made `_` illegal in bare
   identifiers). The baseline deployment therefore builds with the *old*
   toolchain (framework v1.18.1 with its bundled compiler), which is also the
   production-faithful choice.
3. **The modernized sources compile clean with the current compiler.**
   `ampersand proto --no-frontend` v5.9.7 (branch build
   `incremental-evaluation:392e54352`) generates the RAP backend in ~2.5 s,
   with oscillation-risk warnings as the only diagnostics.
4. **Work setup.** RAP work for #1687 lives on RAP branch
   `incremental-evaluation` (worktree `~/git/RAP-incremental-evaluation`,
   based on `feature/interactive-editor`'s committed top `a73cb9b`), so the
   interactive-editor line and its running deployment stay undisturbed.
   `Dockerfile.v2` there now builds from the *published* images
   `ampersandtarski/ampersand:v5.9.7` and
   `ampersandtarski/prototype-framework:v2.6.0` instead of local smoke-test
   images, and `docker-compose.1687.yml` deploys baseline and current-stack
   RAP side by side (`:8081` v1-as-is, `:8089` v2.6.0), each with its own
   MariaDB. Design choice DC-13 records the layout and the rejected
   alternatives.
5. **Both deployments stand and pass their smoke test.** The v1-as-is
   image builds from `origin/main` with context `RAP4/` (the workflow's
   choice; a root context fails on `COPY customizations`). The v2.6 image
   builds clean including the Angular frontend and the two frontend `sed`
   patches, whose grep anchors still hold in framework v2.6.0. On both
   ports the installer reports success and the anonymous navbar serves the
   Login interface — on `:8089` that also confirms the SIAM role bridge
   works unchanged on v2.6.0.
6. **Step 2 lead.** The RAP repo already contains Gatling material
   (`gatling-3-7-2/` and `Testing/Gatling/` with RAP request definitions);
   the load harness of step 2 should start there rather than from scratch.

Step 1 of the plan is herewith complete: the baseline stands, and the
current compiler and framework carry RAP without local patches.

One point carries into the design of step 2: the two deployments run
slightly different models. The baseline serves `origin/main`'s model; the
current-stack deployment serves the modernized sources, which besides the
syntax conversion also carry the single-role SIAM changes of the
interactive-editor line. The step-2 harness must therefore restrict its
fixed mix to pages both models serve identically (script browsing,
diagnosis), or the model drift must first be assessed and accepted
explicitly. The alternative — a minimal, purely syntactic modernization of
`origin/main` as the v2 source base — remains open if the drift turns out
to matter.
