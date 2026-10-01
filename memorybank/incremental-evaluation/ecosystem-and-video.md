# Incremental evaluation: the DBSP ecosystem and its literature

Research notes supporting a plan for incremental evaluation of rule violations in the
Ampersand compiler. Sources were gathered on 2026-08-13 from the pages linked below.

## The Developer Voices episode

**"What If Every SQL Query Could Update Incrementally? (with Lalith Suresh)"** —
Developer Voices podcast, host Kris Jenkins, guest Lalith Suresh (CEO of Feldera).
Published 2026-07-08, duration 1h05.
Video: <https://www.youtube.com/watch?v=CyvnH8OUCUA> ·
Audio: <https://podcasts.apple.com/us/podcast/what-if-every-sql-query-could-update-incrementally/id1687271887?i=1000775966626>

### Key talking points

- **The waste of full re-evaluation.** Since the 1980s, database engines faced with a
  cached query result and newly arrived data throw the cache away and recompute from
  scratch. Some engines maintain "simple" queries incrementally, with arbitrary rules
  about what counts as simple. The DBSP paper proves that *every* query is simple
  enough: any SQL query can be maintained incrementally.
- **Z-sets as the change currency.** A Z-set is a collection where each element carries
  an integer weight. An insert is a row with weight +1, a delete the same row with
  weight -1. That turns database changes into values you can add and subtract, so
  deltas compose algebraically instead of needing per-operator special cases.
- **Incrementalization as a compiler pass.** Four DBSP operators — lift, delay
  (z⁻¹, borrowed directly from digital signal processing), differentiation, and
  integration — suffice to mechanically rewrite any query circuit into its incremental
  version. The transformation is deterministic; no per-query cleverness is needed.
- **State.** The episode distinguishes stateless operators (filter, map) from stateful
  ones (join, aggregate), and shows how the *delta join* falls out of the theory for
  free rather than being a hand-crafted algorithm.
- **Feldera's architecture.** A standalone query engine with its own storage layer
  and an Apache Calcite front-end for SQL parsing/planning; backfills (bootstrapping a
  new view over existing data) are named the real engineering Achilles heel.
- **Positioning.** How this differs from stream processors such as Kafka Streams,
  Flink, and ksqlDB, which expose incremental semantics only for restricted operator
  subsets and push consistency problems onto the user.

### Show-notes links (recovered from the episode description)

- Feldera: <https://www.feldera.com/>
- Feldera sandbox: <https://try.feldera.com/>
- Feldera on GitHub: <https://github.com/feldera/feldera>
- DBSP Rust crate: <https://crates.io/crates/dbsp>
- DBSP paper (arXiv, VLDB 2023): <https://arxiv.org/abs/2203.16684>
- Apache Calcite: <https://calcite.apache.org/>
- Kafka Streams: <https://kafka.apache.org/documentation/streams/>
- Apache Flink: <https://flink.apache.org/>
- ksqlDB: <https://ksqldb.io/>
- Developer Voices Patreon: <https://patreon.com/DeveloperVoices>

## Feldera in practice

Repository: <https://github.com/feldera/feldera> (MIT license, ~2,000+ stars,
~8,600 commits, active development; fault tolerance is marked "preview").

- **Languages and structure.** The SQL-to-DBSP compiler is written in **Java** on top
  of Apache Calcite; the incremental computation core (the DBSP runtime) is **Rust**;
  the WebConsole is TypeScript.
- **Feeding changes in.** Connectors for Kafka, HTTP, CDC streams, S3, data lakes and
  warehouses; plus ad-hoc HTTP ingestion. Input arrives as change streams
  (insert/delete rows, i.e. Z-set deltas).
- **Getting outputs.** Declared SQL views are maintained continuously; results are
  readable at any time over the HTTP API, and output change streams can be pushed to
  sinks (Kafka etc.).
- **Embeddability.** Two levels. The full Feldera pipeline manager is a service
  (Docker or source build) with the Java compiler in the loop. The **`dbsp` Rust
  crate** on crates.io is the bare runtime: embeddable in a Rust program, circuits
  built programmatically, no SQL front-end. There is no Haskell or C API.
- **Storage.** Feldera is a computation engine, not a database: it keeps operator
  state (indexes, integrals) in its own storage layer that spills to disk when state
  exceeds RAM, but the source of truth remains external. It sits *beside* a database,
  consuming its change feed.

## Alternative engines and approaches

| System | Language | License | Approach | Status |
|---|---|---|---|---|
| [Feldera](https://github.com/feldera/feldera) | Rust core, Java SQL compiler | MIT | DBSP circuits; full SQL | Active; company-backed |
| [Materialize](https://github.com/MaterializeInc/materialize) | Rust | BSL 1.1 (each release → Apache 2.0 after 4 years) | Differential dataflow over timely dataflow; Postgres wire protocol | Mature commercial product |
| [differential-dataflow](https://github.com/TimelyDataflow/differential-dataflow) (McSherry) | Rust library | MIT/Apache-2.0 | Multidimensional timestamps; handles iteration/recursion; no SQL layer | Mature library, actively maintained |
| [timely-dataflow](https://github.com/TimelyDataflow/timely-dataflow) | Rust library | MIT | Low-level dataflow substrate under differential | Mature library |
| [pg_ivm](https://github.com/sraoss/pg_ivm) | C (PostgreSQL extension) | PostgreSQL license | Immediate maintenance of materialized views inside PostgreSQL (counting-based) | v1.14 (2026-04); PG 13–18; not recommended for production by several reviewers |
| [DDlog](https://github.com/vmware/differential-datalog) | **Haskell** compiler → Rust runtime | MIT | Incremental Datalog compiled onto differential dataflow | **Archived** by VMware |
| [pydbsp](https://github.com/brurucy/pydbsp) | Python | MIT | Educational DBSP implementation | Small, illustrative |
| MariaDB / MySQL | — | — | No materialized views at all, hence no IVM; workarounds are triggers, Flexviews (unmaintained, ported to LeapDB), or external engines (e.g. Epsio) | Verified absent |

Notes per row:

- **Materialize** ([license](https://materialize.com/docs/license/)) is the direct
  productization of McSherry's differential dataflow; its consistency model
  (linearizability over dataflows) is a differentiator. BSL restricts offering it as a
  service but not internal use.
- **pg_ivm** shows the *in-database* route: views maintained by triggers inside the
  DBMS, counting algorithm, limited query class (restrictions on outer joins,
  aggregates, recursion). It requires PostgreSQL — not portable to MariaDB.
- **MariaDB/MySQL**: confirmed that neither has native materialized views or IVM
  ([epsio overview](https://www.epsio.io/blog/materialized-views-in-mariadb),
  [FromDual](https://fromdual.com/mysql-materialized-views)). Anything incremental on
  MariaDB must be built in the application layer (which is what Ampersand's generated
  code would be) or via binlog-reading external tools.
- **Haskell**: no maintained differential-dataflow port or incremental-Datalog engine
  exists in the Haskell ecosystem. The closest artifact is DDlog, whose *compiler* is
  written in Haskell but which emits Rust and is archived. General-purpose incremental
  computation libraries (e.g. Adapton-style, `incremental` ports) exist but none is a
  relational/Datalog engine. Building on DBSP theory in Haskell means implementing the
  (small) operator algebra oneself; the theory paper is explicitly designed to make
  that mechanical.

## Annotated literature

1. **Gupta & Mumick, "Maintenance of Materialized Views: Problems, Techniques, and
   Applications", IEEE Data Eng. Bulletin 18(2), 1995.** The classic survey that framed
   IVM: which views, under which change types, with which auxiliary data. Still the
   standard vocabulary (immediate vs deferred maintenance, self-maintainability).
2. **Gupta, Mumick & Subrahmanian, "Maintaining Views Incrementally", SIGMOD 1993.**
   Introduces the **counting algorithm** (store per-tuple derivation counts; the
   ancestor of Z-set weights restricted to ℕ) and **DRed** (delete-and-rederive) for
   recursive views: over-delete, then re-derive what still has support. pg_ivm's
   engine descends from this line.
3. **Green, Karvounarakis & Tannen, "Provenance Semirings", PODS 2007.** Shows that
   relational evaluation parameterized by a commutative semiring uniformly captures
   bag semantics, counting, and provenance. Z-sets are exactly this construction over
   the ring ℤ — the algebraic ancestor of DBSP (Tannen co-authored both).
4. **Koch, "Incremental Query Evaluation in a Ring of Databases", PODS 2010.** Makes
   deltas first-class by moving from semirings to a *ring* (deletions = additive
   inverses) and shows delta queries of delta queries terminate — recursive
   incrementalization ("higher-order IVM").
5. **Koch et al., "DBToaster: Higher-order Delta Processing for Dynamic, Frequently
   Fresh Views", VLDB J. 23(2), 2014.** The engine built on (4): compiles SQL into
   recursively maintained delta hierarchies, orders of magnitude faster than
   first-order IVM for aggregate-heavy queries.
6. **McSherry, Murray, Isaacs & Isard, "Differential Dataflow", CIDR 2013.**
   Generalizes incremental computation to *partially ordered* timestamps, so
   incremental updates and fixed-point iteration compose — the basis of Materialize
   and DDlog.
7. **Budiu, Chajed, McSherry, Ryzhyk & Tannen, "DBSP: Automatic Incremental View
   Maintenance for Rich Query Languages", VLDB 2023 (arXiv 2203.16684; extended
   version in The VLDB Journal, 2025; best-paper award).** The episode's subject: a
   four-operator stream algebra in which incrementalization is a mechanical circuit
   transformation covering the whole of SQL including recursion.
8. **Chajed, Lean formalization of DBSP:
   <https://github.com/tchajed/database-stream-processing-theory>.** Machine-checked
   proofs (Lean, building on mathlib) of all theorems in the DBSP paper: streams,
   delay, lifting, fixpoints, differentiation/integration, linearity, the
   incrementalization operator, and the Z-set relational operators. (Lean 3-era
   mathlib, per the repo — verify before reuse; per the house rule, treat
   "machine-checked" as confirmed only after inspecting this repo.)

**Which literature Ampersand's case resembles most.** Ampersand evaluates
relation-algebra terms over *binary* relations with *set* semantics, and its queries
(rule violations) are non-recursive joins, unions, differences and complements over
small per-transaction deltas. That profile matches the Z-set/ring line (3, 4, 7)
directly: binary relations are the simplest possible schema for Z-sets, set semantics
is the `distinct` special case DBSP treats explicitly, and difference/complement — the
awkward cases for counting-based IVM — are native in a ring. Recursion (DRed,
differential dataflow's contribution) is only needed for Ampersand's transitive
closure operators (Kleene star), which is exactly the part DBSP handles with its
nested-stream fixpoint construction.

## Observations for Ampersand (factual)

- **Licensing.** Feldera and the `dbsp` crate are MIT; differential/timely dataflow
  are MIT; Materialize is BSL (4-year delayed Apache 2.0); pg_ivm is PostgreSQL-licensed
  but PostgreSQL-only. Nothing here restricts studying or reimplementing the algorithms;
  the DBSP paper itself presents the transformation as a small set of rewrite rules.
- **Embeddability.** Feldera as a service needs Rust + Java + its own storage; the
  bare `dbsp` crate is Rust-only and has no SQL or Haskell interface. No existing
  engine embeds into a Haskell compiler or into MariaDB.
- **MariaDB offers no IVM.** Confirmed: no materialized views, no incremental
  maintenance; third-party binlog approaches (Flexviews) are unmaintained. Incremental
  evaluation for Ampersand prototypes therefore lives in generated code or in a
  sidecar engine, not in the DBMS.
- **Maturity.** Differential/timely dataflow and Materialize are the most battle-tested;
  Feldera is younger but active and company-backed with fault tolerance still in
  preview; pg_ivm is maintained but flagged by reviewers as not production-grade;
  DDlog is archived.
- **Theory portability.** The DBSP formalization exists as machine-checked Lean proofs,
  and the operator algebra is small (four operators), which is the property that makes
  a from-scratch implementation in a Haskell code generator a bounded task rather than
  a research project.
