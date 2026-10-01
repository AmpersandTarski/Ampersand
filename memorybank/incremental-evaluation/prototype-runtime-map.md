# Prototype-framework runtime map (ground truth for phases 3/4)

Repo: https://github.com/AmpersandTarski/prototype
Commit: aa459d86d8ce023ce454d4e90964eccc89331e1e (shallow clone, default branch)
Date of survey: 2026-08-13
PHP source root: `backend/src/Ampersand/`. All `file:line` references below are
relative to the repo root at this commit.

## 1. Transaction lifecycle

The `Transaction` class (`backend/src/Ampersand/Transaction.php`) is an
application-level unit of work, distinct from the MariaDB transaction it drives.
One open transaction at a time is enforced in the constructor
(Transaction.php:120-125). `AmpersandApp::newTransaction()`
(backend/src/Ampersand/AmpersandApp.php:486-492) creates it;
`getCurrentTransaction()` (AmpersandApp.php:497-506) reuses the open one.

Flow of a client PATCH on an interface
(`ResourceController::putPatchPostResource`,
backend/src/Ampersand/Controller/ResourceController.php:80-158):

1. `newTransaction()` (ResourceController.php:96).
2. `Resource::patch()` interprets the JSON-Patch ops
   (backend/src/Ampersand/Interfacing/Resource.php:274) and maps
   replace/add/remove onto `InterfaceExprObject::set/add/remove/removeAll`
   (backend/src/Ampersand/Interfacing/InterfaceExprObject.php:558,593,623,648),
   which end in `Relation::addLink` / `Relation::deleteLink`
   (backend/src/Ampersand/Core/Relation.php:238,263) and
   `Concept` atom add/delete (AtomEvent dispatch at
   backend/src/Ampersand/Core/Concept.php:576,659).
3. Each `addLink`/`deleteLink` calls
   `$transaction->addAffectedRelations($this)` (Relation.php:245,267), which
   registers the storage plug and lazily issues `START TRANSACTION` on first
   touch (Transaction.php:443-455 → MysqlDB.php:381-389). So the DB transaction
   starts at the first mutation, not at request start; reads outside it run in
   autocommit.
4. The SQL INSERT/DELETE/UPDATE for the pair happens immediately inside the open
   DB transaction (`MysqlDB::addLink` MysqlDB.php:636-663, `deleteLink`
   MysqlDB.php:668-702). After the write, a `LinkEvent::ADDED/DELETED` is
   dispatched with the exact pair (Relation.php:256,273).
5. `$transaction->runExecEngine()` (ResourceController.php:126) — see §4.
6. `$transaction->close($dryRun, false, $defer)` (ResourceController.php:137 →
   Transaction.php:294-342): it (re)evaluates every affected conjunct
   (Transaction.php:316-318), then `checkInvariantRules()`
   (Transaction.php:321, implemented at 501-516: filters affected rules to
   invariants and reads their violations from the just-evaluated in-memory
   cache, `forceReEvaluation = false` via `RuleEngine::getViolations`,
   backend/src/Ampersand/Rule/RuleEngine.php:30-38).
7. Commit decision (Transaction.php:324-338): commit iff invariants hold, or if
   `transactions.ignoreInvariantViolations` / the per-call flag is set;
   otherwise SQL `ROLLBACK`. `commit()` (Transaction.php:347-366) first persists
   the conjunct-violation cache rows (`persistCacheItem`, line 352-356) and only
   then issues `COMMIT` per storage (line 358-361 → MysqlDB.php:394-400), so
   cache rows and data commit atomically on the same connection.
8. After close, `AmpersandApp::checkProcessRules()` reports signal violations
   from the cache (ResourceController.php:142, see §2).

A `dryRun=true` query param evaluates conjuncts + invariants and then rolls back
(Transaction.php:286-289, 324-326). A `defer=true` param (bulk-load mode, DC-07)
commits without evaluating conjuncts at all (Transaction.php:302-313).

## 2. Conjunct evaluation and the existing violation cache

`Conjunct` (backend/src/Ampersand/Rule/Conjunct.php) holds the raw
`violationsSQL` from conjuncts.json (Conjunct.php:94). `evaluate()`
(Conjunct.php:189-216) runs the full query via `MysqlDB::execute`, tags each
row with the conjunct id, and stores the result in a PSR-6 cache item
(`saveDeferred`, line 203). `_SESSION` is substituted into the SQL at call time
(Conjunct.php:146).

**A conjunct violation cache exists today**, and it is a database table:
`MysqlConjunctCache` (backend/src/Ampersand/Plugs/MysqlConjunctCache/MysqlConjunctCache.php)
is a PSR-6 pool over the table `__conj_violation_cache__`
(MysqlConjunctCache.php:44), created by the framework's static DDL
(backend/src/Ampersand/Plugs/MysqlDB/DBStructureQueries.sql:4-13: columns
conjId, src, tgt, ts_insertupdate, index on conjId). Persisting a cache item
replaces all rows of that conjunct: DELETE by conjId, then bulk INSERT
(MysqlConjunctCache.php:189-206). So the cache is a full materialized violation
set per conjunct, refreshed wholesale — never incrementally.

Cache semantics quirk: `MysqlConjunctCacheItem::isHit()` always returns true
(MysqlConjunctCacheItem.php:104-107), and the pool's `hasItem` likewise
(MysqlConjunctCache.php:124-127) — "a CacheItem for each conjunct always
exists, even when there are no violations". Consequently
`Conjunct::getViolations(false)` (Conjunct.php:167-184) always answers from the
cache table and only re-runs SQL when `forceReEvaluation = true`. A settings
flag `transactions.skipUniInjConjuncts` skips UNI/INJ conjuncts entirely
(Conjunct.php:169-173, workaround for issue #535).

Signal vs invariant rules: rules.json is split into `signals` and `invariants`
(Model.php:264-276); each conjunct knows its `invariantRuleNames` and
`signalRuleNames` (Conjunct.php:95-96). Invariants are checked per transaction
against the fresh in-memory results (§1 step 6) and block the commit. Signals
are *not* checked at commit; they are read from the `__conj_violation_cache__`
table on demand: `AmpersandApp::checkProcessRules()` (AmpersandApp.php:797-809)
→ `RuleEngine::getViolationsFromCache` (RuleEngine.php:47-72), also exposed at
`GET .../ruleengine/...` via `RuleEngineController::getSignalViolations`
(backend/src/Ampersand/Controller/RuleEngineController.php:17-32, whose own
comment names the table). An admin endpoint re-evaluates everything and
refreshes the cache (`evaluateAllRules`, RuleEngineController.php:34-59), as
does the tail of `reinstall` (AmpersandApp.php:629-633).

## 3. Affected-conjuncts wiring

The compiler's `affectedConjuncts` lists are loaded once at model init:
per relation from relations.json (Relation.php:131-134) and per concept from
concepts.json (backend/src/Ampersand/Core/Concept.php:168-171); both expose
`getRelatedConjuncts()` (Relation.php:170-173, Concept.php:390-393).

Granularity is **per transaction, not per statement**: each mutation only
appends the Relation/Concept object to the transaction's `affectedRelations` /
`affectedConcepts` lists, deduplicated by `in_array` on entry
(Transaction.php:426-438, 443-455). At close, `getAffectedConjuncts()`
(Transaction.php:462-476) unions the conjunct lists of all affected concepts
and relations and deduplicates with `array_unique`. `getAffectedRules()`
(Transaction.php:483-494) maps those conjuncts to unique rule names. So a
transaction touching relation r evaluates every conjunct mentioning r exactly
once, with its full violation query — the partial incrementality is *which*
conjuncts, never *how* a conjunct is computed.

## 4. ExecEngine

`Transaction::runExecEngine()` (Transaction.php:158-209) is the rerun loop:
rules to check start as the affected rules (or all, on request), each
configured engine runs `checkFixRules`, and the loop repeats while any rule was
fixed and `execengine.autoRerun` is set, guarded by `execengine.maxRunCount`
(Transaction.php:196-201; exceeding it logs an error and stops — it does not
throw). After each pass the rule set is recomputed from the (grown) affected
set (Transaction.php:203).

`ExecEngine::checkFixRules` (backend/src/Ampersand/Rule/ExecEngine.php:150-192)
calls `$rule->checkRule(true)` — **forcing full re-evaluation of the rule's
conjunct queries on every iteration** (ExecEngine.php:167; Rule.php:215-240).
It then iterates the complete violation set and fixes each violation:
`fixViolation` (ExecEngine.php:197-247) splits the rendered violation message
on `{EX}` into function calls, splits parameters on `;` (or `_;`), evaluates
`{php}` parameters, and invokes registered closures
(`ExecEngine::registerFunction`, ExecEngine.php:288-298). Those closures call
the same `Relation::addLink`/`deleteLink`/`Atom` APIs, so their writes re-enter
the affected-tracking machinery — which is what makes the rerun loop converge.

Nothing in the engine requires the *full* violation set semantically: it
processes violations pair-by-pair. A delta feed (only new violations since the
previous run) would slot into `checkFixRules` provided fixes that *remove*
violations need no action anyway. The one caveat is that today's
`checkRule(true)` is also what refreshes the in-memory conjunct results that
the subsequent invariant check and cache persist reuse.

## 5. Database access layer

One class: `MysqlDB` (backend/src/Ampersand/Plugs/MysqlDB/MysqlDB.php),
implementing Concept/Relation/Ifc/View plug interfaces over a single `mysqli`
connection with `sql_mode = ANSI,TRADITIONAL` (MysqlDB.php:135). All SQL —
violation queries, interface queries, link writes, cache writes — funnels
through `execute()`/`doQuery()` (MysqlDB.php:264-347). Strings are escaped and
interpolated; the `prepare()` method (MysqlDB.php:282) exists but conjunct
evaluation does not use it.

The framework does **not** wrap the whole HTTP request in a DB transaction:
`START TRANSACTION` fires lazily on the first registered mutation (§1 step 3),
and COMMIT/ROLLBACK come from `Transaction::commit/rollback`
(Transaction.php:358-361, 373-376 → MysqlDB.php:394-410). GET requests never
open one.

Install/reinstall: `reinstallStorage` (MysqlDB.php:180-191) drops and recreates
the database, then executes the compiler-generated `database.sql` as a
multi-query, then the framework's own static `DBStructureQueries.sql` (the
cache table and `__ampersand_model_history__`). **Extra generated tables ride
along for free**: anything the compiler adds to database.sql is executed
verbatim. The model hash from settings.json (`compiler.modelHash`,
Model.php:179) is registered per install (MysqlDB.php:193-196) and compared on
every request by `VerifyChecksumMiddleware`
(backend/src/Ampersand/API/Middleware/VerifyChecksumMiddleware.php:23) — in
non-production it warns the user; it does not block. A hash change simply asks
for reinstall/migration, which is exactly the path that would (re)create delta
and violation tables.

## 6. Contract consumers

`Model` (backend/src/Ampersand/Model.php:148-163) requires the nine generated
files to exist, then each loader does plain `json_decode` + array indexing.
`Conjunct::__construct` reads exactly four fields of a conjuncts.json entry:
`id`, `violationsSQL`, `invariantRuleNames`, `signalRuleNames`
(Conjunct.php:93-96). There is no schema validation and no contract version
check beyond the model hash — **an added optional field in conjuncts.json (or
relations.json/concepts.json) is silently ignored by this framework and breaks
nothing**. Required-field removal or renaming would fail at load time with PHP
array-access errors, caught only implicitly.

RAP uses this same framework: AmpersandTarski/RAP's `composer.json` declares
`"require-dev": { "ampersandtarski/prototype": "^1.8.5" }` (verified via the
GitHub API on 2026-08-13). Not verified: which exact framework tag RAP's
deployed images pin, or whether RAP patches the framework.

## 7. Feasibility notes for the delta design (factual)

Where the runtime knows the exact changed pairs:

- `MysqlDB::addLink`/`deleteLink` (MysqlDB.php:636,668) receive the concrete
  `Link` — table, src, tgt — one call per pair. A delta-table INSERT here (or
  in `Relation::addLink/deleteLink`, Relation.php:238,263, before the event
  dispatch) runs inside the same DB transaction as the data write.
- Alternatively, the event bus: `LinkEvent::ADDED/DELETED`
  (backend/src/Ampersand/Event/LinkEvent.php:11-13, dispatched at
  Relation.php:256,273) and `AtomEvent::ADDED/DELETED` (Concept.php:576,659)
  carry pair/atom plus the Transaction — a listener could maintain delta tables
  with zero changes to core classes.
- Caveat: the set-level operations `deleteAllLinks` (Relation.php:282-293,
  MysqlDB.php:711-738), `emptyRelation` (Relation.php:298-309), and
  `MysqlDB::removeAtom`'s column-NULLing (MysqlDB.php:500-522) mutate many
  pairs in one SQL statement and dispatch **no per-pair events**. A delta hook
  must either enumerate those pairs first or use SQL of the form
  `INSERT INTO delta SELECT ... FROM r WHERE ...` before the destructive
  statement. Atom deletion cascades (`Concept::deleteAtom` → related relations)
  route through `deleteAllLinks`, so this caveat is on the main path.

Where violation-cache maintenance slots in:

- The natural seam is `Transaction::close` step "evaluate affected conjuncts"
  (Transaction.php:316-318) plus `commit`'s `persistCacheItem` loop
  (Transaction.php:352-356): replace "run full query, DELETE+INSERT all rows"
  with "run per-(conjunct, relation) delta queries against the transaction's
  delta tables, apply row deltas to the materialized table". The existing
  `__conj_violation_cache__` already *is* a materialized violation table with
  the right key (conjId, src, tgt) — it lacks only a weight/count column and
  incremental maintenance. Because cache writes precede COMMIT on the same
  connection (§1 step 7), delta maintenance inherits atomicity for free.
- Readers need no change: invariant checking consumes in-memory results of the
  evaluation step (Transaction.php:501-516), and signals already read the cache
  table (§2). Keeping `Conjunct::evaluate()` as fallback/self-check matches the
  plan's Phase 4.
- The ExecEngine's `checkRule(true)` (ExecEngine.php:167) is the other full-
  query site; with delta-maintained tables it could read the maintained table
  instead, but its per-iteration semantics (fix, then re-check) must keep the
  table fresh within the loop — i.e. delta maintenance must run per ExecEngine
  iteration, not only at close.

Not verified in this survey: frontend behavior (Angular), the
`ampersandtarski/prototype` composer package version at this commit, RAP's
deployed pin (only its composer.json constraint), and runtime behavior under
concurrent requests (single mysqli connection per PHP process; advisory locks
exist, MysqlDB.php:557-573, used for session GC).
