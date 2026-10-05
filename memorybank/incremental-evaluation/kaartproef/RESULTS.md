# The Artefactenkaart trial

Status: measured on 5 October 2026.

This note answers a question Stef Joosten asked after the RAP
and FC5 measurements showed no gain from delta maintenance:
is there an application on which the gain is measurable?
The candidate was the Artefactenkaart,
the web application of the Werkplaats that records which artefacts exist
and how they depend on each other.
Its model has four `ENFORCE` rules that compute a transitive closure,
and its users wait seconds for one write.

## What was measured, and on what

The trial ran on a copy of the model, not on the application the team uses.
The model is the one on `main` of the Werkplaats repository at commit `03a773c`,
compiled with Ampersand v5.9.8.
The stack is a separate compose project (`compose.proef.yml`) with its own containers and ports,
built on a prototype-framework image made locally from the branch that bundles compiler v5.9.8 (prototype pull request 480).

The population is synthetic.
A dump of the team's database was not available to this session,
so `zaai.py` generates one project with N documents in five layers.
Each document in a layer rests on two documents of the layer below,
through an `Afhankelijkheid` whose `gezien` equals the stamp of its source.
The population enters through `admin/import`,
so the application itself computes the derived relations.
The numbers below therefore say how this model behaves at a given size,
and they do not describe the team's population.
`momentopname.sh` takes a data-only dump of the team's database for whoever may read it;
the harness can then run on that population.

| N documents | dependencies | pairs in `bereikt` | rows in the database |
|---|---|---|---|
| 250 | 400 | 2 401 | 4 205 |
| 500 | 800 | 4 993 | 8 097 |
| 1 000 | 1 600 | 10 141 | 15 845 |

For comparison, an earlier session read 11 466 pairs in `bereikt` on the team's application,
so N = 1 000 is near its size on that one relation.
The database has 247 tables and no SQL view.

The workload is the one request that `bin/vastleg.py` of the Werkplaats sends:
a `PATCH` on the API `VastleggenProject`.
Two kinds were replayed, each from the same snapshot under every setting.
The first kind gives a document a new stamp and records a `Vastlegging`;
it leaves `hangtAf` as it is.
The second kind adds a dependency between two documents,
which changes `hangtAf` and so the closure `bereikt`.

Five settings were compared.
`off` is full re-evaluation at the close, the default.
`on` is the delta protocol (`transactions.deltaConjunctMaintenance`).
`shadow` runs both and compares them.
`off+skip` and `on+skip` add `transactions.skipCleanConjuncts`,
which keeps the result of a conjunct that the ExecEngine evaluated in this transaction with no mutation afterwards.

Each request was timed twice: once by the wall clock with tracing off,
and once with OpenTelemetry on, which gives one span per conjunct, per ExecEngine run and per close.
At N = 250 a setting has 20 timed requests per kind after 2 warm-ups;
at N = 500 and N = 1 000 it has 10 after 1.
Two of the 20 dependency requests at N = 250 were rejected in every setting,
because the random workload drew a pair that already had a dependency
and the model allows one per pair.

## Results

### The delta protocol is slower than full evaluation at every size

Median wall-clock time of one request, in milliseconds, tracing off.

| N | kind | off | off+skip | shadow | on | on+skip |
|---|---|---|---|---|---|---|
| 250 | new stamp | 209 | 171 | 300 | 264 | 180 |
| 250 | new dependency | 870 | 632 | 1 059 | 991 | 642 |
| 500 | new stamp | 389 | 298 | 557 | 465 | 308 |
| 500 | new dependency | 2 413 | 1 627 | 2 799 | 2 731 | 1 651 |
| 1 000 | new stamp | 666 | 584 | 1 064 | 863 | 490 |
| 1 000 | new dependency | 8 628 | 5 813 | 9 355 | 9 058 | 5 770 |

`on` is 20 to 30 percent slower than `off` on a new stamp,
and 5 to 14 percent slower on a new dependency.
`off+skip` is 12 to 23 percent faster than `off` on a new stamp,
and 27 to 33 percent faster on a new dependency.
`on+skip` and `off+skip` differ by less than the spread of the measurement,
except for the new stamp at N = 1 000 (490 against 584),
where the traced run does not repeat the difference (501 against 505).

The shadow runs compared the delta result with the full result 995 times in the untraced runs
and 995 times in the traced runs.
All comparisons were identical.

### The time is in the ExecEngine, and one conjunct takes most of it

Median per request with tracing on, in milliseconds, for a new dependency.

| N | setting | request | ExecEngine | close | conjunct queries in the close |
|---|---|---|---|---|---|
| 250 | off | 897 | 589 | 250 | 241 |
| 250 | on | 1 050 | 602 | 364 | 178 |
| 250 | off+skip | 660 | 578 | 21 | 12 |
| 1 000 | off | 8 732 | 5 776 | 2 862 | 2 847 |
| 1 000 | on | 9 159 | 5 753 | 3 292 | 2 581 |
| 1 000 | off+skip | 5 899 | 5 794 | 37 | 25 |

At N = 1 000 the ExecEngine takes 66 percent of the request and the close 33 percent.
The close spends its time re-evaluating conjuncts that the ExecEngine evaluated a moment before,
which is why `skipCleanConjuncts` removes it almost entirely.

One conjunct, `conj_182`, accounts for 7 609 of the 8 732 milliseconds at N = 1 000.
It belongs to the rule `bereikt |- hangtAf+`,
one of the two directions of `ENFORCE bereikt := hangtAf+`.
It has cost class `recursive` and no candidate queries, so the delta protocol cannot take it.
Its time grows from 537 to 1 872 to 7 609 milliseconds as N doubles twice,
while `bereikt` grows linearly.

### Where the delta protocol does apply, it costs more than the query it replaces

For a new stamp at N = 1 000 the close takes 207 milliseconds under `off`,
of which 196 are conjunct queries.
Under `on` the conjunct queries in the close fall to 12 milliseconds,
and the close as a whole rises to 410.
The difference is the protocol itself: its candidate queries and its updates of the violation table.
The conjunct concerned is `conj_203`, of the rule `moetEerst |- (I \/ bereikt);nietActueel`,
with cost class `scan` and candidate queries for three relations.

### The closure is cheap; the shape of the generated query is what costs

`sluiting.py` timed four queries on the database at N = 1 000, on the server, three times each.

| query | median |
|---|---|
| the closure alone (the recursive common table expression), counted | 36 ms |
| `conj_135`: the closure left-joined to the stored table `bereikt` | 70 ms |
| `conj_182` as generated: `bereikt` left-joined to the closure | 2 514 ms |
| the same question as `conj_182`, written as `bereikt EXCEPT` the closure | 136 ms |

The database is the one the last run left behind, so the closure has 10 321 pairs,
the 10 141 of the snapshot plus what the replayed writes added.
The three violation queries return no row, as they should.
An empty answer does not show that the generated form and the `EXCEPT` form ask the same question.
`sluiting-toets.py` therefore inserts two pairs into `bereikt` that are not in the closure,
inside a database transaction that it rolls back.
Both forms then return exactly those two pairs ([data/sluiting-toets.txt](data/sluiting-toets.txt)).
`conj_182` joins a stored table to a derived table that has no index,
and the database answers that with a scan of the derived table per row.
Written with `EXCEPT`, the same question takes 136 milliseconds, a factor 18 less.
The request evaluates this conjunct more than once, in the ExecEngine and again at the close,
which is how 2.5 seconds per evaluation becomes 7.6 seconds per request.

The cause is in how the compiler translates a difference `l - r`: as a left join of `l` onto `r` with a test for a missing partner (`maybeSpecialCase` in `SQL.hs`).
When `r` is a stored relation the join uses its index; when `r` is a computed term the database joins onto a temporary table without one.
`varianten.py` timed five ways to write the difference, for this conjunct and for `conj_203`, whose right-hand side is a composition without a closure
([data/varianten.txt](data/varianten.txt)).
For `conj_182` the generated form takes 2 621 ms, `not exists` 140, `except` 174 and `not in` 137.
For `conj_203` the generated form takes 232 ms, `not exists` 130, `except` 48 and `not in` 46.
All five forms return the same rows when two extra pairs are put into `l`.

## Reading

The trial finds no gain from the delta protocol on this model, and it shows three reasons.
The dominant cost is a closure under `ENFORCE`,
which lies outside the class the candidate calculus supports.
The cost that the protocol can reach is smaller than the protocol's own machinery.
And the ExecEngine evaluates its rules in full regardless of the setting,
so the protocol only ever touches the close.

The trial does find gain elsewhere, in this order of size.
`skipCleanConjuncts` takes a third off a request that changes a dependency,
with no change in what is committed.
The query shape of `r |- s+` holds a factor 18 on the dominant conjunct, in the compiler,
without any incremental machinery.
Maintaining the closure from the changed pairs is the step that would make the cost follow the change; it is Phase 5 of the plan.

This is the test of hypothesis O12 in [design-heuristics.md](../design-heuristics.md).

## Limits

The population is synthetic, layered and five deep; the team's population has another shape,
with other relations filled.
The measurements ran on a laptop with the amd64 image under emulation,
so the absolute times are higher than on a server;
the comparisons between settings ran under the same conditions.
At N = 500 and N = 1 000 a median rests on 10 requests.
The `EXCEPT` form was timed by hand on one database and is not implemented in the compiler;
whether it holds for other models is open.

## Reproducing

`meet.py op` builds and starts the stack;
it expects the model of the Werkplaats in `project/` next to the compose file,
and a local framework image tagged `local-pin-v598`.
`meet.py zaai N` installs, imports the synthetic population and takes a snapshot.
`meet.py draai N` replays the workload under the five settings;
with `--otel` it also reads the traces from Jaeger.
`samenvat.py` turns `data/` into the tables above,
and [data/samenvatting.md](data/samenvatting.md) is its output for this run.
`census.py` counts candidate queries and cost classes in a `conjuncts.json`;
its output for this model is [data/census.txt](data/census.txt).
