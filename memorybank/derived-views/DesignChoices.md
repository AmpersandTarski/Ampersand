# Design choices — derived relations as views

Register of design choices for the work on issue
[#1744](https://github.com/AmpersandTarski/Ampersand/issues/1744): a derived relation
(`ENFORCE r := term`) generated as a database view, the external schema named in the
database, and declarative constraints. Numbers are stable and never reused; the current
state stands here, the history lives in git. Where a choice builds on one of the
incremental-evaluation line, it refers to that register
(`memorybank/incremental-evaluation/DesignChoices.md`, "IE-DC-n" below). Open questions
sit at the bottom under "Still to decide".

## The derived relation

**A derived relation `ENFORCE r := term` is generated as a database view on the SQL of `term`, behind the flag `--derived-views`; without the flag nothing changes.**
*DC-1 · valid · 2026-10-27 · origin: issue #1744, requirements R1 and R5*

With the flag, `database.sql` carries `CREATE VIEW "r" AS <SQL of term>` for every
such statement, after all tables and in dependency order; `r` gets no column, no link
table, no delta table, no `Compute` rules and no conjuncts. Every generated query
reads `r` from the view as it read it from the table.

*Considerations:*

1. The view can never lag behind its definition, and the two `Compute` rules per
   statement leave the ExecEngine's loop: in the Landeneisenregister that is 78 rules
   and 19% of the SQL text of all conjuncts (the research note attached to the issue).
2. The alternative set aside is to keep every derived relation stored and only
   maintain it incrementally (issue #1675). That lowers the cost of maintaining a
   stored copy, but leaves the copy, and the ExecEngine in the write path. The two
   are not exclusive: what `--materialize` keeps stored, the delta route maintains.
3. The flag is off in this release because a prototype framework that does not
   know views cannot delete an atom that occurs in a derived relation:
   `Concept::deleteAllLinksWithAtom`, `deleteAllSpecializationLinks`,
   `Concept::mergeAtoms` and `Relation::emptyRelation` write to every relation of
   a concept, and MariaDB refuses a `DELETE` or `UPDATE` on a view. The framework
   learns the field `storage` of `relations.json` (DC-4) in a pull request of its
   own; the default of the flag can flip when a framework release carries it.
4. Install and read work with any framework: `database.sql` is run statement by
   statement, and a view is read like a table.

*Impact on the specification:* none. The statement means what it meant.

*Impact in production:* with the flag, a prototype's `database.sql` creates views;
`ampersand validate` creates them in its temporary database as well.

**A derived relation stays stored, and the compiler says why, when it is named in `--materialize`, when another ENFORCE statement writes it, when a POPULATION gives it pairs, when its definition is cyclic, or when it belongs to another database.**
*DC-2 · valid · 2026-10-27 · origin: issue #1744, requirement R2 and the researcher's answer 2*

*Considerations:*

1. The flag never refuses a script that compiles without it: every reason is a
   warning, and the relation is then realised as before.
2. A second `ENFORCE` on the same relation adds or removes pairs; a view cannot
   take them. The Landeneisenregister has this case:
   `PrototypeContext.sessionAllowedRoles` has `:=` in the SIAM bridge and `>:` in
   `PrototypeContext.adl`, and the framework image writes the relation itself.
3. A `POPULATION` on a derived relation gives it pairs of its own, which a view
   has no place for.
4. SQL has no view that refers to itself, and MariaDB refuses a cycle of views;
   the compiler finds the cycle by taking away, repeatedly, every candidate whose
   definition mentions no remaining candidate.
5. A view is created in the database of the compiled context, so a relation of
   a context on another database (v5.12.0) stays as it is.
6. The indication is a compiler flag and not a keyword, because a reserved word is
   a language decision that belongs to the owner of the language; see "Still to
   decide".

*Impact on the specification:* none.

*Impact in production:* a warning per relation that stays stored.

**The view is read through a plug with the columns `src` and `tgt`, named like the link table of the relation would have been; `relations.json` keeps a valid `mysqlTable` for it (`tableOf: null`) and gains `"storage": "view"`.**
*DC-3 · valid · 2026-10-27 · origin: issue #1744, design direction 2*

*Considerations:*

1. The SQL generator already reads a fabricated relation through such a plug for
   the shared subterms of issue #1728 (IE-DC-21). Reusing the plug keeps
   `selectRelation` and every consumer of `relations.json` unchanged.
2. The name of the view is chosen in the same scope as the names of the link
   tables and the concept tables, with the same disambiguation, so it clashes with
   no table.
3. `tableOf: null` is what a framework reads as a binary table: it reads the view
   correctly, and a write fails in the database with a clear SQL error until the
   framework reads `storage`.
4. The field `storage` is additive (`"table"` for every other relation), as the
   fields of IE-DC-3 are.

*Impact on the specification:* none.

*Impact in production:* a runtime that knows `storage` skips a view in the write
paths and refuses a direct write with a message; a runtime that does not treats
the view as a table.

**The view query keeps the SQL of the term; the outer `DISTINCT` is left out when the translation is one `SELECT`, and kept when it is a set operation or a `WITH` clause.**
*DC-4 · valid · 2026-10-27 · origin: issue #1744, measurement E5 of the research note*

*Considerations:*

1. MariaDB merges a view into the query that reads it (the `MERGE` algorithm) only
   when the view has no `DISTINCT`, no `UNION` and no recursion; otherwise it
   materialises the whole view at every use (E5: 55 ms for one product). A view of
   one `SELECT` without `DISTINCT` stays mergeable.
2. Every generated query that reads the view applies `SELECT DISTINCT` at its
   top (`getBinQueryExpr` for a term), so a duplicate row in the view changes no
   result. This is the boundary of claim PRF-19.
3. A set operation and a `WITH` clause are temporary tables in any case; their
   `DISTINCT` costs nothing extra and keeps the view a set.
4. The term is translated as written, after `anchorComplements` (issue #562), not
   after `conjNF`: the normal form serves the violation queries, and the view
   serves reads.
5. The measurement on the Landeneisenregister (`measurement-fc5.md`) shows the
   other side of the same coin: a view that MariaDB cannot merge (a `UNION`, a
   `DISTINCT`, a recursion) is a temporary table without an index inside every
   query that joins it, the block-nested-loop join of issue #1708. A rule that
   joins such a view is then slower than the same rule on the stored table, even
   when the view itself reads in a millisecond. The lever is `--materialize` for
   that relation; a translation of a union view that the database can merge is
   future work ("Still to decide").

*Impact on the specification:* none.

*Impact in production:* a view may carry duplicate rows; a consumer outside the
generated queries that counts rows must apply `DISTINCT` itself. A rule that
joins a view with a set operation or a recursion inside can be slower than on
the stored table; the measurement names the cases.

**A conjunct that reads a view is affected by every relation and concept its definition reads, transitively; such a conjunct gets no candidate queries, and its cost profile is classified on the term with the views substituted.**
*DC-5 · valid · 2026-10-27 · origin: issue #1744, the researcher's remark on finding 2; builds on IE-DC-9 and IE-DC-17*

*Considerations:*

1. The runtime re-evaluates, after a transaction, the conjuncts in
   `affectedConjuncts` of the relations and concepts it touched
   (`Transaction::close`). A view is never touched, so without this extension an
   invariant over a derived relation would never be re-checked. The extension is
   itself a claim on the proof track (PRF-19, second part).
2. The candidate calculus (PRF-7) assumes a delta table per relation that
   records every changed pair; a view has none, so a conjunct that reads a view
   keeps full re-evaluation, which is the route of a conjunct without a candidate
   query (IE-DC-9). Substituting the definition into the conjunct before the
   calculus was set aside for this release: it is sound, and it can come when a
   model shows the need.
3. The cost profile (IE-DC-17) names the tables a query reads in full; with the
   views substituted it names the base tables, and a definition with a closure
   makes the conjunct `recursive`.

*Impact on the specification:* none.

*Impact in production:* `relations.json` and `concepts.json` list more affected
conjuncts for the base relations of a view; `conjuncts.json` carries no
`deltaQueries` for a conjunct that reads a view.

**The compiler's own evaluator gives a view the population of its definition, computed in dependency order.**
*DC-6 · valid · 2026-10-27 · origin: issue #1744, requirement R6 (`ampersand validate`)*

*Considerations:*

1. `ampersand validate` compares every generated query with `fullContents` of the
   compiler, and the compiler checks the invariants of a script on its
   population. Both must see the view as the database sees it: always equal to
   its definition. A stored derived relation, in contrast, is empty until the
   ExecEngine of a prototype fills it, and the evaluator says so.
2. The population is computed once per view and appended to the populations of
   the script, a definition after the views it reads, so that `fullContents` needs
   no change.

*Impact on the specification:* none.

*Impact in production:* none; the generated population files list no view.

**An interface field whose term is a view relation, or its converse, is read-only, and a write to a view that the script asks for gets a warning.**
*DC-7 · valid · 2026-10-27 · origin: issue #1744, lesson 4 of the research note*

*Considerations:*

1. A field is editable only when its term is a declared relation or its converse
   (`ifcobjJSONrelation`); for a view the write has no table to land in, so
   `relation` is `null` and `crud` keeps `R` only.
2. The script can still ask for a write: `cRUd` on such a field, or `{EX} InsPair`
   / `DelPair` on the relation in a violation text. The compiler warns where it
   can see it; the framework refuses the write at run time.

*Impact on the specification:* none.

*Impact in production:* a field on a derived relation shows as read-only.

**A view with a Kleene closure terminates on a relation with a cycle, because the generated recursion uses `UNION`; the compiler warns that every use recomputes the closure.**
*DC-8 · valid · 2026-10-27 · origin: issue #1744, the researcher's remark on finding 3*

*Considerations:*

1. The research note asked for a `CYCLE` clause. The compiled closure is a
   `WITH RECURSIVE ... UNION` (not `UNION ALL`), so the recursion stops when a
   step adds no new pair, also on a cycle. A view inherits that.
2. The cost of such a view is constant per use and proportional to the whole
   closure (E5). The warning names `--materialize` as the way out when that costs
   more than maintaining the stored closure.

*Impact on the specification:* none.

*Impact in production:* a warning per view with a closure.

**A property rule of a derived relation that is a view is left out when the shape of the definition guarantees the property; the compiler reports per view how many conjuncts read it.**
*DC-11 · valid · 2026-10-27 · origin: issue #1744, the first measurement on the Landeneisenregister (27 October 2026)*

*Considerations:*

1. A relation declared `[PROP]` or `[UNI,INJ]` gets a rule per property, and the
   prototype evaluates the conjunct of each rule after a transaction that touches
   the relation. For a stored table that costs little. For a view it recomputes the
   view, twice for a rule such as `r;r~ |- I`. In the first measurement the
   property rules of `eisHeeftTweeling := I[Eis] /\ eisFacetGelijk;eisFacetGelijk~`
   alone took 51 of the 76 seconds that all conjuncts took with views (0.6 ms with
   the stored table), and five property rules of `eisInBewerking` another 12.
2. A definition that is a subset of the identity (`I /\ ...`, `I - ...`, a named
   atom, a composition of two such terms) is univalent, injective, symmetric,
   antisymmetric and transitive in every population; univalence, injectivity and
   antisymmetry pass to every subset, so an intersection has them when a side has,
   and a difference when its left side has. `guaranteedProperties` in
   `Ampersand.Classes.Relational` infers that, conservatively: a property the shape
   does not show keeps its rule, as a stored relation keeps all of them.
3. Leaving out every property rule of a view was set aside: a declared property
   that the definition does not guarantee is a claim of the modeller that the
   prototype should keep checking, as it does for a stored derived relation whose
   definition violates it.
4. The compiler's report line per derived relation names the number of conjuncts
   that read the view, so that the modeller sees which views feed many rules and
   can decide on `--materialize` with the measurement in hand; the compiler cannot
   know the cost of a view from the model alone (the lesson of IE-DC-16).

*Impact on the specification:* none; a rule that cannot be violated is left out.

*Impact in production:* fewer conjuncts in `conjuncts.json` for a model with
derived relations that carry properties.

## The database as guardian

**With `--foreign-keys` the database structure carries a FOREIGN KEY from every column that holds an atom to the key column of its concept table, where that key is a PRIMARY KEY, and a UNIQUE constraint on every column that holds the unique side of a relation; `RESTRICT` on delete and update; a conjunct that such a UNIQUE makes empty is `structural`.**
*DC-9 · valid · 2026-10-27 · origin: issue #1744, requirement R4 and the researcher's answer 7; builds on IE-DC-16 and PRF-8*

*Considerations:*

1. Keys, references and uniqueness are what every SQL database enforces cheaply
   and at the moment of writing (Türker & Gertz 2001, in the research note); a
   conjunct that re-checks them after the transaction is a query the database
   has already answered.
2. The prototype framework writes in an order the constraints accept: it adds
   both atoms before it adds a link (`Relation::addLink`), removes the links of an
   atom before it deletes the atom (`Concept::deleteAtom`), and merges atoms by
   deleting and adding links. So `RESTRICT` holds, and a cascade would hide a
   defect in that order rather than help; it would also delete pairs that the
   delta tables of IE-DC-3 never see.
3. A key column of a scalar concept table is not a primary key (issue #341), a
   concept of a type that cannot be listed has no table (issue #1727), and a
   table of another database is a view here (v5.10.0): a column that refers to
   such a concept gets no foreign key. The types of the two columns must be the
   same, which the generator guarantees because both come from the
   representation of the concept.
4. The identity column of a specialisation in a wide table repeats the key of
   its row, so it is unique by construction and gets no constraint of its own; a
   relation column gets one when the layout knows it to be unique (`attUniq`),
   which is the target column of an `INJ` relation and the source column of a
   `UNI` relation stored on its target side or in a link table. The predicate
   `hasUniqueConstraint` is shared by the DDL and by `structurallyEnforced`, so
   the cost profile cannot call a conjunct structural whose constraint does not
   exist (claim PRF-20).
5. A bulk load that writes rows in its own order runs with
   `SET FOREIGN_KEY_CHECKS=0` and switches the checks on again; `ampersand
   validate` does so around the population of its temporary database, and the
   `sqlload` route of the Landeneisenregister needs the same line. The
   delta-SQL harness (`incremental-bench --sql`) inserts synthetic pairs for
   atoms that no concept table holds and is run without the option.
6. `NOT NULL` for `TOT` was set aside, as the issue proposes: MariaDB has no
   deferred constraints, and the framework creates an atom before it fills its
   columns.
7. The option is off by default in this release, so that a deployment chooses
   the moment at which its load scripts are ready for the checks.

*Impact on the specification:* none.

*Impact in production:* with the option, a write that violates a reference or a
uniqueness fails at once with a database error instead of at the end of the
transaction with a violation; the conjuncts of the multiplicities that a
`UNIQUE` enforces carry the cost profile `structural`.

## The external schema

**With `--schema-views` the database structure carries a view per conjunct, `conj_<id>`, and a view per interface field, `ifc_<interface>_<field>` along the path of the field with the names escaped as the REST API escapes them; the JSON files keep their queries.**
*DC-10 · valid · 2026-10-27 · origin: issue #1744, requirement R3 and the researcher's answer 6*

*Considerations:*

1. The queries in `interfaces.json` and `conjuncts.json` are the external schema
   of the application (the ANSI/SPARC term in the research note). As views they
   become objects the database knows: `EXPLAIN` shows them, a `GRANT` applies to
   them, a report or a second front end reads them, and a generator for another
   database translates them instead of reading JSON.
2. The names are stable as long as the model is: the id of a conjunct follows
   the order of the rules in the script, and the name of a field view follows
   the interface and the field names, escaped as the REST API escapes them
   (`Mijn naam` becomes `Mijn_32_naam`), so that a view name and an API field name
   coincide. A name longer than 64 characters, the limit of MariaDB, is cut and
   gets seven characters of a hash; a name that another view has when case is
   ignored gets a hash as well.
3. The view of an interface field selects the pairs of the field's term, the
   binary relation `(src, tgt)`, without the filter on the source atom that the
   prototype adds (`_SRCATOM`) and without the broad query that joins the
   attributes of the target: the consumer filters, which is what a view is for.
   No field query depends on anything but the tables, so no query is left out.
4. The alternative set aside is to document the JSON better: that makes the
   layer readable for people and not for the database.
5. The views are created last, so that they can read the views of the derived
   relations (DC-1); `ampersand validate` creates them as well, which checks
   that every one of them is accepted.
6. The evidence that the Oracle lane of the Landeneisenregister can follow the
   model: the view below is generated from
   `testing/derivedViews/constraints/constraints.adl` with `--schema-views`, and
   next to it stands its translation to Oracle by hand.

   Generated for MariaDB (the comment names the interface, the field and the term):

   ```sql
   /* Interface Steden, field inwoners: woontIn[Persoon*Stad]~ */
   CREATE VIEW "ifc_Steden_inwoners" AS
        select distinct "woontIn" as src, "Persoon" as tgt
             from "Persoon"
             where ("Persoon" is not null)
                   and ("woontIn" is not null);
   ```

   The same view by hand for Oracle 19c, in the style of the `ler_regels` views of the
   Oracle lane (quoted identifiers keep their case; `COMMENT ON` carries the term):

   ```sql
   CREATE OR REPLACE VIEW "ifc_Steden_inwoners" ("src", "tgt") AS
     SELECT DISTINCT p."woontIn", p."Persoon"
       FROM "Persoon" p
      WHERE p."Persoon" IS NOT NULL
        AND p."woontIn" IS NOT NULL;
   COMMENT ON TABLE "ifc_Steden_inwoners" IS 'Interface Steden, field inwoners: woontIn[Persoon*Stad]~';
   ```

   The translation is mechanical: the select list, the table and the predicate carry
   over, the aliases move to the column list of the view, and the comment block becomes
   a `COMMENT ON`. A view with `WITH RECURSIVE` needs Oracle's recursive `WITH` or
   `CONNECT BY`, which is the one place where a translator has work of its own.

*Impact on the specification:* none.

*Impact in production:* with the option, a prototype's database holds a few
hundred views more; nothing reads them unless something outside the prototype
does.

## Still to decide

- **The keyword.** `ENFORCE MATERIALIZED r := term` would put the indication of DC-2
  in the script, next to the statement it concerns, where `--materialize` puts it in
  the build. A reserved word can break a script that uses it as a name; the owner of
  the language decides. Until then the flag is the indication.
- **The default.** `--derived-views` is off (DC-1, consideration 3). It can become the
  default once a prototype framework release reads `storage`.
- **A mergeable translation for a union view.** MariaDB merges a view into the
  query that reads it only without `UNION`, `DISTINCT` and recursion (DC-4). The
  union of two one-`SELECT` terms could be written as one `SELECT` with an `OR`
  when both sides read the same table, which the database can merge; whether that
  pays, and for which shapes, is to be measured.
- **Candidate queries through a view.** Substituting the definition before the
  candidate calculus (DC-5, consideration 2) would give a conjunct that reads a view
  the incremental route; it waits for a model that needs it.
