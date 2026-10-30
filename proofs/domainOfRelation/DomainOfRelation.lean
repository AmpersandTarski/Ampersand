/-
  PRF-21: for the term `I[A] /\ e;e~`, the selection on the concept table and the general
  translation through the self-join of `e` select the same set of pairs (Ampersand issue #1748).

  The SQL generator translates `I[A] /\ e;e~` in `domainCase` (`src/Ampersand/FSpec/SQL.hs`) as

    select ins."A" as src, ins."A" as tgt from "A" as ins
    where ins."A" in (select src from (e) as src) and ins."A" is not null

  where the general translation of an intersection with `I[A]` would compute `e;e~` first:

    select src, tgt from
      ( select fence0.src as src, fence1.tgt as tgt
        from (e) as fence0, (e~) as fence1 where fence0.tgt = fence1.src ) as part2
    where src = tgt and src in (select src from (I[A]))

  The model below is a table of two columns as a list of rows, so duplicates count, as in SQL,
  and a concept table as a list of atoms. The columns hold no NULL: that is the proviso of the
  claim, and here it is the type `α × β`. The claim is about the set of pairs: both generated
  queries are wrapped in `select distinct` by the code that reads them.

  The file uses the Lean core library only.
-/

namespace DomainOfRelation

variable {α β : Type} [DecidableEq α] [DecidableEq β]

/-- A table of two columns without NULL. A list, so a row can occur more than once. -/
abbrev Table (α β : Type) := List (α × β)

/-- The converse `e~`: every row with its columns exchanged. -/
def converse (e : Table α β) : Table β α :=
  e.map (fun p => (p.2, p.1))

/-- `(l) as fence0, (r) as fence1 where fence0.tgt = fence1.src`: the composition `l;r`. -/
def compose {γ : Type} [DecidableEq β] (l : Table α β) (r : Table β γ) : Table α γ :=
  l.flatMap (fun p => (r.filter (fun q => decide (p.2 = q.1))).map (fun q => (p.1, q.2)))

/-- The general translation of `I[A] /\ e;e~`: the rows of `e;e~` on the diagonal, with the
    source among the atoms of `A`. -/
def selfJoinQuery (A : List α) (e : Table α β) : Table α α :=
  (compose e (converse e)).filter (fun r => decide (r.1 = r.2) && decide (r.1 ∈ A))

/-- The special case `domainCase`: the atoms of `A` that are the source of a row of `e`,
    each as the pair (x,x). -/
def domainQuery (A : List α) (e : Table α β) : Table α α :=
  (A.filter (fun x => e.any (fun p => decide (p.1 = x)))).map (fun x => (x, x))

omit [DecidableEq α] in
/-- A pair is in `e;e~` exactly when its two atoms share a partner in `e`. -/
theorem mem_compose_converse (e : Table α β) (x y : α) :
    (x, y) ∈ compose e (converse e) ↔ ∃ z, (x, z) ∈ e ∧ (y, z) ∈ e := by
  constructor
  · intro h
    simp only [compose, converse, List.mem_flatMap, List.mem_map, List.mem_filter] at h
    obtain ⟨p, hp, q, ⟨⟨r, hr, rfl⟩, hpq⟩, hxy⟩ := h
    simp only [Prod.mk.injEq] at hxy
    obtain ⟨rfl, rfl⟩ := hxy
    simp only [decide_eq_true_eq] at hpq
    exact ⟨p.2, hp, by rw [hpq]; exact hr⟩
  · rintro ⟨z, hxz, hyz⟩
    simp only [compose, converse, List.mem_flatMap, List.mem_map, List.mem_filter]
    exact ⟨(x, z), hxz, (z, y), ⟨⟨(y, z), hyz, rfl⟩, by simp⟩, rfl⟩

/-- The general translation selects (x,y) exactly when y = x, x is in A, and x has a partner in e. -/
theorem mem_selfJoinQuery (A : List α) (e : Table α β) (x y : α) :
    (x, y) ∈ selfJoinQuery A e ↔ y = x ∧ x ∈ A ∧ ∃ z, (x, z) ∈ e := by
  calc (x, y) ∈ selfJoinQuery A e
      ↔ (x, y) ∈ compose e (converse e) ∧ (x = y ∧ x ∈ A) := by
          simp [selfJoinQuery, List.mem_filter]
    _ ↔ (∃ z, (x, z) ∈ e ∧ (y, z) ∈ e) ∧ (x = y ∧ x ∈ A) := by
          rw [mem_compose_converse]
    _ ↔ y = x ∧ x ∈ A ∧ ∃ z, (x, z) ∈ e := by
          constructor
          · rintro ⟨⟨z, hxz, _⟩, rfl, hA⟩
            exact ⟨rfl, hA, z, hxz⟩
          · rintro ⟨rfl, hA, z, hxz⟩
            exact ⟨⟨z, hxz, hxz⟩, rfl, hA⟩

omit [DecidableEq β] in
/-- The special case selects the same pairs. -/
theorem mem_domainQuery (A : List α) (e : Table α β) (x y : α) :
    (x, y) ∈ domainQuery A e ↔ y = x ∧ x ∈ A ∧ ∃ z, (x, z) ∈ e := by
  calc (x, y) ∈ domainQuery A e
      ↔ ∃ a, (a ∈ A ∧ ∃ p ∈ e, p.1 = a) ∧ (a, a) = (x, y) := by
          simp [domainQuery, List.mem_map, List.mem_filter, List.any_eq_true]
    _ ↔ y = x ∧ x ∈ A ∧ ∃ z, (x, z) ∈ e := by
          constructor
          · rintro ⟨a, ⟨hA, p, hp, rfl⟩, h⟩
            simp only [Prod.mk.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact ⟨rfl, hA, p.2, hp⟩
          · rintro ⟨hyx, hA, z, hxz⟩
            refine ⟨x, ⟨hA, (x, z), hxz, rfl⟩, ?_⟩
            rw [hyx]

/-- The claim: both translations select the same pairs. -/
theorem same_pairs (A : List α) (e : Table α β) (r : α × α) :
    r ∈ domainQuery A e ↔ r ∈ selfJoinQuery A e := by
  obtain ⟨x, y⟩ := r
  rw [mem_domainQuery, mem_selfJoinQuery]

/-- The range variant `I[B] /\ e~;e` is the domain variant on the converse, so the claim
    carries over: the generator reads the target column of `e` where the converse would be
    the source column. -/
theorem same_pairs_range (B : List β) (e : Table α β) (r : β × β) :
    r ∈ domainQuery B (converse e) ↔ r ∈ selfJoinQuery B (converse e) :=
  same_pairs B (converse e) r

/-- The two translations on a table where three rows share a target: `e;e~` has ten rows,
    the domain has four atoms. -/
example :
    domainQuery ["a1", "a2", "a3", "a4", "a5"]
        ([("a1", "shared"), ("a2", "shared"), ("a3", "shared"), ("a4", "own")] : Table String String)
      = [("a1", "a1"), ("a2", "a2"), ("a3", "a3"), ("a4", "a4")] := by
  decide

example :
    (compose ([("a1", "shared"), ("a2", "shared"), ("a3", "shared"), ("a4", "own")] : Table String String)
        (converse [("a1", "shared"), ("a2", "shared"), ("a3", "shared"), ("a4", "own")])).length = 10 := by
  decide

end DomainOfRelation
