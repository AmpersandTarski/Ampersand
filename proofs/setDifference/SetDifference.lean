/-
  PRF-12: for a difference `l − r`, the left-join translation and the EXCEPT translation
  select the same set of pairs (Ampersand issue #1708).

  The SQL generator translates `l − r` in one of two ways (`leftJoinCase` and `exceptCase`
  in `src/Ampersand/FSpec/SQL.hs`):

    select distinct t1.src, t1.tgt
    from (l) as t1 left join (r) as t2 on t1.src = t2.src and t1.tgt = t2.tgt
    where t2.src is null or t2.tgt is null

    select t1.src, t1.tgt from (l) as t1  except  select t2.src, t2.tgt from (r) as t2

  The model below is a table of two columns as a list of rows, so duplicates count,
  as in SQL. The columns of `l` and `r` hold no NULL: that is the proviso of the claim,
  and here it is the type `α × β`. NULL appears in one place only, where SQL itself
  introduces it: the right-hand columns of a left-join row without a partner.

  The file uses the Lean core library only.
-/

namespace SetDifference

variable {α β : Type} [DecidableEq α] [DecidableEq β]

/-- A table of two columns without NULL. A list, so a row can occur more than once. -/
abbrev Table (α β : Type) := List (α × β)

/-- The right-hand columns of a left-join row: a value, or NULL (`none`). -/
abbrev Padded (α β : Type) := Option α × Option β

/-- The rows of `r` that the join condition `t1.src = t2.src and t1.tgt = t2.tgt` matches with `p`. -/
def partners (r : Table α β) (p : α × β) : Table α β :=
  r.filter (fun q => decide (q = p))

/-- What a left join puts to the right of the row `p`:
    one row per partner, or a single row of NULLs when there is none. -/
def joinRows (r : Table α β) (p : α × β) : List (Padded α β) :=
  if partners r p = [] then [(none, none)]
  else (partners r p).map (fun q => (some q.1, some q.2))

/-- `(l) as t1 left join (r) as t2 on t1.src = t2.src and t1.tgt = t2.tgt`. -/
def leftJoin (l r : Table α β) : List ((α × β) × Padded α β) :=
  l.flatMap (fun p => (joinRows r p).map (fun x => (p, x)))

/-- The test `t2.src is null or t2.tgt is null`. -/
def isPadding (x : Padded α β) : Bool :=
  x.1.isNone || x.2.isNone

/-- The left-join translation of `l − r`, before `select distinct` removes duplicates. -/
def leftJoinQuery (l r : Table α β) : Table α β :=
  ((leftJoin l r).filter (fun row => isPadding row.2)).map (fun row => row.1)

/-- Remove duplicates: keep a row unless it occurs again further on. -/
def distinct {γ : Type} [DecidableEq γ] : List γ → List γ
  | [] => []
  | a :: as => if a ∈ distinct as then distinct as else a :: distinct as

/-- The EXCEPT translation of `l − r`. EXCEPT removes duplicates, hence `distinct`. -/
def exceptQuery (l r : Table α β) : Table α β :=
  distinct (l.filter (fun p => !r.contains p))

/-- Removing duplicates keeps exactly the rows that were there. -/
theorem mem_distinct {γ : Type} [DecidableEq γ] (a : γ) (l : List γ) : a ∈ distinct l ↔ a ∈ l := by
  induction l with
  | nil => simp [distinct]
  | cons b bs ih =>
    unfold distinct
    by_cases h : b ∈ distinct bs
    · simp only [h, ↓reduceIte]
      rw [ih, List.mem_cons]
      constructor
      · exact Or.inr
      · rintro (rfl | hbs)
        · exact ih.mp h
        · exact hbs
    · simp only [h, ↓reduceIte]
      rw [List.mem_cons, List.mem_cons, ih]

/-- After removing duplicates, no row occurs twice. -/
theorem nodup_distinct {γ : Type} [DecidableEq γ] (l : List γ) : (distinct l).Nodup := by
  induction l with
  | nil => simp [distinct]
  | cons b bs ih =>
    unfold distinct
    by_cases h : b ∈ distinct bs
    · simp only [h, ↓reduceIte]; exact ih
    · simp only [h, ↓reduceIte]; exact List.nodup_cons.mpr ⟨h, ih⟩

/-- A row has no partner exactly when it does not occur in `r`. -/
theorem partners_eq_nil (r : Table α β) (p : α × β) : partners r p = [] ↔ p ∉ r := by
  unfold partners
  rw [List.filter_eq_nil_iff]
  constructor
  · intro h hp
    exact h p hp (by simp)
  · intro h q hq hqp
    have : q = p := by simpa using hqp
    exact h (this ▸ hq)

/-- The left join yields a row of NULLs to the right of `p` exactly when `p` does not occur in `r`. -/
theorem padding_iff (r : Table α β) (p : α × β) :
    (∃ x, x ∈ joinRows r p ∧ isPadding x = true) ↔ p ∉ r := by
  unfold joinRows
  by_cases h : partners r p = []
  · have hp : p ∉ r := (partners_eq_nil r p).mp h
    simp [h, hp, isPadding]
  · have hp : ¬ p ∉ r := fun hn => h ((partners_eq_nil r p).mpr hn)
    simp only [h, ↓reduceIte]
    constructor
    · rintro ⟨x, hx, hpad⟩
      -- a row made from a partner holds two values, so the NULL test fails on it
      rcases List.mem_map.mp hx with ⟨q, _, rfl⟩
      simp [isPadding] at hpad
    · intro hn
      exact absurd hn hp

/-- The rows the left-join translation selects. -/
theorem mem_leftJoinQuery (l r : Table α β) (p : α × β) :
    p ∈ leftJoinQuery l r ↔ p ∈ l ∧ p ∉ r := by
  calc p ∈ leftJoinQuery l r
      ↔ ∃ x, (p, x) ∈ leftJoin l r ∧ isPadding x = true := by
          -- definition of leftJoinQuery: keep the padded rows, project on the left columns
          simp [leftJoinQuery, List.mem_map, List.mem_filter]
    _ ↔ ∃ x, (p ∈ l ∧ x ∈ joinRows r p) ∧ isPadding x = true := by
          -- definition of leftJoin: the rows to the right of `p` are `joinRows r p`
          simp [leftJoin, List.mem_flatMap, List.mem_map]
    _ ↔ p ∈ l ∧ ∃ x, x ∈ joinRows r p ∧ isPadding x = true := by
          -- `p ∈ l` does not depend on `x`
          constructor
          · rintro ⟨x, ⟨hl, hx⟩, hpad⟩
            exact ⟨hl, x, hx, hpad⟩
          · rintro ⟨hl, x, hx, hpad⟩
            exact ⟨x, ⟨hl, hx⟩, hpad⟩
    _ ↔ p ∈ l ∧ p ∉ r := by
          -- padding_iff
          rw [padding_iff]

/-- The rows the EXCEPT translation selects. -/
theorem mem_exceptQuery (l r : Table α β) (p : α × β) :
    p ∈ exceptQuery l r ↔ p ∈ l ∧ p ∉ r := by
  simp [exceptQuery, mem_distinct, List.mem_filter]

/-- PRF-12. Both translations of `l − r` select the same pairs:
    those that occur in `l` and do not occur in `r`. -/
theorem same_pairs (l r : Table α β) (p : α × β) :
    p ∈ leftJoinQuery l r ↔ p ∈ exceptQuery l r := by
  calc p ∈ leftJoinQuery l r
      ↔ p ∈ l ∧ p ∉ r := mem_leftJoinQuery l r p
    _ ↔ p ∈ exceptQuery l r := (mem_exceptQuery l r p).symm

/-- The result of the EXCEPT translation carries no duplicates. -/
theorem exceptQuery_nodup (l r : Table α β) : (exceptQuery l r).Nodup := by
  unfold exceptQuery
  exact nodup_distinct _

/-- The flipped variant: the generator replaces `r` by its converse before it translates.
    The claim holds for every right-hand table, so in particular for a converse. -/
def converse (r : Table β α) : Table α β :=
  r.map (fun q => (q.2, q.1))

theorem same_pairs_flipped (l : Table α β) (r : Table β α) (p : α × β) :
    p ∈ leftJoinQuery l (converse r) ↔ p ∈ exceptQuery l (converse r) :=
  same_pairs l (converse r) p

/-- A two-row example in which the answer is not empty. -/
example :
    leftJoinQuery [(1, 2), (3, 4), (3, 4)] [(1, 2)] = [(3, 4), (3, 4)]
      ∧ exceptQuery [(1, 2), (3, 4), (3, 4)] [(1, 2)] = [(3, 4)] := by
  decide

end SetDifference
