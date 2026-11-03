/-
  Candidate queries with shared subterms (Ampersand issue #1728).

  The candidate set of a relation r for a rule term t contains every pair whose
  membership in t may change when a transaction changes r. Claim PRF-7 proves
  the candidate calculus complete (proofs/incremental/Candidates.thy). This
  file proves three claims about the form in which the compiler writes the
  candidate set:

  * PRF-16: the nested form, one union per node, denotes the same relation as
    the list form, one term per occurrence of r; and the pruned nested form
    that the compiler generates denotes the same relation as the calculus.
  * PRF-17: binding subterms to named views and referring to them by name
    leaves the denoted relation unchanged.
  * PRF-18: every subterm of the nested candidate term is one of at most seven
    terms per node of t, so its shared representation is linear in the size
    of t; without sharing, its size on a chain of differences is quadratic.

  Core library only; no axioms beyond Lean's own, no `sorry`.
-/

namespace CandidateSharing

/-! ## Relations as binary predicates -/

abbrev Rel (α : Type) := α → α → Prop

section Ops
variable {α : Type}

def Rel.uni (a b : Rel α) : Rel α := fun x y => a x y ∨ b x y
def Rel.isc (a b : Rel α) : Rel α := fun x y => a x y ∧ b x y
def Rel.dif (a b : Rel α) : Rel α := fun x y => a x y ∧ ¬ b x y
def Rel.cps (a b : Rel α) : Rel α := fun x z => ∃ y, a x y ∧ b y z
def Rel.flp (a : Rel α) : Rel α := fun x y => a y x
def Rel.cpl (a : Rel α) : Rel α := fun x y => ¬ a x y
def Rel.emp : Rel α := fun _ _ => False

end Ops

/-! ## Rule terms

  `cst` stands for a leaf that a transaction does not change: an identity,
  a universal relation, an atom literal. -/

inductive Trm (R : Type) where
  | rel : R → Trm R
  | cst : Nat → Trm R
  | uni : Trm R → Trm R → Trm R
  | isc : Trm R → Trm R → Trm R
  | dif : Trm R → Trm R → Trm R
  | cps : Trm R → Trm R → Trm R
  | flp : Trm R → Trm R
  | cpl : Trm R → Trm R

/-- The number of nodes of a term. -/
def Trm.size {R : Type} : Trm R → Nat
  | .rel _ => 1
  | .cst _ => 1
  | .uni a b => a.size + b.size + 1
  | .isc a b => a.size + b.size + 1
  | .dif a b => a.size + b.size + 1
  | .cps a b => a.size + b.size + 1
  | .flp a => a.size + 1
  | .cpl a => a.size + 1

/-! ## PRF-16: nested form, list form and pruned form

  The envelopes `W` (an upper bound of the old and the new state) and `N` (a
  lower bound) are parameters here: the three forms coincide whatever they
  are. Their properties are the subject of PRF-7. -/

section Forms
variable {α R : Type} (W N : Trm R → Rel α)

/-- The candidate calculus, one union per node, as `candg` in Candidates.thy. -/
def candg (lc : R → Rel α) : Trm R → Rel α
  | .rel r => lc r
  | .cst _ => Rel.emp
  | .uni a b => (candg lc a).uni (candg lc b)
  | .isc a b => ((candg lc a).isc (W b)).uni ((W a).isc (candg lc b))
  | .dif a b => ((candg lc a).dif (N b)).uni ((W a).isc (candg lc b))
  | .cps a b => ((candg lc a).cps (W b)).uni ((W a).cps (candg lc b))
  | .flp a => (candg lc a).flp
  | .cpl a => candg lc a

/-- The list form: one term per occurrence of a changed relation, every union
    distributed to the top. This is what `candidateTerms` generated. A leaf
    assignment `lo` gives `some Δr` for a changed relation and `none` for an
    unchanged one. -/
def candList (lo : R → Option (Rel α)) : Trm R → List (Rel α)
  | .rel r => (lo r).toList
  | .cst _ => []
  | .uni a b => candList lo a ++ candList lo b
  | .isc a b => (candList lo a).map (fun d => d.isc (W b)) ++ (candList lo b).map (fun d => (W a).isc d)
  | .dif a b => (candList lo a).map (fun d => d.dif (N b)) ++ (candList lo b).map (fun d => (W a).isc d)
  | .cps a b => (candList lo a).map (fun d => d.cps (W b)) ++ (candList lo b).map (fun d => (W a).cps d)
  | .flp a => (candList lo a).map Rel.flp
  | .cpl a => candList lo a

/-- A union in which `none` stands for the empty relation. -/
def orU : Option (Rel α) → Option (Rel α) → Option (Rel α)
  | some a, some b => some (a.uni b)
  | some a, none => some a
  | none, q => q

/-- The pruned nested form: a branch whose candidate set is empty, because no
    changed relation occurs below it, is left out. This is what
    `candidateTerm` generates. -/
def candOpt (lo : R → Option (Rel α)) : Trm R → Option (Rel α)
  | .rel r => lo r
  | .cst _ => none
  | .uni a b => orU (candOpt lo a) (candOpt lo b)
  | .isc a b => orU ((candOpt lo a).map (fun d => d.isc (W b))) ((candOpt lo b).map (fun d => (W a).isc d))
  | .dif a b => orU ((candOpt lo a).map (fun d => d.dif (N b))) ((candOpt lo b).map (fun d => (W a).isc d))
  | .cps a b => orU ((candOpt lo a).map (fun d => d.cps (W b))) ((candOpt lo b).map (fun d => (W a).cps d))
  | .flp a => (candOpt lo a).map Rel.flp
  | .cpl a => candOpt lo a

/-- The relation an optional term denotes: `none` denotes the empty relation. -/
def optDen (o : Option (Rel α)) (x y : α) : Prop := ∃ c, o = some c ∧ c x y

/-- The leaf assignment of the calculus that belongs to `lo`. -/
def leafOf (lo : R → Option (Rel α)) (r : R) : Rel α := optDen (lo r)

theorem optDen_orU (p q : Option (Rel α)) (x y : α) :
    optDen (orU p q) x y ↔ optDen p x y ∨ optDen q x y := by
  cases p <;> cases q <;> simp [orU, optDen, Rel.uni]

theorem optDen_map (p : Option (Rel α)) (f : Rel α → Rel α) (x y : α) :
    optDen (p.map f) x y ↔ ∃ c, p = some c ∧ f c x y := by
  cases p <;> simp [optDen]

/-- PRF-16, list form: the nested calculus denotes the union of the list. -/
theorem candg_eq_list (lo : R → Option (Rel α)) :
    ∀ (t : Trm R) (x y : α), candg W N (leafOf lo) t x y ↔ ∃ c ∈ candList W N lo t, c x y := by
  intro t
  induction t with
  | rel r =>
    intro x y
    cases h : lo r <;> simp [candg, candList, leafOf, optDen, h]
  | cst n => intro x y; simp [candg, candList, Rel.emp]
  | uni a b iha ihb =>
    intro x y
    simp only [candg, candList, Rel.uni, iha, ihb, List.mem_append]
    constructor
    · rintro (⟨c, hc, h⟩ | ⟨c, hc, h⟩)
      · exact ⟨c, Or.inl hc, h⟩
      · exact ⟨c, Or.inr hc, h⟩
    · rintro ⟨c, hc | hc, h⟩
      · exact Or.inl ⟨c, hc, h⟩
      · exact Or.inr ⟨c, hc, h⟩
  | isc a b iha ihb =>
    intro x y
    simp only [candg, candList, Rel.uni, Rel.isc, iha, ihb, List.mem_append, List.mem_map]
    constructor
    · rintro (⟨⟨c, hc, h⟩, hw⟩ | ⟨hw, ⟨c, hc, h⟩⟩)
      · exact ⟨_, Or.inl ⟨c, hc, rfl⟩, h, hw⟩
      · exact ⟨_, Or.inr ⟨c, hc, rfl⟩, hw, h⟩
    · rintro ⟨_, ⟨c, hc, rfl⟩ | ⟨c, hc, rfl⟩, h⟩
      · exact Or.inl ⟨⟨c, hc, h.1⟩, h.2⟩
      · exact Or.inr ⟨h.1, ⟨c, hc, h.2⟩⟩
  | dif a b iha ihb =>
    intro x y
    simp only [candg, candList, Rel.uni, Rel.isc, Rel.dif, iha, ihb, List.mem_append, List.mem_map]
    constructor
    · rintro (⟨⟨c, hc, h⟩, hn⟩ | ⟨hw, ⟨c, hc, h⟩⟩)
      · exact ⟨_, Or.inl ⟨c, hc, rfl⟩, h, hn⟩
      · exact ⟨_, Or.inr ⟨c, hc, rfl⟩, hw, h⟩
    · rintro ⟨_, ⟨c, hc, rfl⟩ | ⟨c, hc, rfl⟩, h⟩
      · exact Or.inl ⟨⟨c, hc, h.1⟩, h.2⟩
      · exact Or.inr ⟨h.1, ⟨c, hc, h.2⟩⟩
  | cps a b iha ihb =>
    intro x z
    simp only [candg, candList, Rel.uni, Rel.cps, iha, ihb, List.mem_append, List.mem_map]
    constructor
    · rintro (⟨y, ⟨c, hc, h⟩, hw⟩ | ⟨y, hw, ⟨c, hc, h⟩⟩)
      · exact ⟨_, Or.inl ⟨c, hc, rfl⟩, y, h, hw⟩
      · exact ⟨_, Or.inr ⟨c, hc, rfl⟩, y, hw, h⟩
    · rintro ⟨_, ⟨c, hc, rfl⟩ | ⟨c, hc, rfl⟩, y, h1, h2⟩
      · exact Or.inl ⟨y, ⟨c, hc, h1⟩, h2⟩
      · exact Or.inr ⟨y, h1, ⟨c, hc, h2⟩⟩
  | flp a iha =>
    intro x y
    simp only [candg, candList, Rel.flp, iha, List.mem_map]
    constructor
    · rintro ⟨c, hc, h⟩
      exact ⟨_, ⟨c, hc, rfl⟩, h⟩
    · rintro ⟨_, ⟨c, hc, rfl⟩, h⟩
      exact ⟨c, hc, h⟩
  | cpl a iha => intro x y; simp only [candg, candList, iha]

/-- PRF-16, pruned form: the term the compiler generates denotes the calculus. -/
theorem candg_eq_opt (lo : R → Option (Rel α)) :
    ∀ (t : Trm R) (x y : α), optDen (candOpt W N lo t) x y ↔ candg W N (leafOf lo) t x y := by
  intro t
  induction t with
  | rel r => intro x y; rfl
  | cst n => intro x y; simp [candOpt, candg, optDen, Rel.emp]
  | uni a b iha ihb =>
    intro x y
    simp only [candOpt, candg, optDen_orU, Rel.uni, iha, ihb]
  | isc a b iha ihb =>
    intro x y
    simp only [candOpt, candg, optDen_orU, optDen_map, Rel.uni, Rel.isc]
    constructor
    · rintro (⟨c, hc, h, hw⟩ | ⟨c, hc, hw, h⟩)
      · exact Or.inl ⟨(iha x y).mp ⟨c, hc, h⟩, hw⟩
      · exact Or.inr ⟨hw, (ihb x y).mp ⟨c, hc, h⟩⟩
    · rintro (⟨h, hw⟩ | ⟨hw, h⟩)
      · obtain ⟨c, hc, h'⟩ := (iha x y).mpr h
        exact Or.inl ⟨c, hc, h', hw⟩
      · obtain ⟨c, hc, h'⟩ := (ihb x y).mpr h
        exact Or.inr ⟨c, hc, hw, h'⟩
  | dif a b iha ihb =>
    intro x y
    simp only [candOpt, candg, optDen_orU, optDen_map, Rel.uni, Rel.isc, Rel.dif]
    constructor
    · rintro (⟨c, hc, h, hw⟩ | ⟨c, hc, hw, h⟩)
      · exact Or.inl ⟨(iha x y).mp ⟨c, hc, h⟩, hw⟩
      · exact Or.inr ⟨hw, (ihb x y).mp ⟨c, hc, h⟩⟩
    · rintro (⟨h, hw⟩ | ⟨hw, h⟩)
      · obtain ⟨c, hc, h'⟩ := (iha x y).mpr h
        exact Or.inl ⟨c, hc, h', hw⟩
      · obtain ⟨c, hc, h'⟩ := (ihb x y).mpr h
        exact Or.inr ⟨c, hc, hw, h'⟩
  | cps a b iha ihb =>
    intro x z
    simp only [candOpt, candg, optDen_orU, optDen_map, Rel.uni, Rel.cps]
    constructor
    · rintro (⟨c, hc, y, h, hw⟩ | ⟨c, hc, y, hw, h⟩)
      · exact Or.inl ⟨y, (iha x y).mp ⟨c, hc, h⟩, hw⟩
      · exact Or.inr ⟨y, hw, (ihb y z).mp ⟨c, hc, h⟩⟩
    · rintro (⟨y, h, hw⟩ | ⟨y, hw, h⟩)
      · obtain ⟨c, hc, h'⟩ := (iha x y).mpr h
        exact Or.inl ⟨c, hc, y, h', hw⟩
      · obtain ⟨c, hc, h'⟩ := (ihb y z).mpr h
        exact Or.inr ⟨c, hc, y, hw, h'⟩
  | flp a iha =>
    intro x y
    simp only [candOpt, candg, optDen_map, Rel.flp]
    rw [← iha y x]
    rfl
  | cpl a iha => intro x y; exact iha x y

end Forms

/-! ## PRF-17: named views

  The target terms of the SQL generator, with references to named views. -/

inductive E (L : Type) where
  | leaf : L → E L
  | view : Nat → E L
  | uni : E L → E L → E L
  | isc : E L → E L → E L
  | dif : E L → E L → E L
  | cps : E L → E L → E L
  | flp : E L → E L
  | cpl : E L → E L

section Views
variable {α L : Type} (lv : L → Rel α)

/-- The relation a term denotes, given the relations of its leaves and of the
    views it refers to. -/
def E.sem (ρ : Nat → Rel α) : E L → Rel α
  | .leaf l => lv l
  | .view i => ρ i
  | .uni a b => (a.sem ρ).uni (b.sem ρ)
  | .isc a b => (a.sem ρ).isc (b.sem ρ)
  | .dif a b => (a.sem ρ).dif (b.sem ρ)
  | .cps a b => (a.sem ρ).cps (b.sem ρ)
  | .flp a => (a.sem ρ).flp
  | .cpl a => (a.sem ρ).cpl

/-- Every reference to view `v` replaced by the definition `d`. -/
def E.subst (v : Nat) (d : E L) : E L → E L
  | .leaf l => .leaf l
  | .view i => if i = v then d else .view i
  | .uni a b => .uni (E.subst v d a) (E.subst v d b)
  | .isc a b => .isc (E.subst v d a) (E.subst v d b)
  | .dif a b => .dif (E.subst v d a) (E.subst v d b)
  | .cps a b => .cps (E.subst v d a) (E.subst v d b)
  | .flp a => .flp (E.subst v d a)
  | .cpl a => .cpl (E.subst v d a)

def update (ρ : Nat → Rel α) (v : Nat) (r : Rel α) : Nat → Rel α :=
  fun i => if i = v then r else ρ i

/-- Substitution lemma: inlining a definition means evaluating with the view
    bound to what the definition denotes. -/
theorem sem_subst (v : Nat) (d : E L) :
    ∀ (t : E L) (ρ : Nat → Rel α) (x y : α),
      (E.subst v d t).sem lv ρ x y ↔ t.sem lv (update ρ v (d.sem lv ρ)) x y := by
  intro t
  induction t with
  | leaf l => intro ρ x y; rfl
  | view i =>
    intro ρ x y
    by_cases h : i = v
    · simp [E.subst, E.sem, update, h]
    · simp [E.subst, E.sem, update, h]
  | uni a b iha ihb => intro ρ x y; simp only [E.subst, E.sem, Rel.uni, iha, ihb]
  | isc a b iha ihb => intro ρ x y; simp only [E.subst, E.sem, Rel.isc, iha, ihb]
  | dif a b iha ihb => intro ρ x y; simp only [E.subst, E.sem, Rel.dif, iha, ihb]
  | cps a b iha ihb => intro ρ x y; simp only [E.subst, E.sem, Rel.cps, iha, ihb]
  | flp a iha => intro ρ x y; simp only [E.subst, E.sem, Rel.flp, iha]
  | cpl a iha => intro ρ x y; simp only [E.subst, E.sem, Rel.cpl, iha]

/-- A WITH clause: the views are bound in order, each definition evaluated
    with the views bound before it, and then the body is evaluated. -/
def evalWith : (Nat → Rel α) → List (Nat × E L) → E L → Rel α
  | ρ, [], body => body.sem lv ρ
  | ρ, (v, d) :: bs, body => evalWith (update ρ v (d.sem lv ρ)) bs body

/-- The same query with every view inlined. -/
def inlineAll : List (Nat × E L) → E L → E L
  | [], body => body
  | (v, d) :: bs, body => E.subst v d (inlineAll bs body)

/-- PRF-17: a query with named views denotes what the inlined query denotes. -/
theorem evalWith_eq_inline :
    ∀ (bs : List (Nat × E L)) (body : E L) (ρ : Nat → Rel α) (x y : α),
      evalWith lv ρ bs body x y ↔ (inlineAll bs body).sem lv ρ x y := by
  intro bs
  induction bs with
  | nil => intro body ρ x y; rfl
  | cons b bs ih =>
    intro body ρ x y
    obtain ⟨v, d⟩ := b
    simp only [evalWith, inlineAll]
    rw [ih body _ x y, sem_subst lv v d]

end Views

/-! ## PRF-18: the size of the candidate term

  Leaves of the candidate term: a stored relation, its delta table, and a
  leaf that a transaction does not change. -/

inductive Leaf (R : Type) where
  | base : R → Leaf R
  | delta : R → Leaf R
  | cst : Nat → Leaf R

section Size
variable {R : Type} [DecidableEq R]

/-- The envelopes as terms: `envT true` is W, `envT false` is N. -/
def envT : Bool → Trm R → E (Leaf R)
  | true, .rel r => .uni (.leaf (.base r)) (.leaf (.delta r))
  | false, .rel r => .dif (.leaf (.base r)) (.leaf (.delta r))
  | _, .cst n => .leaf (.cst n)
  | p, .uni a b => .uni (envT p a) (envT p b)
  | p, .isc a b => .isc (envT p a) (envT p b)
  | p, .dif a b => .dif (envT p a) (envT (!p) b)
  | p, .cps a b => .cps (envT p a) (envT p b)
  | p, .flp a => .flp (envT p a)
  | p, .cpl a => .cpl (envT (!p) a)

def orT : Option (E (Leaf R)) → Option (E (Leaf R)) → Option (E (Leaf R))
  | some a, some b => some (.uni a b)
  | some a, none => some a
  | none, q => q

/-- The two branches of the candidate term of a binary node. -/
def branches (cand : Trm R → Option (E (Leaf R))) : Trm R → List (E (Leaf R))
  | .isc a b => ((cand a).map (fun d => .isc d (envT true b))).toList ++ ((cand b).map (fun d => .isc (envT true a) d)).toList
  | .dif a b => ((cand a).map (fun d => .dif d (envT false b))).toList ++ ((cand b).map (fun d => .isc (envT true a) d)).toList
  | .cps a b => ((cand a).map (fun d => .cps d (envT true b))).toList ++ ((cand b).map (fun d => .cps (envT true a) d)).toList
  | _ => []

/-- The candidate term for a change in `r0`, as `candidateTerm` writes it. -/
def candT (r0 : R) : Trm R → Option (E (Leaf R))
  | .rel r => if r = r0 then some (.leaf (.delta r0)) else none
  | .cst _ => none
  | .uni a b => orT (candT r0 a) (candT r0 b)
  | .isc a b => orT ((candT r0 a).map (fun d => .isc d (envT true b))) ((candT r0 b).map (fun d => .isc (envT true a) d))
  | .dif a b => orT ((candT r0 a).map (fun d => .dif d (envT false b))) ((candT r0 b).map (fun d => .isc (envT true a) d))
  | .cps a b => orT ((candT r0 a).map (fun d => .cps d (envT true b))) ((candT r0 b).map (fun d => .cps (envT true a) d))
  | .flp a => (candT r0 a).map .flp
  | .cpl a => candT r0 a

/-- All subterms of a term, the term itself included. -/
def E.subterms {L : Type} : E L → List (E L)
  | .leaf l => [.leaf l]
  | .view i => [.view i]
  | .uni a b => .uni a b :: (a.subterms ++ b.subterms)
  | .isc a b => .isc a b :: (a.subterms ++ b.subterms)
  | .dif a b => .dif a b :: (a.subterms ++ b.subterms)
  | .cps a b => .cps a b :: (a.subterms ++ b.subterms)
  | .flp a => .flp a :: a.subterms
  | .cpl a => .cpl a :: a.subterms

/-- The terms a node of `t` contributes to the candidate term: its two
    envelopes, its candidate term, the two branches of that candidate term,
    and, for a leaf, the leaves themselves. At most seven per node. -/
def nodeTerms (r0 : R) : Trm R → List (E (Leaf R))
  | .rel r => [envT true (.rel r), envT false (.rel r), .leaf (.base r), .leaf (.delta r)] ++ (candT r0 (.rel r)).toList
  | .cst n => [.leaf (.cst n)]
  | t => [envT true t, envT false t] ++ (candT r0 t).toList ++ branches (candT r0) t

/-- The catalogue: the local terms of every node of `t`. -/
def catalogue (r0 : R) : Trm R → List (E (Leaf R))
  | .rel r => nodeTerms r0 (.rel r)
  | .cst n => nodeTerms r0 (.cst n)
  | .uni a b => nodeTerms r0 (.uni a b) ++ catalogue r0 a ++ catalogue r0 b
  | .isc a b => nodeTerms r0 (.isc a b) ++ catalogue r0 a ++ catalogue r0 b
  | .dif a b => nodeTerms r0 (.dif a b) ++ catalogue r0 a ++ catalogue r0 b
  | .cps a b => nodeTerms r0 (.cps a b) ++ catalogue r0 a ++ catalogue r0 b
  | .flp a => nodeTerms r0 (.flp a) ++ catalogue r0 a
  | .cpl a => nodeTerms r0 (.cpl a) ++ catalogue r0 a

end Size


section SizeTheorems
variable {R : Type} [DecidableEq R]

theorem toList_length_le {β : Type} (o : Option β) : o.toList.length ≤ 1 := by
  cases o <;> simp

theorem nodeTerms_length (r0 : R) (t : Trm R) : (nodeTerms r0 t).length ≤ 5 := by
  cases t with
  | rel r =>
    have := toList_length_le (candT r0 (.rel r))
    simp only [nodeTerms, List.length_append, List.length_cons, List.length_nil] at *; omega
  | cst n => simp [nodeTerms]
  | uni a b =>
    have := toList_length_le (candT r0 (.uni a b))
    simp [nodeTerms, branches]; omega
  | isc a b =>
    have h1 := toList_length_le (candT r0 (.isc a b))
    have h2 := toList_length_le ((candT r0 a).map (fun d => E.isc d (envT true b)))
    have h3 := toList_length_le ((candT r0 b).map (fun d => E.isc (envT true a) d))
    simp only [nodeTerms, branches, List.length_append, List.length_cons, List.length_nil] at *; omega
  | dif a b =>
    have h1 := toList_length_le (candT r0 (.dif a b))
    have h2 := toList_length_le ((candT r0 a).map (fun d => E.dif d (envT false b)))
    have h3 := toList_length_le ((candT r0 b).map (fun d => E.isc (envT true a) d))
    simp only [nodeTerms, branches, List.length_append, List.length_cons, List.length_nil] at *; omega
  | cps a b =>
    have h1 := toList_length_le (candT r0 (.cps a b))
    have h2 := toList_length_le ((candT r0 a).map (fun d => E.cps d (envT true b)))
    have h3 := toList_length_le ((candT r0 b).map (fun d => E.cps (envT true a) d))
    simp only [nodeTerms, branches, List.length_append, List.length_cons, List.length_nil] at *; omega
  | flp a =>
    have := toList_length_le (candT r0 (.flp a))
    simp [nodeTerms, branches]; omega
  | cpl a =>
    have := toList_length_le (candT r0 (.cpl a))
    simp [nodeTerms, branches]; omega

/-- The catalogue has at most five terms per node of `t`. -/
theorem catalogue_length (r0 : R) : ∀ t : Trm R, (catalogue r0 t).length ≤ 5 * t.size := by
  intro t
  induction t with
  | rel r => have := nodeTerms_length r0 (.rel r); simp only [catalogue, Trm.size]; omega
  | cst n => have := nodeTerms_length r0 (.cst n); simp only [catalogue, Trm.size]; omega
  | uni a b iha ihb =>
    have := nodeTerms_length r0 (.uni a b); simp only [catalogue, Trm.size, List.length_append]; omega
  | isc a b iha ihb =>
    have := nodeTerms_length r0 (.isc a b); simp only [catalogue, Trm.size, List.length_append]; omega
  | dif a b iha ihb =>
    have := nodeTerms_length r0 (.dif a b); simp only [catalogue, Trm.size, List.length_append]; omega
  | cps a b iha ihb =>
    have := nodeTerms_length r0 (.cps a b); simp only [catalogue, Trm.size, List.length_append]; omega
  | flp a iha =>
    have := nodeTerms_length r0 (.flp a); simp only [catalogue, Trm.size, List.length_append]; omega
  | cpl a iha =>
    have := nodeTerms_length r0 (.cpl a); simp only [catalogue, Trm.size, List.length_append]; omega

/-- Every subterm of an envelope of `t` is in the catalogue of `t`. -/
theorem env_in_catalogue (r0 : R) :
    ∀ (t : Trm R) (p : Bool) (u : E (Leaf R)), u ∈ (envT p t).subterms → u ∈ catalogue r0 t := by
  intro t
  induction t with
  | rel r =>
    intro p u hu
    cases p <;> simp [envT, E.subterms] at hu <;> rcases hu with rfl | rfl | rfl <;> simp [catalogue, nodeTerms, envT]
  | cst n =>
    intro p u hu
    cases p <;> simp [envT, E.subterms] at hu <;> subst hu <;> simp [catalogue, nodeTerms]
  | uni a b iha ihb =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons, List.mem_append] at hu
    rcases hu with rfl | hu | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · exact by simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha p u hu))
    · exact by simp only [catalogue, List.mem_append]; exact Or.inr (ihb p u hu)
  | isc a b iha ihb =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons, List.mem_append] at hu
    rcases hu with rfl | hu | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha p u hu))
    · simp only [catalogue, List.mem_append]; exact Or.inr (ihb p u hu)
  | dif a b iha ihb =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons, List.mem_append] at hu
    rcases hu with rfl | hu | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha p u hu))
    · simp only [catalogue, List.mem_append]; exact Or.inr (ihb (!p) u hu)
  | cps a b iha ihb =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons, List.mem_append] at hu
    rcases hu with rfl | hu | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha p u hu))
    · simp only [catalogue, List.mem_append]; exact Or.inr (ihb p u hu)
  | flp a iha =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons] at hu
    rcases hu with rfl | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · simp only [catalogue, List.mem_append]; exact Or.inr (iha p u hu)
  | cpl a iha =>
    intro p u hu
    simp only [envT, E.subterms, List.mem_cons] at hu
    rcases hu with rfl | hu
    · cases p <;> simp [catalogue, nodeTerms, envT]
    · simp only [catalogue, List.mem_append]; exact Or.inr (iha (!p) u hu)

/-- Every subterm of the candidate term of `t` is in the catalogue of `t`. -/
theorem cand_in_catalogue (r0 : R) :
    ∀ (t : Trm R) (d : E (Leaf R)), candT r0 t = some d → ∀ u, u ∈ d.subterms → u ∈ catalogue r0 t := by
  intro t
  induction t with
  | rel r =>
    intro d hd u hu
    by_cases h : r = r0
    · subst h
      simp [candT] at hd; subst hd
      simp [E.subterms] at hu; subst hu
      simp [catalogue, nodeTerms]
    · simp [candT, h] at hd
  | cst n => intro d hd; simp [candT] at hd
  | uni a b iha ihb =>
    intro d hd u hu
    have self : d ∈ catalogue r0 (.uni a b) := by
      simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inl (Or.inr (by rw [hd]; simp))))
    cases ha : candT r0 a <;> cases hb : candT r0 b <;> simp [candT, orT, ha, hb] at hd <;> subst hd
    · exact by simp only [catalogue, List.mem_append]; exact Or.inr (ihb _ hb u hu)
    · exact by simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha _ ha u hu))
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr (iha _ ha u hu))
      · simp only [catalogue, List.mem_append]; exact Or.inr (ihb _ hb u hu)
  | isc a b iha ihb =>
    intro d hd u hu
    have self : d ∈ catalogue r0 (.isc a b) := by
      simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inl (Or.inr (by rw [hd]; simp))))
    have inA : ∀ v, v ∈ catalogue r0 a → v ∈ catalogue r0 (.isc a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr hv)
    have inB : ∀ v, v ∈ catalogue r0 b → v ∈ catalogue r0 (.isc a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inr hv
    have br : ∀ v, v ∈ branches (candT r0) (.isc a b) → v ∈ catalogue r0 (.isc a b) := by
      intro v hv; simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inr hv))
    cases ha : candT r0 a <;> cases hb : candT r0 b <;> simp [candT, orT, ha, hb] at hd <;> subst hd
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b true u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | (rfl | hu | hu) | (rfl | hu | hu)
      · exact self
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b true u hu)
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
  | dif a b iha ihb =>
    intro d hd u hu
    have self : d ∈ catalogue r0 (.dif a b) := by
      simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inl (Or.inr (by rw [hd]; simp))))
    have inA : ∀ v, v ∈ catalogue r0 a → v ∈ catalogue r0 (.dif a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr hv)
    have inB : ∀ v, v ∈ catalogue r0 b → v ∈ catalogue r0 (.dif a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inr hv
    have br : ∀ v, v ∈ branches (candT r0) (.dif a b) → v ∈ catalogue r0 (.dif a b) := by
      intro v hv; simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inr hv))
    cases ha : candT r0 a <;> cases hb : candT r0 b <;> simp [candT, orT, ha, hb] at hd <;> subst hd
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b false u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | (rfl | hu | hu) | (rfl | hu | hu)
      · exact self
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b false u hu)
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
  | cps a b iha ihb =>
    intro d hd u hu
    have self : d ∈ catalogue r0 (.cps a b) := by
      simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inl (Or.inr (by rw [hd]; simp))))
    have inA : ∀ v, v ∈ catalogue r0 a → v ∈ catalogue r0 (.cps a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inl (Or.inr hv)
    have inB : ∀ v, v ∈ catalogue r0 b → v ∈ catalogue r0 (.cps a b) := by
      intro v hv; simp only [catalogue, List.mem_append]; exact Or.inr hv
    have br : ∀ v, v ∈ branches (candT r0) (.cps a b) → v ∈ catalogue r0 (.cps a b) := by
      intro v hv; simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inr hv))
    cases ha : candT r0 a <;> cases hb : candT r0 b <;> simp [candT, orT, ha, hb] at hd <;> subst hd
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | hu | hu
      · exact self
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b true u hu)
    · simp only [E.subterms, List.mem_cons, List.mem_append] at hu
      rcases hu with rfl | (rfl | hu | hu) | (rfl | hu | hu)
      · exact self
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (iha _ ha u hu)
      · exact inB _ (env_in_catalogue r0 b true u hu)
      · exact br _ (by simp [branches, ha, hb])
      · exact inA _ (env_in_catalogue r0 a true u hu)
      · exact inB _ (ihb _ hb u hu)
  | flp a iha =>
    intro d hd u hu
    have self : d ∈ catalogue r0 (.flp a) := by
      simp only [catalogue, nodeTerms, List.mem_append]; exact Or.inl (Or.inl (Or.inr (by rw [hd]; simp)))
    cases ha : candT r0 a <;> simp [candT, ha] at hd
    subst hd
    simp only [E.subterms, List.mem_cons] at hu
    rcases hu with rfl | hu
    · exact self
    · simp only [catalogue, List.mem_append]; exact Or.inr (iha _ ha u hu)
  | cpl a iha =>
    intro d hd u hu
    have hd' : candT r0 a = some d := by simpa [candT] using hd
    simp only [catalogue, List.mem_append]; exact Or.inr (iha _ hd' u hu)

/-- PRF-18, upper bound: the distinct subterms of the candidate term, the
    nodes of its shared representation, number at most five per node of the
    rule term. -/
theorem shared_size (r0 : R) (t : Trm R) (d : E (Leaf R)) (hd : candT r0 t = some d)
    (nodes : List (E (Leaf R))) (hnd : nodes.Nodup) (hsub : ∀ u ∈ nodes, u ∈ d.subterms) :
    nodes.length ≤ 5 * t.size := by
  have h1 : nodes.length ≤ (catalogue r0 t).length :=
    List.Nodup.length_le_of_subset hnd (fun u hu => cand_in_catalogue r0 t d hd u (hsub u hu))
  have h2 := catalogue_length r0 t
  omega

/-! ### The lower bound without sharing -/

def E.size {L : Type} : E L → Nat
  | .leaf _ => 1
  | .view _ => 1
  | .uni a b => a.size + b.size + 1
  | .isc a b => a.size + b.size + 1
  | .dif a b => a.size + b.size + 1
  | .cps a b => a.size + b.size + 1
  | .flp a => a.size + 1
  | .cpl a => a.size + 1

/-- A chain of `k` differences, each subtracting a term in which `r0` occurs,
    the shape of the term of issue #1728. -/
def chain (r0 s : R) : Nat → Trm R
  | 0 => .rel r0
  | k + 1 => .dif (chain r0 s k) (.isc (.rel r0) (.rel s))

/-- Triangular numbers: `tri k = k (k - 1) / 2`. -/
def tri : Nat → Nat
  | 0 => 0
  | k + 1 => tri k + k

omit [DecidableEq R] in
theorem env_chain_size (r0 s : R) : ∀ (k : Nat) (p : Bool), k ≤ (envT p (chain r0 s k)).size := by
  intro k
  induction k with
  | zero => intro p; simp
  | succ k ih =>
    intro p
    have := ih p
    simp only [chain, envT, E.size]
    omega

/-- PRF-18, lower bound: written as a tree, without sharing, the candidate term
    of a chain of `k` differences has at least `k (k - 1) / 2` nodes. -/
theorem chain_quadratic (r0 s : R) (hs : s ≠ r0) :
    ∀ k : Nat, ∃ d, candT r0 (chain r0 s k) = some d ∧ tri k ≤ d.size := by
  intro k
  induction k with
  | zero => exact ⟨.leaf (.delta r0), by simp [chain, candT], by simp [tri]⟩
  | succ k ih =>
    obtain ⟨d, hd, hsize⟩ := ih
    have hw := env_chain_size r0 s k true
    refine ⟨.uni (.dif d (envT false (.isc (.rel r0) (.rel s))))
        (.isc (envT true (chain r0 s k)) (.isc (.leaf (.delta r0)) (envT true (.rel s)))),
      by simp [chain, candT, hd, orT, hs], ?_⟩
    simp only [tri, E.size]
    omega

end SizeTheorems


/-! ## The link between the syntax and the calculus

  The candidate term `candT` denotes the pruned form `candOpt` of PRF-16,
  with the envelopes `W` and `N` taken as the relations that `envT` denotes. -/

section Link
variable {α R : Type} [DecidableEq R] (lv : Leaf R → Rel α) (ρ : Nat → Rel α) (r0 : R)

theorem candT_denotes : ∀ t : Trm R,
    (candT r0 t).map (E.sem lv ρ) =
      candOpt (fun s => (envT true s).sem lv ρ) (fun s => (envT false s).sem lv ρ)
        (fun r => if r = r0 then some (lv (.delta r0)) else none) t := by
  intro t
  induction t with
  | rel r => by_cases h : r = r0 <;> simp [candT, candOpt, h, E.sem]
  | cst n => rfl
  | uni a b iha ihb =>
    simp only [candT, candOpt, ← iha, ← ihb]
    cases candT r0 a <;> cases candT r0 b <;> rfl
  | isc a b iha ihb =>
    simp only [candT, candOpt, ← iha, ← ihb]
    cases candT r0 a <;> cases candT r0 b <;> rfl
  | dif a b iha ihb =>
    simp only [candT, candOpt, ← iha, ← ihb]
    cases candT r0 a <;> cases candT r0 b <;> rfl
  | cps a b iha ihb =>
    simp only [candT, candOpt, ← iha, ← ihb]
    cases candT r0 a <;> cases candT r0 b <;> rfl
  | flp a iha =>
    simp only [candT, candOpt, ← iha]
    cases candT r0 a <;> rfl
  | cpl a iha => simpa [candT, candOpt] using iha

end Link

end CandidateSharing
