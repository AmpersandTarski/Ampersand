/-
  Multi-Context Information Systems in Ampersand
  Machine-checked counterparts of the lemmas, theorems, and corollaries
  of sections 4 to 7 of the paper.

  Core Lean 4, no Mathlib.
  A fragment is modelled as a set of facts (a predicate on facts);
  a fact is a triple or an instance pair.  Finiteness of fragments
  plays no part in any proof, so it is left out.
-/

namespace MultiContext

/-! ## Reachability: `reach = inc*` and `vis = I ∪ inc` -/

section Reachability
variable {K : Type}

/-- `Reach inc k j`: context `k` reaches `j` by zero or more include steps. -/
inductive Reach (inc : K → K → Prop) (k : K) : K → Prop
  | refl : Reach inc k k
  | step {j i : K} : Reach inc k j → inc j i → Reach inc k i

theorem Reach.trans {inc : K → K → Prop} {k j i : K}
    (h₁ : Reach inc k j) (h₂ : Reach inc j i) : Reach inc k i := by
  induction h₂ with
  | refl => exact h₁
  | step _ hinc ih => exact Reach.step ih hinc

/-- `Vis inc k j`: context `k` sees `j` directly. -/
def Vis (inc : K → K → Prop) (k j : K) : Prop := k = j ∨ inc k j

/-- `inc* ; (I ∪ inc) ⊆ inc*`, the law used in the lemma on schemas. -/
theorem reach_vis {inc : K → K → Prop} {k j i : K}
    (h₁ : Reach inc k j) (h₂ : Vis inc j i) : Reach inc k i := by
  cases h₂ with
  | inl h => exact h ▸ h₁
  | inr h => exact Reach.step h₁ h

/-- `Plus inc k j`: context `k` reaches `j` by one or more include steps. -/
inductive Plus (inc : K → K → Prop) (k : K) : K → Prop
  | single {j : K} : inc k j → Plus inc k j
  | step {j i : K} : Plus inc k j → inc j i → Plus inc k i

theorem Plus.trans {inc : K → K → Prop} {k j i : K}
    (h₁ : Plus inc k j) (h₂ : Plus inc j i) : Plus inc k i := by
  induction h₂ with
  | single h => exact Plus.step h₁ h
  | step _ hinc ih => exact Plus.step ih hinc

/-- `inc* = I ∪ inc+`, from left to right. -/
theorem reach_eq_or_plus {inc : K → K → Prop} {k j : K}
    (h : Reach inc k j) : k = j ∨ Plus inc k j := by
  induction h with
  | refl => exact Or.inl rfl
  | step _ hinc ih =>
    cases ih with
    | inl e => exact Or.inr (Plus.single (e ▸ hinc))
    | inr p => exact Or.inr (Plus.step p hinc)

/-- Lemma "Inclusion order": if inclusion is acyclic, `inc*` is antisymmetric.
    With `Reach.refl` and `Reach.trans`, it is a partial order. -/
theorem reach_antisymm {inc : K → K → Prop} (hac : ∀ k, ¬ Plus inc k k) {k j : K}
    (h₁ : Reach inc k j) (h₂ : Reach inc j k) : k = j := by
  cases reach_eq_or_plus h₁ with
  | inl e => exact e
  | inr p₁ =>
    cases reach_eq_or_plus h₂ with
    | inl e => exact e.symm
    | inr p₂ => exact absurd (Plus.trans p₁ p₂) (hac k)

end Reachability

/-! ## Facts, fragments, restriction -/

/-- A fact is a triple `⟨a, r, b⟩` or an instance pair `⟨a, C⟩`. -/
inductive Fact (A R C : Type)
  | triple : A → R → A → Fact A R C
  | inst : A → C → Fact A R C

/-- A fragment is a set of facts. -/
def Frag (A R C : Type) := Fact A R C → Prop

/-- Owners and signatures of relation symbols and concept symbols. -/
structure Symbols (K R C : Type) where
  ownR : R → K
  ownC : C → K
  src : R → C
  tgt : R → C

section Fragments
variable {K A R C : Type}

/-- The owner of the symbol of a fact. -/
def owner (o : Symbols K R C) : Fact A R C → K
  | .triple _ r _ => o.ownR r
  | .inst _ c => o.ownC c

/-- Restriction of a fragment to a set `X` of contexts (equation 3 of the paper). -/
def restr (o : Symbols K R C) (F : Frag A R C) (X : K → Prop) : Frag A R C :=
  fun f => F f ∧ X (owner o f)

/-- A fragment is a data set if every triple is typed by instance pairs. -/
def WellTyped (o : Symbols K R C) (F : Frag A R C) : Prop :=
  ∀ a r b, F (.triple a r b) → F (.inst a (o.src r)) ∧ F (.inst b (o.tgt r))

/-- A state assigns a fragment to every context; `Confined` says that the
    fragment of `k` holds facts owned by `k` only. -/
def Confined (o : Symbols K R C) (σ : K → Frag A R C) : Prop :=
  ∀ k f, σ k f → owner o f = k

/-- The view of context `k`: the union of the fragments of the contexts it reaches. -/
def view (inc : K → K → Prop) (σ : K → Frag A R C) (k : K) : Frag A R C :=
  fun f => ∃ j, Reach inc k j ∧ σ j f

/-- A violation function is local to `X` if it is insensitive to facts owned outside `X`. -/
def LocalTo {V : Type} (o : Symbols K R C) (viol : Frag A R C → V) (X : K → Prop) : Prop :=
  ∀ F, viol F = viol (restr o F X)

/-- Lemma "Restricted view", first claim. -/
theorem restricted_view (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) (k : K) (X : K → Prop) :
    restr o (view inc σ k) X = fun f => ∃ j, Reach inc k j ∧ X j ∧ σ j f := by
  funext f
  apply propext
  constructor
  · intro ⟨⟨j, hr, hs⟩, hx⟩
    have hj : owner o f = j := hc j f hs
    exact ⟨j, hr, hj ▸ hx, hs⟩
  · intro ⟨j, hr, hx, hs⟩
    have hj : owner o f = j := hc j f hs
    refine ⟨⟨j, hr, hs⟩, ?_⟩
    rw [hj]
    exact hx

/-- Lemma "Restricted view", second claim. -/
theorem view_restr_reach (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) {k j : K} (hkj : Reach inc k j) :
    restr o (view inc σ k) (Reach inc j) = view inc σ j := by
  rw [restricted_view o inc σ hc k]
  funext f
  apply propext
  constructor
  · intro ⟨i, _, hji, hs⟩
    exact ⟨i, hji, hs⟩
  · intro ⟨i, hji, hs⟩
    exact ⟨i, hkj.trans hji, hji, hs⟩

/-- A view is its own restriction to what its context reaches. -/
theorem view_restr_self (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) (k : K) :
    restr o (view inc σ k) (Reach inc k) = view inc σ k :=
  view_restr_reach o inc σ hc Reach.refl

/-- Theorem "Truth is imported unchanged". -/
theorem truth_is_imported {V : Type} (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) {k j : K} (hkj : Reach inc k j)
    (viol : Frag A R C → V) (hloc : LocalTo o viol (Reach inc j)) :
    viol (view inc σ k) = viol (view inc σ j) :=
  calc viol (view inc σ k)
      = viol (restr o (view inc σ k) (Reach inc j)) := hloc _            -- u is local to reach(j)
    _ = viol (view inc σ j) := by rw [view_restr_reach o inc σ hc hkj]   -- restricted view

/-- Requirement on signatures: a relation symbol is typed by concepts that its owner sees directly. -/
def SigVisible (o : Symbols K R C) (inc : K → K → Prop) : Prop :=
  ∀ r, Vis inc (o.ownR r) (o.ownC (o.src r)) ∧ Vis inc (o.ownR r) (o.ownC (o.tgt r))

/-- Lemma on schemas, the step that the paper spells out:
    a relation of a reached context has its concepts owned by reached contexts. -/
theorem closure_has_concepts (o : Symbols K R C) (inc : K → K → Prop)
    (hsig : SigVisible o inc) {k : K} (r : R) (h : Reach inc k (o.ownR r)) :
    Reach inc k (o.ownC (o.src r)) ∧ Reach inc k (o.ownC (o.tgt r)) :=
  ⟨reach_vis h (hsig r).1, reach_vis h (hsig r).2⟩

/-- Lemma: a view is a data set iff every reached context is well-typed. -/
theorem wellTyped_view_iff (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) (hsig : SigVisible o inc) (k : K) :
    WellTyped o (view inc σ k) ↔ ∀ j, Reach inc k j → WellTyped o (view inc σ j) := by
  constructor
  · intro h j hkj a r b ⟨i, hji, hs⟩
    have hr : o.ownR r = i := hc i _ hs
    have ht := h a r b ⟨i, hkj.trans hji, hs⟩
    constructor
    · have ⟨i', _, hs'⟩ := ht.1
      have hi' : o.ownC (o.src r) = i' := hc i' _ hs'
      have hv : Vis inc i i' := hr ▸ hi' ▸ (hsig r).1
      exact ⟨i', reach_vis hji hv, hs'⟩
    · have ⟨i', _, hs'⟩ := ht.2
      have hi' : o.ownC (o.tgt r) = i' := hc i' _ hs'
      have hv : Vis inc i i' := hr ▸ hi' ▸ (hsig r).2
      exact ⟨i', reach_vis hji hv, hs'⟩
  · intro h
    exact h k Reach.refl

/-- Theorem "Flattening", the part about invariants:
    the invariants of all reached contexts are satisfied on the view of `k`
    iff every reached context satisfies its own invariants on its own view. -/
theorem flattening {U V : Type} (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) (k : K)
    (invariantOf : K → U → Prop) (viol : U → Frag A R C → V) (none : V)
    (hloc : ∀ j u, invariantOf j u → LocalTo o (viol u) (Reach inc j)) :
    (∀ j u, Reach inc k j → invariantOf j u → viol u (view inc σ k) = none) ↔
    (∀ j u, Reach inc k j → invariantOf j u → viol u (view inc σ j) = none) := by
  constructor
  · intro h j u hkj hu
    rw [← truth_is_imported o inc σ hc hkj (viol u) (hloc j u hu)]
    exact h j u hkj hu
  · intro h j u hkj hu
    rw [truth_is_imported o inc σ hc hkj (viol u) (hloc j u hu)]
    exact h j u hkj hu

/-! ## Conservative extension -/

/-- Every context that an old context reaches is old. -/
theorem reach_old (inc : K → K → Prop) (old : K → Prop)
    (hclosed : ∀ k j, old k → inc k j → old j)
    {k : K} (hk : old k) {j : K} (h : Reach inc k j) : old j := by
  induction h with
  | refl => exact hk
  | step _ hinc ih => exact hclosed _ _ ih hinc

/-- In an extension, an old context reaches the same contexts, and they are old. -/
theorem reach_extension (inc inc' : K → K → Prop) (old : K → Prop)
    (hsame : ∀ k j, old k → (inc' k j ↔ inc k j))
    (hclosed : ∀ k j, old k → inc k j → old j)
    {k : K} (hk : old k) (j : K) :
    Reach inc' k j ↔ Reach inc k j ∧ old j := by
  constructor
  · intro h
    induction h with
    | refl => exact ⟨Reach.refl, hk⟩
    | step _ hinc ih =>
      have hi := (hsame _ _ ih.2).mp hinc
      exact ⟨Reach.step ih.1 hi, hclosed _ _ ih.2 hi⟩
  · intro h
    have hr := h.1
    clear h
    induction hr with
    | refl => exact Reach.refl
    | step hr' hinc ih =>
      exact Reach.step ih ((hsame _ _ (reach_old inc old hclosed hk hr')).mpr hinc)

/-- Theorem "Conservative extension", the part about views:
    the view of an old context depends on the fragments of old contexts only. -/
theorem view_extension (inc inc' : K → K → Prop) (old : K → Prop)
    (hsame : ∀ k j, old k → (inc' k j ↔ inc k j))
    (hclosed : ∀ k j, old k → inc k j → old j)
    (σ σ' : K → Frag A R C) (hσ : ∀ j, old j → σ' j = σ j)
    {k : K} (hk : old k) :
    view inc' σ' k = view inc σ k := by
  funext f
  apply propext
  constructor
  · intro ⟨j, hr, hs⟩
    have h := (reach_extension inc inc' old hsame hclosed hk j).mp hr
    refine ⟨j, h.1, ?_⟩
    rw [← hσ j h.2]
    exact hs
  · intro ⟨j, hr, hs⟩
    have hold : old j := reach_old inc old hclosed hk hr
    refine ⟨j, (reach_extension inc inc' old hsame hclosed hk j).mpr ⟨hr, hold⟩, ?_⟩
    rw [hσ j hold]
    exact hs

/-! ## Change -/

/-- A step of context `i` leaves the fragment of every context that `i` does not write. -/
def Step (wr : K → K → Prop) (i : K) (σ σ' : K → Frag A R C) : Prop :=
  ∀ j, ¬ wr i j → σ' j = σ j

/-- Theorem "Non-interference", first claim. -/
theorem frame (o : Symbols K R C) (inc wr : K → K → Prop)
    (σ σ' : K → Frag A R C) (hc : Confined o σ) (hc' : Confined o σ')
    {i : K} (hstep : Step wr i σ σ') (X : K → Prop) (hX : ∀ j, X j → ¬ wr i j) (k : K) :
    restr o (view inc σ' k) X = restr o (view inc σ k) X := by
  rw [restricted_view o inc σ' hc' k X, restricted_view o inc σ hc k X]
  funext f
  apply propext
  constructor
  · intro ⟨j, hr, hx, hs⟩
    refine ⟨j, hr, hx, ?_⟩
    rw [← hstep j (hX j hx)]
    exact hs
  · intro ⟨j, hr, hx, hs⟩
    refine ⟨j, hr, hx, ?_⟩
    rw [hstep j (hX j hx)]
    exact hs

/-- Theorem "Non-interference", second claim. -/
theorem non_interference {V : Type} (o : Symbols K R C) (inc wr : K → K → Prop)
    (σ σ' : K → Frag A R C) (hc : Confined o σ) (hc' : Confined o σ')
    {i : K} (hstep : Step wr i σ σ') (X : K → Prop) (hX : ∀ j, X j → ¬ wr i j) (k : K)
    (viol : Frag A R C → V) (hloc : LocalTo o viol X) :
    viol (view inc σ' k) = viol (view inc σ k) :=
  calc viol (view inc σ' k)
      = viol (restr o (view inc σ' k) X) := hloc _                             -- u is local to X
    _ = viol (restr o (view inc σ k) X) := by rw [frame o inc wr σ σ' hc hc' hstep X hX k]
    _ = viol (view inc σ k) := (hloc _).symm                                   -- u is local to X

/-- Corollary: if `i` does not affect `k` (`i` writes nothing that `k` reaches),
    a step of `i` leaves the view of `k` unchanged. -/
theorem not_affected (o : Symbols K R C) (inc wr : K → K → Prop)
    (σ σ' : K → Frag A R C) (hc : Confined o σ) (hc' : Confined o σ')
    {i k : K} (hstep : Step wr i σ σ') (hna : ∀ j, Reach inc k j → ¬ wr i j) :
    view inc σ' k = view inc σ k :=
  calc view inc σ' k
      = restr o (view inc σ' k) (Reach inc k) := (view_restr_self o inc σ' hc' k).symm
    _ = restr o (view inc σ k) (Reach inc k) := frame o inc wr σ σ' hc hc' hstep _ hna k
    _ = view inc σ k := view_restr_self o inc σ hc k

/-- Corollary on admissible steps: a guarded, consistent context stays consistent
    if the stepping context writes it or does not affect it.
    `cons k F` says that fragment `F`, as the view of `k`, makes `k` consistent. -/
theorem preservation (o : Symbols K R C) (inc wr : K → K → Prop)
    (σ σ' : K → Frag A R C) (hc : Confined o σ) (hc' : Confined o σ')
    (cons : K → Frag A R C → Prop) (guarded : K → Prop)
    {i k : K} (hstep : Step wr i σ σ')
    (hadm : ∀ j, guarded j → wr i j → cons j (view inc σ' j))
    (hg : guarded k) (hk : cons k (view inc σ k))
    (hcase : wr i k ∨ ∀ j, Reach inc k j → ¬ wr i j) :
    cons k (view inc σ' k) := by
  cases hcase with
  | inl hw => exact hadm k hg hw
  | inr hna =>
    rw [not_affected o inc wr σ σ' hc hc' hstep hna]
    exact hk

/-! ## Migration: the moment of completion -/

/-- Theorem "Moment of completion", the part about invariants.
    `invN u` says that `u` is an invariant of the desired context `n`;
    `isNew u` that it is a new blocking invariant; `adopt u` is the copy that
    the migration context `m` maintains. -/
theorem moment_of_completion {U V : Type} (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) {m n : K} (hmn : Reach inc m n)
    (invN isNew : U → Prop) (adopt : U → U) (viol : U → Frag A R C → V) (none : V)
    (hloc : ∀ u, invN u → LocalTo o (viol u) (Reach inc n))
    (hadopt : ∀ u, viol (adopt u) = viol u)
    (hm : ∀ u, invN u → ¬ isNew u → viol (adopt u) (view inc σ m) = none)
    (hnew : ∀ u, invN u → isNew u → viol u (view inc σ m) = none) :
    ∀ u, invN u → viol u (view inc σ n) = none := by
  intro u hu
  have himp : viol u (view inc σ m) = viol u (view inc σ n) :=
    truth_is_imported o inc σ hc hmn (viol u) (hloc u hu)
  cases Classical.em (isNew u) with
  | inl h =>
    rw [← himp]
    exact hnew u hu h
  | inr h =>
    calc viol u (view inc σ n)
        = viol u (view inc σ m) := himp.symm               -- truth is imported
      _ = viol (adopt u) (view inc σ m) := by rw [hadopt u]  -- adopt u has the violations of u
      _ = none := hm u hu h                                -- m is consistent

/-- Theorem "Moment of completion", the part about the data set:
    if the view of `m` is a data set, so is the view of `n`. -/
theorem moment_of_completion_typed (o : Symbols K R C) (inc : K → K → Prop)
    (σ : K → Frag A R C) (hc : Confined o σ) (hsig : SigVisible o inc)
    {m n : K} (hmn : Reach inc m n) (hm : WellTyped o (view inc σ m)) :
    WellTyped o (view inc σ n) :=
  (wellTyped_view_iff o inc σ hc hsig m).mp hm n hmn

end Fragments

/-! ## References -/

/-- A reference as a script writes it: a local name, or a prefix with a local name. -/
inductive QName (N : Type)
  | simple : N → QName N
  | qual : N → N → QName N

section Names
variable {K N T Kd : Type}

/-- The scope of context `k`.  `pre k p j` says that some include statement of `k`
    includes `j` and has `p` as a prefix: `p` is the name of `j` or the alias of the statement.
    The things that context `j` declares are the things with owner `j`. -/
inductive Scope (own : T → K) (nm : T → N) (pre : K → N → K → Prop) (k : K) :
    QName N → T → Prop
  | local {x : T} : own x = k → Scope own nm pre k (.simple (nm x)) x
  | prefixed {j : K} {p : N} {x : T} :
      pre k p j → own x = j → Scope own nm pre k (.qual p (nm x)) x

/-- A reference with a prefix arises from the second rule only. -/
theorem scope_prefixed (own : T → K) (nm : T → N) (pre : K → N → K → Prop) (k : K)
    (m : QName N) (y : T) (h : Scope own nm pre k m y) (p n : N) (hm : m = .qual p n) :
    ∃ j, pre k p j ∧ own y = j ∧ nm y = n := by
  cases h with
  | «local» _ => cases hm
  | prefixed hpre hy =>
    injection hm with h1 h2
    exact ⟨_, h1 ▸ hpre, hy, h2⟩

/-- A reference without a prefix arises from the first rule only. -/
theorem scope_local (own : T → K) (nm : T → N) (pre : K → N → K → Prop) (k : K)
    (m : QName N) (y : T) (h : Scope own nm pre k m y) (n : N) (hm : m = .simple n) :
    own y = k ∧ nm y = n := by
  cases h with
  | «local» hy =>
    injection hm with h1
    exact ⟨hy, h1⟩
  | prefixed _ _ => cases hm

/-- Theorem "References are unambiguous", for a reference without a prefix. -/
theorem local_reference_unambiguous (own : T → K) (nm : T → N) (kind : T → Kd)
    (pre : K → N → K → Prop) (k : K)
    (hid : ∀ x y, own x = own y → nm x = nm y → kind x = kind y → x = y)
    {n : N} {x y : T} (hx : Scope own nm pre k (.simple n) x)
    (hy : Scope own nm pre k (.simple n) y) (hkind : kind x = kind y) : x = y := by
  have ⟨ox, nx⟩ := scope_local own nm pre k _ x hx n rfl
  have ⟨oy, ny⟩ := scope_local own nm pre k _ y hy n rfl
  exact hid x y (ox.trans oy.symm) (nx.trans ny.symm) hkind

/-- Theorem "References are unambiguous", for a reference with a prefix
    that fits one included context. -/
theorem reference_unambiguous (own : T → K) (nm : T → N) (kind : T → Kd)
    (pre : K → N → K → Prop) (k : K)
    (hid : ∀ x y, own x = own y → nm x = nm y → kind x = kind y → x = y)
    {p n : N} (hproper : ∀ j j', pre k p j → pre k p j' → j = j')
    {x y : T} (hx : Scope own nm pre k (.qual p n) x)
    (hy : Scope own nm pre k (.qual p n) y) (hkind : kind x = kind y) : x = y := by
  have ⟨j, hj, ox, nx⟩ := scope_prefixed own nm pre k _ x hx p n rfl
  have ⟨j', hj', oy, ny⟩ := scope_prefixed own nm pre k _ y hy p n rfl
  have hjj : j = j' := hproper j j' hj hj'
  apply hid x y
  · calc own x = j := ox
      _ = j' := hjj                          -- the prefix fits one context
      _ = own y := oy.symm
  · exact nx.trans ny.symm
  · exact hkind

/-- Theorem "References are complete": if every included context has a prefix that fits
    no other included context, every thing of an included context has a reference
    that denotes it. -/
theorem reference_complete (own : T → K) (nm : T → N) (kind : T → Kd)
    (pre : K → N → K → Prop) (inc : K → K → Prop) (k : K)
    (hid : ∀ x y, own x = own y → nm x = nm y → kind x = kind y → x = y)
    (hdist : ∀ j, inc k j → ∃ p, pre k p j ∧ ∀ j', pre k p j' → j' = j)
    (x : T) (hx : inc k (own x)) :
    ∃ p, Scope own nm pre k (.qual p (nm x)) x ∧
      ∀ y, kind y = kind x → Scope own nm pre k (.qual p (nm x)) y → y = x := by
  have ⟨p, hp, huniq⟩ := hdist (own x) hx
  refine ⟨p, Scope.prefixed hp rfl, ?_⟩
  intro y hkind hy
  have hproper : ∀ j j', pre k p j → pre k p j' → j = j' :=
    fun j j' hj hj' => (huniq j hj).trans (huniq j' hj').symm
  exact reference_unambiguous own nm kind pre k hid hproper hy (Scope.prefixed hp rfl) hkind

end Names

end MultiContext
