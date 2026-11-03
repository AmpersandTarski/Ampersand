/-!
# The absorption laws of the normaliser

The normaliser of Ampersand rewrites terms with the law set `tceDerivRules`
in `src/Ampersand/FSpec/ToFSpec/NormalForms.hs`.
Five of those laws are absorption laws.
This file states them in the set semantics of Ampersand and proves them.
The second law used to read `(r ∪ ¬s) ∩ s = s − r`; that law is false,
and `absorb2_old_refuted` shows a witness.
Its correction, `(r ∪ ¬s) ∩ s = r ∩ s`, is `absorb2`.

A relation of signature `[A*B]` is a predicate on pairs.
The complement `¬s` of Ampersand, written `-s`, is typed: it is `V[A*B] − s`.
So a relation `V` stands for the universe of the signature,
and `r ⊆ V` is the typing premise that every well-typed term satisfies.
Two of the five laws need that premise; the other three hold without it.
-/

namespace RewriteLaws

/-- A relation between `α` and `β`: a set of pairs, given by its membership predicate. -/
def Rel (α β : Type) := α → β → Prop

variable {α β : Type}

/-- `r ⊆ s`: every pair of `r` is a pair of `s`. -/
def Sub (r s : Rel α β) : Prop := ∀ a b, r a b → s a b

/-- `r ∪ s`, written `r \/ s` in Ampersand. -/
def union (r s : Rel α β) : Rel α β := fun a b => r a b ∨ s a b

/-- `r ∩ s`, written `r /\ s` in Ampersand. -/
def inter (r s : Rel α β) : Rel α β := fun a b => r a b ∧ s a b

/-- `r − s`, written `r - s` in Ampersand. -/
def diff (r s : Rel α β) : Rel α β := fun a b => r a b ∧ ¬ s a b

/-- The typed complement `¬s` of Ampersand: `V − s`, where `V` is the universe of the signature. -/
def cpl (V s : Rel α β) : Rel α β := diff V s

/-- Two relations with the same pairs are equal. -/
theorem ext {r s : Rel α β} (h : ∀ a b, r a b ↔ s a b) : r = s := by
  funext a b
  exact propext (h a b)

/-- Law 1: `(r ∪ s) ∩ s = s`. -/
theorem absorb1 (r s : Rel α β) : inter (union r s) s = s := by
  apply ext
  intro a b
  constructor
  · intro h
    exact h.2
  · intro h
    exact ⟨Or.inr h, h⟩

/-- Law 2, corrected: `(r ∪ ¬s) ∩ s = r ∩ s`.
No typing premise is needed, because `-s` and `s` share no pair whatever `V` is. -/
theorem absorb2 (V r s : Rel α β) : inter (union r (cpl V s)) s = inter r s := by
  apply ext
  intro a b
  constructor
  · intro h
    rcases h with ⟨hr | hc, hs⟩
    · exact ⟨hr, hs⟩
    · exact absurd hs hc.2
  · intro h
    exact ⟨Or.inl h.1, h.2⟩

/-- The former law 2, `(r ∪ ¬s) ∩ s = s − r`, is false.
Take `r = s = V = {(a,b)}`: the left-hand side contains `(a,b)`, the right-hand side is empty. -/
theorem absorb2_old_refuted :
    ∃ (V r s : Rel Unit Unit), inter (union r (cpl V s)) s ≠ diff s r := by
  refine ⟨fun _ _ => True, fun _ _ => True, fun _ _ => True, ?_⟩
  intro h
  have hl : inter (union (fun _ _ => True) (cpl (fun _ _ => True) (fun _ _ => True)))
      (fun (_ : Unit) (_ : Unit) => True) () () :=
    ⟨Or.inl trivial, trivial⟩
  have hr : ¬ diff (fun (_ : Unit) (_ : Unit) => True) (fun _ _ => True) () () :=
    fun h' => h'.2 trivial
  exact hr (h ▸ hl)

/-- Law 3: `(r ∩ s) ∪ s = s`. -/
theorem absorb3 (r s : Rel α β) : union (inter r s) s = s := by
  apply ext
  intro a b
  constructor
  · intro h
    rcases h with ⟨_, hs⟩ | hs
    · exact hs
    · exact hs
  · intro h
    exact Or.inr h

/-- Law 4: `(r ∩ ¬s) ∪ s = r ∪ s`, for a well-typed `r` (`r ⊆ V`). -/
theorem absorb4 (V r s : Rel α β) (hr : Sub r V) :
    union (inter r (cpl V s)) s = union r s := by
  apply ext
  intro a b
  constructor
  · intro h
    rcases h with ⟨hra, _⟩ | hs
    · exact Or.inl hra
    · exact Or.inr hs
  · intro h
    rcases h with hra | hs
    · rcases Classical.em (s a b) with hs | hns
      · exact Or.inr hs
      · exact Or.inl ⟨hra, hr a b hra, hns⟩
    · exact Or.inr hs

/-- Law 5: `(r ∩ s) ∪ ¬s = r ∪ ¬s`, for a well-typed `r` (`r ⊆ V`). -/
theorem absorb5 (V r s : Rel α β) (hr : Sub r V) :
    union (inter r s) (cpl V s) = union r (cpl V s) := by
  apply ext
  intro a b
  constructor
  · intro h
    rcases h with ⟨hra, _⟩ | hc
    · exact Or.inl hra
    · exact Or.inr hc
  · intro h
    rcases h with hra | hc
    · rcases Classical.em (s a b) with hs | hns
      · exact Or.inl ⟨hra, hs⟩
      · exact Or.inr ⟨hr a b hra, hns⟩
    · exact Or.inr hc

end RewriteLaws
