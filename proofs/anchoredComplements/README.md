# Anchored-complement rewrite — machine-checked identities

Supports `anchorComplements` in `src/Ampersand/ADL1/Expression.hs`
(issue #562): the rewrite that turns complements with a complement-free
sibling ("anchor") in the same intersection into anchored differences, so
the SQL generator compiles them as anti-joins instead of Cartesian products.

`AnchoredRewrite.thy` proves, over binary relations as sets of pairs with
the typing invariant `term ⊆ V[A*B]`:

- **R1 (absorb):** `g ∩ (V − e) = g − e` — the core identity `g /\ -e = g - e`
- **R2 (distribute):** `g ∩ (p ∪ q) = (g ∩ p) ∪ (g ∩ q)`
- **R3 (push):** `g ∩ (p − q) = (g ∩ p) − q`
- the difference desugaring of the SQL generator: `l − r = l ∩ (V − r)`
- the worked example of issue #562:
  `x ∩ ((¬a ∪ ¬b) − c) = ((x − a) ∪ (x − b)) − c`
- soundness of folding one anchor through several members, and preservation
  of the typing invariant by an anchored result

Build: `isabelle build -D .`
