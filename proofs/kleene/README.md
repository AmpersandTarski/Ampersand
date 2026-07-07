# Machine-checked semantics of the Kleene operators

These Isabelle/HOL theories back the implementation of `r+`, `r*`, and the
transitive reduction `r%` in Ampersand. Design and discussion:
[issue #1651](https://github.com/AmpersandTarski/Ampersand/issues/1651)
(consolidated plan in the comments); the unsound-laws fix is
[issue #1635](https://github.com/AmpersandTarski/Ampersand/issues/1635).

## Contents

`KleeneReduction.thy` — the semantic base:

- `rPlus_def_law`, `rPlus_def_law_right`, `rStar_def_law`,
  `rStar_def_law_right`, `rStar_eq_Id_un_rPlus` — the sound unfoldings that
  replace the four unsound laws in `tceDerivRules`
  (`src/Ampersand/FSpec/ToFSpec/NormalForms.hs`);
- `rStar_naive_law_unsound` — the old law `r* = r;r*` really fails;
- `rRed_def_law` — `r% = r - (r;r+)`, the desugaring of `%`;
- `red_trancl_eq`, `red_minimal` — for finite acyclic `r`, `r%` is *the*
  transitive reduction (closure-preserving and minimum);
- `red_converse` — `%` commutes with `~`, justifying the parser's `flp`;
- `rPlus_is_lfp` — the ENFORCE fixpoint `c >: r \/ r;c` converges to `r+`
  (Knaster–Tarski), so insert-only maintenance cannot oscillate;
- `trancl_absorb_insert` — insertion may extend the stored closure;
- `delete_mono`, `naive_delete_unsound` — deletion must re-derive:
  subtracting deleted pairs from a stored closure is wrong.

`IncrementalDelete.thy` — incremental deletion, cycles included:

- `affected r del = r* ; del ; r*`, `safe r del = r+ - affected r del`;
- `safe_survives`, `overdelete_covers` — the safe part survives exactly;
- `incr_delete_correct` — `(r - del)+ = (safe r del ∪ (r - del))+`;
- `reuse_outside_affected` — outside the affected region the old closure is
  reused verbatim;
- `incr_delete_rStar`, `incr_delete_rRed` — the same for `r*` and `r%`.

## Building

```bash
isabelle build -D .
```

Checked with Isabelle2025-2; no `sorry`, no `quick_and_dirty`. Keep to one
`isabelle build` at a time (concurrent builds corrupt the shared build
database).
