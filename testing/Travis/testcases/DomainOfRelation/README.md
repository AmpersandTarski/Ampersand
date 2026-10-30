# Domain of a relation: `I[A] /\ e;e~` as a selection on the concept table

Regression test for the special case `domainCase` in `src/Ampersand/FSpec/SQL.hs` (issue #1748, claim PRF-21):
the SQL generator translates `I[A] /\ e;e~` as the atoms of `A` that occur as a source of `e`,
and `I[A] /\ e~;e` as the atoms that occur as a target, in both orders of the intersection.
`ampersand validate` compares every rule's SQL with the compiler's evaluator on MariaDB,
and the expected relations pin the outcome, so a wrong translation shows up as a violation.
