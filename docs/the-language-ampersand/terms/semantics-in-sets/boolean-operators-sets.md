# Boolean operators in set theory

A relation is by definition a subset of the Cartesian Product of the source and target sets. So, if two different relations r and s are defined on the same source A and target B, then the ordinary set operators can be applied to produce a new relation.

* intersection : $$r ∩ s$$ is the set that contains the elements that are contained in relation $$r$$ as well as in $$s$$, or $$r ∩ s\ =\ \{ (x,y)\ |\ (x,y) ∈ r ∧ (x,y) ∈ s \}$$
* union : $$r ∪ s$$ is the set that contains all elements that are contained either in relation $$r$$ or in $$s$$, or $$r ∪ s\ =\ \{ (x,y)\ |\ (x,y) ∈ r ∨ (x,y) ∈ s \}$$
* difference : $$r - s$$ is the set that contains the elements of relation $$r$$ that are not contained in $$s$$, or $$r - s\ =\ \{ (x,y)\ |\ (x,y) ∈ r ∧ (x,y) ∉ s \}$$

The complement \(or negation\) of a relation $$r_{[A x B]}$$ is defined by means of the difference operator:

* complement : If $$r$$ is defined as $$r_{A\times B}$$, then $$\overline{r}$$ is the set of all tuples in $$A\times B$$ \(the Cartesian product\) that are not contained in $$r$$. So $$\overline{r} = V_{[A\times B]} - r$$

Note that the complement is defined in terms of $$A$$ and $$B$$. So, two relations with the identical population yet a different type may have different complements.

## How to type boolean operators in your script

[This page](../#notation-on-the-keyboard) shows how you can write these things in your Ampersand script.

## Other explanation

Would you like a different explanation of the boolean operators? [This page](../semantics-in-logic/boolean-operators.md) explains the boolean operators in logic.

