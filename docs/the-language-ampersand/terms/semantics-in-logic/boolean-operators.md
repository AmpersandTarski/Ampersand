# Boolean operators

The notation $$a\ r\ b$$ means that the pair (a,b) is in relation $$r$$. This page defines when pair (a,b) is in relation $$r ∩ s$$ (the intersection of $$r$$ and $$s$$), $$r ∪ s$$ (the union of $$r$$ and $$s$$), $$r-s$$ (the difference of $$r$$ and $$s$$).

* intersection : $$a\ (r ∩ s)\ b\ \Leftrightarrow\ a\ r\ b\ ∧\ a\ s\ b$$ . In other words: if the pair $$(a,b)$$ is both in relation $$r$$ and $$s$$, then it is in the intersection of $$r$$ and $$s$$.
* union : $$a\ (r ∪ s)\ b\ \Leftrightarrow\ a\ r\ b\ \vee\ a\ s\ b$$ . In words: if the pair $$(a,b)$$ is in the relation $$r$$ or in $$s$$, then it is in the union of $$r$$ and $$s$$.
* difference : $$a\ (r-s)\ b\ \Leftrightarrow\ a\ r\ b\ ∧\ \neg(a\ s\ b)$$. In other words, the term $$r-s$$ contains all pairs from $$r$$ that are not in $$s$$.

The complement (or negation) of a relation $$r_{[A x B]}$$ is defined by means of the difference operator:

* complement : If $$r$$ is defined as $$r_{A\times B}$$, then $$\overline{r}$$ is the set of all tuples in $$A\times B$$ (the Cartesian product) that are not contained in $$r$$. So $$\overline{r} = V_{[A\times B]} - r$$

Note that the complement is defined in terms of $$A$$ and $$B$$. So, two relations with an identical population yet a different type may have different complements.

## How to type boolean operators in your script

[This page](../#notation-on-the-keyboard) shows how you can type boolean (and other) operators in your Ampersand script.

## Other explanation

Would you like a different explanation of the boolean operators? [This page](../semantics-in-sets/boolean-operators-sets.md) explains the boolean operators in terms of set theory.
