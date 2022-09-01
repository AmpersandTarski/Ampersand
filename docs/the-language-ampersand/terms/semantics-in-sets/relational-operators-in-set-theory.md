# Relational operators in set theory

## Purpose of relational operators

To say things such as "the name of the owner", we want to string together multiple relations \(viz. `name` and `owner`\). Relational operators allow us to make such statements.

## Converse

A relation that contains pairs of the form $$(a, b)$$ can be altered by swapping the elements of every pair in the relation. Mathematically, $$(a, b)$$ is a different from $$(b,a)$$. This operation is called the converse operator. It produces a new relation from an existing one. It is denoted by writing $$\smallsmile\$$ \(pronounced 'wok' or ’flip’\) after the relation name. This is how converse is defined:

$$
r\smallsmile\ =\ \{ (b, a) | (a, b)∈r \}
$$

If $$r$$ has type $$[A\times B]$$, then $$r\smallsmile\$$ has type $$[B\times A]$$.

## Composition

The composition operator is denoted by a semicolon ; between two terms. It is pronounced as 'composed with', in this case: $$r$$ composed with $$s$$.

The composition operation is defined as follows: Let $$r_{[A\times B]}$$ and $$s_{[B\times C]}$$ be two relations, with the target of r being the same as the source of s. Then the composition of $$r$$ and $$s$$, is a relation with signature $$(r;s)_{[A\times C]}\ =\ \{ (a, c) | ∃ b∈B\ ∙\ a\ r\ b ∧ b\ s\ c \}$$

## Other explanation

Would you like a different explanation of the relational operators? [This page](../semantics-in-logic/relational-operators.md) explains the relational operators in logic. [This page](../semantics-in-natural-language/relational-operators-in-natural-language.md) explains them in natural language. [Click here](../semantics-in-algebra/relational-operators-in-algebra.md) for some algebraic rules about relational operators.

