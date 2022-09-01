# Relational operators in algebra

## Purpose of relational operators

To say things such as "the name of the owner", we want to string together multiple relations \(viz. `name` and `owner`\). Relational operators allow us to make such statements.

There are two relational operators: the converse \($$\smallsmile$$\) and the composition \(semicolon $$;$$ \). This page discusses the most important laws about these operators.

## Converse

There are two things you should know about the converse operator. The first is that the converse of the converse gives you the relation itself, whatever that relation may be:

$$
{r\smallsmile}\smallsmile\ =\ r
$$

The second thing you should know is that arguments switch places if the converse is brought outside \(or inside\) brackets

$$
r\smallsmile ; s\smallsmile\ =\ (s;r)\smallsmile
$$

## Composition

The composition operator is denoted by a semicolon \(;\) between two terms. It is pronounced as 'composed with', in this case: $$r$$ composed with $$s$$.

Composition is associative, which means:

$$
r;(s;t)\ =\ (r;s);t
$$

The meaning stays the same, no matter how you place the brackets. So Ampersand lets you omit brackets entirely. You may write $$r;s;t$$ instead of $$r;(s;t)$$ or $$(r;s);t$$.

Composition has a left and a right identity. Let $$r_{[A\times B]}$$ be a relation, then

$$
I_A;r\ =\ r\ \ \ \text{and}\ \ \ r;I_B\ =\ r
$$

## How to type relational operators in your script

[This page](../#notation-on-the-keyboard) shows how you can write these things in your Ampersand script.

## Other explanation

Would you like a different explanation of the semantics of the relational operators? [Click here](../semantics-in-sets/relational-operators-in-set-theory.md) for an explanation in sets. [This page](../semantics-in-natural-language/relational-operators-in-natural-language.md) explains them in natural language. [This page](../semantics-in-logic/relational-operators.md) explains the relational operators in logic.

