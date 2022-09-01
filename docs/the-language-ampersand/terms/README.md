---
description: >-
  This page describes the notion of term. Its subpages provide several
  interpretations of terms, all of which are valid so you can use each
  interpretation at your own discretion.
---

# Terms

## Purpose

The purpose of a term is to compute pairs that constitute a relation. We use operators to assemble terms from smaller terms, to express in formal language precisely what is meant in the natural language of the business. The smallest term is a single relation.

We noticed that our readers have different backgrounds. They have different preferences about the way we explain the operators in Ampersand. Some prefer an explanation in logic, others in algebra, and still others in set theory. So we decided to explain the operators in many different ways simultaneously, hoping that one of them suits your preference.

## Description

A term is a combination of operators and relations. Its meaning is a set of pairs, which is in fact a newly created relation. The word "expression" may be used as a synonym for "term" in the context of Ampersand.

## Examples

`owner`

`r;s~`

`I /\ goalkeeper;goalkeeper~`

`destination;"Algarve" |- spoken;"Portugese"`

## Syntax

Every term is built out of relations, which are combined by operators. An term has one of the following 8 syntactic structures

```
<Term> <BinaryOperator> <Term>
<UnaryOpPre> <Term>
<Term> <UnaryOpPost>
<RelationRef> <type>?
I <type>?
V <type>?
<atom>
( <Term> )
```

## `Operators`

The operators come in families. We advise novices to study only the rule operators, boolean operators and relational operators. There is a wealth of things you can express with just these operators. The residual operators seem harder to learn and the Kleene operators are not fully implemented yet. You can click the hyperlink to navigate to the semantics of each family.

| Family                                                   |                   binary operators | binding power |       unary operators | binding power |
| -------------------------------------------------------- | ---------------------------------: | ------------- | --------------------: | ------------- |
| rules                                                    |           $$=$$ and $$\subseteq\$$ | 1 (weakest)   |                       |               |
| [boolean](semantics-in-logic/boolean-operators.md)       |      $$\cup$$, $$\cap$$, and $$-$$ | 2             | $$\overline{\strut}$$ | prefix        |
| [relational](semantics-in-logic/relational-operators.md) | $$;$$, $$\times$$, and $$\dagger$$ | 4             |       $$\smallsmile$$ | postfix       |
| [residual](semantics-in-logic/residual-operators.md)     |   $$\backslash$$, $$/$$, and $$♢$$ | 3             |                       |               |
| Kleene                                                   |                                    |               |       $$∗$$ and $$+$$ | postfix       |

## Brackets

Operators with different binding power may be used in the same term without brackets, because the binding power tells how it is interpreted. For example, $$r\cap s;t$$ means $$r\cap(s;t)$$ because $$;$$ has a higher binding power than $$\cap$$.

Operators with the same binding power must be used unambiguously. For example: $$r\cap(s-t)$$ means something different than $$(r\cap s)-t$$. In such cases Ampersand insists on the use of brackets, so readers without knowledge of the binding powers of the operators can read a term unambiguously.

Repeated uses of an associative operator does not require brackets. So $$r\cap s \cap t$$ is allowed because $$\cap$$ is associative.

## Notation on the keyboard

When coding in Ampersand, these operators are typed with characters on the keyboard. The following table shows the operators in math and their equivalent in code:

| operator name                |  code |          math          | remark                                     |                    |
| ---------------------------- | :---: | :--------------------: | ------------------------------------------ | ------------------ |
| equivalence (equal)          |  `=`  |          $$=$$         | use only in a rule                         |                    |
| inclusion                    | `\|-` |      $$\subseteq$$     |                                            | use only in a rule |
| intersect                    |  `/\` |          $$∩$$         | associative, commutative, idempotent       |                    |
| union                        |  `\/` |          $$∪$$         | associative, commutative, idempotent       |                    |
| difference (minus)           |  `-`  |          $$-$$         |                                            |                    |
| complement                   |  `-`  | $$\overline{\strut }$$ | in code: Prefix; in math: Overline         |                    |
| compose                      |  `;`  |          $$;$$         | associative                                |                    |
| converse (flip)              |  `~`  |     $$\smallsmile$$    | postfix                                    |                    |
| left residual                |  `/`  |          $$/$$         |                                            |                    |
| right residual               |  `\`  |     $$\backslash$$     |                                            |                    |
| diamond                      |  `<>` |      $$\Diamond$$      |                                            |                    |
| relational product           |  `!`  |       $$\dagger$$      | associative                                |                    |
| cartesian product            |  `#`  |       $$\times$$       | deprecated                                 |                    |
| reflexive transitive closure |  `*`  |          $$∗$$         | in code: not implemented; in math: Postfix |                    |
| transitive closure           |  `+`  |          $$+$$         | in code: not implemented; in math: Postfix |                    |
