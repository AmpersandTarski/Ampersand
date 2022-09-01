# Patterns

## Purpose

Patterns are meant to isolate discussions and make solutions reusable, as known from [design patterns](http://en.wikipedia.org/wiki/Design\_pattern).

## Description

A pattern is a set of [rules](broken-reference) that describes a theme or a general reusable solution to a commonly occurring problem.

For instance, if specific concerns about security arise, you might want to discuss this with stakeholders in security. With them you can discuss which rules in particular constitute your solution. Divide your problem in smaller pieces and discuss each piece with just the right stakeholders. This allows you to go deeper by talking to the right people. It saves time as well by freeing others from having to participate. An even larger benefit arises if you reuse patterns that have been discussed and scrutinized before. The best thing comes once your stakeholders agree. By that time, your pattern represents their agreement formally in Ampersand, so you can use it in the larger context of the information system.

## Example

```
PATTERN Security 

RELATION required[Subject*Destination]
MEANING "A subject that you must have passed to qualify for the school trip to a destination"

RELATION pass[Subject*Student]
MEANING "The subjects that have been passed by specific students"

RELATION attends[Student*Destination]

PURPOSE RULE guardPrerequisites
{+ This rule prevents students from registering for a trip
without having passed the required courses. +}
RULE guardPrerequisites : attends;required |- pass

ENDPATTERN
```

## Syntax

Every pattern has the following form:

```
PATTERN <pattern name>
    <pattern element>*
ENDPATTERN
```

A pattern consists of any number of pattern elements in an arbitrary order. The following pattern elements are allowed:

|                    |                                                                                                          |
| ------------------ | -------------------------------------------------------------------------------------------------------- |
| `<rule>`           | a statement that declares a [rule](broken-reference)                                                     |
| `<classify>`       | a statement that specifies generalization/specialization of [concepts](broken-reference)                 |
| `<relation>`       | a declaration of a relation, stating the existence of a [relation](relations.md) within the context      |
| `<conceptDef>`     | a description of a [concept](broken-reference), to document its meaning                                  |
| `<representation>` | a statement that defines the atomic type of a [concept](../tutorial-rap3/conceptual-model-enrollment.md) |
| `<roleRule>`       | a statement that makes a role responsible for satisfying a rule                                          |
| `<ident>`          | a rule that defines an identity on a concept                                                             |
| `<viewDef>`        | a statement for presenting facts in a readable sentence                                                  |
| `<purpose>`        | a statement to describe the purpose of a pattern or a pattern element                                    |
| `<population>`     | a statement that sums up the initial population of a relation                                            |

## Good practice

A model can have as many patterns as you want.\
It has no effect on how the code is processed.

The service definition must be outside a pattern

A pattern contains rules in an arbitrary order.\
The context in which these rules are valid must contain the definition for each of the relations that are used in those rules.\
It is good practice to declare all relations in the pattern itself.\
That practice makes the pattern self-contained and therefore more suitable for reuse.

Ampersand advocates **one theme in one pattern**. Stakeholders confine their discussion to one theme, and deliver the result in one pattern.

## Restrictions

In the current implementation of Ampersand, patterns are defined within a context. (This will change in a future version.) If you want to reuse patterns, you have to cut-and-paste them from one context to another. In the future, there will be a better mechanism for reusing patterns in different contexts.
