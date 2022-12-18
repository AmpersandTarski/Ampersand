---
title: The ENFORCE statement
id: the-enforce-statement
slug: /the-enforce-statement
---
# The ENFORCE statement

## Purpose

The purpose of this statement is to automatically modify the population of a relation based on rules.

## Syntax

Since ampersand 4.4.0 the syntax of this statement is:

```
ENFORCE <RelationRef> <type>? 
        <operator>
        <Term>
```

The `<operator>` can be one of **`:=`,** `:<` or `>:` .

This statement may occur anywhere within a context, either inside or outside a pattern.

## Semantics

This statement means the population of the relation will automatically be kept respectively equal ( **`:=`**), a subset (`:<`) or a superset (`>:`) of the population of the given term.

## Examples

```
ENFORCE r := s;t
{- Ampersand will keep the population of the relation r equal to the population
   of the term s;t . It will do so by changing the contents of r
   without affecting the contents of s;t .
   The effect can be observed in the prototype.
-}
```

```
ENFORCE canDrive :< hasCar /\ hasDriverLicence
{- Ampersand will keep the population of the relation canDrive smaller than
   the population of the term hasCar /\ hasDriverLicence .
   It will do so by deleting pairs from the contents of canDrive
   without affecting the contents of hasCar /\ hasDriverLicence .
   So, whenever a person can drive, that person needs to have a car and a driver licence.
   However, if that person has both these assets, it is still possible that he/she 
   cannot drive. 
-}
```

## Miscellaneous

* Both the sources and the targets of the relation and the term must match. An error message is given otherwise.
* The relation must be specified in order to use it here, as is the case with any relation used in a term.
