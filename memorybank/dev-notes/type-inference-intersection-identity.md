# Type Inference for Intersections with Identity Relations

## Overview

This document explains the type inference logic for intersections in Ampersand, with special attention to intersections involving identity relations (I[Concept]). The implementation is in `src/Ampersand/ADL1/P2A_Converters.hs` in the `checkPeri` function.

## Meet vs Join: Why Intersection Uses Meet and Union Uses Join

The `checkPeri` function handles both intersection (`/\`) and union (`\/`). They use different lattice operations:

- **Intersection uses `meet`** (greatest lower bound, `∧`)
- **Union uses `join`** (least upper bound, `∨`)

### Lattice Theory Background

In a concept hierarchy with generalization relations:
- **Meet (∧)** finds the most specific concept that is a generalization of both inputs
- **Join (∨)** finds the most generic concept that both inputs generalize to

Example hierarchy:
```
     Person
     /    \
Employee  Student
     \    /
  Workingstudent
```

- `meet(Employee, Student) = Workingstudent` (most specific common specialization)
- `join(Employee, Student) = Person` (most generic common generalization)

### Why Intersection Uses Meet

For intersection `a /\ b`:
- We want pairs that are in BOTH relations
- The result type must be a **subtype** of both operand types
- We need the **most specific** type that works for both

Example:
```ampersand
r1[Employee*Document] /\ r2[Student*Document]
```

The source must be something that is BOTH Employee AND Student, which is Workingstudent (the meet).
Result: `[Workingstudent*Document]`

### Why Union Uses Join

For union `a \/ b`:
- We want pairs that are in EITHER relation
- The result type must be a **supertype** of both operand types  
- We need the **most generic** type that encompasses both

Example:
```ampersand
r1[Employee*Document] \/ r2[Student*Document]  
```

The source can be ANY Employee OR ANY Student, which includes all Persons (the join).
Result: `[Person*Document]`

## General Rule for Intersection Types

For a general intersection `a /\ b` where:
- `a` has signature `[A*B]`
- `b` has signature `[C*D]`

The standard type inference computes:
```
result = [meet(A,C) * meet(B,D)]
```

This works by independently computing the meet (greatest lower bound) of:
- Sources: `meet(A, C)` - most specific type that works for both sources
- Targets: `meet(B, D)` - most specific type that works for both targets

## General Rule for Union Types

For a general union `a \/ b` where:
- `a` has signature `[A*B]`
- `b` has signature `[C*D]`

The type inference computes:
```
result = [join(A,C) * join(B,D)]
```

This works by independently computing the join (least upper bound) of:
- Sources: `join(A, C)` - most generic type that encompasses both sources
- Targets: `join(B, D)` - most generic type that encompasses both targets

### Example from ARM20-Test5.adl

Consider a basic intersection without identity:
```ampersand
oblTAV[Verplichting*Bedrijfsfunctie] /\ critBF[Criterium*Bedrijfsfunctie]~
```

Type analysis:
- Left: `oblTAV` has type `[Verplichting*Bedrijfsfunctie]`
- Right: `critBF~` has type `[Bedrijfsfunctie*Criterium]`
- Result: `[meet(Verplichting, Bedrijfsfunctie) * meet(Bedrijfsfunctie, Criterium)]`

If no meet exists, the intersection is type-incorrect.

## The Identity Constraint Problem

Identity relations `I[C]` have a special semantic property: they only contain pairs `(x,x)` where both elements are identical. This is expressed in their signature as `ISgn C` (internally) or `[C*C]` (display form).

When we intersect with `I[C]`, we're saying: "Give me only the pairs from the other relation where both atoms can equal each other."

For the intersection to be non-empty, we need atoms that:
1. Are in `Verplichting` (source of right relation)
2. Are in `Criterium` (target of right relation)  
3. Can be equal to themselves (identity constraint)

Since atoms can only equal themselves within a single concept, we need:
```
result = [meet(Verplichting, Criterium) * meet(Verplichting, Criterium)]
```

The result must be **endomorphic** (source = target) because identity forces both sides to be the same atom.

Given `CLASSIFY Verplichting ISA Criterium`, we have:
- `meet(Verplichting, Criterium) = Verplichting`
- Result: `[Verplichting * Verplichting]` ✓

## Implementation Logic

The `checkPeri` function handles four cases for intersections:

### Case 1: Both operands have Sign signatures (standard case)
```haskell
[ ((combinator (expr_a, expr_b), Sign src tgt, pCombinator trm_a trm_b), ...)
| (expr_a, Sign src_a tgt_a, trm_a)<-sgnsa, (expr_b, Sign src_b tgt_b, trm_b)<-sgnsb
, Just src<-[meetORjoin conceptsGraph src_a src_b]
, Just tgt<-[meetORjoin conceptsGraph tgt_a tgt_b]
]
```

This is the standard rule: independently compute meet of sources and targets.

### Case 2: Left has Sign, right has ISgn (identity on right)
```haskell
[ ((combinator (expr_a, expr_b), Sign cpt cpt, pCombinator trm_a trm_b), ...)
| (expr_a, Sign src_a tgt_a, trm_a)<-sgnsa, (expr_b, ISgn cpt_b, trm_b)<-sgnsb
, Just between_a <- [meetORjoin conceptsGraph src_a tgt_a]  -- Step 1
, Just cpt<-[meetORjoin conceptsGraph between_a cpt_b]      -- Step 2
]
```

**Step 1**: Compute `between_a = meet(src_a, tgt_a)` - the overlap between source and target of left relation.

**Step 2**: Compute `cpt = meet(between_a, cpt_b)` - the overlap with the identity's concept.

**Result**: `[cpt * cpt]` - an endomorphic signature.

#### Example
For `oblTAV;critBF~ /\ I[Criterium]`:
- Left: `[Verplichting * Criterium]`
- Right: `I[Criterium]` (ISgn Criterium)
- Step 1: `between_a = meet(Verplichting, Criterium) = Verplichting` (since Verplichting ISA Criterium)
- Step 2: `cpt = meet(Verplichting, Criterium) = Verplichting`
- Result: `[Verplichting * Verplichting]` ✓

### Case 3: Left has ISgn, right has Sign (identity on left)  
```haskell
[ ((combinator (expr_a, expr_b), Sign cpt cpt, pCombinator trm_a trm_b), ...)
| (expr_a, ISgn cpt_a, trm_a)<-sgnsa, (expr_b, Sign src_b tgt_b, trm_b)<-sgnsb
, Just between_b <- [meetORjoin conceptsGraph src_b tgt_b]  -- Step 1
, Just cpt<-[meetORjoin conceptsGraph cpt_a between_b]      -- Step 2
]
```

This is the mirror of Case 2, handling `I[C] /\ r[S*T]`.

#### Example  
For `I[Criterium] /\ oblTAV;critBF~`:
- Left: `I[Criterium]` (ISgn Criterium)
- Right: `[Verplichting * Criterium]`
- Step 1: `between_b = meet(Verplichting, Criterium) = Verplichting`
- Step 2: `cpt = meet(Criterium, Verplichting) = Verplichting`
- Result: `[Verplichting * Verplichting]` ✓

### Case 4: Both have ISgn (both are identities)
```haskell
[ ((combinator (expr_a, expr_b), ISgn cpt, pCombinator trm_a trm_b), ...)
| (expr_a, ISgn cpt_a, trm_a)<-sgnsa, (expr_b, ISgn cpt_b, trm_b)<-sgnsb
, Just cpt<-[meetORjoin conceptsGraph cpt_a cpt_b]
]
```

For `I[A] /\ I[B]`, the result is `I[meet(A,B)]` if the meet exists.

## Why This Matters

Consider this incorrect result with the old logic:
```ampersand
I[Criterium] /\ oblTAV;critBF~  -- Old: [Criterium*Criterium]
```

This would claim that ALL Criterium pairs are in the intersection, which is wrong! Only Verplichting pairs (a subset) should be included, because:
1. `oblTAV;critBF~` only produces pairs where the source is a Verplichting
2. The identity constraint requires source = target
3. Therefore, both must be Verplichting

The correct result `[Verplichting*Verplichting]` accurately represents this constraint.

## Key Insights

1. **Identity is special**: It's not just another endomorphic relation; it enforces that source and target must be the same atom.

2. **Meet propagation**: When intersecting with identity, we must propagate the meet constraint to BOTH source and target, not independently.

3. **Semantic preservation**: The type system must preserve the semantic constraint that intersecting with identity produces only equal pairs.

4. **Order independence**: Cases 2 and 3 ensure that `I[C] /\ r` and `r /\ I[C]` produce the same result.

## Testing

The test case `ARM20-Test5.adl` validates this behavior:
```ampersand
CLASSIFY Verplichting ISA Criterium

RULE testIdentityIntersection:
  I[Criterium] /\ oblTAV;critBF~  :: [Verplichting*Verplichting]
```

The type checks correctly as `[Verplichting*Verplichting]`
