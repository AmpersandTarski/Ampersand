# Ampersand Producttype Pattern

## When do you use a producttype?

**ONLY** when you need **tuples** (pairs, triples, quadruplets) for interfaces or logic.

A producttype is an **algebraic product** of concepts for tuple representation, NOT for automation or data management.

## Anatomy of a producttype

Each producttype automatically includes **functions** (@1, @2, @3, etc.) that provide access to the components of the tuple.

### Example: Triple producttype A × B × C

```ampersand
CONCEPT TripleType "Producttype of A, B and C"

-- Component functions (always UNI,TOT)
RELATION tripleA[TripleType*A] [UNI,TOT]  -- @1 function
RELATION tripleB[TripleType*B] [UNI,TOT]  -- @2 function  
RELATION tripleC[TripleType*C] [UNI,TOT]  -- @3 function
```

### Automatic CREATE/DELETE/ENFORCE rules

**ALWAYS required** with a producttype according to this exact pattern:

```ampersand
-- Input coupling
RELATION inputTriple[InputConcept*TripleType] [UNI]

-- CREATE rule
RULE TripleCreate : 
  (I[InputConcept] /\ compA;compA~ /\ compB;compB~ /\ compC;compC~) - inputTriple;inputTriple~ |- 
  inputTriple;I[TripleType];inputTriple~
VIOLATION ( TXT "{EX} InsAtom;TripleType"
          , TXT "{EX} InsPair;inputTriple;InputConcept;", SRC I, TXT ";TripleType;_NEW"
          , TXT "{EX} InsPair;tripleA;TripleType;_NEW;A;", SRC compA
          , TXT "{EX} InsPair;tripleB;TripleType;_NEW;B;", SRC compB  
          , TXT "{EX} InsPair;tripleC;TripleType;_NEW;C;", SRC compC
          )

-- ENFORCE rules: keep functions synchronous
ENFORCE tripleA := inputTriple~;compA
ENFORCE tripleB := inputTriple~;compB
ENFORCE tripleC := inputTriple~;compC

-- DELETE rule
RULE TripleDelete : 
  inputTriple |- compA;tripleA~ /\ compB;tripleB~ /\ compC;tripleC~
VIOLATION ( TXT "{EX} DelAtom;TripleType;", TGT I )

ROLE ExecEngine MAINTAINS TripleCreate, TripleDelete
```

## Why these rules are needed

1. **Tuple integrity**: Ensures that each tuple has all components
2. **Automatic synchronization**: Input changes propagate to tuple
3. **Cleanup**: Removes tuples when input disappears
4. **Consistency**: Functions remain synchronous with input

## Mistake I made

These rules are **always mandatory** with every producttype, because the functions must exist and be filled with the correct data (total therefore).
Producttype = tuple + automatic function rules.

## ExportEisen.ifc as reference

Always use `PATTERN LandenEisenProductTypes` as template for:
- Component relations with [UNI,TOT]
- CREATE rule with correct conditions
- ENFORCE rules for each component  
- DELETE rule for cleanup
- ExecEngine role assignment
