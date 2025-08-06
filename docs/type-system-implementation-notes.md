# Type System Implementation Notes - Lessons for Documentation

## Overview
These notes capture important lessons learned from mathematical analysis of the `signatures` function in `src/Ampersand/ADL1/P2A_Converters.hs` on August 5, 2025. These insights should be incorporated into user-facing documentation to bridge the gap between simplified explanations and the sophisticated reality of the implementation.

## Key Lessons Learned

### 1. Untyped Identity Relations (`I`)

**Current Understanding:** Untyped identity `I` allows any concept during disambiguation.

**Implementation Detail:**
```haskell
Prim (PI _) -> pure (STnullary [ISgn anyCpt])
```

**Documentation Improvement Needed:**
- **Simplified explanation**: "Identity relations must specify a concept: `I[Person]`"
- **Complete explanation**: "Untyped `I` generates all possible identity signatures during type inference. The disambiguator will either narrow to exactly one concept (success) or produce an error asking for explicit type annotation."

**User Benefit:** Users should understand that `I` without brackets is allowed but may require disambiguation context.

### 2. Composition Requirements - The Meet vs Equality Distinction

**Current Documentation Gap:** Documentation often states "target of left operand must equal source of right operand."

**Mathematical Reality:** Composition `a;b` requires a **meet** (greatest lower bound) between `target(a)` and `source(b)`, not strict equality.

**Implementation Detail:**
```haskell
PCps o a b -> checkIntra o "composition" a b meet true
-- The real check happens here:
Just between<-[meet conceptsGraph tgta srcb]
```

**Documentation Layers Needed:**

#### Beginner Level:
- "For composition `a;b`, the target concept of `a` must match the source concept of `b`"
- Examples with identical concepts: `owner[Person*Building];location[Building*City]`

#### Intermediate Level:
- "Composition works when target and source concepts are compatible through IS-A relationships"
- Examples: `student[Session*Student];name[Person*Name]` (where `Student ISA Person`)

#### Advanced Level:
- "Composition requires a non-empty meet between target and source concepts in the concept hierarchy"
- Technical details about concept lattice operations

**Real-World Examples to Add:**
```
-- This works because Student ISA Person
students[Course*Student];name[Person*Name]

-- This works because Manager ISA Employee  
manages[Department*Manager];salary[Employee*Amount]

-- This fails because no relationship exists
color[Car*Color];temperature[Weather*Degrees]
```

### 3. Two-Phase Disambiguation Architecture

**Current Documentation Gap:** The sophisticated two-phase approach isn't well explained.

**Key Insight:** The `true` parameter in composition isn't "allowing anything" - it's part of a careful two-phase strategy:

1. **Phase 1 (Permissive)**: Generate all mathematically possible signature combinations
2. **Phase 2 (Precise)**: Apply exact compatibility checks using meet/join operations

**Documentation Enhancement:**
```
The type checker uses a two-phase approach:
1. Signature Generation: Collect all possible type interpretations
2. Signature Refinement: Apply mathematical constraints to find valid combinations

This allows the system to handle complex concept hierarchies while providing precise error messages.
```

### 4. Boolean Operators and Concept Compatibility

**Current Understanding:** Boolean operators require compatible signatures.

**Implementation Precision:** Uses meet/join operations on both source and target concepts:
```haskell
checkPeri ... = do
  conceptsSrc = [src | ..., Just src<-[meetORjoin conceptsGraph (source sgn_a) (source sgn_b)]]
  conceptsTgt = [tgt | ..., Just tgt<-[meetORjoin conceptsGraph (target sgn_a) (target sgn_b)]]
```

**Documentation Addition:**
- Explain that `r ∪ s` requires compatible source AND target concepts
- Show examples with concept hierarchies
- Clarify difference between intersection (meet) and union (join) in type checking

### 5. Error Message Quality

**Current State:** Implementation provides detailed error messages with signature mismatches.

**Documentation Opportunity:**
- Include examples of actual error messages users will see
- Explain how to interpret signature notation `[Person*Building]`
- Provide guidance on how to fix common type errors

## Specific Documentation Files to Update

### `docs/reference-material/terms.md`
- Add "Advanced Composition" section explaining meet-based compatibility
- Include concept hierarchy examples
- Clarify when equality is sufficient vs when meet is needed

### `docs/reference-material/syntax-of-ampersand.md`
- Expand identity relation section to explain untyped `I` behavior
- Add troubleshooting section for common type errors

### New File: `docs/reference-material/type-system-details.md`
- Technical explanation of two-phase disambiguation
- Concept lattice and meet/join operations
- Advanced examples with complex hierarchies

## Examples to Develop

### Composition with Hierarchies
```ampersand
CONCEPT Person
CONCEPT Student ISA Person
CONCEPT Course

RELATION enrolled[Student*Course]
RELATION name[Person*Text]

-- This works: Student ISA Person, so meet(Student,Person) = Student
VIEW StudentNames: Student(enrolled;name)
```

### Error Message Walkthrough
```
Error: "Cannot match the signatures of the two sides of the composition"
What it means: The target concept of the left expression and source concept of the right expression have no relationship in the concept hierarchy.
How to fix: Check your IS-A relationships or use explicit type conversion.
```

## Implementation Validation

**Status:** Mathematical analysis confirms all cases in `signatures` function are correct:
- ✓ Primitive terms (identity, atoms, relations)  
- ✓ Boolean operators (union, intersection, difference)
- ✓ Composition with meet-based compatibility
- ✓ Residual operators with proper ordering
- ✓ Product operations

**Quality Assurance:** The implementation is more sophisticated than simplified documentation suggests, supporting advanced features like concept hierarchies seamlessly.

## The Two-Phase Type System Architecture (August 6, 2025)

**ARCHITECTURAL PRINCIPLE:** The type system implements a clean separation between signature generation and signature selection through a bottom-up then top-down flow:

### Bottom-Up Phase: `signatures` - Signature Bubbling

**Purpose:** Generate ALL mathematically valid signature possibilities without premature filtering.

**Key Properties:**
1. **Pure Generation** - Only produces type-correct possibilities, never invalid types
2. **Complete Coverage** - Captures all valid signatures from concept hierarchy relationships  
3. **No Premature Filtering** - Preserves all mathematically sound combinations
4. **Structural Mirroring** - `OpTree Signature` perfectly mirrors `Term TermPrim` structure

**Implementation Pattern:**
```haskell
signatures :: ContextInfo -> Term TermPrim -> Guarded (OpTree Signature)

-- Composition example: bubbles up ALL valid composition signatures
signatures contextInfo (PCps o a b) = do
  leftTree  <- signatures contextInfo a  -- Get all valid signatures for left operand
  rightTree <- signatures contextInfo b  -- Get all valid signatures for right operand
  
  -- Generate ALL mathematically valid compositions
  let validCompositions = [ Sign (source sigL) (target sigR)
                          | sigL <- opSigns leftTree
                          , sigR <- opSigns rightTree  
                          , isJust (meet conceptsGraph (target sigL) (source sigR)) ]
  pure $ STbinary leftTree rightTree validCompositions
```

### Top-Down Phase: `t2e` - Signature Filtering to Singletons

**Purpose:** Filter signature possibilities down to unique choices, propagating constraints from parent to children.

**The Singleton Invariant:** Every subterm MUST end with exactly one signature when `t2e` completes.

**Key Properties:**
1. **Pure Selection** - Never generates new signatures, only filters existing ones
2. **Constraint Propagation** - Parent signature constrains child signature sets
3. **Error at Ambiguity** - Multiple signatures at any level → type error with clear message
4. **Recursive Refinement** - Each level filters its children based on selected signature

**Implementation Pattern:**
```haskell
t2e :: OpTree Signature -> Term TermPrim -> Guarded Expression

-- Composition: selected signature constrains left/right operands  
t2e sgnTree@(STbinary leftTree rightTree [uniqueCompSig]) (PCps o a b) = do
  -- uniqueCompSig = Sign srcComp tgtComp constrains operands:
  let filteredLeft  = filterTargetConcepts (source uniqueCompSig) leftTree   
  let filteredRight = filterSourceConcepts (target uniqueCompSig) rightTree  
  ECps <$> ((,) <$> t2e filteredLeft a <*> t2e filteredRight b)

-- Type error when ambiguous
t2e (STbinary _ _ sigs) (PCps o _ _) | length sigs /= 1 = 
  Errors $ return $ CTXE o ("Ambiguous composition signatures: " <> show sigs)
```

**Signature Filtering Functions:**
```haskell
-- Filter left operand: only keep signatures whose target is compatible with composition source
filterTargetConcepts :: A_Concept -> OpTree Signature -> OpTree Signature
filterTargetConcepts requiredSrc tree = 
  tree { opSigns = filter (isCompatibleTarget requiredSrc) (opSigns tree) }
  where isCompatibleTarget req sig = isJust (meet conceptsGraph (target sig) req)

-- Filter right operand: only keep signatures whose source is compatible with composition target  
filterSourceConcepts :: A_Concept -> OpTree Signature -> OpTree Signature  
filterSourceConcepts requiredTgt tree = 
  tree { opSigns = filter (isCompatibleSource requiredTgt) (opSigns tree) }
  where isCompatibleSource req sig = isJust (meet conceptsGraph (source sig) req)
```

### Algorithm Flow Summary

```haskell
-- The complete flow:
term2Expr :: ContextInfo -> Term TermPrim -> Guarded Expression
term2Expr contextInfo term = do
  sgnTree <- signatures contextInfo term    -- Bottom-up: bubble all valid signatures  
  t2e sgnTree term                         -- Top-down: filter to singletons
```

**Flow Diagram:**
```
Term TermPrim  ──signatures──→  OpTree [Signature]  ──t2e──→  Expression
(parse tree)    (bottom-up)     (all possibilities)  (top-down)  (unique sigs)
```

### Debugging and Troubleshooting for Contributors

**Common Patterns:**
1. **Signature Generation Issues**: Check `signatures` - are all valid possibilities being generated?
2. **Ambiguity Errors**: Check `t2e` - is parent signature properly constraining children?
3. **Type Mismatches**: Check concept hierarchy - are `meet`/`join` operations working correctly?

**Trace Output Interpretation:**
```haskell
-- The trace shows the signature bubbling process
trace ("--- " <> showP trm <> " " <> tshow (opSigns sgnTree)) $
-- Indicates: term + all possible signatures at this level
```

**Key Functions and Their Roles:**
- `checkIntra`: For operations requiring compatible interior concepts (composition, residuals)
- `checkPeri`: For operations requiring compatible perimeter concepts (union, intersection)  
- `meet`: Greatest lower bound in concept hierarchy (for composition compatibility)
- `join`: Least upper bound in concept hierarchy (for union operations)

### The Power of Clean Separation

This architecture provides:
1. **Maintainability**: Clear separation of concerns between generation and selection
2. **Debuggability**: Easy to trace whether issues are in signature generation or filtering
3. **Extensibility**: New operators just need to implement their signature generation logic
4. **Performance**: No redundant computation - each phase does exactly what it needs to do

---

**Date:** August 6, 2025  
**Analysis Context:** Mathematical verification of type system implementation  
**Major Update:** Complete architectural documentation of bottom-up signature bubbling and top-down singleton filtering  
**Next Steps:** 
1. Use this documentation to guide new contributors to the type system
2. Reference this architecture when debugging type inference issues
3. Incorporate key insights into user-facing documentation
