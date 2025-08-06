# Ampersand Disambiguation Algorithm - Technical Analysis

## Architecture Overview

**OpTree Purpose:** The `OpTree Signature` data structure serves exclusively as **disambiguation scaffolding**. It temporarily mirrors the term structure to support type resolution but is not part of the final AST.

**Structural Correspondence:** `OpTree` maintains perfect 1:1 structural correspondence with `Term TermPrim`:
- Binary operations (composition, union, etc.) → `STbinary` nodes with left/right subtrees  
- Primitives and atoms → `STnullary` nodes containing signature lists

## Data Structure Definition

```haskell
data OpTree a = STbinary {lSigns :: OpTree a, rSigns :: OpTree a, opSigns :: [a]}
              | STnullary {opSigns :: [a]}
```

## Two-Phase Disambiguation Process

### Phase 1: Signature Collection (`signatures` function)
```haskell
signatures :: ContextInfo -> Term TermPrim -> Guarded (OpTree Signature)
```

**Process:**
- **Recursively traverse** the `Term TermPrim` structure
- **Compute all possible signatures** for each subterm based on context
- **Store signature sets** in corresponding `OpTree` nodes
- **Result:** `OpTree Signature` with identical shape to input term, containing all possible type interpretations

### Phase 2: Disambiguation (`t2e` function)  
```haskell
t2e :: OpTree Signature -> Term TermPrim -> Guarded Expression
```

**Process:**
- **Pattern match** on `(Term, OpTree)` pairs to ensure structural alignment
- **Apply type constraints** to narrow down signature choices
- **Recursively refine** subterm signatures based on compatibility rules
- **Produce final** `Expression` with unique, consistent signatures

## Refinement Algorithm

The core mechanism works through **progressive signature set refinement**:

1. **Start** with all possible signatures at each node
2. **Apply constraints** from binary operations (composition requires compatible source/target)
3. **Eliminate incompatible** signature combinations  
4. **Propagate constraints** up and down the tree
5. **Continue until** unique signatures remain (or error if ambiguous/impossible)

## Key Functions

### `term2Expr` - Main Entry Point
```haskell
term2Expr contextInfo term = do
  sgnTree <- signatures contextInfo term  -- Phase 1: Build OpTree with all possible signatures
  t2e sgnTree term                        -- Phase 2: Disambiguate by matching term+OpTree patterns
```

### Pattern Matching Examples
```haskell
case (trm, sgnTree) of
  (PCps _ a b, STbinary stLeft stRight sgns) -> -- Composition: binary term + binary OpTree
  (Prim tp,    STnullary sgns)               -> -- Primitive: nullary term + nullary OpTree
```

## Issue #1417: Infinite Loop Bug

**Date Discovered:** August 5, 2025  
**Location:** `src/Ampersand/ADL1/P2A_Converters.hs`, lines 1245-1256  
**Specific Line:** Line 1252: `Just between<-[meet conceptsGraph tgta srcb]`

### Bug Description
The infinite loop occurs in **Phase 2** during composition signature refinement when processing expressions like:
```
sessionActiveRoles [SESSION*PrototypeContext.Role];navItemRoles~;isVisible [SESSION*PrototypeContext.NavMenuItem]
```

### Root Cause Analysis Update (August 5, 2025)

**IMPORTANT CORRECTION:** The `meet` and `join` functions are **NOT** the source of the infinite loop.

**Evidence from `src/Ampersand/Core/AbstractSyntaxTree.hs` (lines 1339-1380):**
- **Well-defined terminating algorithms** using `reflexiveClosure (transitiveClosure conceptsGraph)`
- **Extensive test results** in comments showing systematic testing on various concept pairs  
- **Proper graph theory implementation** - correctly compute greatest lower bound (meet) and least upper bound (join)
- **Guaranteed termination** through use of transitive closure on directed acyclic graphs

The infinite loop occurs **during the disambiguation refinement process** but **not** in the core graph operations themselves.

### Debugging Trace Analysis
```
Signatures yields:
   sessionActiveRoles [SESSION*PrototypeContext.Role];navItemRoles~;isVisible [SESSION*PrototypeContext.NavMenuItem]
      sessionActiveRoles [SESSION*PrototypeContext.Role] [SESSION*PrototypeContext.Role]
      navItemRoles~;isVisible [PrototypeContext.Role*PrototypeContext.NavMenuItem]
---   sessionActiveRoles [SESSION*PrototypeContext.Role];navItemRoles~;isVisible [[SESSION*PrototypeContext.NavMenuItem]]
conceptsGraph: overlay (vertex ONE) (edges [...])
sgnsa; [[SESSION*PrototypeContext.Role]], sgnsb; [[PrototypeContext.Role*PrototypeContext.NavMenuItem]], opSigns stLeft; [[SESSION*PrototypeContext.Role]]
```

**Key Observation:** The double-bracketed signatures `[[...]]` indicate nested signature processing where the refinement gets stuck.

### Affected Code Section
```haskell
triples = case [ trace ("Between "<>showP a<>" and  "<>showP b<>" (meet): "<>tshow between<>"\n   "<>tshow (Sign srca tgtb, Sign srca between, Sign between tgtb))
                 (Sign srca tgtb, Sign srca between, Sign between tgtb)
               | Sign srca tgta<-sgnsa, Sign srcb tgtb<-sgnsb
               , Just between<-[meet conceptsGraph tgta srcb]  -- ← INFINITE LOOP IN CALLING CONTEXT
               ] of
```

**Note:** The `meet` function itself is correct and terminates. The infinite loop occurs in the **calling context** - likely in the signature refinement logic that repeatedly calls these functions.

### Design Assessment
**The fundamental architecture is sound.** The issue is not with the disambiguation approach but with **missing cycle detection** in the signature refinement process during concept graph traversal.

## Proposed Solution Direction
- **Do NOT modify `meet`/`join` functions** - they are correct and well-tested
- Add **cycle detection** in the signature refinement calling logic
- Implement **termination bounds** for the disambiguation refinement process
- Debug the **list comprehension** or **recursive calling pattern** that invokes `meet` repeatedly
- Focus on the **"way down" in `term2Expr`** as mentioned in current work

## Notes
- This analysis was conducted during active debugging session on August 5, 2025
- The bug manifests consistently across different test files when PrototypeContext elements are involved
- The issue is specifically in the type checker, not in database connectivity as initially suspected
- **August 5, 2025 Update:** Confirmed that `meet`/`join` functions in `AbstractSyntaxTree.hs` are correct and terminate properly
- The infinite loop is in the **disambiguation refinement process** that calls these functions, not in the functions themselves
- Focus area: **"the way down" in `term2Expr` function** where signature refinement occurs
