# Ampersand Language Theory and Type System Notes

## Mathematical Foundation

### Core Laws of Relation Algebra
- **Flip Law**: `r~~ = r` - The flip of the flip of a relation is always the relation itself
- **Composition Associativity**: `r;(s;t) = (r;s);t`
- **Identity Laws**: `I[A];r = r` and `r;I[B] = r` for relation `r[A*B]`
- **Converse Distribution**: `(r;s)~ = s~;r~`
- **Boolean Laws**: Union and intersection are commutative, associative, and idempotent

## Relation Types and Signatures

### Basic Types
- **Concept**: User-defined types (start with uppercase, e.g., `Person`, `Building`)
- **Built-in Types**: `ONE`, `SESSION`, plus technical types like `Alphanumeric`, `Date`, etc.
- **Signature**: `[Source*Target]` defines the type of a relation
- **Type Checking**: Relations must be type-compatible for operations

### Special Relations
- **Identity**: `I[C]` - relates every atom in concept C to itself
- **Complete Relation**: `V[A*B]` - relates every atom in A to every atom in B
- **Empty Relation**: Can be denoted by `-V` (negation of complete relation)

## Operators and Their Types

### Boolean Operators (Binding Power 2)
- **Union** (`\/`, `∪`): `r ∪ s` contains pairs in either r or s
- **Intersection** (`/\`, `∩`): `r ∩ s` contains pairs in both r and s  
- **Difference** (`-`): `r - s` contains pairs in r but not in s
- **Complement** (`-`): `¬r = V[A*B] - r` for relation `r[A*B]`

### Relational Operators (Binding Power 4 - Highest)
- **Composition** (`;`): `r;s` - chains relations through common intermediate concept
- **Converse/Flip** (`~`): `r~` - swaps source and target of all pairs
- **Cartesian Product** (`#`): Syntactic sugar for `;V;`

### Residual Operators (Binding Power 3)
These are the complex ones for type checking:

#### Left Residual (`/`)
- **Syntax**: `s/r` 
- **Logic**: `a (s/r) b ⟺ ∀x: b r x → a s x`
- **Type Rule**: For `s[A*C]` and `r[B*C]`, result is `(s/r)[A*B]`
- **Meaning**: "All x where if b relates to x via r, then a relates to x via s"

#### Right Residual (`\`)  
- **Syntax**: `r\s`
- **Logic**: `a (r\s) b ⟺ ∀x: x r a → x s b` 
- **Type Rule**: For `r[C*A]` and `s[C*B]`, result is `(r\s)[A*B]`
- **Meaning**: "All x where if x relates to a via r, then x relates to b via s"

#### Diamond (`<>`)
- **Syntax**: `r<>s`
- **Logic**: `a (r<>s) b ⟺ ∀x: a r x ⟺ x s b`
- **Meaning**: Bidirectional implication

### Rule Operators (Binding Power 1 - Weakest)
- **Equivalence** (`=`): Both sides have same population
- **Inclusion** (`|-`): Left side is subset of right side

## Type System Architecture

### Concepts and Representations
- **Object vs Value**: Concepts with no `REPRESENT` statement are Objects; others are Values
- **Type Representations**: `ALPHANUMERIC`, `INTEGER`, `DATE`, `BOOLEAN`, etc.
- **Specialization**: `CLASSIFY A ISA B` creates type hierarchy

### Signature Matching
- **Exact Match**: Source and target concepts must be identical
- **Generalization**: Can match if one concept generalizes another via ISA relationships
- **Meet Operation**: Finds most specific common concept in type lattice
- **Join Operation**: Finds most general common concept

## Type Checking Algorithm

### Expression Trees (OpTree)
```haskell
data OpTree a = STbinary (OpTree a) (OpTree a) [a]
              | STnullary [a]
```
- Each node contains possible (expression, signature) pairs
- Binary nodes have left/right subtrees for operands
- Type checking reduces possibilities by applying operator constraints

### Disambiguation Process
1. **Signature Generation**: Create OpTree with all possible type interpretations
2. **Constraint Application**: Apply operator-specific type rules
3. **Ambiguity Resolution**: Reduce to single signature per node
4. **Error Reporting**: Report type mismatches or ambiguities

### Residual Operator Type Checking
Key insight from code analysis:

#### Left Residual (`PLrs a b`)
In `signatures`: `ELrs a (flp b) join isGeq`
- Processes as `ELrs a b~` (b is flipped in signature generation)
- In `t2e`: Must handle that `stRight` already represents `b~`
- By flip law: `flp(flp b) = b`, so `flpRight = fmap flp stRight` gets back to original `b`

#### Right Residual (`PRrs a b`) 
In `signatures`: `ERrs (flp a) b join isLeq`
- Processes as `ERrs a~ b` (a is flipped in signature generation)  
- Similar pattern but with left operand flipped

### Type Inference Rules

#### Composition (`r;s`)
- Requires `target(r)` meets `source(s)` 
- Result: `source(r)` to `target(s)`
- Uses `meet` operation to find common concept

#### Boolean Operations (`r ∪ s`, `r ∩ s`, `r - s`)
- Both operands must have compatible signatures
- Uses `meet` on both source and target concepts
- Result has meet of sources and meet of targets

#### Residual Operations
Complex type rules involving generalization relationships:
- Left residual: `isGeq` comparison (target of left ≥ target of right)  
- Right residual: `isLeq` comparison (source of left ≤ source of right)

## Error Handling

### Type Errors
- **Signature Mismatch**: When operators can't find compatible types
- **Ambiguous Signatures**: Multiple valid type interpretations  
- **Undefined Relations**: References to non-existent relations
- **Arity Errors**: Wrong number of operands

### Disambiguation Errors
- Provide specific suggestions for resolving ambiguity
- Show all possible interpretations
- Include context-specific error messages

## Implementation Notes for Ampersand Compiler

### Key Files
- `P2A_Converters.hs`: Main type checking and disambiguation
- `signatures` function: Generates type possibilities  
- `t2e` function: Reduces to single expression per term
- `checkIntra`/`checkPeri`: Type checking for different operator classes

### Critical Functions
- `meet`/`join`: Concept lattice operations
- `isGeq`/`isLeq`: Generalization comparisons
- `termPrim2Expr`: Converts primitive terms with type constraints
- `flp`: Converse operation (satisfies flip law)

### Common Patterns
- Composition and residuals use similar type checking (`checkIntra`)
- Boolean operators use different pattern (`checkPeri`) 
- Identity and complete relations need special handling for `ANY` concept
- Error messages should be context-specific and helpful

## Debugging Type Issues

### Trace Information
- Use `trace` statements to show signature derivation
- `showOpTree` provides readable format for OpTree structures
- Debug mode shows detailed type analysis

### Common Issues
- Double-flipping in residual operators
- Incorrect concept hierarchy handling
- Ambiguous relation references
- Missing type annotations where needed

## Relation to Mathematical Theory

### Residuation Theory
Ampersand implements residuated Boolean algebra where:
- `/` is left residual: `r;(s/r) ⊆ s`
- `\` is right residual: `(r\s);r ⊆ s`  
- `<>` is diamond (equivalence): `r<>s = (r/s~) ∩ (s~/r)`

### Boolean Algebra
Standard Boolean algebra with:
- Distributive laws
- De Morgan's laws (via complement)
- Absorption laws

This forms the mathematical foundation for Ampersand's type system and expression evaluation.
