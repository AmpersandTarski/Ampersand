# Compilation process

Compilation of an Ampersand script proceeds as follows:

1. **Scripts**

   The script consists of a number of files, one of which is the main script.
   They typically have extensions '.adl' (default, for ordinary scripts), '.docadl' (for documentation, typically contains many purpose-statements), or '.ifc' (to isolate interfaces).
   The INCLUDE-statement ties the files together, with the main script as the root of the include tree.
   INCLUDE cycles are allowed because Ampersand compiles all files that are in the transitive closure of all include statements that are reachable from the main script.
2. **Lexing**

   Parsing starts with tokenization, so the parser gets to parse token sequences.
3. **Parsing**

   The parser produces a P_Context, which is a Haskell data type.
   All data types related to parsing, including all data types that start with "P_", are part of Ampersand's "P-structure" (which is not a Haskell term, by the way).
   The P_Context corresponds to the parse tree.
   It just reproduces the script in a structured way without comments and layout.
   It is Guarded to catch parser errors.
4. **Type checking**

   The function pCtx2aCtx does the type checking. It takes a P_Context and produces a Guarded A_Context, which is Guarded because it produces error messages.
   All data types related to type checking, such as A_Concept, A_Context, Expression, and all other data types that start with "A_", are part of Ampersand's "A-structure" (which is not a Haskell term either).
   The A_Context is type correct. This means:
   - Every term (Haskell: Term) in the script corresponds to precisely one expression (Haskell: Expression). Every expression 'e' has precisely one signature, which is available through 'sign e'.
5. **Upstream enrichment**

   Further enrichment takes place in pCtx2Fspec. Every algorithmically complex computation is done in the A_Context, so these algorithms may assume a type-correct script.
   pCtx2Fspec produces an FSpec.
   Steps 1 thru 5 are the upstream steps.
6. **Generation**

   Generation of code, documentation, quality analysis, etc. takes place on FSpec.
   Since algorithmically complex stuff has been done, this boils down to downstream algorithms(e.g. transforming relation algebra to relational algebra) and rendering.

## Parsing

The CLASSIFY statement places concepts in a specialization relation. `CLASSIFY `$A$` ISA `$B$ means that every instance of concept $A$ is also an instance of concept $B$.
This places concepts in a lexical hierarchy which we call *Taxonomy*. Every concept is member of precisely one taxonomy, even if it is the only member.
The parsing process builds up these taxonomies in the P-structure. It must not use information that requires type checking because that creates a bootstrap conflict.
This chapter discusses the parsing process.

### Collect all P_Concepts → `allPConceptsForGraph`

First we collect all concepts in the entire script and all classify statements as a preparation for making a concept graph:

```Haskell
let allPConceptsForGraph :: Set.Set P_Concept
    allPConceptsForGraph = 
      pConcs (p_relations <> concatMap pt_dcs p_patterns) `Set.union`
      pConcs alleGens `Set.union`
      pConcs (p_conceptdefs <> concatMap pt_cds p_patterns) `Set.union`
      -- ... etc
``` 

The initial concept graph is given by:

```Haskell
   makePGraph alleGens allPConceptsForGraph
``` 

### Build typologies → `typologies`

Cycles in the graph are treated as alias sets. As a result, if $A$ isa $B$ and $B$ isa $A$, then $A$ and $B$ are synonym to each other.
We use `makeAliasGraph` to compute alias sets and create a directed acyclic graph from it.
A typology is a weakly connected component within the alias graph.
The function `makeTypologies` creates a guarded graph. It is guarded because the initial graph may contain errors.
The set of typologies is computed by

```Haskell
   typologies <- makeTypologies (makeAliasGraph (makePGraph alleGens allPConceptsForGraph))
``` 

### Create `pCpt2aCpt` from typologies
NOW we can create pCpt2aCpt

```Haskell
   let pCpt2aCpt = makePCpt2ACpt typologies
``` 

### NOW convert `decls` using `pCpt2aCpt`
We need `pCpt2aCpt` to create a table of declarations:


### Build ContextInfo



## Type checking

### Concepts

#### Lattice Theory Foundation

Ampersand's type system for concepts is inspired by **lattice theory** (a mathematical branch of algebra).
In lattice theory, a **partially ordered set** (poset) can form a **lattice** if every pair of elements has both:
- A **join** (⊔, least upper bound/supremum): the most specific concept that generalizes both
- A **meet** (⊓, greatest lower bound/infimum): the most general concept that specializes both

A **bounded lattice** additionally has:
- A **top element** (⊤): more generic than all other elements
- A **bottom element** (⊥): more specific than all other elements

In Ampersand's concept hierarchy:
- The **partial order** is the ISA relation (specialization)
- **Join** finds the least common generalization of two concepts (e.g. join of "Animal" "Primate" is "Animal")
- **Meet** finds the greatest common specialization of two concepts  (e.g. meet of "Animal" "Primate" is "Primate")
- **topCpt** serves as ⊤ (the universal generalization, the largest concept. Everything ISA ⊤.)
- **botCpt** serves as ⊥ (the universal specialization, the smallest concept, Nothing ISA ⊥)

This lattice structure ensures that type operations are mathematically well-defined and enables sound type inference.  `topCpt` and `botCpt` yield type errors, but are needed while checking.

Ampersand expects the user to ensure that two comparable concepts have a unique join, and returns a type error if he fails to accomplish that.
If, however, a user fails to define a meet for two comparable concepts, Ampersand generates it by creating an intersection concept.
This ensures that every concept is part of a lattice, even if it is merely a singleton lattice.

#### Concept Graph and Alias Graph

Every context has a concept graph, which is used to compute meets and joins of concepts.
The P-structure contains PClassify objects, each of which represents an edge in the concept graph of the context.
In this graph, an edge from concept A to concept B means 'A ISA B'
(A is more specific than B and B is more generic than A. Every atom in A is also an atom of B, always).
The concept graph has type `AdjacencyMap P_Concept`.

Every cycle in the concept graph represents a set of aliases.
Ampersand treats aliases as identical concepts.
From the concept graph, we compute strongly connected components (SCCs) because all P_Concepts in one SCC are each other's aliases.
So, we create an alias graph in which each SCC is one node.
Ampersand uses alias graphs to define join and meet operations over Named types.
The internal helper functions `joinX` and `meetX` operate on alias graphs:

- `meetX, joinX :: (Named a, Eq a) => AdjacencyMap (Set.Set Name) -> a -> a -> Maybe a`

Every P_Concept without aliases yields an SCC with just this one P_Concept in it.
From the alias graph, we compute weakly connected components (WCC's), each of which we use in a typology:

```Haskell
data Typology = Typology
  { tyroot :: !(Set.Set Name)              -- Root alias set (most generic)
  , tyCpts :: ![Set.Set Name]              -- All alias sets, generic to specific
  , tyGrph :: AdjacencyMap (Set.Set Name)  -- Subgraph of alias graph (a WCC)
  }
```
This data structure always satisfies:

- `tyGrph` is one weakly connected component (and therefore a subgraph) of the alias graph
- `tyroot` is the most generic node in `tyGrph`.
  It is the unique root of this typology (a node with no incoming edges from more generic concepts in the directed graph).
- `tyCpts` contains exactly the vertices of `tyGrph`, sorted from generic to specific
- if two typologies (within the same context) have the same tyroot, they are equal.

The type checker ensures these invariants and generates error message when needed.
As a consequence, every node in `tyCpts` is a specialization of `tyroot` or is equal to it.
The root is the largest (i.e. most generic) node in the typology and all other nodes are smaller than the root.
Here is an example:

```Ampersand
CLASSIFY Mammal ISA Animal
CLASSIFY Primate ISA Mammal
```

This creates a concept hierarchy where:

- `Animal` is the most generic concept (root)
- `Mammal` is more specific than `Animal`
- `Primate` is the most specific concept (leaf)

In this example, `tyCpts` contains the name sets ordered from generic to specific: `[{Animal}, {Mammal}, {Primate}]`.

The typology is part of the `A_Concept`, which allows the compiler to compute the meet and join of different concepts within a context.
Two `A_Concepts` in the same typology always have a join.
Two `A_Concepts` in different typologies never have a join.
Every `A_Concept` is an object with 1 or more names (the names of the `P_concepts` that are aliases) and contains its typology.

An `A_Concept` has its typology available as an attribute.
The typology contains an alias graph (stored in the `tyGrph` field) which is used for internal join and meet calculations.
The alias graph has type `AdjacencyMap (Set.Set Name)` and is computed from the concept graph.
It contains the names of all P_Concepts in the corresponding weakly connected component.

#### Algorithm Flow: From Concept Graph to Typologies

The transformation from user-defined CLASSIFY statements to the internal typology structure follows these steps:

1. **Build Concept Graph**: Each `CLASSIFY A ISA B` statement creates a directed edge from A to B in the concept graph (type `AdjacencyMap P_Concept`).

2. **Compute Strongly Connected Components (SCCs)**: Using `Alga.scc`, identify all cycles in the concept graph. Each SCC represents a set of mutually aliased concepts (synonyms).

3. **Create Alias Graph**: Transform the concept graph into an alias graph (type `AdjacencyMap (Set.Set Name)`) where each node is a `Set.Set Name` containing all aliases in one SCC. An edge between two alias sets represents the ISA relation between any concepts in those sets.

4. **Compute Weakly Connected Components (WCCs)**: From the alias graph, find WCCs by overlaying the graph with its transpose and using depth-first search. Each WCC represents a connected hierarchy of concepts.

5. **Build Typologies**: For each WCC, create a `Typology` with:
   - `tyroot`: The unique most generic alias set (node with no incoming edges)
   - `tyCpts`: All alias sets sorted from generic to specific
   - `tyGrph`: The WCC subgraph itself

6. **Embed in A_Concepts**: Each `PlainConcept` carries its typology, enabling O(1) access to the alias graph for join/meet operations.

#### Universal Concepts

In addition to user-defined concepts, Ampersand maintains two universal concepts for lattice completeness:

- **`topCpt`** (named `_TOP`): The universal upper bound (⊤). It is more generic than every other concept. It is defined as a `PlainConcept` with `emptyTypology`.
  
- **`botCpt`** (named `_Cannot_Assign_A_type`): The universal lower bound (⊥). It is more specific than every other concept. Also defined as a `PlainConcept` with `emptyTypology`.

`topCpt` and `botCpt` are used in the type derivation as a placeholder for "any concept" and "no concept". If they cannot be resolved, the user gets a type error.

These universal concepts ensure that:
- Every concept has a join with `topCpt` (result: `topCpt`)
- Every concept has a meet with `botCpt` (result: `botCpt`)
- The type system as a whole forms a bounded lattice.

#### Join and Meet Operations

The public API for A_Concept operations includes:

- `join :: A_Concept -> A_Concept -> Maybe A_Concept`
- `meet :: A_Concept -> A_Concept -> Maybe A_Concept`

## Technical types (TType)

Every A_Concept gets precisely one technical type (TType) to determines its physical representation (in databases or whatever else).
The assignment of TTypes to concepts happens in `pCtx2aCtx` through the following process:

### 1. Collect Explicit Representations

From the P-structure, we collect all explicit `REPRESENT` statements as `P_Representation` objects. Each maps one or more `P_Concept`s to a `TType`.

### 2. Extract Object Representations

After type checking interfaces, we identify concepts that must be represented as `Object`:
In an interface, all BOX concepts need to have TType Object to ensure interfaces are physically implementable.
So, the source concept of every interface expression must be Object.
Additionally, only expressions that have a BOX subinterface following them need their target concept to be Object.
Box item expressions without a BOX (i.e., leaf/endpoint expressions) do NOT require their target concepts to be Object.

For example:

```Ampersand
INTERFACE MyInterface : I[Person] BOX
  [ "address" : lives   BOX          -- lives[Person*Address]: target Address must be OBJECT (has BOX)
      [ "street" : street             -- street[Address*Street]: target Street can be ALPHANUMERIC (no BOX, endpoint)
      , "number" : houseNr            -- houseNr[Address*Integer]: target Integer can be INTEGER (no BOX, endpoint)
      ]
  ]
```

The function `getObjReprs` extracts these concepts from the A-structure and creates `A_Representation` entries mapping them to `Object`.
Needless to say that getObjReprs must be called after interfaces have been type checked, because source and target objects are defined on Expressions (so that is in the A-structure) and not on Terms (in the P-structure).
Every concept without a representation and which is not an object, defaults to Alphanumeric.

### 3. Convert to A_Representation

We convert `P_Representation` to `A_Representation` using `pRepr2aRepr`, mapping `P_Concept`s to `A_Concept`s via the `conceptMap`.

### 4. Validate No Duplicate TTypes

The `checkDuplicateReprTypes` function validates that no concept has multiple conflicting TTypes assigned. This ensures:
- All concepts in a typology can share the same TType
- Conflicting representations are caught early with clear error messages

### 5. Build the Representation Function

Finally, `enrichedReprType` creates the function `A_Concept -> TType` that:
- Maps `ONE` and `SESSION` to `Object` (special built-in concepts)
- Maps all other concepts according to explicit and object representations
- Defaults to `Alphanumeric` if no representation is specified

This function is stored in `ctxReprType` and used throughout code generation to determine database column types.

## The use of mConstraintSig
Here is a problem:
The target of a box expression (type Expression) must be equal or narrower than the source of a box item expression.
So we cannot simply check every term in isolation because we would miss this constraint that links a box expression to its box item expressions.

The compiler uses a Signature, mConstraintSig, to link the two.
After a box expression gets its type, the target is known and used as the source of mConstraintSig.
That in turn is used as a constraint when checking the box item expressions in the box.
The type checker ensures that all box item expressions are wider than the constraint.
When checking, the target of mConstraintSig is topCpt because there is no constraint on the target of a box item expression.
Since interfaces have a recursive structure, this mechanism is used recursively throughout the interface's box tree.
The same mechanism is used for IDENT statements, VIOLATION statements, and VIEW statements, albeit they are not recursive.