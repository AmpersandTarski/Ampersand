# Ampersand Synonym Mechanism and Type System Architecture

## Summary
Ampersand's type system builds a concept graph from ISA declarations and uses graph algorithms.
It uses strongly connected components (SCCs) to identify and merge synonymous concepts.
It then uses weakly connected components to prepare concepts for sharing database tables.
Finally it makes a complete lattice of every WCC to allow restriction-free storage of instances from a WCC in a single database table.

This page explains the mechanism and its theoretical foundation.

## The Concept Graph

The concept graph is a directed graph where:
- **Vertices** represent concepts
- **Edges** represent ISA relationships  
- An edge A → B means "A is more specific than B" or "atoms of A are a subset of atoms of B"

This partial ordering allows us to use both lattice theory (meet, join) and graph theory (vertices, edges) terminology interchangeably.

## Synonym Detection via Strongly Connected Components (SCCs)
Synonyms occur naturally during collaborative development, where different team members use different terminology, or when merging codebases with different naming conventions.
Time can also cause synonyms, when further development of code sees legacy terminology evolve into newer terminology.
For such reasons, Ampersand allows developers to use synonyms rather than forcing immediate standardization.

Two concepts A and B are **synonymous** when:
- A ISA B (A is more specific than B)
- B ISA A (B is more specific than A)

This mutual specialization creates a cycle in the concept graph, meaning A and B represent the same semantic entity with different names.

### Detection Algorithm
The compiler detects synonyms in the `graph2dag` function by computing strongly connected components (SCC).
An SCC is a maximal set of vertices where every vertex can reach every other vertex via directed paths. This captures precisely the synonym relationship.

```haskell
graph2dag :: AdjacencyMap A_Concept -> AdjacencyMap A_Concept
graph2dag conceptsGraph = overlay (edges dagEdges) (vertices sccReps)
  where
    sccGraph = Alga.scc conceptsGraph  -- Find SCCs
    sccs = vertexList sccGraph
    sccReps = map mintConcept sccs     -- Merge each SCC into one concept
```

When an SCC contains multiple concepts, the function `mintConcept` preserves these concepts as aliases. This allows the compiler to recognize any synonym while internally using a unified representation.

```haskell
mintConcept :: NonEmpty.AdjacencyMap A_Concept -> A_Concept
mintConcept scc = case L.sortBy (\a b -> compare (fullName a) (fullName b)) 
                        (NE.toList $ NonEmpty.vertexList1 scc) of
  []           -> fatal "Empty SCC"
  [c]          -> c  -- Singleton SCC, no synonyms
  primary:others -> PlainConcept { 
    aliases = Set.unions (collectAliases primary : map collectAliases others) 
  }
```

After merging synonyms, cycles have been eliminated and the graph becomes a Directed Acyclic Graph (DAG).
This acyclic structure is needed for the lattice operations meet and join,
to produce type hierarchies.
As a result the type checker can derive a single unique type for every (sub-)term
or produce a type-error otherwise.

## Typologies
If `Animal → Dog → Poodle` has no ISA relationship to `Vehicle → Car → Tesla`, they form separate typologies. This reflects semantic independence - concepts in different domains sharing no ISA-relationships.

A typology is an independent concept hierarchy - a tree-like structure with:
- A **root concept** (most general)
- **Ordered concepts** from specific to generic
- No ISA relationships to concepts outside the typology

The `wcComponents` function partitions the DAG:

```haskell
wcComponents :: AdjacencyMap A_Concept -> [AdjacencyMap A_Concept]
wcComponents dagGraph = map (createSubgraph dagGraph) (Alga.dfsForest symmetricGraph)
  where
    -- Convert DAG to undirected by overlaying with transpose
    symmetricGraph = overlay dagGraph (transpose dagGraph)
```

All concepts in a WCC have the same technical type: ALPHANUMERIC, INTEGER, DATE, OBJECT, FLOAT, or any other technical type Ampersand supports.

To represent all concepts in a WCC in the same database table,
every record in that table represents an instance of the most general concept.
This implies that there is a single most generic concept in the WCC.
From a hierarchical point of view, we say the WCC needs a single root concept.
In lattice theory we say that every pair of concepts in the WCC must have one join,
i.e. the `join` of those concepts exists and is unique.
Also, we want to allow that two instances of a different concept are equal.
For example, a `Person` can be `Female` and `Italian` at the same time.
This requires that every pair of concepts in the WCC has one meet.
In order to allow every possible combination of concepts from the same WCC to reside in the same database table,
the function `completeLattice` enhances every WCC to become a complete lattice.

```haskell
completeWCCs = [ overlay wcc (completeLattice wcc)
               | wcc <- wcComponents dagGraph ]
```

This function adds synthetic concepts where needed to guarantee that every pair with a join has a meet, and vice versa.

## Implementation Details

### File Locations

- **Synonym detection**: `src/Ampersand/ADL1/P2A_Converters.hs` - `graph2dag` function
- **Typology creation**: `src/Ampersand/ADL1/P2A_Converters.hs` - `wcComponents`, `makeTypologies` 
- **Lattice operations**: `src/Ampersand/Core/AbstractSyntaxTree.hs` - `meet`, `join` functions
- **Disambiguation**: `src/Ampersand/ADL1/P2A_Converters.hs` - `signatures`, `term2Expr`

### Processing Pipeline

1. **Parse** ISA declarations and relation signatures
2. **Build concept graph** with all concepts as vertices, ISA as edges
3. **Detect synonyms** via SCC analysis → merge into DAG
4. **Partition into typologies** via WCC analysis on DAG
5. **Complete lattices** by adding synthetic meet/join concepts
6. **Type check expressions** using meet/join on completed lattices
7. **Disambiguate** relation references using signature refinement
