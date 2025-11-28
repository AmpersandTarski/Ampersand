<!-- For contributors. We expect a contributor to have a basic understanding of sets.  -->
# Concept Hierarchies and Database Mapping

## Overview

This document explains how Ampersand transforms `CLASSIFY` statements into relational database schemas. The process applies graph theory and lattice mathematics to create efficient database structures.

## Interpretation of concepts as sets of atoms
A `CLASSIFY` statement represents a subset relationship:

```ampersand
CLASSIFY Car ISA Vehicle
```
In mathematical notation, we informally say `Car ⊆ Vehicle`, but we actually mean that the set of atoms associated with `Car` is a subset of the set of atoms associated with `Vehicle`.

For an Ampersand user, this classify statement means that every atom that is a Car is also a Vehicle.

### Building the Concept Graph

Multiple `CLASSIFY` statements create a directed graph where nodes represent concepts and edges represent subset relationships.

**Example:**
```ampersand
CLASSIFY Car ISA Vehicle
CLASSIFY Van ISA Vehicle  
CLASSIFY Minivan ISA Van
CLASSIFY Cabrio ISA Car
```

This creates the graph:
```
Vehicle
├── Car
│   └── Cabrio
└── Van
    └── Minivan
```
Every classify statement `CLASSIFY <A> ISA <B>` yields an edge between concepts `A` and `B` in the concept graph. From this concept graph, Ampersand deduces:
1. For every `<A>`,  `CLASSIFY <A> ISA <A>` is valid, meaning that any concept is a subset of itself.
2. For every `<A>`,`<B>`,`<Z>`, if `CLASSIFY <A> ISA <B>` and `CLASSIFY <B> ISA <Z>` are valid, then so is `CLASSIFY <A> ISA <Z>`

This has implications. Suppose `CLASSIFY <A> ISA <B>` and `CLASSIFY <B> ISA <A>`. Ampersand will treat `<A>` and `<B>` as synonyms. For an Ampersand user, this means `<A>` and `<B>` can be used interchangeably.

### Strongly Connected Components
The Ampersand compiler computes strongly connected components (SCC) to identify cycles in the graph.
It condenses every SCC into one single concept, which is then known by multiple names (aliases).
For this purpose, Ampersand uses the library Algebra.Graph.AdjacencyMap.Algorithm’ (algebraic-graphs-0.7).
This removes all cycles and creates a directed acyclic graph (DAG).

**Before condensation:**
```
A → B → A    (cycle)
C → B
```

**After condensation:**
```
AB    (A and B are synonyms)
↑
C
```

## Connected Components as Join-Semilattices

### Finding concept hierarchies

Once the concept graph is a DAG, Ampersand identifies concept hierarchies in the graph by partitioning the graph into weakly connected components (WCC) in the DAG.
The purpose of that is to create database tables, one for each component.
To make that possible, Ampersand must ensure that every component has a single root.
The compiler does this by enforcing:

D → A and D → B imply there is a concept C such that A → C and B → C

Violation of this rule yields a type error.
This ensures each WCC is a join-semilattice, which the Ampersand programmer perceives as a concept hierarchy. 

### Join-Semilattice Properties

Each connected component has these mathematical properties:

1. **Partial order**: The subset relation `⊆` creates a partial order on concepts
2. **Joins exist**: Any two concepts in the component have a least upper bound (join)
3. **Unique maximal element**: Each component has exactly one most general concept (the root)

**Example component:**
```
Vehicle         (root - most general)
├── Car
│   └── Cabrio
│       └── ORF
├── Van    
│   └── Minivan
│   └── ORF
└── Motorcycle
```

In this component:
- `join(Car, Van) = Vehicle`
- `join(Minivan, Motorcycle) = Vehicle`  
- `Vehicle` is the unique root
- `ORF` (stands for Open Roof Vehicle) is both a `Van` and a `Cabrio`, but that's all right because in the end it is a vehicle, so we still have a valid concept hierarchy.

## Database Table Mapping

### One Table Per Component

Each WCC maps to exactly one database table.
The atoms of all concepts in a WCC will be maintained in this table.
Since the WCCs for a partition of all concepts, every concept has its own, unique table.

NOTE: If Ampersand were to allow the atoms of one concept to be stored in multiple tables,
we could lift the restriction of a unique root.
This seems an attractive enhancement of Ampersand because it gives us multiple inheritance.
We will save this enhancement for the future.

### Table Structure

The database table contains:
- A primary key column for atoms of the root concept identifiers
- A concept column indicating the most specific concept for each atom
- Relation columns for relations between concepts in this component

**Example table for the Vehicle component:**

| AtomID | Car  | Van  | Minivan | Cabrio | ORF  | Motorcycle | Brand      | Doors | Payload |
|--------|------|------|---------|--------|------|------------|------------|-------|---------|
| v001   | v001 | NULL | NULL    | v001   | NULL | NULL       | Toyota     | 4     | NULL    |
| v002   | NULL | v002 | v002    | NULL   | NULL | NULL       | Ford       | 5     | 1200    |
| v003   | v003 | NULL | NULL    | v003   | NULL | NULL       | BMW        | 2     | NULL    |
| v004   | NULL | NULL | NULL    | NULL   | NULL | v004       | Honda      | NULL  | NULL    |


### Query Examples

## Algorithm Summary

Ampersand's concept-to-table mapping follows these steps:

1. **Parse CLASSIFY statements** into a directed graph of subset relationships
2. **Find SCCs** to identify concept synonyms
3. **Condense SCCs** to create a DAG
4. **Find WCCs** in the DAG
5. **Verify join-semilattice properties** by checking the constraint of a unique root concept per WCC.
6. **Generate one database table** per WCC
