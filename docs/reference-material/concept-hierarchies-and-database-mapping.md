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

The root of a hierarchy also decides what the object model draws: that picture keeps one box per root and leaves the specialisations out, so a hierarchy appears in it as the single concept the whole component stands for. See [data-model pictures](./data-model-pictures.md) for the three pictures Ampersand draws of a model and what each one shows.

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

## Storing specialisations in tables of their own (MULTITABLE)

A wide table reaches the row-size limit of MySQL and MariaDB, 65535 bytes, when a hierarchy has many specialisations with univalent relations of their own: every identity column and every relation column costs up to 1022 bytes.
The compiler now estimates the row size of every table it generates and reports a table that would not fit, with the remedy below.

The remedy is to mark the concept whose direct specialisations should be stored apart:

```ampersand
CLASSIFY Document, Claim ISA Artefact
REPRESENT Artefact TYPE MULTITABLE
RELATION naam[Artefact*Text] [UNI]
```

This gives three tables.
The table `Artefact` holds the key column and the relations declared on `Artefact`, such as `naam`, and it holds a row for every artefact, also for the atoms that are neither a `Document` nor a `Claim`.
The table `Document` holds the key column and the relations declared on `Document`, and likewise for `Claim`.
A document is one row in `Artefact` and one row in `Document`; the two rows carry the same atom, and a query joins them on that value.
A specialisation that is not marked itself stays in one table together with its own specialisations, so a hierarchy below `Document` is still one wide table.

The storage graph is the concept graph without the edges that lead into a marked concept.
Every weakly connected component of that graph with one root becomes one wide table.
A component with several roots arises when a declared meet, `CLASSIFY DocClaim IS Document /\ Claim`, sits below two separately stored siblings; it is cut loose at that meet, so the meet gets a table of its own.

The mark also decides what the type checker accepts.
Two concepts with a join that is not marked have a meet, even without a declared common specialisation: an atom that is both fits in one record of the shared table, so a term such as `I[Document] /\ I[Claim]` or `r;s` through `Document` and `Claim` is accepted.
That meet gets no vertex in the concept graph; the compiler keeps it to itself, as the intersection of the two concepts, and reads it from the shared table where both columns are filled.
Two concepts whose join is marked have no meet: the mark says they are disjoint, and the same terms are type errors.

When the root should not have atoms of its own, declare it as the union of its members:

```ampersand
CLASSIFY Artefact IS Document \/ Claim
REPRESENT Artefact TYPE MULTITABLE
RELATION naam[Artefact*Text] [UNI]
```

This gives two tables, `Document` and `Claim`, each with its own column `naam`.
A read of `naam`, or of `I[Artefact]`, is the union over the two tables, and a pair of `naam` is written in the table that holds its atom.
`Artefact` has no table and no atoms of its own, so no interface can create one; the generated rule `I[Artefact] |- I[Document] \/ I[Claim]` reports an artefact that is neither.
Because `naam` is stored in two tables, its univalence is not enforced by a key column: an atom that enters both tables could hold two names, and the runtime evaluates the univalence rule as a query for such a relation.

The runtime learns the layout from `concepts.json` and `relations.json`: `conceptTables` lists every table that holds a row for an atom of a concept, `allAtomsQuery` lists the atoms of a union concept, and `mysqlTables` lists every table that stores a part of a relation.

*Proof track: [PRF-13 — an atom of a concept has a row in the table of every component that contains that concept or a generalisation of it](../proofs/README.md#prf-13).*

## Algorithm Summary

Ampersand's concept-to-table mapping follows these steps:

1. **Parse CLASSIFY statements** into a directed graph of subset relationships
2. **Find SCCs** to identify concept synonyms
3. **Condense SCCs** to create a DAG
4. **Find WCCs** in the DAG
5. **Verify join-semilattice properties** by checking the constraint of a unique root concept per WCC.
6. **Generate one database table** per WCC
