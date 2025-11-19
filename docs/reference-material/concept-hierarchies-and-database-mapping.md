# Concept Hierarchies and Database Mapping

## Overview

This document explains how Ampersand transforms `CLASSIFY` statements into relational database schemas. The process applies graph theory and lattice mathematics to create efficient database structures.

## From CLASSIFY to Set Theory

### Basic Interpretation

A `CLASSIFY` statement represents a subset relationship:

```ampersand
CLASSIFY Car ISA Vehicle
```

**Mathematical meaning:** `Car ⊆ Vehicle`

Every atom that is a Car is also a Vehicle.

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

## Handling Concept Synonyms

### Strongly Connected Components

Consider these statements:
```ampersand
CLASSIFY A ISA B
CLASSIFY B ISA A
```

Both `A ⊆ B` and `B ⊆ A` hold, which means `A = B`. The concepts A and B contain identical atoms.

Ampersand detects such cycles using strongly connected components (SCC) in the concept graph. Each SCC becomes a single concept with multiple names (aliases).

### SCC Condensation

After identifying strongly connected components, Ampersand condenses each SCC into one concept node. This removes all cycles and creates a directed acyclic graph (DAG).

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

### Finding Connected Components

After SCC condensation produces a DAG, Ampersand finds connected components by treating the DAG as an undirected graph. Each connected component forms a concept hierarchy.

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
├── Van    
│   └── Minivan
└── Motorcycle
```

In this component:
- `join(Car, Van) = Vehicle`
- `join(Minivan, Motorcycle) = Vehicle`  
- `Vehicle` is the unique root

## Database Table Mapping

### One Table Per Component

Each connected component maps to exactly one database table. This mapping exploits the join-semilattice structure.

**Mathematical justification:**
- All concepts in the component share the same root concept
- Every atom belongs to at least the root concept
- Specialization creates a hierarchy within the same atom space

### Table Structure

The database table contains:
- A primary key column for atom identifiers
- A concept column indicating the most specific concept for each atom
- Relation columns for relations between concepts in this component

**Example table for the Vehicle component:**

| AtomID | Concept    | Brand   | Doors | Payload |
|--------|------------|---------|-------|---------|
| v001   | Car        | Toyota  | 4     | NULL    |
| v002   | Minivan    | Ford    | 5     | 1200    |
| v003   | Cabrio     | BMW     | 2     | NULL    |
| v004   | Motorcycle | Honda   | NULL  | NULL    |

### Storage Efficiency

This mapping provides efficiency gains:
- **No joins needed** for queries within the component
- **Sparse storage** handles specialization attributes efficiently  
- **Type checking** ensures data consistency at the concept level

### Query Examples

Finding all vehicles:
```sql
SELECT * FROM VehicleTable;
```

Finding only cars:
```sql
SELECT * FROM VehicleTable WHERE Concept = 'Car';
```

Finding vehicles with payload capacity (vans only):
```sql
SELECT * FROM VehicleTable WHERE Payload IS NOT NULL;
```

## Algorithm Summary

Ampersand's concept-to-table mapping follows these steps:

1. **Parse CLASSIFY statements** into a directed graph of subset relationships
2. **Find strongly connected components** to identify concept synonyms
3. **Condense SCCs** to create a directed acyclic graph
4. **Find connected components** in the undirected version of the DAG
5. **Verify join-semilattice properties** for each component
6. **Generate one database table** per connected component

## Mathematical Foundation

This approach rests on established mathematical principles:
- **Graph theory** for handling concept relationships
- **Lattice theory** for understanding hierarchical structures  
- **Relational algebra** for efficient database operations

The join-semilattice structure guarantees that each connected component can be stored in a single relational table while preserving all concept relationships and enabling efficient queries.
