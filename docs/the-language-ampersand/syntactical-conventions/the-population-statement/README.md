# Population

## Purpose

To store data in a database corresponds to populating the relations in a context. Atoms are the data and pairs of atoms are inserted and deleted during the lifetime of a relation.

## Description

All pairs in a relation are called the population of that relation. All atoms in a concept constitute the population of that concept. The population of all relations and concepts in a context make the population of that context.

There are two ways to populate a concept with atoms:

* A `POPULATION` statement defines the initial population of a  concept or a relation.
* An `INCLUDE` statement defines the initial population from an xlsx-file \(i.e. an Excel speadsheet\)

[Using spreadsheets](data-in-spreadsheets.md) to define an initial population allows you to work with larger populations. Often you can use an existing spreadsheet and adapt it to become acceptable as Ampersand input.

## Syntax

You can define atoms separately and you can define the pairs in a relation. Both methods result in added population for each concept.

```text
POPULATION Tree CONTAINS
    [ "Oak"
    , "Birch"
    , "Willow"
    ]
```

```text
POPULATION personBank[Person*Bank] CONTAINS
    [ ("John", "ING")
    , ("Jane", "TRIODOS")
    ]
```

The list of pairs is a comma-separated list between square brackets. Pairs are comma-separated pairs between round brackets. Each atom is enclosed in double quotes.

