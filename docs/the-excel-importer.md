---
title: The Spreadsheet Importer
id: the-excel-importer
---

# Importing data from spreadsheets

The Spreadsheet Importer allows you to import data from a spreadsheet in .xlsx format into your prototype.
This adds the data from your spreadsheet to the population of your Ampersand prototype.
It saves you from tediously writing too many `POPULATION` statements.
It is also useful if you have existing spreadsheets filled with tables, which you want to use as population in your Ampersand-script.

## Installation
Nothing special is required for installation; this extension is enabled by default

## Importing data from your .xlsx-file
There are two ways of importing spreadsheets: at compile time and at run time.
1. Compile time
   The Ampersand compiler will read population from your .xlsx-file and add it to the (initial) population of your prototype. It is as though you would have written POPULATION statements with much less work than you would otherwise have. The compile-time importer has a bit more functionality, so you have more options to import data as compared to the runtime importer.
2. Run time
   You add population to a running prototype. In general this is somewhat faster because runtime environment checks all the rules remain satisfied.  Please note that some editing of your .xlsx-file may be required, to make Ampersand understand how columns match Ampersand-relations.

To import data at compile time, write (one or more) `INCLUDE` statements in your script. Please include file names between double quotes, like this:
```Ampersand
INCLUDE "mySpreadsheet.xlsx"
INCLUDE "./project/Clasmates.xlsx"
```
These statements must be inside a `CONTEXT`, like all statements in Ampersand.

To import data into your script at run time, look for the importer on the HOME page of your running prototype.
The importer will guide you through the process.

## How to create importable spreadsheets
You must prepare your .xlsx-file, so the importer understands where to look for content.
You have two options. Pick one depending on whether you want to keep interfaces or relations stable as you develop your prototype iteratively.
1. prepare to match your data to interfaces from your script.
   Every column in the spreadsheet must match a field in the interface.
   This allows you to keep the interface constant as you are developing the relations in your script.
2. prepare to match you data to relations from your script.
   Every column in the spreadsheet must match a relation in your script.
   This allows you to work on rules and interfaces if the set of relations is relatively stable.

Both options will import data for you into your prototype.
If the name of a sheet in your .xlsx-file matches the name of one of your interfaces (case sensitively), the importer will use the INTERFACE-approach.
It parses all other data in the RELATION approach.


### Using the INTERFACE approach
The benefit of this method is that you do not have to change your spreadsheets when you modify the names of relations in your Amperand script.
It is also more readable for third parties because the spreadsheet has a direct correspondence with the field labels of the prototype on your screen.

This method is used to import data from a page in a spreadsheet document whenever the title of the page is the name of an INTERFACE that you have defined in your script. So if your page is called `Accounts`, and you have defined

```text
INTERFACE "Accounts": I[Account] cRud BOX
   [ "Username": accUserid cRUd
   , "Password": accPassword cRUd
   , "Role": accAllowedRoles cRUd
   ]
```

the corresponding sheet in your .xlsx-file could look like this:
| `Account` | `Username` | `Password` | `Role` |
| :--- | :--- | :--- | :--- |
| `_NEW` | `peterpan` | `Stud1PW` | `Student` |
| `_NEW` | `dollydot` | `Stud2PW` | `Student` |

Impoprting this page creates two accounts, one for `peterpan` and another for `dollydot`.

As you can see, the first cell must have the concept of the INTERFACE \(`Account`\), and subsequent header fields have the names of the labels in the INTERFACE. You can change the order of the columns, as long as the first column is left as is.

### Using the RELATION approach

The importer uses relations from your script. As an example, assume you have written:
```Ampersand
   RELATION rAA[A*A] [PROP]
   RELATION rAB[A*B] [UNI]
   RELATION rAC[A*C]
   RELATION sAB[AB] [UNI, TOT]
   RELATION tAD[A*Delta]
   RELATION uBA[B*A]
```
The importer will look for blocks of data in your .xlsx-file, to fill these relations with.
It recognizes a block by a block starter, which is a square bracket in the leftmost field of any row that is not in an INTERFACE sheet.
The block continues until the importer finds the next block starter in column A.

The importer looks for such blocks throughout the .xlsx-file.
You can have blocks on different sheets and you can have multiple blocks on one sheet.

Here is an example of a 'block'.

| `[A]` | `rAA` | `rAB` | `rAC` | `[rAC,]` | `rAC` | `sAB` | `[tAD/]` | `uBA~` |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `A` | `A` | `B` | `C` | `C` | `C` | `B` | `Delta` | `B` |
| `alfa1` | `alfa1` | `beta1` | `char1` | `char2` | `char3` | `beta2` | `d1/d2` | `beta1` |
|  | `CMT` |  |  |  |  |  |  | `beta2` |
| `alfa2` |  | `beta2` |  | `char2` |  |  | `d2/pete/d1` | `beta3` |
| `alfa3` |  |  | `char4` | `char3, char4` | `char2` | `beta1` | `d1` |  |

Here is what to remember:
1. Each 'block' consists of 2 header rows followed by an arbitrary number of rows containing data.
2. A 'block' terminates whenever a next block starts or the end of a sheet is reached.
   The importer disregards rows of which the first column is empty. (This creates room for comments, computations, or whatever you like.)
3. Every cell in the leftmost column whose contents starts with the character '\[' is the first cell in the first header row of a block.
   The importer disregards all other contents of this cell.
4. Every other cell in the first header row contains either:
   * nothing, which causes the importer to disregard the entire column in that block, or
   * the name of a relation that is known in your Ampersand script, or
   * the name of a relation that is known in your Ampersand script followed by a `~` \(flip\).

   In our example, the cells contain '', `rAA`, `rAB`, `rAC`, `sAB`, `tAD` and `uBA~` because these relations are known in the Ampersand script.
   The importer will fail if it encounters a string that is not a relation.
5. The second header row contains either:
   * empty cells, or
   * cells that contain a concept name, which occurs in the script, or
   * cells that specify a concept name, which occurs in the script, together with a delimiter.

   In our example, such cells may only contain '', `A`, `B`, `C`, or `Delta` because these are the only concepts defined in the script.
   The importer will fail if it encounters a string that is not a concept.
6. Since Ampersand allows reuse of relation names, the importer harvests the signature of each relation name from the second row.
   The first cell in that row represents the source \(left\) concept of every relation specified in the first header row. The importer will fail if that cell is empty.
   The cell directly below the relation name represents the target concept. If it is empty, the importer disregards the entire column of that block.
   So, the relation in the fourth column has name `rAC`, source `A`, and target `C`.
7. The exception to the previous is: if a relation name in the first row is followed by `~`, the source and target switch places.
   So, the relation in the last column is in fact `uBA[B*A]`.
8. Every subsequent row in this block is called a data row. Cells in a data row are either empty or non-empty.
   If a non-empty cell contains a formula, this formula is evaluated to obtain the cell contents.

So, assuming that concept $A$ is in the first column of row 2, a relation name $r$ in the first row and directly underneath a concept name $B$ constitutes a relation $r[A*B]$, which is valid if it is declared in your script. Data rows are interpreted as follows:

9. In any data row, the leftmost cell and the cell underneath a relation are considered a pair in that relation, provided they are not empty. So, the cells in row A together with the cells in a column constitute the contents of the relation specified in the first two rows.
10. Sometimes, people put multiple values in one cell, separated by a delimiter like comma, semicolon, or whatever.
    If the second row of a column specifies not only a concept, but also a delimiter, you can handle this situation.
    The importer recognizes `[rAC,]` as a multi-column, which means that cells can contain multiple values separated by a comma.
    Similarly, `[tAD/]` means that cells can contain multiple values separated by a slash-symbol.

This means that the example is equivalent with the following population specification.
Note that the importer disregards the cell containing 'CMT'.

```text
POPULATION rAA CONTAINS [ ("alfa1"), ("alfa1") ]

POPULATION rAB CONTAINS [ ("alfa1"), ("beta1")
                        , ("alfa2"), ("beta2") ]

POPULATION rAC CONTAINS [ ("alfa1"), ("char1")
                        , ("alfa1"), ("char2")
                        , ("alfa1"), ("char3")
                        , ("alfa2"), ("char2")
                        , ("alfa3"), ("char2")
                        , ("alfa3"), ("char3")
                        , ("alfa3"), ("char4") ]

POPULATION sAB CONTAINS [ ("alfa1"), ("beta2")
                        , ("alfa3"), ("beta1") ]

POPULATION tAD CONTAINS [ ("alfa1"), ("d1")
                        , ("alfa1"), ("d2")
                        , ("alfa2"), ("d1")
                        , ("alfa2"), ("d2")
                        , ("alfa2"), ("pete")
                        , ("alfa3"), ("d1") ]

POPULATION uBA CONTAINS [ ("beta1"), ("alfa1")
                        , ("beta3"), ("alfa2") ]
```

## NOTES

1. This way of importing data is independent of the database implementation internally. You need NOT know about the internals of the database to use the importer.
2. In your spreadsheet you will typically have formulas too.
   The importer uses the result of each formula \(and convert it to an atom\), so it can insert pairs of atoms into the database.
   This allows for dynamic construction of identifiers, precomputation of tables, date adaptations to the date of today, etc.
   Note, however, that Excel has flaws, which cause some functions to misbehave. Always check your results.
   (We know, for example, that functions `VLOOKUP` and `HLOOKUP` have produced errors in the past, so you may avoid such functions.)
3. If you use '\_NEW' in the first column, the importer generates a new atom that differs from all other atoms. If you use '\_NEW' in a subsequent column on the same row, this stands for the newly generated atom from the first column \(which you can use e.g. to populate property-relations\).
4. It is possible to store all sorts of data in the spreadsheet that will not interfere with the database population. The contents of the following cells is disregarded and can therefore be used for other purposes:
   * cells in a row whose first cell is empty.
   * cells in a column where the cell that specifies the relation name or the TGT concept is empty.
5. When you use something like 'CLASSIFY X ISA Y' in your model, and want to populate an atom 'xy', then you should populate it in the block where 'X's are populated. In this block, you can not only populate relations that have source concept X, but also relations that have source concept Y.

## TODO's
1. Performance limits

   It is not quite clear how large spreadsheets can be before exceeding the capacity of the importers. Several hundreds of records is usually fine, which satisfies most prototyping needs.
2. Error handling and validation

   Error messages, validation feedback, or debugging is not yet implemented in the runtime importer, so you get crappy error messages if anything fails.

3. Differences between the compile-time and run-time importer:
   1. The runtime importer does not implement multi-columns yet.

4. built-in datatypes

   The importer recognizes the built-in datatypes of spreadsheet: strings, dates, numbers, etc.
   It transforms these datatypes to Ampersand's built-in datatypes without you noticing it. However, if it expects a different type than your .xlsx-file contains, it gives an error.
   This documentation must be refined to specify this transformation, so you can understand what happens.
   The error messages in the importers, especially the run-time importer, are yet to be improved to assist in such situations.

## Design considerations
Data import has been designed to facilitate the reuse of existing spreadsheets in .xlsx format.
This allows you to make prototypes with (a limited amount of) real user data.

