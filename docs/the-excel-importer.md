# The Excel Importer

The Excel Importer allows you to import data from an Excel file in a \(working\) prototype, effectively adding it to the population of a running prototype. This text is meant for the Ampersand user who has more population than can be dealt with by `POPULATION` statements. It is also useful if you have existing spreadsheets filled with tables, which you want to use as population in your Ampersand-script. Please note that some editing of your Excel-file may be required, to make Ampersand understand how columns match Ampersand-relations.

## Installation

Nothing special is required for installation; this extension is enabled by default

## Importing Excel files in a running prototype

If the user has the required access rights to import a population \(by default no login/roles are required\), the excel import is available via the Applications icon in the right half of the menu bar.

## How to create importable Excel files

When you have a prototype running for an Ampersand context, you can import data for that prototype from an Excel file, which effectively adds the population specified in the Excel file to the population that is currently already in the database. This section describes how to construct an Excel file that can be used to do this.

There are 2 ways to import an Excel file. One is by using an appropriate INTERFACE definition \(automatically detected based on sheet name equals INTERFACE name\). The benefit of using this method is that you do not have to change your Excel files when you modify the names of relations in your amperand model. It is also more readable for third parties. The other is the \(traditional\) \[import block\] syntax.

### Using an INTERFACE defintion

To be further specified. Functionality is already implemented.

This method is used to import data from a page in a spreadsheet document whenever the title of the page is the name of an INTERFACE that you have defined in your script. So if your page is called `Accounts`, and you have defined

```text
INTERFACE "Accounts": I[Account] cRud BOX
   [ "Username": accUserid cRUd
   , "Password": accPassword cRUd
   , "Role": accAllowedRoles cRUd
   ]
```

your page could look like this:

| Account | Username | Password | Role |
| :--- | :--- | :--- | :--- |
| \_NEW | Student1 | Stud1PW | Student |
| \_NEW | S2 | Stud2PW | Student |

Impoprting this page will create two accounts, one for Student1 and another for S2.

As you can see, the first cell must have the concept of the INTERFACE \(`Account`\), and subsequent header fields have the names of the labels in the INTERFACE. You can change the order of the columns, as long as the first column is left as is.

### Using the \[import block\] syntax

Let us consider a small \(useless\) Ampersand model, defined as follows:

```text
rAA :: A*A [PROP]
rAB :: A*B [UNI]
rAC :: A*C
sAB :: A->B
tAD :: A*Delta
uBA :: B*A
```

If you want to specify data elements in an Excel file in order to populate such a model, you must define so called 'blocks' that contain this data. The importer looks for such blocks throughout the Excel file \(meaning that you can have blocks on different sheets - all sheets will be inspected\).

Here is an example of such a 'block' \(note: all blocks must start in the leftmost column; if not, they are disregarded\):

| \[A's\] | rAA | rAB | rAC | rAC | rAC | sAB | tAD | uBA~ |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| A | A | B | C | C | C | B | Delta | B |
| alfa1 | alfa1 | beta1 | char1 | char2 | char3 | beta2 | d1 | beta1 |
|  | CMT |  |  |  |  |  |  | beta2 |
| alfa2 |  | beta2 |  | char2 |  |  | d2 | beta3 |
| alfa3 |  |  | char4 | char3 | char2 | beta1 | d1 |  |

Here is the specification of a block: 1. A 'block' consists of 2 header rows followed by lines of data. A 'block' terminates whenever a next block starts or the end of a sheet is reached. Empty lines are disregarded. 2. Every cell in the leftmost column whose contents starts with the character '\[' is the first cell in the first header row of a block. The contents of this cell is further disregarded. 3. Subsequent cells in the first header row must either be empty, or contain the name of a relations that is known in your Ampersand model, optionally followed by a `~` \(flip\) character. In our example, such cells may hence only contain '', `rAA`, `rAB`, `rAC`, `sAB`, `tAD` or `uBA~`. 4. The second header row only contains cells that are either empty or contain a concept name, or contain a concept name and delimiter. In our example, such cells may only contain '', `A`, `B`, `C`, or `Delta`. 5. The first cell \(in the second header row\) must contain the source \(left\) concept of all relations specified in the first header row. It may not be empty. 6. Every subsequent cell \(in the second header row\) must either be empty, or contain the name of the target \(right\) concept of the relation that is specified in the same column in the first header row. 7. There is an exception to the previous two specifications: if a cell in the first header row specifies a flipped relation, the leftmost cell in the second header row specifies the target concept for the \(unflipped\) relation, and the cell below the flipped relation specifies the source concept for the \(unflipped\) relation. In the example, all relations in the first header row have source concept `A`, except for relation `uBA`, which has concept `A` as its target concept, and `B` as its source concept. 8. Every subsequent row in this block is called a data row. Cells in a data row are either empty or non-empty. If a non-empty cell contains a formula, this formula is evaluated to obtain the cell contents. If a non-empty does not contain a formula, its contents is obtained as is \(see notes for errors in formula-evaluation\). From here on, when we talk about 'the contents of a cell', the obtained value from \(evaluating the term in\) that cell is meant.

Data rows are interpreted as follows:

* When the first cell in a data row is empty, the content of all other cells in that row is disregarded \(you may use such cells to include comments, computations, or whatever else you like\)
* When the first cell in a data row is not empty, the content of all other non-empty cells and the content of the first cell may define a set of pairs \(srcAtom,tgtAtom\), each of which is to be inserted into the population of the Ampersand model, where
  * 'srcAtom' is the contents of the first cell
  * 'tgtAtom' is the \(untrimmed\) contents of a non-empty cell
  * for the relation to which the pair \(srcAtom,tgtAtom\) is to be added,
    * its name is specified in the first row of the block in the same column as 'tgtAtom'
    * its SRC Concept is specified on the second header row in the first column;
    * its TGT Concept is specified on the second header row in the same column as 'tgtAtom'.

This means that the example is equivalent with the following population specification \(note that the cell containing 'CMT' is disregarded as it is comment\):

```text
POPULATION rAA CONTAINS [ ("alfa1"), ("alfa1") ] 

POPULATION rAB CONTAINS [ ("alfa1"), ("beta1") ] 
POPULATION rAB CONTAINS [ ("alfa2"), ("beta2") ] 

POPULATION rAC CONTAINS [ ("alfa1"), ("char1") ] 
POPULATION rAC CONTAINS [ ("alfa1"), ("char2") ] 
POPULATION rAC CONTAINS [ ("alfa1"), ("char3") ] 
POPULATION rAC CONTAINS [ ("alfa2"), ("char2") ] 
POPULATION rAC CONTAINS [ ("alfa3"), ("char2") ] 
POPULATION rAC CONTAINS [ ("alfa3"), ("char3") ] 
POPULATION rAC CONTAINS [ ("alfa3"), ("char4") ] 

POPULATION sAB CONTAINS [ ("alfa1"), ("beta2") ] 
POPULATION sAB CONTAINS [ ("alfa3"), ("beta1") ] 

POPULATION tAD CONTAINS [ ("alfa1"), ("d1") ] 
POPULATION tAD CONTAINS [ ("alfa2"), ("d2") ] 
POPULATION tAD CONTAINS [ ("alfa3"), ("d1") ] 

POPULATION uBA CONTAINS [ ("beta1"), ("alfa1") ] 
POPULATION uBA CONTAINS [ ("beta3"), ("alfa2") ]
```

## NOTES

1. You need NOT know about the internals of the database to use this plugin \(at least, that's the idea\).
2. You may specify formulae instead of texts. The result of the formula will be read \(and converted to text\) before being inserted into the database. This allows for dynamic construction of identifiers, precomputation of tables, date adaptations to the date of today, etc. Note, however, that this does not always work flawlessly. In particular, the functions `VLOOKUP` and `HLOOKUP` are known to produce errors \(that we are not capable of fixing\), so such functions should be avoided.
3. If you use '\_NEW' in the first column, the \(dirty\) identifier for the atom will be automatically generated. If you use '\_NEW' in a subsequent column on the same row, this will be replaced with the \(dirty\) identifier for the source atom \(which you can use e.g. to populate property-relations\). Note that as we also support formulae, you may use those to achieve the same result \(and excercise control over the actual dirty identifiers used\)
4. It is possible to store all sorts of data in the spreadsheet that will not interfere with the database population. The contents of the following cells is disregarded and can therefore be used for other purposes:
   * cells in a row whose first cell is empty.
   * cells in a column where the cell that specifies the relation name or the TGT concept is empty.
5. When you use something like 'CLASSIFY X ISA Y' in your model, and want to populate an atom 'xy', then you should populate it in the block where 'X's are populated. In this block, you can not only populate relations that have source concept X, but also relations that have source concept Y.

That's all, Folks!

