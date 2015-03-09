--[Introduction]--
This is the README file for the plugin 'ImportExcelFiles', which allows you to import a population into a (running) Ampersand prototype, where this population is specified in an Excel file.

--[Plugin installation]--
This is done automatically [Michiel: please verify!] 

--[Plugin use]--
When you have a prototype running for an Ampersand context, you can import data for that prototype from an Excel file, which effectively adds the population specified in the Excel file to the population that is currently already in the database.

Let us consider a small (useless) Ampersand model, defined as follows:

   rAA :: A*A [PROP]
   rAB :: A*B [UNI]
   rAC :: A*C
   sAB :: A->B

A usable Excel file consists of one Excel page (tab), that contains a sequence of so-called 'blocks'.
Here is an example for the above defined model (the '|' character denotes a cell boundary and any (sequence of) adjacent spaces are here for readability and should not be entered in a real excel file):

  | [A's] |  rAA  |  rAB  |  rAC  |  rAC  |  rAC  |  sAB  |
  |   A   |   A   |   B   |   C   |   C   |   C   |   B   |
  | alfa1 | alfa1 | beta1 | char1 | char2 | char3 | beta2 |
  |       | CMT   |       |       |       |       |       |
  | alfa2 |       | beta2 |       | char2 |       |       |
  | alfa3 |       |       | char4 | char3 | char2 | beta1 |

A 'block' consists of 2 header rows followed by lines of data. A 'block' terminates whenever a next block starts or the end of file is reached. Empty lines are disregarded.

Every cell in the leftmost column whose contents starts with the character '[' is the first cell in the first header row of a block.
The contents of this cell is further disregarded. Subsequent cells in the first header row must either be empty, or  names of relations that are known in your Ampersand model.

The second header row only contains names of concepts, or empty cells. 
The first cell (in the second header row) must contain the source (left) concept of all relations specified in the first header row.
In the example, all relations in the first header row have source concept 'A'
Every subsequent cell (in the second header row) must either be empty or contain the target (right) concept of the relation that is specified in the same column in the first header row.

Every subsequent row in this block is called a data row. Cells in a data row are either empty or non-empty. If a non-empty cell contains a formula, this formula is evaluated to obtain the cell contents. If a non-empty does not contain a formula, its contents is obtained as is. From here on, when we talk about 'the contents of a cell', the obtained value from (evaluating the expressin in) that cell is meant. 
Data rows are interpreted as follows:
- When the first cell in a data row is empty, the content of all other cells in that row is disregarded (you may use it as comment fields)
- When the first cell in a data row is not empty, the content of all other non-empty cells and the content of the first cell may define a pair (srcAtom,tgtAtom) that is to be inserted into the population of the Ampersand model, where
  - 'srcAtom' is the contents of the first cell
  - 'tgtAtom' is the contents of a non-empty cell
  - for the relation to which the pair (srcAtom,tgtAtom) is to be added,
    - its name is specified in the first row of the block in the same column as 'tgtAtom'
      if the relation name does not exist, the pair is not added to the ampersand population;
    - its SRC Concept is specified on the second header row in the first column;
    - its TGT Concept is specified on the second header row in the same column as 'tgtAtom'.
      if the TGT Concept is not specified, the pair is not added to the ampersand population;

This means that the example is equivalent with the following population specification (note that the cell containing 'CMT' is disregarded as it is comment):

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

--[NOTES]--
1) You need NOT know about the internals of the database to use this plugin.
2) You may specify formulae instead of texts. The result of the formula will be read (and converted to text) before being inserted into the database. This allows for dynamic construction of identifiers, precomputation of tables, date adaptations to the date of today, etc.
3) If you use '&' in the first column, the (dirty) identifier for the atom will be automatically generated. If you use '&' in a subsequent column, this will be replaced with the (dirty) identifier for the source atom (which you can use e.g. to populate property-relations)
4) It is possible to store all sorts of data in the spreadsheet that will not interfere with the database population. The contents of the following cells is disregarded and can therefore be used for other purposes:
- cells in a row whose first cell is empty.
- cells in a column where the cell that specifies the relation name or the TGT concept is empty.
- cells that are on other sheets than sheet 1.