# The Excel Importer
The Excel Importer allows you to import data from an Excel file in a (working) prototype, effectively adding it to the population of a running prototype. This text is meant for the Ampersand user who has more population than can be dealt with by `POPULATION` statements. It is also useful if you have existing spreadsheets filled with tables, which you want to use as population in your Ampersand-script. Please note that some editing of your Excel-file may be required, to make Ampersand understand how columns match Ampersand-relations.

This text applies to the Ampersand tooling in the new framework (as of the March 2016 in branch_#228)

## Installation
Nothing special is required for installation; this extension is enabled by default 

## Importing Excel files in a running prototype
In the menu bar, there are some icons at the right hand side. One of them is a square containing 3x3 squares. This is the selector for extensions. Click on the icon, and select the extension 'Excel Import'.
A page is shown in which you may specify excel files and upload them. This is quite self-explanatory.

## How to create importable Excel files
When you have a prototype running for an Ampersand context, you can import data for that prototype from an Excel file, which effectively adds the population specified in the Excel file to the population that is currently already in the database. This section describes how to construct an Excel file that can be used to do this.

Let us consider a small (useless) Ampersand model, defined as follows:

	rAA :: A*A [PROP]
	rAB :: A*B [UNI]
	rAC :: A*C
	sAB :: A->B
	tAD :: A*Delta
	uBA :: B*A

If you want to specify data elements in an Excel file in order to populate such a model, you must define so called 'blocks' that contain this data. The importer looks for such blocks throughout the Excel file (meaning that you can have blocks on different sheets - all sheets will be inspected).

Here is an example of such a 'block' (note: all blocks must start in the leftmost column; if not, they are disregarded):

| [A's] |  rAA  |  rAB  |  rAC  |  rAC  |  rAC  |  sAB  |   tAD    |  uBA~ |
| -- | -- | -- | -- | -- | -- | -- | -- | -- |
|   A   |   A   |   B   |   C   |   C   |   C   |   B   | [Delta;] |   B   |
| alfa1 | alfa1 | beta1 | char1 | char2 | char3 | beta2 | d1;d2;d3 | beta1 |
|       |  CMT  |       |       |       |       |       |          | beta2 |
| alfa2 |       | beta2 |       | char2 |       |       | d2 ;  d3 | beta3 |
| alfa3 |       |       | char4 | char3 | char2 | beta1 |   d1     |       |

Here is the specification of a block:
1. A 'block' consists of 2 header rows followed by lines of data. A 'block' terminates whenever a next block starts or the end of file is reached. Empty lines are disregarded.
2. Every cell in the leftmost column whose contents starts with the character '[' is the first cell in the first header row of a block. The contents of this cell is further disregarded. 
3. Subsequent cells in the first header row must either be empty, or contain the name of a relations that is known in your Ampersand model, optionally followed by a `~` (flip) character. In our example, such cells may hence only contain '', `rAA`, `rAB`, `rAC`, `sAB`, `tAD` or `uBA~`.
4. The second header row only contains cells that are either empty or contain a concept name, or contain a concept name and delimiter. In our example, such cells may only contain '', `A`, `B`, `C`, or `Delta`. Note that such concepts can be surrounded by block-quotes, in which case the last character before the terminating quote is treated as a delimiter that is used to separate multiple atoms in subsequnet rows. An example of this is `[Delta;]`.
5. The first cell (in the second header row) must contain the source (left) concept of all relations specified in the first header row. It may not be empty nor may it contain a delimiter.
6. Every subsequent cell (in the second header row) must either be empty, or contain the name of the target (right) concept of the relation that is specified in the same column in the first header row, or contain the name of the target concept, followed with a (single character) delimiter, all of which is surrounded with block-quotes '[' and ']'.
7. There is an exception to the previous two specifications: if a cell in the first header row specifies a flipped relation, the leftmost cell in the second header row specifies the target concept for the (unflipped) relation, and the cell below the flipped relation specifies the source concept for the (unflipped) relation. In the example, all relations in the first header row have source concept `A`, except for relation `uBA`, which has concept `A` as its target concept, and `B` as its source concept.
8 Every subsequent row in this block is called a data row. Cells in a data row are either empty or non-empty. If a non-empty cell contains a formula, this formula is evaluated to obtain the cell contents. If a non-empty does not contain a formula, its contents is obtained as is (see notes for errors in formula-evaluation). From here on, when we talk about 'the contents of a cell', the obtained value from (evaluating the expression in) that cell is meant.

Data rows are interpreted as follows:
- When the first cell in a data row is empty, the content of all other cells in that row is disregarded (you may use such cells to include comments, computations, or whatever else you like)
- When the first cell in a data row is not empty, the content of all other non-empty cells and the content of the first cell may define a set of pairs (srcAtom,tgtAtoms[i]), each of which is to be inserted into the population of the Ampersand model, where
  - 'srcAtom' is the contents of the first cell
  - 'tgtAtoms[i]' is the (untrimmed) contents of a non-empty cell if no delimiter was specified, or the trimmed i-th element of the delimiter-separated list contained in the cell. Trimming means that both leading and trailing spaces, tabs etc. are removed from the element.
  - for the relation to which the pair (srcAtom,tgtAtom[i]) is to be added,
    - its name is specified in the first row of the block in the same column as 'tgtAtom'
      - if the relation name does not exist, the pair is not added to the ampersand population;
      - if the relation name contains a `~` as a last character, it is flipped, meaning that the pair (tgtAtom[i],srcAtom) is populated rather than the pair (srcAtom,tgtAtom[i])
    - its SRC Concept is specified on the second header row in the first column;
    - its TGT Concept is specified on the second header row in the same column as 'tgtAtom'.
      if the TGT Concept is not specified, the pair is not added to the ampersand population;
      a specified TGT Concept can be followed by a single-character delimiter, in which case both are surrounded by square brackets '[', ']', e.g. as in `[Delta;]` in the example. 

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
	
	POPULATION tAD CONTAINS [ ("alfa1"), ("d1") ] 
	POPULATION tAD CONTAINS [ ("alfa1"), ("d2") ] 
	POPULATION tAD CONTAINS [ ("alfa1"), ("d3") ] 
	POPULATION tAD CONTAINS [ ("alfa2"), ("d2") ] 
	POPULATION tAD CONTAINS [ ("alfa2"), ("d3") ] 
	POPULATION tAD CONTAINS [ ("alfa3"), ("d1") ] 
	
	POPULATION uBA CONTAINS [ ("beta1"), ("alfa1") ] 
	POPULATION uBA CONTAINS [ ("beta3"), ("alfa2") ] 

## NOTES
1. You need NOT know about the internals of the database to use this plugin (at least, that's the idea).
2. You may specify formulae instead of texts. The result of the formula will be read (and converted to text) before being inserted into the database. This allows for dynamic construction of identifiers, precomputation of tables, date adaptations to the date of today, etc. Note, however, that this does not always work flawlessly. In particular, the functions `VLOOKUP` and `HLOOKUP` are known to produce errors (that we are not capable of fixing), so such functions should be avoided.
3. If you use '_NEW' in the first column, the (dirty) identifier for the atom will be automatically generated. If you use '_NEW' in a subsequent column on the same row, this will be replaced with the (dirty) identifier for the source atom (which you can use e.g. to populate property-relations). Note that as we also support formulae, you may use those to achieve the same result (and excercise control over the actual dirty identifiers used)
4. It is possible to store all sorts of data in the spreadsheet that will not interfere with the database population. The contents of the following cells is disregarded and can therefore be used for other purposes:
  - cells in a row whose first cell is empty.
  - cells in a column where the cell that specifies the relation name or the TGT concept is empty.
  - cells that are on other sheets than sheet 1.
5. When you use something like 'CLASSIFY X ISA Y' in your model, and want to populate an atom 'xy', then you should populate it in the block where 'X's are populated. In this block, you can not only populate relations that have source concept X, but also relations that have source concept Y.
6. When you use something like 'CLASSIFY X ISA Y' in your model, every atom that is an element of X must be defined (i.e.: its first appearance must be) in the block where 'X's are populated. Further down, it is allowed to add attributes to this atom in a block where 'Y's are populated
7. When you populate a cell with a list of atoms, the separator is a single 'regular' character. Good separators include ';', ',', space, 's' (i.e. a regular character), '_', '-' etc. You cannot use a tab or multiple characters as separator.
8. When you populate a cell with a list of atoms, the elements in the list are trimmed before being added to the population. Trimming means that leading as well as trailing whitespace etc. is removed from such elements.

That's all, Folks!
