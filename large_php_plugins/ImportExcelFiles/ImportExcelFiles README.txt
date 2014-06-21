--[Introduction]--
This is the README file for the plugin 'ImportExcelFiles', which allows you to import a population into a (running) Ampersand prototype, where this population is specified in an Excel file.

--[Plugin installation]--
Sources for this plugin are located in the directory '<OUrepository>\trunk\large_php_plugins\ImportExcelFiles'.
Installation of this plugin consists of executing the command

   xcopy "%OUrepository%\trunk\large_php_plugins\ImportExcelFiles\*.*" "%prototypehtdocs%\ImportExcelFiles\" /s

where %OUrepository% is the directory where you keep your local copy of the SVN repository http://svnint.ou.nl:8080/svn/ADL/
  and %prototypehtdocs% is the directory where you keep your prototype code (i.e. the directory in which the file 'index.php' of your prototype is located). 

--[Calling the plugin when running a prototype]--
Suppose your prototype is callable at 'http:\\localhost\prototype', where 'prototype' is the name of the prototype (context).
Suppose you want to import a population into that prototype, that is specified  in file '<excelfile>.xlsx' that is located in directory '<dir>' (where <dir> includes the path).
In the address bar of the browser, you type the url 'http:\\localhost\prototype\ImportExcelFiles'
A page appears in which you are prompted to enter the file name.
You can browse to the file, click on it and then upload it.

Then a parser starts to parse the excel file, and one by one pairs are added to the population
You will see a log of this happening on the screen, so if any odd things happen, you may very well see where this started.
Also, if you did any typo's in relation names or concept names, you are likely to get an error, which you then can fix and retry.
If all is well, the rules are checked, including any ExecEngine rules, and ultimately the invariants, precisely as if you had entered the population in one big transaction.

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
The contents of this cell is further disregarded. Subsequent cells in the first header row must be names of relations that are known in your Ampersand model.

The second header row only contains names of concepts. 
The first cell (in the second header row) contains the source (left) concept of all relations specified in the first header row.
In the example, all relations in the first header row have source concept 'A'
Every subsequent cell (in the second header row) contains the target (right) concept of the relation that specified in the same column of the first header row.

Every subsequent row in this block is called a data row. Cells in a data row may contain either nothing (empty cell), the value for an atom, or comment, as follows:
- When the first cell in a data row is empty, the content of all other cells in that row is treated as comment (i.e.: is disregarded)
- WHen the first cell in a data row is not empty, the content of all other non-empty cells and the content of the first cell define a pair  (srcAtom,tgtAtom) that is to be inserted into the population of the Ampersand model, where 
  - 'srcAtom' is the contents of the first cell
  - 'tgtAtom' is the contents of a non-empty cell
  - for the relation to which the pair (srcAtom,tgtAtom) is to be added,
    - its name is specified in the first row of the block in the same column as 'tgtAtom';
    - its SRC Concept is specified on the second header row in the first column;
    - its TGT Concept is specified on the second header row in the same column as 'tgtAtom'.

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

That's it, Folks!