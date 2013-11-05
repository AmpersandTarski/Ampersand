<?php 
/* Please forward any comments to the author: michiel.stornebrink@tno.nl

   This file defines the functions 'InsPair', 'DelPair', InsAtom, DelAtom and NewStruct
   There are no guarantees with respect to their 100% functioning. Have fun...
*/

/*
   Example of rule that automatically inserts pairs into a relation (analogous stuff holds for DelPair):
   ROLE ExecEngine MAINTAINS "New Customers"
   RULE "New Customers": customerOrder[Person*Order];companyOrder[Company*Order]~ |- customerOf[Person*Company]
   MEANING "If a person places an order at a company, the person is a customer of that company"
   VIOLATION (TXT "{EX} InsPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use:  VIOLATION (TXT "{EX} InsPair;<relation>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
{ /* 
    $relationTableInfo from Generics.php 
    contains array with all relations, for each relation the following is specified: 
     - srcConcept : srcConcept of relation
     - tgtConcept : tgtConcept of relation
     - table : database table in which the relation is populated
     - srcCol : column of database table in which the srcConcept is placed
     - tgtCol : column of database table in which the tgtConcept is placed
    */
    global $relationTableInfo;
    /* 
    $tableColumInfo from Generics.php 
    contains array with all database tables and their columns, for each tablecolumn the following is specified: 
     - concept : the atoms of which concept are set here
     - unique : whether or not the value in the column must be unique. 'true' for properties
     - null   : whether or not the value in the column can be NULL. in case of UNI relations
    */
    global $tableColumnInfo;
// check if $relation appears in $relationTableInfo
  $found = false;
  foreach($relationTableInfo as $key => $arr)
    {   if($key == $relation && $arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept)
        { $found = true;
          $table = $arr['table'];
            $srcCol = $arr['srcCol'];
            $tgtCol = $arr['tgtCol'];
        }
    }
    if (!$found)
    { // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
      ExecEngineSHOUTS("ERROR: Cannot find $relation\[$srcConcept\*$tgtConcept\] signature.");
      ExecEngineSays("InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
      ExecEngineSays("If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration.");
      die;
    }
// if srcAtom is specified as NULL, a new atom of srcConcept is created
    if($srcAtom == "NULL") 
    { $srcAtom = InsAtom($srcConcept);
    }   
// if tgtAtom is specified as NULL, a new atom of tgtConcept is created
    if($tgtAtom == "NULL") 
    { $tgtAtom = InsAtom($tgtConcept);
    }
// get table column properties for $srcCol and $tgtCol
    $srcColUnique = $tableColumnInfo[$table][$srcCol]['unique'];
    $srcColNull = $tableColumnInfo[$table][$srcCol]['null'];
    $tgtColUnique = $tableColumnInfo[$table][$tgtCol]['unique'];
    $tgtColNull = $tableColumnInfo[$table][$tgtCol]['null'];
// SQL escape table, column and atom names
    $tableEsc = escapeSQL($table);
    $srcColEsc = escapeSQL($srcCol);
    $tgtColEsc = escapeSQL($tgtCol);
    $srcAtomEsc = escapeSQL($srcAtom);
    $tgtAtomEsc = escapeSQL($tgtAtom);
// generate database query
    if($srcColUnique || $tgtColUnique) // srcCol, tgtCol or both are unique ==> update query
    {   if($srcColUnique)
        { $query = "UPDATE `$tableEsc` SET `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc' WHERE `$srcColEsc`='$srcAtomEsc'";
          ExecEngineWhispers ("Update $relation($srcConcept*$tgtConcept) with (<b>$srcAtom</b>,$tgtAtom)");
        }else
        { $query = "UPDATE `$tableEsc` SET `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc' WHERE `$tgtColEsc`='$tgtAtomEsc'";
          ExecEngineWhispers ("Update $relation($srcConcept*$tgtConcept) with ($srcAtom,<b>$tgtAtom</b>)");
        }
    }else
    { // neither srcCol nor tgtCol is unique ==> insert query
      $query = "INSERT INTO `$tableEsc` (`$srcColEsc`, `$tgtColEsc`) VALUES ('$srcAtomEsc', '$tgtAtomEsc')";
      ExecEngineWhispers ("INSERT $relation($srcConcept*$tgtConcept) with (<b>$srcAtom,$tgtAtom</b>)");
    }
/* Trying to implement 'on duplicate key update' (doesn't seem to work like this...)
$updatequery = '';
if ($tgtColUnique) $updatequery = "ON DUPLICATE KEY UPDATE `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc'";// WHERE `$tgtColEsc`='$tgtAtomEsc'";
if ($srcColUnique) $updatequery = "ON DUPLICATE KEY UPDATE `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc'";// WHERE `$srcColEsc`='$srcAtomEsc'";
$query = "INSERT INTO `$tableEsc` (`$srcColEsc`, `$tgtColEsc`) VALUES ('$srcAtomEsc', '$tgtAtomEsc') " . $updatequery;
*/    
// execute database query
    queryDb($query); 
// log
    emitLog ("InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
    emitLog ($query);
}

/*
Example of a rule that automatically deletes pairs from a relation:
  ROLE ExecEngine MAINTAINS "Remove Customers"
  RULE "Remove Customers": customerOf[Person*Company] |- customerOrder[Person*Order];companyOrder[Company*Order]~
  MEANING "Customers of a company for which no orders exist (any more), are no longer considered customers"
  VIOLATION (TXT "{EX} DelPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use: VIOLATION (TXT "{EX} DelPair;<rel>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
{   /* 
    $relationTableInfo from Generics.php 
    contains array with all relations, for each relation the following is specified: 
     - srcConcept : srcConcept of relation
     - tgtConcept : tgtConcept of relation
     - table : database table in which the relation is populated
     - srcCol : column of database table in which the srcConcept is placed
     - tgtCol : column of database table in which the tgtConcept is placed
    */
    global $relationTableInfo;
    /* 
    $tableColumInfo from Generics.php 
    contains array with all database tables and their columns, for each tablecolumn the following is specified: 
     - concept : the atoms of which concept are set here
     - unique : whether or not the value in the column must be unique. 'true' for properties
     - null   : whether or not the value in the column can be NULL. in case of UNI relations
    */
    global $tableColumnInfo;
    // check if $relation appears in $relationTableInfo
  $found = false;
  foreach($relationTableInfo as $key => $arr)
    {   if($key == $relation && $arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept)
        { $found = true;
          $table = $arr['table'];
            $srcCol = $arr['srcCol'];
            $tgtCol = $arr['tgtCol'];
        }
    }
    if (!$found)
    { // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
      ExecEngineSHOUTS("ERROR: Cannot find $relation\[$srcConcept\*$tgtConcept\] signature.");
      ExecEngineSays("DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
      ExecEngineSays("If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration.");
      die;
    }
// get table column properties for $srcCol and $tgtCol
    $srcColUnique = $tableColumnInfo[$table][$srcCol]['unique'];
    $srcColNull = $tableColumnInfo[$table][$srcCol]['null'];
    $tgtColUnique = $tableColumnInfo[$table][$tgtCol]['unique'];
    $tgtColNull = $tableColumnInfo[$table][$tgtCol]['null'];
// SQL escape table, column and atom names
    $tableEsc = escapeSQL($table);
    $srcColEsc = escapeSQL($srcCol);
    $tgtColEsc = escapeSQL($tgtCol);
    $srcAtomEsc = escapeSQL($srcAtom);
    $tgtAtomEsc = escapeSQL($tgtAtom);
// generate database query
    if($srcColNull xor $tgtColNull) // srcCol xor tgtCol can be null ==> update query
    {   if($srcColNull)
          { $query = "UPDATE `$tableEsc` SET `$srcColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'";
        }else
        { $query = "UPDATE `$tableEsc` SET `$tgtColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'";
        }
    }elseif($srcColNull and $tgtColNull) // both srcCol and tgtCol can be null ==> delete query       -- REMARK: maybe this should be an update instead of delete query
      { $query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
      }else
      { // neither srcCol nor tgtCol can be null ==> delete query
          $query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
      }
// execute database query
    queryDb($query); 
// log
    ExecEngineWhispers("Delete pair ($srcAtom,$tgtAtom) from $relation($srcConcept*$tgtConcept)");
    emitLog ("DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
    emitLog ($query);
}

/* The function 'NewStruct' creates a new atom in some concept and uses this
   atom to create two links (in relations in which the concept is SRC or TGT).

   Example:
   
   r :: ConceptA * ConceptB
   r1 :: ConceptA * ConceptC [INJ] -- multiplicity must be there (I think...)
   r2 :: ConceptC * ConceptB [UNI] -- multiplicity must be there (I think...)
   
   RULE "equivalence": r = r1;r2 -- this rule is to be maintained automatically
   
   ROLE ExecEngine MAINTAINS "insEquivalence" -- Creation of the atom
   RULE "insEquivalence": r |- r1;r2
   VIOLATION (TXT "{EX} NewStruct;ConceptC;NULL" -- 'NULL' specifies that a name is to be generated
             ,TXT ";r1;ConceptA;", SRC I, TXT";ConceptC;NULL"  -- Always use NULL as ConceptC atom
             ,TXT ";r2;ConceptC;NULL;ConceptB;atomB;", TGT I   -- Always use NULL as ConceptC atom
              )

   ROLE ExecEngine MAINTAINS "delEquivalence" -- Deletion of the atom
   RULE "delEquivalence": I[ConceptC] |- r1~;r;r2~ 
   VIOLATION (TXT "{EX} DelAtom;ConceptC;" SRC I) -- all links in other relations in which the atom occurs are deleted as well.
*/

function NewStruct() // arglist: ($ConceptC[,$newAtom][,$relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom]+)
{ 
// We start with parsing the first one or two arguments
  $ConceptC = func_get_arg(0);              // Name of concept for which atom is to be created
  $AtomC = mkUniqueAtomByTime($ConceptC);   // Default marker for atom-to-be-created.
  if (func_num_args() % 5 == 2)             // Check if name of new atom is explicitly specified
  {  $AtomC = func_get_arg(1);              // If so, we'll be using this to create the new atom
  } elseif (func_num_args() % 5 != 1)       // check for valid number of arguments
  {  ExecEngineSHOUTS("NewStruct: Illegal number of arguments: ".func_num_args());
     die;
  }
// Then, we create a new atom of type $ConceptC
  ExecEngineWhispers ("Creating a structure based on an atom '$AtomC' for concept '$ConceptC'");
    addAtomToConcept($AtomC, $ConceptC);     // insert new atom in database

// Next, for every relation that follows in the argument list, we create a link
  for ($i = func_num_args() % 5; $i < func_num_args(); $i = $i+5)
  { emitLog ("i = $i");
    $relation   = func_get_arg($i);
    $srcConcept = func_get_arg($i+1);
    $srcAtom    = func_get_arg($i+2);
    $tgtConcept = func_get_arg($i+3);
    $tgtAtom    = func_get_arg($i+4);
// populate relation r1, first checking for allowed syntax:
    if (!($srcAtom == 'NULL' or $tgtAtom == 'NULL')) // Note: when populating a [PROP] relation, both atoms can be NULL
    {  ExecEngineSHOUTS("NewStruct: relation $relation requires that atom $srcAtom or $tgtAtom must be NULL");
       die;
    }
    if (!($srcConcept == $ConceptC or $tgtConcept == $ConceptC)) // Note: when populating a [PROP] relation, both atoms can be NULL
    {  ExecEngineSHOUTS("NewStruct: relation $relation requires that concept $srcConcept or $tgtConcept must be $ConceptC");
       die;
    }
    if ($srcConcept == $ConceptC)
    {  if ($srcAtom == 'NULL')
       {  $srcAtom = $AtomC;
       } else // While it strictly not necessary to err here, for most cases this helps to find errors in the ADL script
       {  ExecEngineSHOUTS ("NewStruct: $srcAtom must be NULL when $ConceptC is the concept (in relation $relation)");
          die;
       }
    }
    if ($tgtConcept == $ConceptC)
    {  if ($tgtAtom == 'NULL')
       {  $tgtAtom = $AtomC;
       } else // While it strictly not necessary to err here, for most cases this helps to find errors in the ADL script
       {  ExecEngineSHOUTS ("NewStruct: $tgtAtom must be NULL when $ConceptC is the concept (in relation $relation)");
          die;
       }
    }
// Any logging is done by InsPair:
    InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom);
  }
  ExecEngineWhispers ("Completed structure creation.");
}

// Use: VIOLATION (TXT "{EX} InsAtom;<concept>") -- this may not be of any use in Ampersand, though.
function InsAtom($concept)
{ // call function from DatabaseUtils.php
    $atom = mkUniqueAtomByTime($concept); // create new atom name   
    addAtomToConcept($atom, $concept); // insert new atom in databse
// log
    ExecEngineWhispers("New atom $atom ($concept) created");
    emitLog("addAtomToConcept($atom, $concept)");
// return created atom identifier
    return $atom;
}

// Use: VIOLATION (TXT "{EX} DelAtom;<concept>;<atom>")
function DelAtom($concept, $atom)
{ // call function from DatabaseUtils.php
    deleteAtom($atom, $concept); // delete atom + all relations with other atoms
// log
   ExecEngineWhispers("Atom $atom ($concept) deleted");
   emitLog("deleteAtom($atom, $concept)");
}

?>