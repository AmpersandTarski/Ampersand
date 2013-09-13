<?php 
/* Please forward any comments to the author: michiel.stornebrink@tno.nl

   This file defines the functions 'InsPair', 'DelPair', InsAtom and DelAtom
   There are no guarantees with respect to their 100% functioning. Have fun...

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
 	 - null	: whether or not the value in the column can be NULL. in case of UNI relations
 	*/
 	global $tableColumnInfo;
// if srcAtom is specified as NULL, a new atom of srcConcept is created
 	if($srcAtom == "NULL") 
 	{ $srcAtom = InsAtom($srcConcept);
 	}	
// if tgtAtom is specified as NULL, a new atom of tgtConcept is created
 	if($tgtAtom == "NULL") 
 	{ $tgtAtom = InsAtom($tgtConcept);
 	}
// check if $relation appears in $relationTableInfo
 	if (array_key_exists($relation, $relationTableInfo))
 	{ foreach($relationTableInfo as $key => $arr)
 		 {	if($key == $relation && $arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept)
 			  { $table = $arr['table'];
 				   $srcCol = $arr['srcCol'];
 				   $tgtCol = $arr['tgtCol'];
 		  	}
 		 }
 	} else
 	{ // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
 	  die("ERROR: Relation $relation does not exist in InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
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
 	{	if($srcColUnique)
 			{ $query = "UPDATE `$tableEsc` SET `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc' WHERE `$srcColEsc`='$srcAtomEsc'";
 		 }else
 		 { $query = "UPDATE `$tableEsc` SET `$srcColEsc`='$srcAtomEsc', `$tgtColEsc`='$tgtAtomEsc' WHERE `$tgtColEsc`='$tgtAtomEsc'";
 		 }
 	}else
 	{ // neither srcCol nor tgtCol is unique ==> insert query
 		 $query = "INSERT INTO `$tableEsc` (`$srcColEsc`, `$tgtColEsc`) VALUES ('$srcAtomEsc', '$tgtAtomEsc')";
 	}
// execute database query
 	queryDb($query); 
// log
 	emitAmpersandExecEngine ("Insert pair ($srcAtom,$tgtAtom) into $relation($srcConcept*$tgtConcept)");
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
{	/* 
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
 	 - null	: whether or not the value in the column can be NULL. in case of UNI relations
 	*/
 	global $tableColumnInfo;
 	// check if $relation appears in $relationTableInfo
 	if (array_key_exists($relation, $relationTableInfo))
 	{ // due to relationname overloading in ADL, $relation may occus multiple times in $relationTableInfo ==> find right srcConcept-relation-tgtConcept combination
  		foreach($relationTableInfo as $key => $arr) 
  		{ if($key == $relation && $arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept)
  			 { $table = $arr['table'];
  				  $srcCol = $arr['srcCol'];
  				  $tgtCol = $arr['tgtCol'];
   			}
  		}
 	} else
 	{ // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
 	  die("ERROR: Relation $relation does not exist in InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
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
 	{	if($srcColNull)
 			{ $query = "UPDATE `$tableEsc` SET `$srcColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'";
 		 }else
 		 { $query = "UPDATE `$tableEsc` SET `$tgtColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'";
 		 }
 	}elseif($srcColNull and $tgtColNull) // both srcCol and tgtCol can be null ==> delete query 		-- REMARK: maybe this should be an update instead of delete query
 	  { $query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
 	  }else
 	  { // neither srcCol nor tgtCol can be null ==> delete query
 		   $query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
 	  }
// execute database query
 	queryDb($query); 
// log
 	emitAmpersandExecEngine("Delete pair ($srcAtom,$tgtAtom) from $relation($srcConcept*$tgtConcept)");
 	emitLog ("DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)");
 	emitLog ($query);
}

// Use: VIOLATION (TXT "{EX} InsAtom;<concept>") -- this may not be of any use in Ampersand, though.
function InsAtom($concept)
{ // call function from DatabaseUtils.php
 	$atom = mkUniqueAtomByTime($concept); // create new atom name	
 	addAtomToConcept($atom, $concept); // insert new atom in databse
// log
 	emitAmpersandExecEngine("New atom $atom ($concept) created");
 	emitLog("addAtomToConcept($atom, $concept)");
// return created atom identifier
 	return $atom;
}

// Use: VIOLATION (TXT "{EX} DelAtom;<concept>;<atom>")
function DelAtom($concept, $atom)
{ // call function from DatabaseUtils.php
	 deleteAtom($atom, $concept); // delete atom + all relations with other atoms
// log
	emitAmpersandExecEngine("Atom $atom ($concept) deleted");
	emitLog("deleteAtom($atom, $concept)");
}

?>