<?php 

/* Please forward any comments to the author: michiel.stornebrink@tno.nl

   This file defines the functions 'InsPair', 'DelPair', InsAtom, DelAtom and NewStruct
   There are no guarantees with respect to their 100% functioning. Have fun...
   
   This file has been modified to produce Exceptions rather than that it dies...
   Such exceptions need to be caught. The syntax for doing this is as follows:
   
   try { <insert code here>;
         throw new Exception("identification string of the exception");
         <insert other code if needed>; 
       }
  catch (Exception $e)
       { <insert exception handling code here>;
         <the exception identifier is in variable $e>;
       }
*/

/*
   Example of rule that automatically inserts pairs into a relation (analogous stuff holds for DelPair):
   ROLE ExecEngine MAINTAINS "New Customers"
   RULE "New Customers": customerOrder[Person*Order];companyOrder[Company*Order]~ |- customerOf[Person*Company]
   MEANING "If a person places an order at a company, the person is a customer of that company"
   VIOLATION (TXT "InsPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use:  VIOLATION (TXT "InsPair;<relation>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom){
	$database = Database::singleton();
	
	// Check if combination of $relation, $srcConcept, $tgtConcept exists. Otherwise this causes database issues.
	if (!Relation::isCombination($relation, $srcConcept, $tgtConcept)){
		// Tip: If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration. TODO: create ticket for Prototype generator to fix this.
		throw new Exception('Cannot find ' . $relation . '[' . $srcConcept . '*' . $tgtConcept.']');
	}
	
	// if srcAtom is specified as NULL, a new atom of srcConcept is created
    if($srcAtom == "NULL") $srcAtom = $database->createNewAtom($srcConcept);
	
	// if tgtAtom is specified as NULL, a new atom of tgtConcept is created
	if($tgtAtom == "NULL") $tgtAtom = $database->createNewAtom($tgtConcept);
	
	$database->editUpdate($relation, false, $srcAtom, $tgtAtom, 'child', '');
}

/*
Example of a rule that automatically deletes pairs from a relation:
  ROLE ExecEngine MAINTAINS "Remove Customers"
  RULE "Remove Customers": customerOf[Person*Company] |- customerOrder[Person*Order];companyOrder[Company*Order]~
  MEANING "Customers of a company for which no orders exist (any more), are no longer considered customers"
  VIOLATION (TXT "DelPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use: VIOLATION (TXT "DelPair;<rel>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function DelPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom){
	$database = Database::singleton();
	
	// Check if combination of $relation, $srcConcept, $tgtConcept exists. Otherwise this causes database issues.
	if (!Relation::isCombination($relation, $srcConcept, $tgtConcept)){
		// Tip: If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration. TODO: create ticket for Prototype generator to fix this.
		throw new Exception('Cannot find ' . $relation . '[' . $srcConcept . '*' . $tgtConcept.']');
	}
	
	$database->editDelete($relation, false, $srcAtom, $tgtAtom);
}

// TODO: NewStruct afstemmen met Rieks, moet nog bijgewerkt worden naar nieuwe Database functies
/* The function 'NewStruct' creates a new atom in some concept and uses this
   atom to create two links (in relations in which the concept is SRC or TGT).

   Example:
   
   r :: ConceptA * ConceptB
   r1 :: ConceptA * ConceptC [INJ] -- multiplicity must be there (I think...)
   r2 :: ConceptC * ConceptB [UNI] -- multiplicity must be there (I think...)
   
   RULE "equivalence": r = r1;r2 -- this rule is to be maintained automatically
   
   ROLE ExecEngine MAINTAINS "insEquivalence" -- Creation of the atom
   RULE "insEquivalence": r |- r1;r2
   VIOLATION (TXT "NewStruct;ConceptC;NULL" -- 'NULL' specifies that a name is to be generated
             ,TXT ";r1;ConceptA;", SRC I, TXT";ConceptC;NULL"  -- Always use NULL as ConceptC atom
             ,TXT ";r2;ConceptC;NULL;ConceptB;atomB;", TGT I   -- Always use NULL as ConceptC atom
              )

   ROLE ExecEngine MAINTAINS "delEquivalence" -- Deletion of the atom
   RULE "delEquivalence": I[ConceptC] |- r1~;r;r2~ 
   VIOLATION (TXT "DelAtom;ConceptC;" SRC I) -- all links in other relations in which the atom occurs are deleted as well.
*/
function NewStruct(){ // arglist: ($ConceptC[,$newAtom][,$relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom]+)

	// We start with parsing the first one or two arguments
	$ConceptC = func_get_arg(0);              // Name of concept for which atom is to be created
	$AtomC = mkUniqueAtomByTime($ConceptC);   // Default marker for atom-to-be-created.
	if (func_num_args() % 5 == 2) {            // Check if name of new atom is explicitly specified
		$AtomC = func_get_arg(1);              // If so, we'll be using this to create the new atom
	} elseif (func_num_args() % 5 != 1){       // check for valid number of arguments
		// ExecEngineSHOUTS("NewStruct: Illegal number of arguments: ".func_num_args());
		die;
	}
	
	// Then, we create a new atom of type $ConceptC
	// ExecEngineWhispers ("Creating a structure based on an atom '$AtomC' for concept '$ConceptC'");
	addAtomToConcept($AtomC, $ConceptC);     // insert new atom in database

	// Next, for every relation that follows in the argument list, we create a link
	for ($i = func_num_args() % 5; $i < func_num_args(); $i = $i+5){
		// emitLog ("i = $i");
		$relation   = func_get_arg($i);
		$srcConcept = func_get_arg($i+1);
		$srcAtom    = func_get_arg($i+2);
		$tgtConcept = func_get_arg($i+3);
		$tgtAtom    = func_get_arg($i+4);
		// populate relation r1, first checking for allowed syntax:
		
		if (!($srcAtom == 'NULL' or $tgtAtom == 'NULL')){ // Note: when populating a [PROP] relation, both atoms can be NULL
			// ExecEngineSHOUTS("NewStruct: relation $relation requires that atom $srcAtom or $tgtAtom must be NULL");
			throw new Exception("Failure 1 in NewStruct in InsDelPairAtom.php");
		}
	
		if (!($srcConcept == $ConceptC or $tgtConcept == $ConceptC)){ // Note: when populating a [PROP] relation, both atoms can be NULL
			//ExecEngineSHOUTS("NewStruct: relation $relation requires that concept $srcConcept or $tgtConcept must be $ConceptC");
			throw new Exception("Failure 2 in NewStruct in InsDelPairAtom.php");
		}
	
		if ($srcConcept == $ConceptC){
			if ($srcAtom == 'NULL'){
				$srcAtom = $AtomC;
			}else{ // While it strictly not necessary to err here, for most cases this helps to find errors in the ADL script
				ExecEngineSHOUTS ("NewStruct: $srcAtom must be NULL when $ConceptC is the concept (in relation $relation)");
				throw new Exception("Failure 3 in NewStruct in InsDelPairAtom.php");
			}
		}
	
		if ($tgtConcept == $ConceptC){  
			if ($tgtAtom == 'NULL'){  
				$tgtAtom = $AtomC;
			}else{ // While it strictly not necessary to err here, for most cases this helps to find errors in the ADL script
				// ExecEngineSHOUTS ("NewStruct: $tgtAtom must be NULL when $ConceptC is the concept (in relation $relation)");
				throw new Exception("Failure 4 in NewStruct in InsDelPairAtom.php");
			}
		}
		
		// Any logging is done by InsPair:
		InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom);
	}
	// ExecEngineWhispers ("Completed structure creation.");
}

// Use: VIOLATION (TXT "InsAtom;<concept>") -- this may not be of any use in Ampersand, though.
function InsAtom($concept){ 
	$database = Database::singleton();
 
	return $database->createNewAtom($concept); // insert new atom in database
	
}

// Use: VIOLATION (TXT "DelAtom;<concept>;<atom>")
function DelAtom($concept, $atom){ 
	$database = Database::singleton();
	
	$database->deleteAtom($atom, $concept); // delete atom + all relations with other atoms
	
}

?>