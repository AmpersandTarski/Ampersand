<?php 

/* Please forward any comments to the author: michiel.stornebrink@tno.nl

   This file defines the functions:
   - 'InsPair', 'DelPair': insert and delete links in relations
   - 'NewStruct': create a new atom and populate relations
   - 'InsAtom', 'DelAtom': create/delete a single atom
   - 'MergeAtoms': unify two atoms that have been discovered to be identical
   - 'SetConcept', 'ClearConcept'
   - 'InsPairCond', 'SetConceptCond': conditionally execute an 'InsPair' or 'SetConcept'
   There are no guarantees with respect to their 100% functioning. Have fun...
   
   This file has been modified to produce Exceptions rather than that it dies...
   Such exceptions may be caught. The syntax for doing this is as follows:
   
   try { <insert code here>;
         throw new Exception("identification string of the exception", httpStatusCode);
         <insert other code if needed>; 
       }
  catch (Exception $e)
       { <insert exception handling code here>;
         <the exception identifier is in variable $e>;
       }
*/

use Ampersand\Log\Logger;
use Ampersand\Database\Database;
use Ampersand\Core\Relation;
use Ampersand\Core\Concept;
use Ampersand\Core\Atom;
use Ampersand\Session;
use Ampersand\Extension\ExecEngine\ExecEngine;

/*
   Example of rule that automatically inserts pairs into a relation (analogous stuff holds for DelPair):
   ROLE ExecEngine MAINTAINS "New Customers"
   RULE "New Customers": customerOrder[Person*Order];companyOrder[Company*Order]~ |- customerOf[Person*Company]
   MEANING "If a person places an order at a company, the person is a customer of that company"
   VIOLATION (TXT "InsPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use:  VIOLATION (TXT "InsPair;<relation>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function InsPair($relationName,$srcConceptName,$srcAtom,$tgtConceptName,$tgtAtom){
	Logger::getLogger('EXECENGINE')->info("InsPair($relationName,$srcConceptName,$srcAtom,$tgtConceptName,$tgtAtom)");
    if(func_num_args() != 5) throw new Exception("InsPair() expects 5 arguments, but you have provided ".func_num_args(), 500);
	try{		
		// Check if relation signature exists: $relationName[$srcConceptName*$tgtConceptName]
        $srcConcept = Concept::getConceptByLabel($srcConceptName);
        $tgtConcept = Concept::getConceptByLabel($tgtConceptName);
		$relation = Relation::getRelation($relationName, $srcConcept, $tgtConcept);
		
		// if either srcAtomIdStr or tgtAtom is not provided by the pairview function (i.e. value set to '_NULL'): skip the insPair
		if($srcAtom == '_NULL' or $tgtAtom == '_NULL'){
            Logger::getLogger('EXECENGINE')->debug("InsPair ignored because src and/or tgt atom is _NULL");
            return;
        }
		
		// if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($srcAtom == "_NEW") $srcAtom = ExecEngine::$_NEW->id;
		if($tgtAtom == "_NEW") $tgtAtom = ExecEngine::$_NEW->id;
		
		$srcAtomIds = explode('_AND', $srcAtom);
		$tgtAtomIds = explode('_AND', $tgtAtom);
		foreach($srcAtomIds as $a){
			$src = new Atom($a, $srcConcept);
		    foreach($tgtAtomIds as $b){
				$tgt = new Atom($b, $tgtConcept);
				$relation->addLink($src, $tgt, false, 'ExecEngine');
			}
		}
		
		Logger::getLogger('EXECENGINE')->debug("Tuple ('{$srcAtom}', '{$tgtAtom}') inserted into '{$relation->__toString()}'");
	}catch(Exception $e){
		Logger::getUserLogger()->error('InsPair: ' . $e->getMessage());
	}
}

/*
	Example of a rule that automatically deletes pairs from a relation:
	ROLE ExecEngine MAINTAINS "Remove Customers"
	RULE "Remove Customers": customerOf[Person*Company] |- customerOrder[Person*Order];companyOrder[Company*Order]~
	MEANING "Customers of a company for which no orders exist (any more), are no longer considered customers"
	VIOLATION (TXT "DelPair;customerOf;Person;", SRC I, TXT";Company;", TGT I)
*/
// Use: VIOLATION (TXT "DelPair;<rel>;<srcConcept>;<srcAtom>;<tgtConcept>;<tgtAtom>")
function DelPair($relationName,$srcConceptName,$srcAtom,$tgtConceptName,$tgtAtom){
	Logger::getLogger('EXECENGINE')->info("DelPair($relationName,$srcConceptName,$srcAtom,$tgtConceptName,$tgtAtom)");
    if(func_num_args() != 5) throw new Exception("DelPair() expects 5 arguments, but you have provided ".func_num_args(), 500);
	try{		
		// Check if relation signature exists: $relationName[$srcConceptName*$tgtConceptName]
        $srcConcept = Concept::getConceptByLabel($srcConceptName);
        $tgtConcept = Concept::getConceptByLabel($tgtConceptName);
		$relation = Relation::getRelation($relationName, $srcConcept, $tgtConcept);
		
		// if either srcAtomIdStr or tgtAtom is not provided by the pairview function (i.e. value set to '_NULL'): skip the insPair
		if($srcAtom == '_NULL' or $tgtAtom == '_NULL'){
            Logger::getLogger('EXECENGINE')->debug("DelPair ignored because src and/or tgt atom is _NULL");
            return;
        }
        
        // if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($srcAtom == "_NEW") $srcAtom = ExecEngine::$_NEW->id;
		if($tgtAtom == "_NEW") $tgtAtom = ExecEngine::$_NEW->id;
        
		$srcAtoms = explode('_AND', $srcAtom);
		$tgtAtoms = explode('_AND', $tgtAtom);
		if(count($srcAtoms) > 1) throw new Exception('DelPair function call has more than one src atom', 501); // 501: Not implemented
		if(count($tgtAtoms) > 1) throw new Exception('DelPair function call has more than one tgt atom', 501); // 501: Not implemented
		
		foreach($srcAtoms as $a){
		    $src = new Atom($a, $srcConcept);
		    foreach($tgtAtoms as $b){
				$tgt = new Atom($b, $tgtConcept);
		        $relation->deleteLink($src, $tgt, false, 'ExecEngine');
			}
		}
		
		Logger::getLogger('EXECENGINE')->debug("Tuple ('{$srcAtom}', '{$tgtAtom}') deleted from '{$relation->__toString()}'");
	}catch(Exception $e){
		Logger::getUserLogger()->error('DelPair: ' . $e->getMessage());
	}
}

/* The function 'NewStruct' creates a new atom in some concept and uses this
   atom to create links (in relations in which the concept is SRC or TGT).

   Example:
   
   r :: ConceptA * ConceptB
   r1 :: ConceptA * ConceptC [INJ] -- multiplicity must be there (I think...)
   r2 :: ConceptC * ConceptB [UNI] -- multiplicity must be there (I think...)
   
   RULE "equivalence": r = r1;r2 -- this rule is to be maintained automatically
   
   ROLE ExecEngine MAINTAINS "insEquivalence" -- Creation of the atom
   RULE "insEquivalence": r |- r1;r2
   VIOLATION (TXT "NewStruct;ConceptC[;AtomC]" -- AtomC is optional. If not provided then create new, else used specified Atom
             ,TXT ";r1;ConceptA;", SRC I, TXT";ConceptC;_NEW"  -- Always use _NEW as ConceptC atom
             ,TXT ";r2;ConceptC;_NEW;ConceptB;atomB;", TGT I   -- Always use _NEW as ConceptC atom
              )

*/
function NewStruct(){ // arglist: ($ConceptC[,$newAtom][,$relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom]+)
	try{		
		// We start with parsing the first one or two arguments
		$c = Concept::getConceptByLabel(func_get_arg(0)); // Concept for which atom is to be created
		$atom = $c->createNewAtom(); // Default marker for atom-to-be-created.

		Logger::getLogger('EXECENGINE')->info("Newstruct for concept '{$c}'");
		
		// Check if name of new atom is explicitly specified
		if (func_num_args() % 5 == 2) $atom = new Atom(func_get_arg(1), $c); // If so, we'll be using this to create the new atom
		// Check for valid number of arguments
		elseif(func_num_args() % 5 != 1) throw new Exception("Wrong number of arguments supplied for function Newstruct(): ".func_num_args()." arguments", 500);
		
		// Add atom to concept
		$atom->addAtom();
		
		// Make newly created atom available within scope of violation for use of other functions
		ExecEngine::$_NEW = $atom;
	
		// Next, for every relation that follows in the argument list, we create a link
		for ($i = func_num_args() % 5; $i < func_num_args(); $i = $i+5){
			
			$relation   = func_get_arg($i);
			$srcConcept = Concept::getConceptByLabel(func_get_arg($i+1));
			$srcAtomId    = func_get_arg($i+2);
			$tgtConcept = Concept::getConceptByLabel(func_get_arg($i+3));
			$tgtAtomId    = func_get_arg($i+4);
			
			if($srcAtomId == "NULL" or $tgtAtomId == "NULL") throw new Exception("NewStruct: use of keyword NULL is deprecated, use '_NEW'", 500);
			
			// NewStruct requires that atom $srcAtomId or $tgtAtomId must be _NEW
			// Note: when populating a [PROP] relation, both atoms can be new
			if (!($srcAtomId == '_NEW' or $tgtAtomId == '_NEW')) throw new Exception("NewStruct: relation '{$relation}' requires that atom '{$srcAtomId}' or '{$tgtAtomId}' must be '_NEW'", 500);
			
			// NewStruct requires that concept $srcConcept or $tgtConcept must be concept $c
			if (!in_array($srcConcept, $c->getGeneralizationsIncl()) && !in_array($tgtConcept, $c->getGeneralizationsIncl())) throw new Exception("NewStruct: relation '{$relation}' requires that src or tgt concept must be '{$c}' (or any of its generalizations)", 500);
		
			// Replace atom by the newstruct atom if _NEW is used
			if(in_array($srcConcept, $c->getGeneralizationsIncl()) && $srcAtomId == '_NEW') $srcAtomId = $atom->id;
			if(in_array($tgtConcept, $c->getGeneralizationsIncl()) && $tgtAtomId == '_NEW') $tgtAtomId = $atom->id;
			
			// Any logging is done by InsPair
			InsPair($relation,$srcConcept->name,$srcAtomId,$tgtConcept->name,$tgtAtomId);
		}
		Logger::getLogger('EXECENGINE')->debug("Newstruct: atom '{$atom}' created");
	
	}catch(Exception $e){
		Logger::getUserLogger()->error("NewStruct: {$e->getMessage()}");
	}
}

// Use: VIOLATION (TXT "InsAtom;<concept>") -- this may not be of any use in Ampersand, though.
function InsAtom($conceptName){
	Logger::getLogger('EXECENGINE')->info("InsAtom($conceptName)");
    if(func_num_args() != 1) throw new Exception("InsAtom() expects 1 argument, but you have provided ".func_num_args(), 500);
	try{
		$database = Database::singleton();
		
		$concept = Concept::getConceptByLabel($conceptName);
        $atom = $concept->createNewAtom();
		$atom->addAtom(); // insert new atom in database
		
		// Make newly created atom available within scope of violation for use of other functions
		ExecEngine::$_NEW = $atom;
		
		Logger::getLogger('EXECENGINE')->debug("Atom '{$atom->__toString()}' created and added to database");
		
	}catch(Exception $e){
		Logger::getUserLogger()->error('InsAtom: ' . $e->getMessage());
	}
	
}

/* 
	ROLE ExecEngine MAINTAINS "delEquivalence" -- Deletion of the atom
	RULE "delEquivalence": I[ConceptC] |- r1~;r;r2~
	VIOLATION (TXT "DelAtom;ConceptC;" SRC I) -- all links in other relations in which the atom occurs are deleted as well.
*/
// Use: VIOLATION (TXT "DelAtom;<concept>;<atom>")
function DelAtom($concept, $atomId){
	Logger::getLogger('EXECENGINE')->info("DelAtom($concept,$atomId)");
    if(func_num_args() != 2) throw new Exception("DelAtom() expects 2 arguments, but you have provided ".func_num_args(), 500);
	try{
		// if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($atomId == "_NEW") $atom = ExecEngine::$_NEW;
	    else $atom = new Atom($atomId, Concept::getConceptByLabel($concept));
		
		$atom->deleteAtom(); // delete atom + all pairs shared with other atoms
		Logger::getLogger('EXECENGINE')->debug("Atom '{$atom}' deleted");
	
	}catch(Exception $e){
		Logger::getUserLogger()->error('DelAtom: ' . $e->getMessage());
	}
	
}

/* 
	ROLE ExecEngine MAINTAINS "Person" -- unify two atoms
	RULE Person : name;name~ |- I
    VIOLATION (TXT "{EX} MrgAtoms;Person;", SRC I, TXT ";Person;", TGT I )
	 * Parameters
	 * @param Concept $conceptA   -- The most specific concept A such that $srcAtomId pop A
	 * @param Atom $srcAtomId     -- The atom to be made equal to $tgtAtomId
	 * @param Concept $conceptB   -- The most specific concept B such that $tgtAtomId pop B
	 * @param Atom $tgtAtomId     -- The atom to be made equal to $srcAtomId
	 * @return void
*/
// Use: VIOLATION (TXT "{EX} MrgAtoms;<conceptA>;", SRC I, TXT ";<conceptB>;", TGT I )
function MrgAtoms($conceptA, $srcAtomId, $conceptB, $tgtAtomId){
	Logger::getLogger('EXECENGINE')->info("MrgAtoms($conceptA, $srcAtomId, $conceptB, $tgtAtomId)");
    if(func_num_args() != 4) throw new Exception("MrgAtoms() expects 4 arguments, but you have provided ".func_num_args(), 500);
	try{		
		$srcAtom = new Atom($srcAtomId, Concept::getConceptByLabel($conceptA));
		$tgtAtom = new Atom($tgtAtomId, Concept::getConceptByLabel($conceptB));
		
		// if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($srcAtomId == "_NEW") $srcAtom = ExecEngine::$_NEW;
		if($tgtAtomId == "_NEW") $tgtAtom = ExecEngine::$_NEW;
		
		$srcAtom->unionWithAtom($tgtAtom); // union of two records plus substitution in all occurences in binary relations.
		Logger::getLogger('EXECENGINE')->debug("Atom '{$tgtAtom}' unified with '{$srcAtom}' and then deleted");
	
	}catch(Exception $e){
		Logger::getUserLogger()->error('MrgAtoms: ' . $e->getMessage());
	}
	
}

/*
 ROLE ExecEngine MAINTAINS "SetConcept" -- Adding an atomId[ConceptA] as member to ConceptB set. This can only be done when ConceptA and ConceptB are in the same classification tree.
 RULE "SetConcept": I[ConceptA] |- expr
 VIOLATION (TXT "SetConcept;ConceptA;ConceptB;" SRC I)
 */
// Use: VIOLATION (TXT "SetConcept;<ConceptA>;<ConceptB>;<atomId>")
function SetConcept($conceptA, $conceptB, $atomId){
	Logger::getLogger('EXECENGINE')->info("SetConcept($conceptA,$conceptB,$atomId)");
    if(func_num_args() != 3) throw new Exception("SetConcept() expects 3 arguments, but you have provided ".func_num_args(), 500);
	try{
		$database = Database::singleton();
		
		// if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($atomId == "_NEW") $atom = ExecEngine::$_NEW;
	    else $atom = new Atom($atomId, Concept::getConceptByLabel($conceptA));
		
        $conceptB = Concept::getConceptByLabel($conceptB);
		$database->atomSetConcept($atom, $conceptB);
		Logger::getLogger('EXECENGINE')->debug("Atom '{$atom}' added as member to concept '{$conceptB}'");
	
	}catch(Exception $e){
		Logger::getUserLogger()->error('SetConcept: ' . $e->getMessage());
	}
}

/*
 ROLE ExecEngine MAINTAINS "ClearConcept" -- Removing an atom as member from a Concept set. This can only be done when the concept is a specialization of another concept.
 RULE "ClearConcept": I[Concept] |- expr
 VIOLATION (TXT "ClearConcept;Concept;" SRC I)
 */
// Use: VIOLATION (TXT "ClearConcept;<Concept>;<atom>")
function ClearConcept($concept, $atomId){
	Logger::getLogger('EXECENGINE')->info("ClearConcept($concept,$atomId)");
    if(func_num_args() != 2) throw new Exception("ClearConcept() expects 2 arguments, but you have provided ".func_num_args(), 500);
	try{
		$database = Database::singleton();
        
        // if atom id is specified as _NEW, the latest atom created by NewStruct or InsAtom (in this VIOLATION) is used
	    if($atomId == "_NEW") $atom = ExecEngine::$_NEW;
		$atom = new Atom($atomId, Concept::getConceptByLabel($concept));
		
		$database->atomClearConcept($atom);
		Logger::getLogger('EXECENGINE')->debug("Atom '{$atom}' removed as member from concept '$concept'");

	}catch(Exception $e){
		Logger::getUserLogger()->error('ClearConcept: ' . $e->getMessage());
	}
}


/**************************************************************
 * Conditional variants of the above functions
 *************************************************************/
 
// InsPairCond is skipped when $bool string value equals: "0", "false", "off", "no", "" or "_NULL"
function InsPairCond($relationName, $srcConceptName, $srcAtom, $tgtConceptName, $tgtAtom, $bool){
	Logger::getLogger('EXECENGINE')->info("InsPairCond($relationName, $srcConceptName, $srcAtom, $tgtConceptName, $tgtAtom, $bool)");
    
	try{
        if(func_num_args() != 6) throw new Exception("InsPairCond() expects 6 arguments, but you have provided ".func_num_args(), 500);
        
        // Skip when $bool evaluates to false or equals '_NULL'. 
        // _Null is the exec-engine special for zero results from Ampersand expression
        if(filter_var($bool, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false || $bool === '_NULL'){
            Logger::getLogger('EXECENGINE')->debug("InsPairCond skipped because bool evaluation results in false");
            return;
        }
        
        InsPair($relationName, $srcConceptName, $srcAtom, $tgtConceptName, $tgtAtom);
	
	}catch(Exception $e){
		Logger::getUserLogger()->error('InsPairCond: ' . $e->getMessage());
	}
}

// SetConcept is skipped when $bool string value equals: "0", "false", "off", "no", "" or "_NULL"
function SetConceptCond($conceptA, $conceptB, $atom, $bool){
	Logger::getLogger('EXECENGINE')->info("SetConceptCond($conceptA, $conceptB, $atom, $bool)");
    
	try{
        if(func_num_args() != 4) throw new Exception("SetConceptCond() expects 4 arguments, but you have provided ".func_num_args(), 500);
        
        // Skip when $bool evaluates to false or equals '_NULL'. 
        // _Null is the exec-engine special for zero results from Ampersand expression
        if(filter_var($bool, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false || $bool === '_NULL'){
            Logger::getLogger('EXECENGINE')->debug("SetConcept skipped because bool evaluation results in false");
            return;
        }
        
        SetConcept($conceptA, $conceptB, $atom);
	
	}catch(Exception $e){
		Logger::getUserLogger()->error('SetConceptCond: ' . $e->getMessage());
	}
}

function SetNavToOnCommit($navTo){
	Logger::getLogger('EXECENGINE')->info("setNavToOnCommit($navTo)");
	
	if(strpos($navTo, '_NEW') !== false){
		$navTo = str_replace('_NEW', ExecEngine::$_NEW->id, $navTo); // Replace _NEW with latest atom created by NewStruct or InsAtom (in this VIOLATION)
		Logger::getLogger('EXECENGINE')->debug("replaced navTo string with '{$navTo}'");
	}
	
	$session = Session::singleton();
	$session->navToOnCommit = $navTo;
}
    
function SetNavToOnRollback($navTo){
	Logger::getLogger('EXECENGINE')->info("setNavToOnRollback($navTo)");
	
	if(strpos($navTo, '_NEW') !== false){
		$navTo = str_replace('_NEW', ExecEngine::$_NEW->id, $navTo); // Replace _NEW with latest atom created by NewStruct or InsAtom (in this VIOLATION)
		Logger::getLogger('EXECENGINE')->debug("replaced navTo string with '{$navTo}'");
	}
	
	$session = Session::singleton();
	$session->navToOnRollback = $navTo;
}
?>