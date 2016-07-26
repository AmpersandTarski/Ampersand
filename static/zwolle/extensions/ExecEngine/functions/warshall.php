<?php 
/* This file defines the (php) function 'TransitiveClosure', that computes the transitive closure of a relation.

   Suppose you have a relation r :: C * C, and that you need the transitive closure r+ of that relation.
   Since r* is not supported in the prototype generator as is, we need a way to instruct the ExecEngine
   to populate a relation rPlus :: C * C that contains the same population as r+
   Maintaining the population of rPlus correctly is not trivial, particularly when r is depopulated.
   The easiest way around this is to compute rPlus from scratch (using Warshall's algorithm).
   However, you then need to know that r is being (de)populated, so we need a copy of r.

   This leads to the following pattern:
   
   relation :: Concept*Concept
   relationCopy :: Concept*Concept -- copied value of 'relation' allows for detecting modifcation events
   relationStar :: Concept*Concept -- transitive closure of relation
   
   ROLE ExecEngine MAINTAINS "relationCompTransitiveClosure"
   RULE "relationCompTransitiveClosure": relation = relationCopy
   VIOLATION (TXT "{EX} TransitiveClosure;relation;Concept;relationCopy;relationStar")

   NOTES:
   1) The above example is made for ease of use. This is what you need to do:
      a) copy and paste the above example into your own ADL script;
      b) replace the names of 'relation' and 'Concept' (cases sensitive, also as part of a word) with what you need
   2) Of course, there are all sorts of alternative ways in which 'TransitiveClosure' can be used.
   3) There are ways to optimize the below code, e.g. by splitting the function into an 'InsTransitiveClosure'
      and a 'DelTransitiveClosure'
   4) Rather than defining/computing rStar (for r*), you may use the expression (I \/ rPlus)
*/

use Ampersand\Log\Logger;
use Ampersand\Extension\ExecEngine\ExecEngine;
use Ampersand\Database\Database;
use Ampersand\Core\Relation;

function TransitiveClosure($r,$C,$rCopy,$rPlus){
	if(func_num_args() != 4) throw new Exception("Wrong number of arguments supplied for function TransitiveClosure(): ".func_num_args()." arguments", 500);
	Logger::getLogger('EXECENGINE')->debug("Exeucte TransitiveClosure($r,$C,$rCopy,$rPlus)");
	
	$warshallRunCount = $GLOBALS['ext']['ExecEngine']['functions']['warshall']['runCount'];
	$execEngineRunCount = ExecEngine::$runCount;

	if($GLOBALS['ext']['ExecEngine']['functions']['warshall']['warshallRuleChecked'][$r]){
		if($warshallRunCount == $execEngineRunCount){
			Logger::getLogger('EXECENGINE')->debug("Skipping TransitiveClosure($r,$C,$rCopy,$rPlus)");
			return;  // this is the case if we have executed this function already in this transaction
		}		
	}
		
	$GLOBALS['ext']['ExecEngine']['functions']['warshall']['warshallRuleChecked'][$r] = true;
	$GLOBALS['ext']['ExecEngine']['functions']['warshall']['runCount'] = ExecEngine::$runCount;
	
	// Compute transitive closure following Warshall's algorithm
	$closure = RetrievePopulation($r, $C); // get adjacency matrix
	
	OverwritePopulation($closure, $rCopy, $C); // store it in the 'rCopy' relation
	
	// Get all unique atoms from this population
	$atoms = array_keys($closure); // 'Src' (left) atoms of pairs in $closure
	
	foreach ($closure as $tgtAtomsList){ // Loop to add 'Tgt' atoms that not yet exist
		$tgtAtoms = array_keys($tgtAtomsList);
		foreach ($tgtAtoms as $tgtAtom){
			if (!in_array($tgtAtom, $atoms)) $atoms[] = $tgtAtom;
		}
	}
	
	foreach ($atoms as $k){
		foreach ($atoms as $i){
			if ($closure[$i][$k]){
				foreach ($atoms as $j){
					$closure[$i][$j] = $closure[$i][$j] || $closure[$k][$j];
				}
			}
		}
	}
	
	OverwritePopulation($closure, $rPlus, $C);
}

function RetrievePopulation($relationName, $conceptName){
	try{
		$database = Database::singleton();
		
		$relation = Relation::getRelation($relationName, $conceptName, $conceptName);
		$relationTable = $relation->getMysqlTable();
		$srcCol = $relationTable->srcCol();
		$tgtCol = $relationTable->tgtCol();
		
		$query = "SELECT * FROM `{$relationTable->name}`";
		$result = $database->Exe($query);
		
		// initialization of 2-dimensional array
		$array = array();
		foreach($result as $row){
			$array[$row[$srcCol->name]][$row[$tgtCol->name]] = !is_null($row[$tgtCol->name]);
		}
		
		return $array;
	}catch(Exception $e){
		throw new Exception('RetrievePopulation: ' . $e->getMessage(), 500);
	}
}

// Overwrite contents of &-relation $r with contents of php array $rArray
function OverwritePopulation($rArray, $relationName, $conceptName){
	try{
		$database = Database::singleton();
		
		$relation = Relation::getRelation($relationName, $conceptName, $conceptName);
		$relationTable = $relation->getMysqlTable();
		$srcCol = $relationTable->srcCol();
		$tgtCol = $relationTable->tgtCol();
		
		$query = "DELETE FROM `{$relationTable->name}`"; // Do not use TRUNCATE statement, this causes an implicit commit
		$database->Exe($query);
		
		foreach($rArray as $src => $tgtArray){
			foreach($tgtArray as $tgt => $bool){
				if($bool){
					$query = "INSERT INTO `{$relationTable->name}` (`{$srcCol->name}`, `{$tgtCol->name}`) VALUES ('$src','$tgt')";
					$database->Exe($query);
				}
			}
		}
		
	}catch(Exception $e){
		throw new Exception('OverwritePopulation: ' . $e->getMessage(), 500);
	}
}
?>