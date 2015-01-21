<?php

class Relation {

	public static function isCombination($relationName, $srcConcept, $tgtConcept){
		/*
		$relationTableInfo from Generics.php 
		contains array with all relations, for each relation the following is specified: 
		 - srcConcept : srcConcept of relation
		 - tgtConcept : tgtConcept of relation
		*/
		global $relationTableInfo;
		
		foreach($relationTableInfo as $key => $relationInfo){
			if($relationInfo['name'] == $relationName && $relationInfo['srcConcept'] == $srcConcept && $relationInfo['tgtConcept'] == $tgtConcept)
				return $key; // relation signature: 'rel_<relation>_<srcConcept>_<tgtConcept>'
			
			if($key == $relationName && $relationInfo['srcConcept'] == $srcConcept && $relationInfo['tgtConcept'] == $tgtConcept)
				return $key;
		}
		
		return false;
	}
	
	public static function getTable($fullRelationSignature){
		global $relationTableInfo;
		
		return $relationTableInfo[$fullRelationSignature]['table'];
	}
	public static function getSrcCol($fullRelationSignature){
		global $relationTableInfo;
	
		return $relationTableInfo[$fullRelationSignature]['srcCol'];
	}
	public static function getTgtCol($fullRelationSignature){
		global $relationTableInfo;
	
		return $relationTableInfo[$fullRelationSignature]['tgtCol'];
	}
}

?>