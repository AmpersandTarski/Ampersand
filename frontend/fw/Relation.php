<?php

class Relation {

	public static function isCombination($relation, $srcConcept, $tgtConcept){
		/*
		$relationTableInfo from Generics.php 
		contains array with all relations, for each relation the following is specified: 
		 - srcConcept : srcConcept of relation
		 - tgtConcept : tgtConcept of relation
		*/
		global $relationTableInfo;
		
		$result = false;
		
		foreach($relationTableInfo as $key => $relationInfo){
			if($relationInfo['name'] == $relation && $relationInfo['srcConcept'] == $srcConcept && $relationInfo['tgtConcept'] == $tgtConcept)
				return $key; // relation signature: 'rel_<relation>_<srcConcept>_<tgtConcept>'
		}
		
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