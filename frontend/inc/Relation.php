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
		
		return array_key_exists("rel_" . $relation . "_" . $srcConcept . "_" . $tgtConcept, $relationTableInfo);
		
	}
}

?>