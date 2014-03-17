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
		
		// check if $relation appears in $relationTableInfo
		foreach($relationTableInfo as $key => $arr){   
			if($key == $relation and $arr['srcConcept'] == $srcConcept and $arr['tgtConcept'] == $tgtConcept){ 
				return true;
			}
		}
		
		return false;
	}
}

?>