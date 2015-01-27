<?php

class Relation {

	public static function isCombination($relationName, $srcConcept, $tgtConcept){
		/*
		$allRelations from Generics.php 
		contains array with all relations, for each relation the following is specified: 
		 - srcConcept : srcConcept of relation
		 - tgtConcept : tgtConcept of relation
		*/
		global $allRelations;
		
		foreach($allRelations as $key => $relationInfo){
			if($relationInfo['name'] == $relationName && $relationInfo['srcConcept'] == $srcConcept && $relationInfo['tgtConcept'] == $tgtConcept)
				return $key; // relation signature: 'rel_<relation>_<srcConcept>_<tgtConcept>'
			
			if($key == $relationName && $relationInfo['srcConcept'] == $srcConcept && $relationInfo['tgtConcept'] == $tgtConcept)
				return $key;
		}
		
		return false;
	}
	
	public static function getTable($fullRelationSignature){
		global $allRelations;
		
		return $allRelations[$fullRelationSignature]['table'];
	}
	
	public static function getSrcCol($fullRelationSignature){
		global $allRelations;
	
		return $allRelations[$fullRelationSignature]['srcCol'];
	}
	
	public static function getTgtCol($fullRelationSignature){
		global $allRelations;
	
		return $allRelations[$fullRelationSignature]['tgtCol'];
	}
	
	public static function getTableColumnInfo($table, $column){
		global $tableColumnInfo;
		
		if(!array_key_exists($table, $tableColumnInfo)) throw new Exception('Table $table does not exists in tableColumnInfo');
		if(!array_key_exists($column, $tableColumnInfo[$table])) throw new Exception('Column $column does not exists in table $table');
		
		return $tableColumnInfo[$table][$column];
	}
	
	public static function getAffectedSigConjunctIds($fullRelationSignature){
		global $allRelations; // from Generics.php
	
		if(!array_key_exists($fullRelationSignature, $allRelations)) throw new Exception("Relation $fullRelationSignature does not exists in allRelations");
	
		return (array)$allRelations[$fullRelationSignature]['affectedSigConjunctIds'];
	}
	
	public static function getAffectedInvConjunctIds($fullRelationSignature){
		global $allRelations; // from Generics.php
	
		if(!array_key_exists($fullRelationSignature, $allRelations)) throw new Exception("Relation $fullRelationSignature does not exists in allRelations");
	
		return (array)$allRelations[$fullRelationSignature]['affectedInvConjunctIds'];
	}
}

?>