<?php

class Relation {

	public static function getAllRelations(){
		global $allRelations; // from Generics.php
		
		return $allRelations;
	}
	
	/*
	 * $relationName may be specified as fullRelationSignature (as provided in Generics) or as Ampersand relation name (as specified in Ampersand script)
	 */
	public static function isCombination($relationName, $srcConcept, $tgtConcept){
		$allRelations = Relation::getAllRelations();
		
		foreach($allRelations as $key => $relationInfo){
		    $relSrcConcept = Concept::getConcept($relationInfo['srcConcept']);
		    $relTgtConcept = Concept::getConcept($relationInfo['tgtConcept']);
		    
			// Match relationName with relation name as specified in Ampersand script.
			// Includes support for specializations.
			if($relationInfo['name'] == $relationName 
					&& ($relSrcConcept->name == $srcConcept || $relSrcConcept->hasSpecialization($srcConcept))
					&& ($relTgtConcept->name == $tgtConcept || $relTgtConcept->hasSpecialization($tgtConcept))
					)
				return $key; // return fullRelationSignature
						
			// Match relationName with fullRelationSignature (format: 'rel_<relationName>_<srcConcept>_<tgtConcept>')
			if($key == $relationName 
					&& ($relSrcConcept->name == $srcConcept || $relSrcConcept->hasSpecialization($srcConcept))
					&& ($relTgtConcept->name == $tgtConcept || $relTgtConcept->hasSpecialization($tgtConcept))
					)
				return $key; // return fullRelationSignature
		}
		
		// If relation not found in $allRelations
		throw new Exception("Cannot find relation with signature '" . $relationName . "[" . $srcConcept . "*" . $tgtConcept . "]'", 500);
	}
	
	public static function getTable($fullRelationSignature){
		$allRelations = Relation::getAllRelations();
		
		return $allRelations[$fullRelationSignature]['table'];
	}
	
	public static function getSrcCol($fullRelationSignature){
		$allRelations = Relation::getAllRelations();
	
		return $allRelations[$fullRelationSignature]['srcCol'];
	}
	
	public static function getTgtCol($fullRelationSignature){
		$allRelations = Relation::getAllRelations();
	
		return $allRelations[$fullRelationSignature]['tgtCol'];
	}
	
	public static function getTableColumnInfo($table, $column){
		global $tableColumnInfo;
		
		if(!array_key_exists($table, $tableColumnInfo)) throw new Exception("Table \'$table\' does not exists in tableColumnInfo", 500);
		if(!array_key_exists($column, $tableColumnInfo[$table])) throw new Exception("Column \'$column\' does not exists in table \'$table\'", 500);
		
		return $tableColumnInfo[$table][$column];
	}
	
	/**
	 * 
	 * @param string $fullRelationSignature
	 * @throws Exception
	 * @return Conjunct[]
	 */
	public static function getAffectedSigConjuncts($fullRelationSignature){
		$allRelations = Relation::getAllRelations();
	
		if(!array_key_exists($fullRelationSignature, $allRelations)) throw new Exception("Relation \'$fullRelationSignature\' does not exists in allRelations", 500);
		
		$conjuncts = array();
		foreach ((array)$allRelations[$fullRelationSignature]['affectedSigConjunctIds'] as $conjId){
		    $conjuncts[] = Conjunct::getConjunct($conjId);
		}
		return $conjuncts;
	}
	
	/**
	 * 
	 * @param string $fullRelationSignature
	 * @throws Exception
	 * @return Conjunct[]
	 */
	public static function getAffectedInvConjuncts($fullRelationSignature){
		$allRelations = Relation::getAllRelations();
	
		if(!array_key_exists($fullRelationSignature, $allRelations)) throw new Exception("Relation \'$fullRelationSignature\' does not exists in allRelations", 500);
		
		$conjuncts = array();
		foreach ((array)$allRelations[$fullRelationSignature]['affectedInvConjunctIds'] as $conjId){
		    $conjuncts[] = Conjunct::getConjunct($conjId);
		}
		return $conjuncts;		
	}
}

?>