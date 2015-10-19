<?php

class Concept {
	public $name;
	
	public function __construct($conceptName){
		$this->name = $conceptName;
	}
	
	public function hasSpecialization($conceptName){
		return in_array($conceptName, Concept::getSpecializations($this->name));
	}
	
	public function hasGeneralization($conceptName){
		return in_array($conceptName, Concept::getGeneralizations($this->name));
	}
	
	public static function getConcept($concept){
		global $allConcepts;
		
		if(!array_key_exists($concept, $allConcepts)) throw new Exception("Concept $concept not defined in \$allConcepts (Generics.php)", 500);
		return $allConcepts[$concept];
	}
	
	public static function getAllConcepts(){
		global $allConcepts; // from Generics.php
	
		return array_keys($allConcepts);
	}
	
	public static function getTypeRepresentation($concept){
		$conceptInfo = Concept::getConcept($concept);
		
		if(!array_key_exists('type', $conceptInfo)) throw new Exception("Type not defined for concept $concept in \$allConcepts (Generics.php)", 500);
		return $conceptInfo['type'];
	}

	public static function getSpecializations($concept) {
		global $allSpecializations; // from Generics.php
		
		$conceptInfo = Concept::getConcept($concept); // only to check if concept is defined
		return isset($allSpecializations[$concept]) ? $allSpecializations[$concept] : array ();
	}
	
	public static function getGeneralizations($concept){
		global $allSpecializations; // from Generics.php
		
		$conceptInfo = Concept::getConcept($concept); // only to check if concept is defined
		
		$generalizations = array();
		foreach ($allSpecializations as $key => $specializations){
			if(in_array($concept, $specializations)) $generalizations[] = $key;
		}
		return $generalizations;
	}
	
	public static function inSameClassificationTree($conceptA, $conceptB){
		if(in_array($conceptA, Concept::getSpecializations($conceptB))) return true;
		if(in_array($conceptB, Concept::getSpecializations($conceptA))) return true;
		
		// else
		return false;
	}
	
	public static function getAllAtomObjects($concept){
		
		foreach (Concept::getAllAtomIds($concept) as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId, $concept);
			$arr[] = $tgtAtom->getAtom();
		}
		return $arr;
	}
	
	public static function getAllAtomIds($concept){
		$database = Database::singleton();
		
		$conceptTableInfo = Concept::getConceptTableInfo($concept);
		$conceptTable = $conceptTableInfo['table'];
		$firstConceptCol = $conceptTableInfo['cols'][0]; // We can query an arbitrary concept col for checking the existence of an atom
		
		// Query all atoms in table
		$query = "SELECT DISTINCT `$firstConceptCol` FROM `$conceptTable` WHERE `$firstConceptCol` IS NOT NULL";
		return $existingAtoms = array_column($database->Exe($query), $firstConceptCol); // no need to filter duplicates and NULLs
		
	}
	
	public static function createNewAtom($concept){
		if(strpos($concept, '_AUTOINCREMENT') !== false){ // TODO: change to type definition when Ampersand is supporting IT-TYPE
			$database = Database::singleton();
			$tableInfo = Concept::getConceptTableInfo($concept);
			
			$table = $tableInfo['table'];
			$col = $tableInfo['cols'][0];
			
			$query = "SELECT MAX(`$col`) as `MAX` FROM `$table`";
			$result = array_column($database->Exe($query), 'MAX');
			
			if(empty($result)) $atomId = 1;
			else $atomId = $result[0] + 1;
				
		}else{
			$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
			$atomId = $concept.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds  
		}
		return $atomId;
	}
	
	public static function getView($concept, $viewId = null){
		global $allViews; // from Generics.php
		
		if(is_null($viewId)) $viewId = Concept::getDefaultViewId($concept); // Get defaultViewId
		
		// No view defined for this concept
		if(is_null($viewId)){
			return null;

		// Get specified view
		}else{
			// Selecting all relevant views for this concept from $allViews in Generics.php
			foreach ((array)$allViews as $view){
				if($view['label'] == $viewId){
					if (!($concept == $view['concept'] || in_array($concept, Concept::getSpecializations($view['concept'])))) throw new Exception("View '$viewId' is not for concept '$concept'");
					return $view;
				}
			}
		}
		
		// Otherwise throw exception
		throw new Exception("View '$viewId' is not defined");
	}
	
	public static function getDefaultViewId($concept){		
		$concept = Concept::getConcept($concept);
		
		if(isset($concept['defaultViewId'])) return $concept['defaultViewId'];
		else return null;
	}
	
	public static function getConceptTableInfo($concept){
		$conceptInfo = Concept::getConcept($concept);
		
		// $allConcepts[$concept]['conceptTables] is an array of tables with arrays of columns maintaining $concept.
		// (we have an array rather than a single column because of generalizations) 
		// TODO: still the right solution?, because generalizations/specializations are in one table
		
		if(empty($conceptInfo['conceptTables'][0]['table'])) throw new Exception("No database table defined for concept $concept in \$allConcepts (Generics.php)", 500);
		if(empty($conceptInfo['conceptTables'][0]['cols'])) throw new Exception("No columns defined for concept $concept in \$allConcepts (Generics.php)", 500);
		
		return $conceptInfo['conceptTables'][0]; // return only first item in array, because there are never more than one.
	}
	
	public static function getAffectedSigConjuncts($concept){
		$conceptInfo = Concept::getConcept($concept);
		
		return (array)$conceptInfo['affectedSigConjunctIds'];
	}
	
	public static function getAffectedInvConjuncts($concept){
		$conceptInfo = Concept::getConcept($concept);
	
		return (array)$conceptInfo['affectedInvConjunctIds'];
	}

}

?>