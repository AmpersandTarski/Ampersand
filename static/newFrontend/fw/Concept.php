<?php

class Concept {

	public static function getAllConcepts(){
		global $allConcepts; // from Generics.php
		
		return array_keys($allConcepts);
	}

	public static function getSpecializations($concept) {
		global $allSpecializations; // from Generics.php
		
		return isset($allSpecializations[$concept]) ? $allSpecializations[$concept] : array ();
		
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
		$conceptTable = $conceptTableInfo[0]['table'];
		
		// invariant: all concept tables (which are columns) are maintained properly, so we can query an arbitrary col for checking the existence of a concept
		// TODO: check if this also works with the ISA solution?
		$firstConceptCol = $conceptTableInfo[0]['cols'][0]; // for lookup, we just take the first table and its first column
		
		// Query all atoms in table
		return $existingAtoms = array_column($database->Exe("SELECT DISTINCT `$firstConceptCol` FROM `$conceptTable` WHERE `$firstConceptCol` IS NOT NULL"), $firstConceptCol); // no need to filter duplicates and NULLs
		
	}	
	
	public static function isAtomInConcept($atom, $concept) {
		return in_array(strtolower($atom), array_map('strtolower', (array)Concept::getAllAtomIds($concept))); // in_array is case sensitive ("true" != "TRUE"), but Mysql is case insensitive for Primary keys. Therefore first to lowercase.
	}
	
	public static function createNewAtom($concept){
		$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
		$atomId = $concept.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds  
		
		return $atomId;
	}
	
	public static function getView($concept){
		global $allViews; // from Generics.php
		
		foreach ((array)$allViews as $view){
			if ($concept == $view['concept'] || in_array($concept, Concept::getSpecializations($view['concept']))) return $view;
		}
		return null;
	}
	
	public static function getConceptTableInfo($concept){
		global $allConcepts; // from Generics.php
		
		// $allConcepts[$concept]['conceptTables] is an array of tables with arrays of columns maintaining $concept.
		// (we have an array rather than a single column because of generalizations) 
		// TODO: still the right solution?, because generalizations/specializations are in one table
		
		if(!array_key_exists($concept, $allConcepts)) throw new Exception("Concept $concept does not exists in allConcepts", 500);
		
		return (array)$allConcepts[$concept]['conceptTables'];
	}
	
	public static function getAffectedSigConjuncts($concept){
		global $allConcepts; // from Generics.php
		
		if(!array_key_exists($concept, $allConcepts)) throw new Exception("Concept $concept does not exists in allConcepts", 500);
		
		return (array)$allConcepts[$concept]['affectedSigConjunctIds'];
	}
	
	public static function getAffectedInvConjuncts($concept){
		global $allConcepts; // from Generics.php
	
		if(!array_key_exists($concept, $allConcepts)) throw new Exception("Concept $concept does not exists in allConcepts", 500);
	
		return (array)$allConcepts[$concept]['affectedInvConjunctIds'];
	}
	
	public static function getAllInterfaces($concept){
		global $allInterfaceObjects; // from Generics.php
		
		$interfaces = array();
		
		foreach ($allInterfaceObjects as $interfaceId => $interface){
			if ($interface['srcConcept'] == $concept) $interfaces[] = $interfaceId;
		}
		
		return $interfaces;
	}

}

?>