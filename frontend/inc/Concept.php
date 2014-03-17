<?php

class Concept {

	public static function getAllConcepts(){
		global $conceptTableInfo; // from Generics.php
		
		return array_keys($conceptTableInfo);
	}

	public static function getSpecializations($concept) {
		global $allSpecializations;
		
		return isset($allSpecializations[$concept]) ? $allSpecializations[$concept] : array ();
		
	}
	
	public static function getAllAtoms($concept){
		$database = Database::singleton();
		global $conceptTableInfo;

		$conceptTable = $conceptTableInfo[$concept][0]['table']; // $conceptTableInfo[$concept] is an array of tables with arrays of columns maintaining $concept
		$conceptCol = $conceptTableInfo[$concept][0]['cols'][0]; // for lookup, we just take the first table and its first column
		$conceptTableEsc = addslashes($conceptTable);
		$conceptColEsc = addslashes($conceptCol);

		return array_column($database->Exe("SELECT DISTINCT `$conceptColEsc` FROM `$conceptTableEsc` WHERE `$conceptColEsc` IS NOT NULL"),$conceptColEsc);
	}
	
	public static function isAtomInConcept($atom, $concept) {
		return in_array( $atom, Concept::getAllAtoms($concept) );
	}
	
	public static function createNewAtom($concept){
		$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
		$atom = $concept.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds  
		
		return $atom;
	}
}

?>