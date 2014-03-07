<?php


class Concept {

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

		return array_column($database->Exe("SELECT DISTINCT `$conceptColEsc` FROM `$conceptTableEsc` WHERE `$conceptColEsc` IS NOT NULL"),1);
	}
	
	public static function isAtomInConcept($atom, $concept) {
		return in_array( $atom, Concept::getAllAtoms($concept) );
	}
}

?>