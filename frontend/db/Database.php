<?php
error_reporting(E_ALL ^ E_DEPRECATED ^ E_NOTICE);
ini_set("display_errors", 1); // TODO: error handling instellen

require_once (__DIR__ . '/config.php');

class Database
{	
	private $dblink;
	private $dbname;
	
	private static $_instance = null;
	
	// prevent any outside instantiation of this object
	private function __construct()
	{
		global $DB_host, $DB_user, $DB_pass, $DB_name; // from config.php
		$this->dbname = $DB_name;
		
		$this->dblink = mysql_connect($DB_host, $DB_user, $DB_pass);
		if (mysql_error()) throw new Exception(mysql_error());
		
		mysql_select_db($this->dbname, $this->dblink);
		if (mysql_error()) throw new Exception(mysql_error());
	}
	
	// Prevent any copy of this object
	private function __clone()
	{
		
	}
	
	public static function singleton()
	{
		if(!is_object (self::$_instance) ) self::$_instance = new Database();
		return self::$_instance;
	}

	public function Exe($query)
	{
		//TODO: add mysql_real_escape_string() on query and remove addslashes() elsewhere
		$result = mysql_query($query,$this->dblink);

		if (mysql_error()) throw new Exception(mysql_error());

		if ($result === false) return false;
		if ($result === true) return true;

		$resultarray = array();
		while(($resultarray[] = mysql_fetch_array($result)) || array_pop($resultarray)); // or mysql_fetch_assoc()??
		return $resultarray;
		
	}
	
	public function transaction($commandArray, $roleId){
		
		$this->Exe("START TRANSACTION"); // start database transaction
		
		foreach ($commandArray as $command){
			if (!isset($command->dbCmd)) throw new Exception("Malformed command, missing 'dbCmd'");
			
			switch ($command->dbCmd){ 
				case 'addToConcept':
					if (array_key_exists('atom', $command) && array_key_exists('concept', $command))
						$this->addAtomToConcept($command->atom, $command->concept);
					else 
						throw new Exception("Command '" .$command->dbCmd . "' is missing parameters");
					break;
				case 'update':
					if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) &&
						array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command) &&
						array_key_exists('parentOrChild', $command) && array_key_exists('originalAtom', $command))
						$this->editUpdate($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom,$command->parentOrChild, $command->originalAtom);
					else 
						throw new Exception("Command '" .$command->dbCmd . "' is missing parameters");
					break;
				case 'delete':
					if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) && array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command))
						$this->editDelete($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom);
					else 
						throw new Exception("Command " .$command->dbCmd . " is missing parameters");
					break;
				default:
					throw new Exception("Unkown command: '" .$command->dbCmd . "'");
			}
		}

		// Process rules checken hoeft niet, aangezien dat niet noodzakelijk is voor een transaction // checkRoleRules($selectedRoleNr);
		
		// TODO: ExecEngine check hier invoegen

		// Run all stored procedures in the database
		// Doing so AFTER running the ExecEngine allows any problems with stored procedures to be 'fixed'
		// 2do this: create a rule with the same ruleexpression and handle the violation with th ExecEngine
		// runAllProcedures();
		
		RuleEngine::checkProcessRules($roleId);
		
		foreach ((array)$GLOBALS['hooks']['before_Database_transaction_checkInvariantRules'] as $hook) call_user_func($hook);
		$invariantRulesHold = RuleEngine::checkInvariantRules();

		if ($invariantRulesHold) {
			$this->setLatestUpdateTime();
			$this->Exe("COMMIT"); // commit database transaction
			return true;
		} else {
			$this->Exe("ROLLBACK"); // rollback database transaction
			return false; // TODO return invariant violations
		}
	
	}

	public static function Escape($item)
	{
		if (is_object($item) OR is_array($item)) die("Escape item is not a variable but an object or array.");
		return mysql_escape_string($item);
	}
	
	public function error()
	{
		return mysql_error($this->dblink);
	}
	
	public function createNewAtom($concept){
		$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
		$atom = $concept.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds  
		
		$this->addAtomToConcept($atom, $concept);
		return $atom;
	}
	
	// TODO: make private function
	public function addAtomToConcept($newAtom, $concept) // Insert 'newAtom' only if it does not yet exist...
	{ 
		global $conceptTableInfo;

		foreach ($conceptTableInfo[$concept] as $conceptTableCol) { 
			// $conceptTableInfo[$concept] is an array of tables with arrays of columns maintaining $concept.
			// (we have an array rather than a single column because of generalizations)
			
			$conceptTable = $conceptTableCol['table']; 
			$conceptCols = $conceptTableCol['cols'];   // We insert the new atom in each of them.

			$conceptTableEsc = addslashes($conceptTable);
			$newAtomEsc = addslashes($newAtom); 

			// invariant: all concept tables (which are columns) are maintained properly, so we can query an arbitrary one for checking the existence of a concept
			$firstConceptColEsc = addslashes($conceptCols[0]);

			$existingAtoms = array_column($this->Exe("SELECT `$firstConceptColEsc` FROM `$conceptTableEsc`"), $firstConceptColEsc); // no need to filter duplicates and NULLs

			if (!in_array($newAtom, $existingAtoms)) {
				$allConceptColsEsc = '`'.implode('`, `', $conceptCols).'`';
				$newAtomsEsc = array_fill(0, count($conceptCols), $newAtomEsc);
				$allValuesEsc = "'".implode("', '", $newAtomsEsc)."'";

				$this->Exe("INSERT INTO `$conceptTableEsc` ($allConceptColsEsc) VALUES ($allValuesEsc)");
			}
		}
	}
	
	// NOTE: if $originalAtom == '', editUpdate means insert for n-ary relations
	// TODO: make private function
	public function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom)
	{ 
		global $relationTableInfo;
		global $tableColumnInfo;

		/* There seems to be a bug in 'editUpdate', nl. when a $relation occurs multiple times as KEY in the relationTableInfo (which we have seen happening when you overload an (Ampersand) relation (name). The following code may be used to find the right entry in the relationTableInfo, but that is not used by 'editUpdate'.
		
		// check if $relation appears in $relationTableInfo
		if (array_key_exists($relation, $relationTableInfo)){
			foreach($relationTableInfo as $key => $arr){
				if($key == $relation){ 
					if($arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept){ 
						$table = $arr['table'];
						$srcCol = $arr['srcCol'];
						$tgtCol = $arr['tgtCol'];
						echo "<br>[FOUND: table=$table, srcCol=$srcCol, tgtCol=$tgtCol]";
					}
				}
			}
		}else{ 
			echo "ERROR: Relation $relation does not exist (in table info)";
		}
		*/
		$table = $relationTableInfo[$rel]['table'];
		$srcCol = $relationTableInfo[$rel]['srcCol'];
		$tgtCol = $relationTableInfo[$rel]['tgtCol'];
		$parentCol = $isFlipped ? $tgtCol : $srcCol;
		$childCol =  $isFlipped ? $srcCol : $tgtCol;

		$modifiedCol = $parentOrChild == 'parent' ? $parentCol : $childCol;
		$modifiedAtom= $parentOrChild == 'parent' ? $parentAtom : $childAtom;
		$stableCol   = $parentOrChild == 'parent' ? $childCol : $parentCol;
		$stableAtom  = $parentOrChild == 'parent' ? $childAtom: $parentAtom;

		$tableEsc = addslashes($table);
		$modifiedColEsc = addslashes($modifiedCol);
		$stableColEsc = addslashes($stableCol);
		$modifiedAtomEsc = addslashes($modifiedAtom);
		$stableAtomEsc = addslashes($stableAtom);
		$originalAtomEsc = addslashes($originalAtom);

		// only if the stable column is unique, we do an update
		// TODO: maybe we can do updates also in non-unique columns
		if ($tableColumnInfo[$table][$stableCol]['unique']){ // note: this uniqueness is not set as an SQL table attribute
			
			$this->Exe("UPDATE `$tableEsc` SET `$modifiedColEsc`='$modifiedAtomEsc' WHERE `$stableColEsc`='$stableAtomEsc'");
		
		} else { 
			/* 
			if ($tableColumnInfo[$table][$modifiedCol]['unique']){
				// todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an	insert, not an update.
				$query = "UPDATE `$tableEsc` SET `$stableColEsc`='$stableAtomEsc' WHERE `$modifiedColEsc`='$modifiedAtomEsc'";
				emitLog ($query);
				queryDb($query);
			} else { 
			*/
			
			// delete only if there was an $originalAtom
			if ($originalAtom!=''){ 
				$this->Exe("DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc'");
			}		
		
			$this->Exe("INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')");
		
		}

		// ensure that the $modifiedAtom is in the concept tables for $modifiedConcept
		$childConcept = $isFlipped ? $relationTableInfo[$rel]['srcConcept'] : $relationTableInfo[$rel]['tgtConcept'];
		$parentConcept =  $isFlipped ? $relationTableInfo[$rel]['tgtConcept'] : $relationTableInfo[$rel]['srcConcept'];
		$modifiedConcept = $parentOrChild == 'parent' ? $parentConcept : $childConcept;
		// emitLog ("adding to concept tables: $modifiedAtom : $modifiedConcept");
		
		$this->addAtomToConcept($modifiedAtom, $modifiedConcept);
		// TODO: errors here are not reported correctly
	}
	
	// TODO: make private function
	public function editDelete($rel, $isFlipped, $parentAtom, $childAtom)
	{ 
		global $relationTableInfo;
		global $tableColumnInfo;

		$srcAtom = $isFlipped ? $childAtom : $parentAtom;
		$tgtAtom = $isFlipped ? $parentAtom : $childAtom;

		$table = $relationTableInfo[$rel]['table'];
		$srcCol = $relationTableInfo[$rel]['srcCol'];
		$tgtCol = $relationTableInfo[$rel]['tgtCol'];

		$tableEsc = addslashes($table);
		$srcAtomEsc = addslashes($srcAtom);
		$tgtAtomEsc = addslashes($tgtAtom);
		$srcColEsc = addslashes($srcCol);
		$tgtColEsc = addslashes($tgtCol);

		if ($tableColumnInfo[$table][$tgtCol]['null']){ // note: this uniqueness is not set as an SQL table attribute
			$this->Exe ("UPDATE `$tableEsc` SET `$tgtColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'");
		} else {
			$this->Exe ("DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'");
		}

	}
	
	// Remove all occurrences of $atom in the database (all concept tables and all relations)
	// In tables where the atom may not be null, the entire row is removed. 
	// TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't
	//       happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
	function deleteAtom($atom, $concept) {
		global $tableColumnInfo;

		foreach ($tableColumnInfo as $table => $tableInfo){
			foreach ($tableInfo as $column => $fieldInfo) {
				// TODO: could be optimized by doing one query per table. But deleting per column yields the same result.
				//       (unlike adding)
				if ($fieldInfo['concept']==$concept) {
					$tableEsc = addslashes($table);
					$columnEsc = addslashes($column);
					$atomEsc = addslashes($atom);

					if ($fieldInfo['null'])  // if the field can be null, we set all occurrences to null
						$this->Exe("UPDATE `$tableEsc` SET `$columnEsc`=NULL WHERE `$columnEsc`='$atomEsc'");
					else // otherwise, we remove the entire row for each occurrence
						$this->Exe("DELETE FROM `$tableEsc` WHERE `$columnEsc` = '$atomEsc'");
				}
			}
		}
	}
	
	// return the most recent modification time for the database (only Ampersand edit operations are recorded)
	public function getLatestUpdateTime(){	
		try {
			$timestampRow = $this->Exe("SELECT MAX(`Seconds`) FROM `__History__`");
		} catch (Exception $e) {
			return '0';
		}
		
		return $timestampRow[0][0];
	
	}
	
	// TODO: onderstaande timestamp generatie opschonen. Kan de database ook zelf doen, bij insert/update/delete
	private function setLatestUpdateTime(){
		
		$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
		$microseconds = substr($time[0], 2,6); // we drop the leading "0." and trailing "00"  from the microseconds
		$seconds =$time[1].$microseconds;  
		$date = date("j-M-Y, H:i:s.").$microseconds; 
		$this->Exe("INSERT INTO `__History__` (`Seconds`,`Date`) VALUES ('$seconds','$date')");
		
	}
}


?>
