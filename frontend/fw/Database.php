<?php
error_reporting(E_ALL ^ E_DEPRECATED ^ E_NOTICE);
ini_set("display_errors", 1); // TODO: error handling instellen

require_once (__DIR__ . '/../localSettings.php');

class Database
{	
	private $dblink;
	private $dbname;
	private $transaction;
	
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
		ErrorHandling::addLog('QUERY: ' . $query);

		if (mysql_error()) throw new Exception(mysql_error(). " in query:" . $query);

		if ($result === false) return false;
		if ($result === true) return true;

		$resultarray = array();
		while(($resultarray[] = mysql_fetch_array($result)) || array_pop($resultarray)); // or mysql_fetch_assoc()??
		return $resultarray;
		
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
	
	// TODO: make private function
	public function addAtomToConcept($newAtom, $concept) // Insert 'newAtom' only if it does not yet exist...
	{
		// this function is under control of transaction check!
		if (!isset($this->transaction)) $this->startTransaction();
		
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
			
			if (!in_array(strtolower($newAtom), array_map('strtolower', $existingAtoms))) { // in_array is case sensitive ("true" != "TRUE"), but Mysql is case insensitive for Primary keys. Therefore first to lowercase. 
				$allConceptColsEsc = '`'.implode('`, `', $conceptCols).'`';
				$newAtomsEsc = array_fill(0, count($conceptCols), $newAtomEsc);
				$allValuesEsc = "'".implode("', '", $newAtomsEsc)."'";

				$this->Exe("INSERT INTO `$conceptTableEsc` ($allConceptColsEsc) VALUES ($allValuesEsc)");
			}
			
		}
		
		return $newAtomEsc;
	}
	
	// NOTE: if $originalAtom == '', editUpdate means insert for n-ary relations
	public function editUpdate($rel, $isFlipped, $parentAtom, $parentConcept, $childAtom, $childConcept, $parentOrChild, $originalAtom)
	{
		// this function is under control of transaction check!
		if (!isset($this->transaction)) $this->startTransaction();
		
		global $relationTableInfo;
		global $tableColumnInfo; 
		
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
			if ($originalAtom != ''){ 
				$this->Exe("DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc'");
			}		
			
			$this->Exe("INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')");
		
		}

		// ensure that the $modifiedAtom is in the concept tables for $modifiedConcept
		$childConcept = $isFlipped ? $relationTableInfo[$rel]['srcConcept'] : $relationTableInfo[$rel]['tgtConcept'];
		$parentConcept =  $isFlipped ? $relationTableInfo[$rel]['tgtConcept'] : $relationTableInfo[$rel]['srcConcept'];
		$modifiedConcept = $parentOrChild == 'parent' ? $parentConcept : $childConcept;
		
		$this->addAtomToConcept($modifiedAtom, $modifiedConcept);
		// TODO: errors here are not reported correctly
	}
	
	public function editDelete($rel, $isFlipped, $parentAtom, $parentConcept, $childAtom, $childConcept)
	{
		// this function is under control of transaction check!
		if (!isset($this->transaction)) $this->startTransaction();
		
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
			$this->Exe ("UPDATE `$tableEsc` SET `$tgtColEsc`= NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'");
		} else {
			$this->Exe ("DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc'");
		}

	}
	
	// Remove all occurrences of $atom in the database (all concept tables and all relations)
	// In tables where the atom may not be null, the entire row is removed. 
	// TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't
	//       happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
	function deleteAtom($atom, $concept) {
		
		// this function is under control of transaction check!
		if (!isset($this->transaction)) $this->startTransaction();
		
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
	
	private function startTransaction(){
		
		$this->Exe("START TRANSACTION"); // start database transaction
		$this->transaction = rand();
		
	}
	
	// TODO: changes to private function, currently also used in Session class for session atom initiation.
	public function commitTransaction(){
		$this->Exe("COMMIT"); // start database transaction
		unset($this->transaction);
		
	}
	
	public function rollbackTransaction(){
		$this->Exe("ROLLBACK"); // rollback database transaction
		unset($this->transaction);
	
	}
	
	public function closeTransaction($tryCommit = true){
		$session = Session::singleton();
		
		$invariantRulesHold = RuleEngine::checkInvariantRules($session->interface->interfaceInvariantConjunctNames); // only invariants rules that might be violated after edits in this interface are checked.
		
		if ($invariantRulesHold && $tryCommit) {
			$this->setLatestUpdateTime();
			$this->commitTransaction();
			
			ErrorHandling::addSuccess('Changes committed to the database');
			
			return true;
		} else {
			$this->rollbackTransaction();
			
			return false;
		}
		unset($this->transaction);
		
	}
}


?>
