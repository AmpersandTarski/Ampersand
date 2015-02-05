<?php
error_reporting(E_ALL ^ E_DEPRECATED ^ E_NOTICE);
ini_set("display_errors", 1); // TODO: error handling instellen

require_once (__DIR__ . '/../localSettings.php');

// TODO: change to mysqli
class Database
{	
	private $dblink;
	private $dbname;
	private $transaction;
	private $affectedConcepts = array(); // array with all affected Concepts during a transaction.
	private $affectedRelations = array(); // array with all affected Relations during a transaction (must be fullRelationSignature! i.e. rel_<relationName>_<srcConcept>_<tgtConcept>).
	
	private static $_instance = null;
	
	// Prevent any outside instantiation of this object
	private function __construct()
	{
		global $DB_host, $DB_user, $DB_pass, $DB_name; // from config.php
		$this->dbname = $DB_name;
		
		$this->dblink = mysql_connect($DB_host, $DB_user, $DB_pass);
		if (mysql_error()) throw new Exception(mysql_error(), 500);
		
		mysql_select_db($this->dbname, $this->dblink);
		if (mysql_error()) throw new Exception(mysql_error(), 500);
	}
	
	// Prevent any copy of this object
	private function __clone(){
		
	}
	
	public static function singleton(){
		if(!is_object (self::$_instance) ) self::$_instance = new Database();
		return self::$_instance;
	}
	
	/*
	 *  TODO: 
	 *  Create private equivalent that is used by addAtomToConcept(), editUpdate(), editDelete() and deleteAtom() functions, to perform any INSERT, UPDATE, DELETE
	 *  The public version should be allowed to only do SELECT queries.
	 *  This is needed to prevent Extensions or ExecEngine functions to go around the functions in this class that keep track of the affectedConjuncts.
	 */
	public function Exe($query){
		$query = str_replace('_SESSION', session_id(), $query); // Replace _SESSION var with current session id.
		$query = str_replace('__MYSESSION__', session_id(), $query); // Replace __MYSESSION__ var with current session id.
		
		//TODO: add mysql_real_escape_string() on query and remove addslashes() elsewhere
		$result = mysql_query($query,$this->dblink);
		Notifications::addLog($query, 'QUERY');

		if (mysql_error()) throw new Exception(mysql_error(). " in query:" . $query, 500);

		if ($result === false) return false;
		if ($result === true) return true;

		$resultarray = array();
		while(($resultarray[] = mysql_fetch_array($result)) || array_pop($resultarray)); // or mysql_fetch_assoc()??
		return $resultarray;
		
	}

	public static function Escape($item){
		if (is_object($item) OR is_array($item)) die("Escape item is not a variable but an object or array.");
		return mysql_escape_string($item);
	}
	
	public function error(){
		return mysql_error($this->dblink);
	}

// =============================== CHANGES TO DATABASE ===========================================================
	
	/* Insert $newAtom into $concept
	 * The function checks if the atom is already in the database.
	 */
	// TODO: make private function
	public function addAtomToConcept($newAtom, $concept){
		Notifications::addLog("addAtomToConcept($newAtom, $concept)");
		try{
			// this function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			foreach (Concept::getConceptTableInfo($concept) as $conceptTableInfo) {  
				
				// Get table properties
				$conceptTable = $conceptTableInfo['table']; 
				$conceptCols = $conceptTableInfo['cols'];   // We insert the new atom in each of them.
	
				// If $newAtom is not in $concept
				if(!Concept::isAtomInConcept($newAtom, $concept)) { 
					// Create query string: `<col1>`, `<col2>`, etc
					$allConceptCols = '`' . implode('`, `', $conceptCols) . '`';
					
					// Create query string: '<newAtom>', '<newAtom', etc
					$newAtomsArray = array_fill(0, count($conceptCols), $newAtom);
					$allValues = "'".implode("', '", $newAtomsArray)."'";
	
					$this->Exe("INSERT INTO `$conceptTable` ($allConceptCols) VALUES ($allValues)");
					
					if(!in_array($concept, $this->affectedConcepts)) $this->affectedConcepts[] = $concept; // add $concept to affected concepts. Needed for conjunct evaluation.
					
					Notifications::addLog("Atom $newAtom added into concept $concept");
				}else{
					Notifications::addLog("Atom $newAtom already in concept $concept");
				}
				
			}
		
			return $newAtom;
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}
	}
	
	/* How to use editUpdate:
	 * r :: A * B
	 * editUpdate(r, false, a1, A, b1, B);
	 * editUpdate(r, true, b1, B, a1, A);
	 * 
	 * The $stableAtom and $stableConcept are used to identify which row must be updated.
	 * 
	 * NOTE: if $originalAtom is provided, this means that tuple rel(stableAtom, originalAtom) is replaced by rel(stableAtom, modifiedAtom).
	 */
	public function editUpdate($rel, $isFlipped, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $originalAtom = null){
		Notifications::addLog("editUpdate($rel, " . var_export($isFlipped, true) . ", $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $originalAtom)");
		try{			
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			// Check if $rel, $srcConcept, $tgtConcept is a combination
			$srcConcept = $isFlipped ? $modifiedConcept : $stableConcept;
			$tgtConcept = $isFlipped ? $stableConcept : $modifiedConcept;
			$fullRelationSignature = Relation::isCombination($rel, $srcConcept, $tgtConcept);
			
			// Get table properties
			$table = Relation::getTable($fullRelationSignature);
			$srcCol = Relation::getSrcCol($fullRelationSignature);
			$tgtCol = Relation::getTgtCol($fullRelationSignature);
			
			// Determine which Col must be editited and which must be used in the WHERE statement
			$stableCol = $isFlipped ? $tgtCol : $srcCol;
			$modifiedCol =  $isFlipped ? $srcCol : $tgtCol;
	
			$tableColumnInfo = Relation::getTableColumnInfo($table, $stableCol);
			// Only if the stable column is unique, we do an update // TODO: maybe we can do updates also in non-unique columns
			if ($tableColumnInfo['unique']){ // note: this uniqueness is not set as an SQL table attribute
				
				$this->Exe("UPDATE `$table` SET `$modifiedCol`='$modifiedAtom' WHERE `$stableCol`='$stableAtom'");
			
			// Otherwise, binary table, so perform a insert.
			}else{
				$this->Exe("INSERT INTO `$table` (`$stableCol`, `$modifiedCol`) VALUES ('$stableAtom', '$modifiedAtom')");
				
				// If $originalAtom is provided, delete tuple rel(stableAtom, originalAtom)
				if (!is_null($originalAtom)) $this->Exe("DELETE FROM `$table` WHERE `$stableCol`='$stableAtom' AND `$modifiedCol`='$originalAtom'");			
			}
			
			if(!in_array($fullRelationSignature, $this->affectedRelations)) $this->affectedRelations[] = $fullRelationSignature; // add $fullRelationSignature to affected relations. Needed for conjunct evaluation.
	
			// ensure that the $modifiedAtom is in the concept tables for $modifiedConcept						
			$this->addAtomToConcept($modifiedAtom, $modifiedConcept);
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}
	}
	
	/* How to use editDelete:
	 * r :: A * B
	 * editDelete(r, false, a1, A, b1, B); 
	 * editDelete(r, true, b1, B, a1, A);
	 */
	public function editDelete($rel, $isFlipped, $leftAtom, $leftConcept, $rightAtom, $rightConcept){
		Notifications::addLog("editDelete($rel, " . var_export($isFlipped, true) . ", $leftAtom, $leftConcept, $rightAtom, $rightConcept)");
		try{			
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			// Check if $rel, $srcConcept, $tgtConcept is a combination
			$srcConcept = $isFlipped ? $rightConcept : $leftConcept;
			$tgtConcept = $isFlipped ? $leftConcept : $rightConcept;
			$fullRelationSignature = Relation::isCombination($rel, $srcConcept, $tgtConcept);
			
			// Determine srcAtom and tgtAtom
			$srcAtom = $isFlipped ? $rightAtom : $leftAtom;
			$tgtAtom = $isFlipped ? $leftAtom : $rightAtom;

			// Get table properties
			$table = Relation::getTable($fullRelationSignature);
			$srcCol = Relation::getSrcCol($fullRelationSignature);
			$tgtCol = Relation::getTgtCol($fullRelationSignature);
			
			$tableColumnInfo = Relation::getTableColumnInfo($table, $tgtCol);
			// If the tgtCol can be set to null, we do an update
			if ($tableColumnInfo['null']){ // note: this uniqueness is not set as an SQL table attribute
				$this->Exe("UPDATE `$table` SET `$tgtCol`= NULL WHERE `$srcCol`='$srcAtom' AND `$tgtCol`='$tgtAtom'");
			// Otherwise, binary table, so perform a delete
			} else {
				$this->Exe("DELETE FROM `$table` WHERE `$srcCol`='$srcAtom' AND `$tgtCol`='$tgtAtom'");
			}
			
			if(!in_array($fullRelationSignature, $this->affectedRelations)) $this->affectedRelations[] = $fullRelationSignature; // add $fullRelationSignature to affected relations. Needed for conjunct evaluation.
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}

	}
	
	/* Remove all occurrences of $atom in the database (all concept tables and all relation tables)
	 * In tables where the atom may not be null, the entire row is removed.
	 * TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
	 */    
	function deleteAtom($atom, $concept){
		Notifications::addLog("deleteAtom($atom, $concept)");
		try{
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			global $tableColumnInfo;
	
			foreach ($tableColumnInfo as $table => $tableInfo){
				foreach ($tableInfo as $column => $fieldInfo) {
					// TODO: could be optimized by doing one query per table. But deleting per column yields the same result (unlike adding)
					if ($fieldInfo['concept'] == $concept) {
						
						// If the field can be null, we set all occurrences to null
						if ($fieldInfo['null']) $this->Exe("UPDATE `$table` SET `$column`=NULL WHERE `$column`='$atom'");
						
						// Otherwise, we remove the entire row for each occurrence
						else 
							$this->Exe("DELETE FROM `$table` WHERE `$column` = '$atom'");
					}
				}
			}
			
			if(!in_array($concept, $this->affectedConcepts)) $this->affectedConcepts[] = $concept; // add $concept to affected concepts. Needed for conjunct evaluation.
			
			Notifications::addLog("Atom $atom (and all related links) deleted in database");
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}
	}
	
// =============================== TRANSACTIONS ===========================================================
	
	private function startTransaction(){
		Notifications::addLog('========================= STARTING TRANSACTION =========================');
		$this->Exe("START TRANSACTION"); // start database transaction
		$this->transaction = rand();
		
	}
	
	// TODO: make private function, now also used by in Session class for session atom initiation
	public function commitTransaction(){
		Notifications::addLog('------------------------- COMMIT -------------------------');
		$this->setLatestUpdateTime();
		$this->Exe("COMMIT"); // commit database transaction
		unset($this->transaction);
	}
	
	private function rollbackTransaction(){
		Notifications::addLog('------------------------- ROLLBACK -------------------------');
		$this->Exe("ROLLBACK"); // rollback database transaction
		unset($this->transaction);
	}
	
	/*
	 * $checkAllInvariantConjuncts 
	 * 		true: checkAllInvariantConjuncts, 
	 * 		false: check only InvariantRules that are relevant for the interface of the current session.
	 * 		default: true
	 */
	public function closeTransaction($succesMessage = 'Updated', $checkAllConjucts = true){
		$session = Session::singleton();
		
		Notifications::addLog('========================= CLOSING TRANSACTION =========================');
		
		foreach ((array)$GLOBALS['hooks']['before_Database_transaction_checkInvariantRules'] as $hook) call_user_func($hook);
		
		if($checkAllConjucts){
			Notifications::addLog("Check all conjuncts");
			
			// Evaluate all invariant conjuncts. Conjuncts are cached.
			$invariantRulesHold = RuleEngine::checkInvariantRules();
			
			// Evaluate all signal conjuncts. Conjuncts are cached
			RuleEngine::checkProcessRules();
			
		}else{
			Notifications::addLog("Check all affected conjuncts");
			
			// Evaluate all affected invariant conjuncts. Conjuncts are cached.
			$invariantRulesHold = RuleEngine::checkInvariantRules(RuleEngine::getAffectedInvConjuncts($this->affectedConcepts, $this->affectedRelations), true);
			
			// Evaluate all affected signal conjuncts. Conjuncts are cached
			RuleEngine::checkConjuncts(RuleEngine::getAffectedSigConjuncts($this->affectedConcepts, $this->affectedRelations), true);
			
			// Check only those process rules that are relevant for the current role
			if(isset($session->role->id)) RuleEngine::checkProcessRules($session->role->id);
		}
		
		unset($this->affectedConcepts, $this->affectedRelations);
		
		if($invariantRulesHold){
			$this->commitTransaction(); // commit database transaction
			Notifications::addSuccess($succesMessage);
			return true;
		}else{
			$this->rollbackTransaction(); // rollback database transaction
			return false;
		}
		
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
