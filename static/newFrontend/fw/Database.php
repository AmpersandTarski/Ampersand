<?php
error_reporting(E_ALL ^ E_DEPRECATED ^ E_NOTICE);
ini_set("display_errors", 1); // TODO: error handling instellen

require_once (__DIR__ . '/../localSettings.php');

// TODO: change to mysqli
class Database
{	
	private $db_link;
	
	private $db_host;
	private $db_user;
	private $db_pass;
	private $db_name;
	
	private $transaction;
	private $affectedConcepts = array(); // array with all affected Concepts during a transaction.
	private $affectedRelations = array(); // array with all affected Relations during a transaction (must be fullRelationSignature! i.e. rel_<relationName>_<srcConcept>_<tgtConcept>).
	
	private static $_instance = null;
	
	// Prevent any outside instantiation of this object
	private function __construct()
	{
		global $DB_host, $DB_user, $DB_pass, $DB_name; // from config.php
		$this->db_host = $DB_host;
		$this->db_user = $DB_user;
		$this->db_pass = $DB_pass;
		$this->db_name = $DB_name;
		
		// Connect to mysql
		$this->connect();
		
		// Select DB
		try{
			$this->selectDB();
		}catch (Exception $e){
			Notifications::addLog($e->getMessage(), 'DATABASE');
			$this->createDB();
			$this->selectDB();
			$this->installDB();
		}
	}
	
	// Prevent any copy of this object
	private function __clone(){
		
	}
	
	public static function singleton(){
		if(!is_object (self::$_instance) ) self::$_instance = new Database();
		return self::$_instance;
	}
	
	public function resetDatabase(){
		$this->dropDB();
		$this->createDB();
		$this->selectDB();
		$this->installDB();	
	}
	
	private function connect(){
		$this->db_link = mysql_connect($this->db_host, $this->db_user, $this->db_pass);
		if (mysql_error()) throw new Exception(mysql_error(), 500);
		
	}
	
	private function createDB(){
		$this->Exe("CREATE DATABASE $this->db_name DEFAULT CHARACTER SET UTF8");
		if (mysql_error()) throw new Exception(mysql_error(), 500);
	}
	private function dropDB(){
		$this->Exe("DROP DATABASE $this->db_name");
		if (mysql_error()) throw new Exception(mysql_error(), 500);
	}
	
	private function selectDB(){
		mysql_select_db($this->db_name, $this->db_link);
		if (mysql_error()) throw new Exception(mysql_error(), 500);
		
		// Set sql_mode to ANSI
		$this->Exe("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
	}
	
	private function installDB(){
		global $allDBstructQueries; // from Generics.php
		global $allDefPopQueries; // from Generics.php
		
		Notifications::addLog('========= INSTALLER ==========');
		
		$this->startTransaction();
		Notifications::addLog('---------- DB structure queries ------------');
		foreach($allDBstructQueries as $query){
			$this->Exe($query);
			
		}
		Notifications::addLog('---------- DB population queries -----------');
		foreach($allDefPopQueries as $query){
			$this->Exe($query);
		}
		Notifications::addLog('========= END OF INSTALLER ==========');
		
		$this->closeTransaction('Database reset to initial state', true);
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
		$result = mysql_query($query,$this->db_link);
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
		return mysql_error($this->db_link);
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
				if(!$this->atomExists($newAtom, $concept)){ 
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
	
	public function atomExists($atomId, $concept){
		$tableInfo = Concept::getConceptTableInfo($concept);
		$table = $tableInfo[0]['table'];
		$conceptCol = $tableInfo[0]['cols'][0];
		
		$query = "/* Check if atom exists */ SELECT $conceptCol FROM $table WHERE $conceptCol = '$atomId'";
		$result = $this->Exe($query);
		
		if(empty($result)) return false;
		else return true;
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
	
			// ensure that the $modifiedAtom is in the concept tables for $modifiedConcept						
			$this->addAtomToConcept($modifiedAtom, $modifiedConcept);
			
			$tableStableColumnInfo = Relation::getTableColumnInfo($table, $stableCol);
			$tableModifiedColumnInfo = Relation::getTableColumnInfo($table, $modifiedCol);
			// If the stable column is unique, we do an update // TODO: maybe we can do updates also in non-unique columns
			if ($tableStableColumnInfo['unique']){
				
				$this->Exe("UPDATE `$table` SET `$modifiedCol`='$modifiedAtom' WHERE `$stableCol`='$stableAtom'");
			
			// Elseif the modified column is unique, we do an update
			}elseif ($tableModifiedColumnInfo['unique']){
				
				$this->Exe("UPDATE `$table` SET `$stableCol`='$stableAtom' WHERE `$modifiedCol`='$modifiedAtom'");
			
			// Otherwise, binary table, so perform a insert.
			}else{
				$this->Exe("INSERT INTO `$table` (`$stableCol`, `$modifiedCol`) VALUES ('$stableAtom', '$modifiedAtom')");
				
				// If $originalAtom is provided, delete tuple rel(stableAtom, originalAtom)
				if (!is_null($originalAtom)) $this->Exe("DELETE FROM `$table` WHERE `$stableCol`='$stableAtom' AND `$modifiedCol`='$originalAtom'");			
			}
			
			if(!in_array($fullRelationSignature, $this->affectedRelations)) $this->affectedRelations[] = $fullRelationSignature; // add $fullRelationSignature to affected relations. Needed for conjunct evaluation.
	
			
			
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
	public function editDelete($rel, $isFlipped, $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept){
		Notifications::addLog("editDelete($rel, " . var_export($isFlipped, true) . ", $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept)");
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
					
			$tableStableColumnInfo = Relation::getTableColumnInfo($table, $stableCol);
			$tableModifiedColumnInfo = Relation::getTableColumnInfo($table, $modifiedCol);
			
			// If the modifiedCol can be set to null, we do an update
			if ($tableModifiedColumnInfo['null']){
				$this->Exe("UPDATE `$table` SET `$modifiedCol`= NULL WHERE `$stableCol`='$stableAtom' AND `$modifiedCol`='$modifiedAtom'");
			
			// Elseif the stableCol can be set to null, we do an update
			}elseif ($tableStableColumnInfo['null']){
				
				$this->Exe("UPDATE `$table` SET `$stableCol`= NULL WHERE `$stableCol`='$stableAtom' AND `$modifiedCol`='$modifiedAtom'");
			
			// Otherwise, binary table, so perform a delete
			}else{
				$this->Exe("DELETE FROM `$table` WHERE `$stableCol`='$stableAtom' AND `$modifiedCol`='$modifiedAtom'");
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
		$this->affectedConcepts = array(); $this->affectedRelations = array();
		
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
