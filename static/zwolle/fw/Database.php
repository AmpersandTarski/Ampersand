<?php

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
		
		// Connect to MYSQL database
		$this->db_link = new mysqli($this->db_host, $this->db_user, $this->db_pass);
		if ($this->db_link->connect_error) throw new Exception($this->db_link->connect_error, 500);
		
		// Set sql_mode to ANSI
		$this->db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
		
		// Select DB
		try{
			$this->selectDB();
		}catch (Exception $e){
			Notifications::addLog($e->getMessage(), 'DATABASE');
			throw $e;
		}
	}
	
	// Prevent any copy of this object
	private function __clone(){
		
	}
	
	public static function singleton(){
		if(!is_object (self::$_instance) ) self::$_instance = new Database();
		return self::$_instance;
	}
	
	public static function createDB(){
		global $DB_host, $DB_user, $DB_pass, $DB_name; // from config.php
		
		// Connect to MYSQL database
		$db_link = new mysqli($DB_host, $DB_user, $DB_pass);
		if ($db_link->connect_error) throw new Exception($db_link->connect_error, 500);
		
		// Set sql_mode to ANSI
		$db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
		
		$db_link->query("DROP DATABASE $DB_name");
		
		$db_link->query("CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8");
		if ($db_link->error) throw new Exception($db_link->error, 500);
			
	}
	
	private function selectDB(){
		$this->db_link->select_db($this->db_name);
		
		if ($this->db_link->error) throw new Exception($this->db_link->error . '. Please <a href="#/installer" class="alert-link">Reinstall database</a>', 500);
		
	}
	
	public function reinstallDB(){
		global $allDBstructQueries; // from Generics.php
		global $allDefPopQueries; // from Generics.php
		
		Notifications::addLog('========= INSTALLER ==========', 'INSTALLER');
		
		Notifications::addLog('---------- DB structure queries ------------', 'INSTALLER');
		foreach($allDBstructQueries as $query){
			$this->Exe($query);
			
		}
		
		if(CHECK_DEF_POP) $this->startTransaction(); // default must be true: when CHECK_DEF_POP is undefined, this is true
		
		Notifications::addLog('---------- DB population queries -----------', 'INSTALLER');
		foreach($allDefPopQueries as $query){
			$this->Exe($query);
		}
		Notifications::addLog('========= END OF INSTALLER ==========', 'INSTALLER');
		
		$this->closeTransaction('Database successfully reinstalled', true, true, false);
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
		
		$result = $this->db_link->query($query);
		Notifications::addLog($query, 'QUERY');

		if ($this->db_link->error) throw new Exception($this->db_link->error . " in query:" . $query, 500);

		if ($result === false) return false;
		elseif ($result === true) return true;
		
		$arr = array();
		while($row = mysqli_fetch_array($result)){
			$arr[] = $row;
		}
		return $arr;
		
	}
	
	/*
	 * See:
	 * - http://php.net/manual/en/language.types.string.php#language.types.string.parsing
	 * - http://php.net/manual/en/mysqli.real-escape-string.php
	 * 
	 */
	public function escape($param){
		return $this->db_link->real_escape_string($param);
	}
	

// =============================== CHANGES TO DATABASE ===========================================================
	
	/* Insert $newAtom into $concept
	 * The function checks if the atom is already in the database.
	 */
	// TODO: make private function
	public function addAtomToConcept($newAtom, $concept){
		Notifications::addLog("addAtomToConcept($newAtom, $concept)", 'DATABASE');
		try{
			// this function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();

			$newAtom = $this->typeConversion($newAtom, $concept);
			
			// If $newAtom is not in $concept
			if(!$this->atomExists($newAtom, $concept)){
				// Get table properties
				$conceptTableInfo = Concept::getConceptTableInfo($concept);
				$conceptTable = $conceptTableInfo['table'];
				$conceptCols = $conceptTableInfo['cols']; // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
				
				// Create query string: `<col1>`, `<col2>`, etc
				$allConceptCols = '`' . implode('`, `', $conceptCols) . '`';
				
				$newAtomEsc = $this->escape($newAtom);
				// Create query string: '<newAtom>', '<newAtom', etc
				$newAtomsArray = array_fill(0, count($conceptCols), $newAtomEsc);
				$allValues = "'".implode("', '", $newAtomsArray)."'";
				
				$this->Exe("INSERT INTO `$conceptTable` ($allConceptCols) VALUES ($allValues)");
				
				if(!in_array($concept, $this->affectedConcepts)){
					$this->affectedConcepts[] = $concept; // add $concept to affected concepts. Needed for conjunct evaluation.
					Notifications::addLog("Mark concept $concept as affected concept", 'CONJUNCTS');
				}
				
				Notifications::addLog("Atom $newAtom added into concept $concept", 'DATABASE');
			}else{
				Notifications::addLog("Atom $newAtom already in concept $concept", 'DATABASE');
			}
			
			return $newAtom;
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}
	}
	
	// Adding an atom[ConceptA] as member to ConceptB set. This can only be done when ConceptA and ConceptB are in the same classification tree.
	public function atomSetConcept($conceptA, $atom, $conceptB){
		Notifications::addLog("atomSetConcept($conceptA, $atom, $conceptB)", 'DATABASE');
		try{
			// Check if conceptA and conceptB are in the same classification tree
			if(!Concept::inSameClassificationTree($conceptA, $conceptB)) throw new Exception("Concepts $conceptA and $conceptB are not in the same classification tree", 500);
			
			// Check if atom is part of conceptA
			if(!$this->atomExists($atom, $conceptA)) throw new Exception("Atom $atom is not a member of $conceptA", 500);
			
			// this function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			// Get table info
			$conceptTableInfoB = Concept::getConceptTableInfo($conceptB);
			$conceptTableB = $conceptTableInfoB['table'];
			$conceptColsB = $conceptTableInfoB['cols']; // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$atomEsc = $this->escape($atom);
			$queryString = "\"" . implode("\" = '$atomEsc', \"", $conceptColsB) . "\" = '$atomEsc'";
			
			$conceptTableInfoA = Concept::getConceptTableInfo($conceptA);
			$conceptTableA = $conceptTableInfoA['table'];
			$anyConceptColForA = current($conceptTableInfoA['cols']);
			
			// Perform update
			$this->Exe("UPDATE \"$conceptTableB\" SET $queryString WHERE \"$anyConceptColForA\" = '$atomEsc'");
			
			if(!in_array($conceptB, $this->affectedConcepts)){
				$this->affectedConcepts[] = $conceptB; // add $concept to affected concepts. Needed for conjunct evaluation.
				Notifications::addLog("Mark concept $conceptB as affected concept", 'CONJUNCTS');
			}
			
			Notifications::addLog("Atom '$atom' added as member to concept '$conceptB'", 'DATABASE');
		
		}catch(Exception $e){
			throw $e;
		}
	}
	
	// Removing an atom as member from a Concept set. This can only be done when the concept is a specialization of another concept.
	public function atomClearConcept($concept, $atom){
		Notifications::addLog("atomSetConcept($conceptA, $atom, $conceptB)", 'DATABASE');
		try{
			// Check if concept is a specialization of another concept
			$conceptGeneralizations = Concept::getGeneralizations($concept);
			if(empty($conceptGeneralizations)) throw new Exception("Concept $concept has no generalizations, atom can therefore not be removed as member from this set", 500);
				
			// Check if atom is part of conceptA
			if(!$this->atomExists($atom, $concept)) throw new Exception("Atom $atom is not a member of $concept", 500);
				
			// this function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
				
			// Get col information for $concept and its specializations
			$cols = array();
			$conceptTableInfo = Concept::getConceptTableInfo($concept);
			$conceptTable = $conceptTableInfo['table'];
			$conceptCol = reset($conceptTableInfo['cols']);
			
			$cols[] = $conceptCol;
			foreach(Concept::getSpecializations($concept) as $specConcept){
				$conceptTableInfo = Concept::getConceptTableInfo($specConcept);
				$cols[] = reset($conceptTableInfo['cols']);
			}			
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$atomEsc = $this->escape($atom);
			$queryString = "\"" . implode("\" = NULL, \"", $cols) . "\" = NULL";
			
			$this->Exe("UPDATE \"$conceptTable\" SET $queryString WHERE \"$conceptCol\" = '$atomEsc'");
			
			if(!in_array($concept, $this->affectedConcepts)){
				$this->affectedConcepts[] = $concept; // add $concept to affected concepts. Needed for conjunct evaluation.
				Notifications::addLog("Mark concept $concept as affected concept", 'CONJUNCTS');
			}
			
			Notifications::addLog("Atom '$atom' removed as member from concept '$concept'", 'DATABASE');
			
		}catch(Exception $e){
			throw $e;
		}
	}
	
	/* 
	 * Note! Mysql is case insensitive for primary keys, e.g. atom 'True' ==  'TRUE'
	 */
	public function atomExists($atomId, $concept){
		$tableInfo = Concept::getConceptTableInfo($concept);
		$table = $tableInfo['table'];
		$conceptCol = $tableInfo['cols'][0];
		
		$newAtom = $this->typeConversion($atomId, $concept);
		
		$atomIdEsc = $this->escape($atomId);
		$query = "/* Check if atom exists */ SELECT `$conceptCol` FROM `$table` WHERE `$conceptCol` = '$atomIdEsc'";
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
		Notifications::addLog("editUpdate($rel, " . var_export($isFlipped, true) . ", $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept, $originalAtom)", 'DATABASE');
		try{			
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			$stableAtom = $this->typeConversion($stableAtom, $stableConcept);
			$modifiedAtom = $this->typeConversion($modifiedAtom, $modifiedConcept);
			
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
	
			// Ensure that the $modifiedAtom is in the concept tables for $modifiedConcept						
			$this->addAtomToConcept($modifiedAtom, $modifiedConcept);
			
			// Escape atoms for use in query
			$modifiedAtomEsc = $this->escape($modifiedAtom); 
			$stableAtomEsc = $this->escape($stableAtom);
			$originalAtomEsc = $this->escape($originalAtomEsc);
			
			// Get database table information
			$tableStableColumnInfo = Relation::getTableColumnInfo($table, $stableCol); // unique=true, null=true
			$tableModifiedColumnInfo = Relation::getTableColumnInfo($table, $modifiedCol); // unique=true, null=false
			
			// If the modified column is unique, we do an update
			// This is placed first, because of INJ constraints
			if ($tableModifiedColumnInfo['unique'] && !$tableStableColumnInfo['unique']){
			
				$this->Exe("UPDATE `$table` SET `$stableCol` = '$stableAtomEsc' WHERE `$modifiedCol` = '$modifiedAtomEsc'");
			}
			// Elseif the stable column is unique, we do an update // TODO: maybe we can do updates also in non-unique columns
			elseif ($tableStableColumnInfo['unique']){
				
				$this->Exe("UPDATE `$table` SET `$modifiedCol` = '$modifiedAtomEsc' WHERE `$stableCol` = '$stableAtomEsc'");
			// Otherwise, binary table, so perform a insert.
			}else{
				$this->Exe("INSERT INTO `$table` (`$stableCol`, `$modifiedCol`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')");
				
				// If $originalAtom is provided, delete tuple rel(stableAtom, originalAtom)
				if (!is_null($originalAtom)) $this->Exe("DELETE FROM `$table` WHERE `$stableCol` = '$stableAtomEsc' AND `$modifiedCol` = '$originalAtomEsc'");			
			}
			
			if(!in_array($fullRelationSignature, $this->affectedRelations)) {
				$this->affectedRelations[] = $fullRelationSignature; // add $fullRelationSignature to affected relations. Needed for conjunct evaluation.
				Notifications::addLog("Mark relation $fullRelationSignature as affected relation", 'CONJUNCTS');
			}
	
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
		Notifications::addLog("editDelete($rel, " . var_export($isFlipped, true) . ", $stableAtom, $stableConcept, $modifiedAtom, $modifiedConcept)", 'DATABASE');
		try{			
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			$stableAtom = $this->typeConversion($stableAtom, $stableConcept);
			$modifiedAtom = $this->typeConversion($modifiedAtom, $modifiedConcept);
			
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
			
			// Escape atoms for use in query
			$modifiedAtomEsc = $this->escape($modifiedAtom);
			$stableAtomEsc = $this->escape($stableAtom);
					
			// Get database table information
			$tableStableColumnInfo = Relation::getTableColumnInfo($table, $stableCol);
			$tableModifiedColumnInfo = Relation::getTableColumnInfo($table, $modifiedCol);
			
			// If the modifiedCol can be set to null, we do an update
			if ($tableModifiedColumnInfo['null']){
				$this->Exe("UPDATE `$table` SET `$modifiedCol` = NULL WHERE `$stableCol` = '$stableAtomEsc' AND `$modifiedCol` = '$modifiedAtomEsc'");
			
			// Elseif the stableCol can be set to null, we do an update
			}elseif ($tableStableColumnInfo['null']){
				
				$this->Exe("UPDATE `$table` SET `$stableCol` = NULL WHERE `$stableCol` = '$stableAtomEsc' AND `$modifiedCol` = '$modifiedAtomEsc'");
			
			// Otherwise, binary table, so perform a delete
			}else{
				$this->Exe("DELETE FROM `$table` WHERE `$stableCol` = '$stableAtomEsc' AND `$modifiedCol` = '$modifiedAtomEsc'");
			}
			
			if(!in_array($fullRelationSignature, $this->affectedRelations)){
				Notifications::addLog("Mark relation $fullRelationSignature as affected relation", 'CONJUNCTS');
				$this->affectedRelations[] = $fullRelationSignature; // add $fullRelationSignature to affected relations. Needed for conjunct evaluation.
			}
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}

	}
	
	/* Remove all occurrences of $atom in the database (all concept tables and all relation tables)
	 * In tables where the atom may not be null, the entire row is removed.
	 * TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
	 */    
	function deleteAtom($atom, $conceptName){
		Notifications::addLog("deleteAtom($atom, $concept)", 'DATABASE');
		try{
			$concept = new Concept($conceptName);
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			$atom = $this->typeConversion($atom, $concept->name);
			
			global $tableColumnInfo;
			
			$atomEsc = $this->escape($atom);
			foreach ($tableColumnInfo as $table => $tableInfo){
				foreach ($tableInfo as $column => $fieldInfo) {
					// TODO: could be optimized by doing one query per table. But deleting per column yields the same result (unlike adding)
					if ($fieldInfo['concept'] == $concept->name || $concept->hasGeneralization($fieldInfo['concept'])) {
						
						// If the field can be null, we set all occurrences to null
						if ($fieldInfo['null']) $this->Exe("UPDATE `$table` SET `$column` = NULL WHERE `$column` = '$atomEsc'");
						
						// Otherwise, we remove the entire row for each occurrence
						else 
							$this->Exe("DELETE FROM `$table` WHERE `$column` = '$atomEsc'");
					}
				}
			}
			
			if(!in_array($concept->name, $this->affectedConcepts)){
				Notifications::addLog("Mark concept $concept->name as affected concept", 'CONJUNCTS');
				$this->affectedConcepts[] = $concept->name; // add $concept to affected concepts. Needed for conjunct evaluation.
			}
			
			Notifications::addLog("Atom $atom (and all related links) deleted in database", 'DATABASE');
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addError($e->getMessage());
		}
	}
	
// =============================== TRANSACTIONS ===========================================================
	
	private function startTransaction(){
		Notifications::addLog('========================= STARTING TRANSACTION =========================', 'DATABASE');
		$this->Exe("START TRANSACTION"); // start database transaction
		$this->transaction = rand();
		
	}
	
	// TODO: make private function, now also used by in Session class for session atom initiation
	public function commitTransaction(){
		Notifications::addLog('------------------------- COMMIT -------------------------', 'DATABASE');
		$this->setLatestUpdateTime();
		$this->Exe("COMMIT"); // commit database transaction
		unset($this->transaction);
	}
	
	private function rollbackTransaction(){
		Notifications::addLog('------------------------- ROLLBACK -------------------------', 'DATABASE');
		$this->Exe("ROLLBACK"); // rollback database transaction
		unset($this->transaction);
	}
	
	/*
	 * $checkAllInvariantConjuncts 
	 * 		true: checkAllInvariantConjuncts, 
	 * 		false: check only InvariantRules that are relevant for the interface of the current session.
	 * 		default: true
	 */
	public function closeTransaction($succesMessage = 'Updated', $checkAllConjucts = true, $databaseCommit = false, $setNewContent = true){
		$session = Session::singleton();
		
		Notifications::addLog('========================= CLOSING TRANSACTION =========================', 'DATABASE');
		
		foreach ((array)$GLOBALS['hooks']['before_Database_transaction_checkInvariantRules'] as $hook) call_user_func($hook);
		
		if($checkAllConjucts){
			Notifications::addLog("Check all conjuncts", 'DATABASE');
			
			// Evaluate all invariant conjuncts. Conjuncts are cached.
			$invariantRulesHold = RuleEngine::checkInvariantRules();
			
			// Evaluate all signal conjuncts. Conjuncts are cached
			RuleEngine::checkProcessRules();
			
		}else{
			Notifications::addLog("Check all affected conjuncts", 'DATABASE');
			
			// Evaluate all affected invariant conjuncts. Conjuncts are cached.
			$invariantRulesHold = RuleEngine::checkInvariantRules(RuleEngine::getAffectedInvConjuncts($this->affectedConcepts, $this->affectedRelations), true);
			
			// Evaluate all affected signal conjuncts. Conjuncts are cached
			RuleEngine::checkConjuncts(RuleEngine::getAffectedSigConjuncts($this->affectedConcepts, $this->affectedRelations), true);
			
			// Check only those process rules that are relevant for the current role
			if(isset($session->role->id)) RuleEngine::checkProcessRules($session->role->id);
		}
		
		unset($this->affectedConcepts, $this->affectedRelations);
		$this->affectedConcepts = array(); $this->affectedRelations = array();
		
		if($setNewContent && isset($session->atom)) $session->atom->setNewContent($session->interface); // e.g. not needed in Atom::delete() function
		
		if($invariantRulesHold && $databaseCommit){
			$this->commitTransaction(); // commit database transaction
			Notifications::addSuccess($succesMessage);
		}elseif(defined(COMMIT_INV_VIOLATIONS) && COMMIT_INV_VIOLATIONS){
			$this->commitTransaction();
			Notifications::addError("Transaction committed with invariant violations");
		}elseif($invariantRulesHold){
			$this->rollbackTransaction(); // rollback database transaction
			Notifications::addInfo($succesMessage);
		}else{
			$this->rollbackTransaction(); // rollback database transaction
		}
		
		return $invariantRulesHold;
		
	}
	
	// TODO: onderstaande timestamp generatie opschonen. Kan de database ook zelf doen, bij insert/update/delete
	private function setLatestUpdateTime(){
	
		$time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
		$microseconds = substr($time[0], 2,6); // we drop the leading "0." and trailing "00"  from the microseconds
		$seconds =$time[1].$microseconds;
		$date = date("j-M-Y, H:i:s.").$microseconds;
		$this->Exe("INSERT INTO `__History__` (`Seconds`,`Date`) VALUES ('$seconds','$date')");
	
	}
	
	/*
	 * Conversion to MYSQL types
	 */
	public function typeConversion($value, $concept){
		switch(Concept::getTypeRepresentation($concept)){
			case "DATE" :
				$date = new DateTime($value);
				return $date->format('Y-m-d');
			case "DATETIME" :
				$datetime = new DateTime($value);
				return $datetime->format('Y-m-d H:i:s');
			case "INTEGER" :
				return (int) $value;
			case "BOOLEAN" :
				return (bool) $value;
			case "DECIMAL" :
				return (float) $value;
			default : 
				return $value;
		}
		
	}
}


?>
