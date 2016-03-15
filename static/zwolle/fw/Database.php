<?php

class Database {
    /**
     * Contains a connection to the mysql database
     */
	private $db_link;
	
	/**
	 * Host/server of mysql database
	 * @var string
	 */
	private $db_host;
	
	/**
	 * Username for mysql database
	 * @var string
	 */
	private $db_user;
	
	/**
	 * Password for mysql database
	 * @var string
	 */
	private $db_pass;
	
	/**
	 * Database name
	 * @var string
	 */
	private $db_name;
	
	/**
	 * Specifies transaction number (random int) when a transaction is started
	 * @var int
	 */
	private $transaction;
	
	/**
	 * Specifes if affected conjuncts should be registered
	 * @var boolean
	 */
	private $trackAffectedConjuncts = true;
	
	/**
	 * Contains all affected Concepts during a transaction
	 * @var Concept[]
	 */
	private $affectedConcepts = array();
	
	/**
	 * Contains all affected relations during a transaction
	 * Relations are specified with their 'fullRelationSignature' (i.e. 'rel_<relationName>_<srcConcept>_<tgtConcept>')
	 * @var array
	 */
	private $affectedRelations = array(); // 
	
	/**
	 * Specifies if invariant rules hold. Null if no transaction has occurred (yet)
	 * @var boolean|NULL
	 */
	private $invariantRulesHold = null;
	
	/**
	 * Specifies requested transaction type (i.e. 'feedback' or 'promise')
	 * @var string
	 */
	private $requestType = 'feedback';
	
	/**
	 * Contains reference to database instance (singleton pattern)
	 * @var Database
	 */
	private static $_instance = null;
	
	/**
	 * Constructor of database class
	 * Singleton pattern: private function to prevent any outside instantiantion of this object. 
	 * Use Database::singleton() instead
	 * 
	 * @throws Exception
	 */
	private function __construct(){
	    try{
    	    $this->db_host = Config::get('dbHost', 'mysqlDatabase');
    		$this->db_user = Config::get('dbUser', 'mysqlDatabase');
    		$this->db_pass = Config::get('dbPassword', 'mysqlDatabase');
    		$this->db_name = Config::get('dbName', 'mysqlDatabase');
    		
    		// Connect to MYSQL database
    		$this->db_link = new mysqli($this->db_host, $this->db_user, $this->db_pass);
    		
    		// Set sql_mode to ANSI
    		$this->db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
    		
    		// Select DB
			$this->selectDB();
			
		}catch (Exception $e){
		    // Convert mysqli_sql_exceptions into 500 errors
		    throw new Exception($e->getMessage(), 500);
		}
	}
	
	/**
	 * Singleton pattern: private function to prevent any copy/clone of database instance
	 * Use Database::singleton() instead
	 */
	private function __clone(){
		
	}
	
	/**
	 * Function to return the database instance
	 * @return Database
	 */
	public static function singleton(){
		if(!is_object (self::$_instance) ) self::$_instance = new Database();
		return self::$_instance;
	}
	
	/**
	 * Function to create new database. Drops database (and loose all data) if already exists
	 * @throws Exception
	 * @return void
	 */
	public static function createDB(){
	    try{
    		$DB_host = Config::get('dbHost', 'mysqlDatabase');
    		$DB_user = Config::get('dbUser', 'mysqlDatabase');
    		$DB_pass = Config::get('dbPassword', 'mysqlDatabase');
    		$DB_name = Config::get('dbName', 'mysqlDatabase');
    		
    		// Connect to MYSQL database
    		$db_link = new mysqli($DB_host, $DB_user, $DB_pass);
    		
    		// Set sql_mode to ANSI
    		$db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
    		
    		// Drop database
    		$db_link->query("DROP DATABASE $DB_name");
    		
    		// Create new database
    		$db_link->query("CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8");
		
		}catch (Exception $e){
		    // Convert mysqli_sql_exceptions into 500 errors
		    throw new Exception($e->getMessage(), 500);
		}
	}
	
	/**
	 * Function to select database
	 * @return void
	 */
	private function selectDB(){
		try{
	        $this->db_link->select_db($this->db_name);
		}catch(Exception $e){
	        Notifications::addLog($this->db_link->error . '. Please <a href="#/admin/installer" class="alert-link">Reinstall database</a>', 'DATABASE');
	    }
	}
	
	/**
	 * Function to reinstall database structure and load default population
	 * @return void
	 */
	public function reinstallDB(){
		$queries = file_get_contents(__DIR__ . '/../generics/mysql-installer.json');
		$queries = json_decode($queries, true);
		
		Notifications::addLog('========= INSTALLER ==========', 'INSTALLER');
		
		Notifications::addLog('---------- DB structure queries ------------', 'INSTALLER');
		foreach($queries['allDBstructQueries'] as $query){
			$this->Exe($query);
		}
		
		if(Config::get('checkDefaultPopulation', 'transactions')) $this->startTransaction();
		
		Notifications::addLog('---------- DB population queries -----------', 'INSTALLER');
		foreach($queries['allDefPopQueries'] as $query){
			$this->Exe($query);
		}
		
		Hooks::callHooks('postDatabaseReinstallDB', get_defined_vars());
		
		Notifications::addLog('========= END OF INSTALLER ==========', 'INSTALLER');
		
		$this->closeTransaction('Database successfully reinstalled', true, true);
		
		if (version_compare(PHP_VERSION, '5.6', '<')) {
		   Notifications::addError("Support for PHP version <= 5.5 will stop in the summer of 2016. Please upgrade to 5.6. Note! Ampersand framework does not support PHP 7 yet. You are on version: " . PHP_VERSION, 500);
		}
	}
	
	/**
	 * Execute query on database. Function replaces reserved words by their corresponding value (e.g. _SESSION)
	 * @param string $query
	 * @return boolean|array
	 * 
	 * TODO:
	 * Create private equivalent that is used by addAtomToConcept(), editUpdate(), editDelete() and deleteAtom() functions, to perform any INSERT, UPDATE, DELETE
	 * The public version should be allowed to only do SELECT queries.
	 * This is needed to prevent Extensions or ExecEngine functions to go around the functions in this class that keep track of the affectedConjuncts.
	 */
	public function Exe($query){
		$query = str_replace('_SESSION', session_id(), $query); // Replace _SESSION var with current session id.
		$query = str_replace('__MYSESSION__', session_id(), $query); // Replace __MYSESSION__ var with current session id.
		
		$result = $this->doQuery($query);
		Notifications::addLog($query, 'QUERY');

		if ($result === false) return false;
		elseif ($result === true) return true;
		
		$arr = array();
		while($row = mysqli_fetch_array($result)){
			$arr[] = $row;
		}
		return $arr;
		
	}
	
	/**
	 * Execute query on database.
	 * @param string $query
	 * @return mixed
	 * @throws Exception
	 */
	private function doQuery($query){
	    try{
	        return $this->db_link->query($query);
        }catch (Exception $e){
            // Convert mysqli_sql_exceptions into 500 errors
            throw new Exception("MYSQL error " . $e->getCode() . ": " . $e->getMessage() . " in query:" . $query, 500);
        }
	}
	
	/**
	 * Function to escape identifiers for use in database queries 
	 * @param string $escapestr
	 * @return NULL|string
	 * 
	 * http://php.net/manual/en/language.types.string.php#language.types.string.parsing
	 * http://php.net/manual/en/mysqli.real-escape-string.php
	 */
	public function escape($escapestr){
		if(is_null($escapestr)) return null;
		else return $this->db_link->real_escape_string($escapestr);
	}
	
	/**
	 * Check if atom exists in database
	 * Note! Mysql is case insensitive for primary keys, e.g. atom 'True' ==  'TRUE'
	 * @param Atom $atom
	 * @return boolean
	 */
	public function atomExists($atom){
	    $tableInfo = $atom->concept->getConceptTableInfo();
	    $table = $tableInfo['table'];
	    $conceptCol = $tableInfo['cols'][0];
	
	    $query = "/* Check if atom exists */ SELECT `$conceptCol` FROM `$table` WHERE `$conceptCol` = '{$atom->idEsc}'";
	    $result = $this->Exe($query);
	
	    if(empty($result)) return false;
	    else return true;
	}

/**************************************************************************************************
 *
 * Functions to perform database transactions
 *
 *************************************************************************************************/
	
	/**
	 * Add atom to database
	 * @param Atom $atom
	 * @return void
	 */
	public function addAtomToConcept($atom){
	    Notifications::addLog("addAtomToConcept({$atom->id}[{$atom->concept->name}])", 'DATABASE');
	    
		try{
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			// If $atomId is not in $concept
			if(!$this->atomExists($atom)){
			    			    
				// Get table properties
				$conceptTableInfo = $atom->concept->getConceptTableInfo();
				$conceptTable = $conceptTableInfo['table'];
				$conceptCols = $conceptTableInfo['cols']; // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
				
				// Create query string: `<col1>`, `<col2>`, etc
				$allConceptCols = '`' . implode('`, `', $conceptCols) . '`';
				
				
				// Create query string: '<newAtom>', '<newAtom', etc
				$atomIdsArray = array_fill(0, count($conceptCols), $atom->idEsc);
				$allValues = "'".implode("', '", $atomIdsArray)."'";
				
				foreach($conceptCols as $col) $str .= ", `$col` = '{$atom->idEsc}'";
				$duplicateStatement = substr($str, 1);
				
				$this->Exe("INSERT INTO `$conceptTable` ($allConceptCols) VALUES ($allValues)"
						  ." ON DUPLICATE KEY UPDATE $duplicateStatement");
				
				$this->addAffectedConcept($atom->concept); // add concept to affected concepts. Needed for conjunct evaluation.
				
				Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' added to database", 'DATABASE');
				
				Hooks::callHooks('postDatabaseAddAtomToConceptInsert', get_defined_vars());
			}else{
				Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' already exists in database", 'DATABASE');
				
				Hooks::callHooks('postDatabaseAddAtomToConceptSkip', get_defined_vars());
			}
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addErrorException($e);
		}
	}
	
	/**
	 * Adding an atom[ConceptA] as member to ConceptB set. 
	 * This can only be done when concept of atom (ConceptA) and ConceptB are in the same classification tree.
	 * @param Atom $atom
	 * @param string $conceptBName
	 * @throws Exception
	 * @return void
	 */
	public function atomSetConcept($atom, $conceptBName){
	    Notifications::addLog("atomSetConcept({$atom->id}[{$atom->concept->name}], $conceptBName)", 'DATABASE');
	    
	    $conceptB = Concept::getConcept($conceptBName);
		try{
		    // This function is under control of transaction check!
		    if (!isset($this->transaction)) $this->startTransaction();
		    
			// Check if conceptA and conceptB are in the same classification tree
			if(!$atom->concept->inSameClassificationTree($conceptB)) throw new Exception("Concepts '[{$atom->concept->name}]' and '[{$conceptB->name}]' are not in the same classification tree", 500);
			
			// Check if atom is part of conceptA
			if(!$this->atomExists($atom)) throw new Exception("Atom '{$atom->id}[{$atom->concept->name}]' does not exists", 500);
			
			// Get table info
			$conceptTableInfoB = $conceptB->getConceptTableInfo();
			$conceptTableB = $conceptTableInfoB['table'];
			$conceptColsB = $conceptTableInfoB['cols']; // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$queryString = "\"" . implode("\" = '{$atom->idEsc}', \"", $conceptColsB) . "\" = '{$atom->idEsc}'";
			
			$conceptTableInfoA = $atom->concept->getConceptTableInfo();
			$conceptTableA = $conceptTableInfoA['table'];
			$anyConceptColForA = current($conceptTableInfoA['cols']);
			
			// Perform update
			$this->Exe("UPDATE \"$conceptTableB\" SET $queryString WHERE \"$anyConceptColForA\" = '{$atom->idEsc}'");
			
			$this->addAffectedConcept($conceptB); // add concept to affected concepts. Needed for conjunct evaluation.
			
			Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' added as member to concept '[{$conceptB->name}]'", 'DATABASE');
		
		}catch(Exception $e){
			throw $e;
		}
	}
	
	/**
	 * Removing an atom as member from a Concept set. 
	 * This can only be done when the concept is a specialization of another concept.
	 * @param Atom $atom
	 * @throws Exception
	 * @return void
	 */
	public function atomClearConcept($atom){
		Notifications::addLog("atomClearConcept({$atom->id}[{$atom->concept->name}])", 'DATABASE');
		
		try{
		    // This function is under control of transaction check!
		    if (!isset($this->transaction)) $this->startTransaction();
		    
			// Check if concept is a specialization of another concept
			if(empty($atom->concept->getGeneralizations())) throw new Exception("Concept '[{$atom->concept->name}]' has no generalizations, atom can therefore not be removed as member from this set", 500);
				
			// Check if atom is part of conceptA
			if(!$this->atomExists($atom)) throw new Exception("Atom '{$atom->id}[{$atom->concept->name}]' does not exists", 500);
				
			// Get col information for concept and its specializations
			$cols = array();
			$conceptTableInfo = $atom->concept->getConceptTableInfo();
			$conceptTable = $conceptTableInfo['table'];
			$conceptCol = reset($conceptTableInfo['cols']);
			
			$cols[] = $conceptCol;
			foreach($atom->concept->getSpecializations() as $specConcept){
				$conceptTableInfo = $specConcept->getConceptTableInfo();
				$cols[] = reset($conceptTableInfo['cols']);
			}
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$queryString = "\"" . implode("\" = NULL, \"", $cols) . "\" = NULL";
			
			$this->Exe("UPDATE \"$conceptTable\" SET $queryString WHERE \"$conceptCol\" = '{$atom->idEsc}'");
			
			$this->addAffectedConcept($atom->concept); // add concept to affected concepts. Needed for conjunct evaluation.
			
			Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' removed as member from concept '$atom->concept->name'", 'DATABASE');
			
		}catch(Exception $e){
			throw $e;
		}
	}
	
	/**
	 * How to use editUpdate:
	 * r :: A * B
	 * editUpdate(r, false, a1[A], b1[B]);
	 * editUpdate(r, true, b1[B], a1[A]);
	 * 
	 * @param Relation $relation
	 * @param boolean $isFlipped
	 * @param Atom $stableAtom
	 * @param Atom $modifiedAtom
	 * @param Atom $originalAtom
	 * @param string $source specifies the source of the editUpdate command (e.g. ExecEngine). Defaults to 'User'. 
	 * @return void
	 * 
	 * NOTE: if $originalAtom is provided, this means that tuple rel(stableAtom, originalAtom) is replaced by rel(stableAtom, modifiedAtom).
	 */
	public function editUpdate($relation, $isFlipped, $stableAtom, $modifiedAtom, $originalAtom = null, $source = 'User'){	    
	    Notifications::addLog("editUpdate('{$relation->__toString()}" . ($isFlipped ? '~' : '') . "', '{$stableAtom->id}[{$stableAtom->concept->name}]', '{$modifiedAtom->id}[{$modifiedAtom->concept->name}]', '{$originalAtom->id}[{$originalAtom->concept->name}]')", 'DATABASE');
	    
		try{			
			// This function is under control of transaction check!
			if (!isset($this->transaction)) $this->startTransaction();
			
			// Check if rel, srcConcept and tgtConcept is a combination
			$srcConcept = $isFlipped ? $modifiedAtom->concept : $stableAtom->concept;
			$tgtConcept = $isFlipped ? $stableAtom->concept : $modifiedAtom->concept;
			if($relation->srcConcept != $srcConcept || $relation->tgtConcept != $tgtConcept) throw new Exception ("Provided atoms for editDelete do not match relation signature '{$relation->__toString()}'", 500);
			
			// Get table properties
			$tableInfo = $relation->getTableInfo();
			$table = $tableInfo['tableName'];
			
			// Determine which Col must be editited and which must be used in the WHERE statement
			$stableCol = $isFlipped ? $tableInfo['tgtCol'] : $tableInfo['srcCol'];
			$modifiedCol =  $isFlipped ? $tableInfo['srcCol'] : $tableInfo['tgtCol'];
	
			// Ensure that the stable and modified atoms exists in their concept tables
			$this->addAtomToConcept($stableAtom);
			$this->addAtomToConcept($modifiedAtom);
			
			/* Complicated code to determine UPDATE or INSERT statement: see Github #169 for explanation */
			if($modifiedCol['unique'] && $stableCol['unique']){
				// If both columns are 'unique', we have to check the 'null' possibility
				if($modifiedCol['null']) $this->Exe("UPDATE `$table` SET `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}' WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}'");
				else $this->Exe("UPDATE `$table` SET `{$stableCol['header']}` = '{$stableAtom->idEsc}' WHERE `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}'");
					
				Hooks::callHooks('postDatabaseUpdate', get_defined_vars());
			}			
			elseif ($modifiedCol['unique'] && !$stableCol['unique']){
			
				$this->Exe("UPDATE `$table` SET `{$stableCol['header']}` = '{$stableAtom->idEsc}' WHERE `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}'");
				
				Hooks::callHooks('postDatabaseUpdate', get_defined_vars());
			}
			elseif (!$modifiedCol['unique'] && $stableCol['unique']){
				
				$this->Exe("UPDATE `$table` SET `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}' WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}'");
				
				Hooks::callHooks('postDatabaseUpdate', get_defined_vars());
			// Otherwise, binary table, so perform a insert.
			}else{
				$this->Exe("INSERT INTO `$table` (`{$stableCol['header']}`, `{$modifiedCol['header']}`) VALUES ('{$stableAtom->idEsc}', '{$modifiedAtom->idEsc}')");
				
				Hooks::callHooks('postDatabaseInsert', get_defined_vars());
				
				// If $originalAtom is provided, delete tuple rel(stableAtom, originalAtom)
				if (!is_null($originalAtom)) {
					$this->Exe("DELETE FROM `$table` WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}' AND `{$modifiedCol['header']}` = '{$originalAtom->idEsc}'");
					Hooks::callHooks('postDatabaseDelete', get_defined_vars());
				}
			}
			
			$this->addAffectedRelations($relation); // add relation to affected relations. Needed for conjunct evaluation.
	
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addErrorException($e);
		}
	}
	
	/**
	 * How to use editDelete:
	 * r :: A * B
	 * editDelete(r, false, a1[A], b1[B]);
	 * editDelete(r, true, b1[B], a1[A]);
	 * 
	 * @param Relation $relation
	 * @param boolean $isFlipped
	 * @param Atom $stableAtom
	 * @param Atom $modifiedAtom
	 * @param string $source specifies the source of the editDelete command (e.g. ExecEngine). Defaults to 'User'. 
	 * @throws Exception
	 */
	public function editDelete($relation, $isFlipped, $stableAtom, $modifiedAtom, $source = 'User'){	    
		Notifications::addLog("editDelete('{$relation->__toString()}" . ($isFlipped ? '~' : '') . "', '{$stableAtom->id}[{$stableAtom->concept->name}]', '{$modifiedAtom->id}[{$modifiedAtom->concept->name}]')", 'DATABASE');
		
		try{			
			// This function is under control of transaction check!
		    if (!isset($this->transaction)) $this->startTransaction();
			
			// Check if stableAtom is provided (i.e. not null)
			if(is_null($stableAtom->id)) throw new Exception("Cannot perform editDelete, because stable atom is null", 500);
			
			// Check if rel, srcConcept and tgtConcept is a combination
			$srcConcept = $isFlipped ? $modifiedAtom->concept : $stableAtom->concept;
			$tgtConcept = $isFlipped ? $stableAtom->concept : $modifiedAtom->concept;
			if($relation->srcConcept != $srcConcept || $relation->tgtConcept != $tgtConcept) throw new Exception ("Provided atoms for editDelete do not match relation signature '{$relation->__toString()}'", 500);
			
			// Get table properties
			$tableInfo = $relation->getTableInfo();
			$table = $tableInfo['tableName'];
			
			// Determine which Col must be editited and which must be used in the WHERE statement
			$stableCol = $isFlipped ? $tableInfo['tgtCol'] : $tableInfo['srcCol'];
			$modifiedCol =  $isFlipped ? $tableInfo['srcCol'] : $tableInfo['tgtCol'];
			
			/* Complicated code to determine UPDATE or INSERT statement: see Github #169 for explanation */
			// If the modifiedCol can be set to null, we do an update
			if ($modifiedCol['null']){
				if(is_null($modifiedAtom->id)) $this->Exe("UPDATE `$table` SET `{$modifiedCol['header']}` = NULL WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}'");
				else $this->Exe("UPDATE `$table` SET `{$modifiedCol['header']}` = NULL WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}' AND `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}'");
			
			// Elseif the stableCol can be set to null, we do an update
			}elseif ($stableCol['null']){
				if(is_null($modifiedAtom->id) && !$stableCol['unique']) throw new Exception("Cannot perform editDelete, because modified atom is null and stable column is not unique", 500);
				else $this->Exe("UPDATE `$table` SET `{$stableCol['header']}` = NULL WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}'");
			
			// Otherwise, binary table, so perform a delete
			}else{
				if(is_null($modifiedAtom->id)) throw new Exception("Cannot perform editDelete, because modified atom is null", 500);
				else $this->Exe("DELETE FROM `$table` WHERE `{$stableCol['header']}` = '{$stableAtom->idEsc}' AND `{$modifiedCol['header']}` = '{$modifiedAtom->idEsc}'");
				
			}
			
			$this->addAffectedRelations($relation); // add relation to affected relations. Needed for conjunct evaluation.
			
			Hooks::callHooks('postDatabaseDelete', get_defined_vars());
			
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addErrorException($e);
		}

	}
	    
	/**
	 * Remove all occurrences of $atom in the database (all concept tables and all relation tables)
	 * In tables where the atom may not be null, the entire row is removed.
	 * TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
	 * @param Atom $atom
	 * @return void
	 */
	function deleteAtom($atom){
		Notifications::addLog("deleteAtom({$atom->id}[{$atom->concept->name}])", 'DATABASE');
		try{
		    // This function is under control of transaction check!
		    if (!isset($this->transaction)) $this->startTransaction();
		    
		    global $tableColumnInfo;
		    
			$concept = Concept::getConcept($atom->concept->name);
			
			foreach ($tableColumnInfo as $table => $tableInfo){
				foreach ($tableInfo as $column => $fieldInfo) {
					// TODO: could be optimized by doing one query per table. But deleting per column yields the same result (unlike adding)
					if ($fieldInfo['concept'] == $concept->name || $concept->hasGeneralization($fieldInfo['concept'])) {
						
						// If the field can be null, we set all occurrences to null
						if ($fieldInfo['null']) $this->Exe("UPDATE `$table` SET `$column` = NULL WHERE `$column` = '{$atom->idEsc}'");
						
						// Otherwise, we remove the entire row for each occurrence
						else 
							$this->Exe("DELETE FROM `$table` WHERE `$column` = '{$atom->idEsc}'");
					}
				}
			}
			
			$this->addAffectedConcept($concept); // add concept to affected concepts. Needed for conjunct evaluation.
			
			Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' (and all related links) deleted in database", 'DATABASE');
			
			Hooks::callHooks('postDatabaseDeleteAtom', get_defined_vars());
		}catch(Exception $e){
			// Catch exception and continue script
			Notifications::addErrorException($e);
		}
	}
	
/**************************************************************************************************
 *
 * Database transaction handling
 *
 *************************************************************************************************/
	
	/**
	 * Function to start/open a database transaction to track of all changes and be able to rollback
	 * @return void
	 */
	private function startTransaction(){
		Notifications::addLog('========================= STARTING TRANSACTION =========================', 'DATABASE');
		$this->Exe("START TRANSACTION"); // start database transaction
		$this->transaction = rand();
		
		Hooks::callHooks('postDatabaseStartTransaction', get_defined_vars());
	}
	
	/**
	 * Function to commit the open database transaction
	 * @return void
	 * 
	 * TODO: make private function, now also used by in Session class for session atom initiation
	 */
	public function commitTransaction(){
		Notifications::addLog('------------------------- COMMIT -------------------------', 'DATABASE');
		
		$this->Exe("COMMIT"); // commit database transaction
		unset($this->transaction);
		
		Hooks::callHooks('postDatabaseCommitTransaction', get_defined_vars());
	}
	
	/**
	 * Function to rollback changes made in the open database transaction
	 * @return void
	 */
	private function rollbackTransaction(){
		Notifications::addLog('------------------------- ROLLBACK -------------------------', 'DATABASE');
		
		$this->Exe("ROLLBACK"); // rollback database transaction
		unset($this->transaction);
		
		Hooks::callHooks('postDatabaseRollbackTransaction', get_defined_vars());
	}
	
	/**
	 * Function to request closing the open database transaction
	 * @param string $succesMessage specifies success/info message when invariants hold
	 * @param boolean $checkAllConjucts specifies to check all (true) or only the affected conjuncts (false)
	 * @param boolean $databaseCommit specifies to commit (true) or rollback (false) when all invariants hold
	 * @param Atom $atomStoreNewContent specifies to store the new content for the updated/created atom
	 * @return boolean specifies if invariant rules hold (true) or not (false)
	 */
	public function closeTransaction($succesMessage = 'Updated', $checkAllConjucts = true, $databaseCommit = null, &$atomStoreNewContent = null){
		$session = Session::singleton();
		
		Hooks::callHooks('preDatabaseCloseTransaction', get_defined_vars());
		
		Notifications::addLog('========================= CLOSING TRANSACTION =========================', 'DATABASE');
		
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
			
			// Check only those process rules that are relevant for the activate roles
			RuleEngine::checkProcessRules($session);
		}
		
		unset($this->affectedConcepts, $this->affectedRelations);
		$this->affectedConcepts = array(); $this->affectedRelations = array();
		
		if(!is_null($atomStoreNewContent)) $atomStoreNewContent->setStoredContent();
		
		// Determine if transaction should be committed or not when all invariant rules hold based on $requestType
		if(is_null($databaseCommit)) $databaseCommit = $this->processRequestType();
		
		if($invariantRulesHold && $databaseCommit){
			$this->commitTransaction(); // commit database transaction
			Notifications::addSuccess($succesMessage);
		}elseif(Config::get('ignoreInvariantViolations', 'transactions') && $databaseCommit){
			$this->commitTransaction();
			Notifications::addError("Transaction committed with invariant violations");
		}elseif($invariantRulesHold){
			$this->rollbackTransaction(); // rollback database transaction
			Notifications::addInfo($succesMessage);
		}else{
			$this->rollbackTransaction(); // rollback database transaction
		}
		
		Hooks::callHooks('postDatabaseCloseTransaction', get_defined_vars());
		
		return $this->invariantRulesHold = $invariantRulesHold;
		
	}
	
/**************************************************************************************************
 *
 * Helper functions
 *
 *************************************************************************************************/
	
	/**
	 * Checks request type and returns boolean to determine database commit
	 * @param string $requestType
	 * @throws Exception when unknown request type specified (allowed: 'feedback' and 'promise')
	 * @return boolean (true for 'promise', false for 'feedback')
	 */
	private function processRequestType(){
		switch($this->requestType){
			case 'feedback' : return false;
			case 'promise' : return true;
			default : throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
	}
    
	/**
	 * Mark a concept as affected within the open transaction
	 * @param Concept $concept
	 * @return void
	 */
	private function addAffectedConcept($concept){
		
		if($this->trackAffectedConjuncts && !in_array($concept, $this->affectedConcepts)){
			Notifications::addLog("Mark concept '{$concept->name}' as affected concept", 'DATABASE');
			$this->affectedConcepts[] = $concept; // add concept to affected concepts. Needed for conjunct evaluation.
		}
		
	}
	
	/**
	 * Mark a relation as affected within the open transaction
	 * @param Relation $relation
	 * @return void
	 */
	private function addAffectedRelations($relation){
	
		if($this->trackAffectedConjuncts && !in_array($relation, $this->affectedRelations)){
			Notifications::addLog("Mark relation '{$relation->__toString()}' as affected relation", 'DATABASE');
			$this->affectedRelations[] = $relation;
		}
	}
	
/**************************************************************************************************
 *
 * GETTERS and SETTERS
 *
 *************************************************************************************************/
	
	public function getAffectedConcepts(){
		return $this->affectedConcepts;	
	}
	
	public function getAffectedRelations(){
		return $this->affectedRelations;
	}
	
	public function getInvariantRulesHold(){
		return $this->invariantRulesHold;
	}
	
	public function getRequestType(){
		return $this->requestType;
	}
	
	public function setTrackAffectedConjuncts($bool){
		$this->trackAffectedConjuncts = $bool;
	}
	
	public function setRequestType($requestType){
		$this->requestType = $requestType;
	}	
}

?>
