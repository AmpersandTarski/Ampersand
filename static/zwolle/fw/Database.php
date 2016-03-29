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
    		$this->db_link = mysqli_init();
    		$this->db_link->real_connect($this->db_host, $this->db_user, $this->db_pass, null, null, null, MYSQLI_CLIENT_FOUND_ROWS);
    		
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
		
		// Initial conjunct evaluation
		Conjunct::evaluateConjuncts(null, true); // Evaluate, cache and store all conjuncts, not only those that are affected (done by closeTransaction() function)
		
		$this->closeTransaction('Database successfully reinstalled', true);
		
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
	 * Create private equivalent that is used by addAtomToConcept(), addLink(), deleteLink() and deleteAtom() functions, to perform any INSERT, UPDATE, DELETE
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
	    $firstCol = current($tableInfo->getCols());
	
	    $query = "/* Check if atom exists */ SELECT `$firstCol->name` FROM `{$tableInfo->name}` WHERE `$firstCol->name` = '{$atom->idEsc}'";
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
				$conceptTable = $conceptTableInfo->name;
				$conceptCols = $conceptTableInfo->getCols(); // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
				
				// Create query string: `<col1>`, `<col2>`, etc
				$allConceptCols = '`' . implode('`, `', $conceptTableInfo->getColNames()) . '`';
				
				
				// Create query string: '<newAtom>', '<newAtom', etc
				$atomIdsArray = array_fill(0, count($conceptCols), $atom->idEsc);
				$allValues = "'".implode("', '", $atomIdsArray)."'";
				
				foreach($conceptCols as $col) $str .= ", `$col->name` = '{$atom->idEsc}'";
				$duplicateStatement = substr($str, 1);
				
				$this->Exe("INSERT INTO `$conceptTable` ($allConceptCols) VALUES ($allValues)"
						  ." ON DUPLICATE KEY UPDATE $duplicateStatement");
				
				// Check if query resulted in an affected row
				if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No record inserted in Database::addAtomToConcept({$atom->__toString()})", 500);
				
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
			$conceptTableB = $conceptTableInfoB->name;
			$conceptColsB = $conceptTableInfoB->getColNames(); // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$queryString = "\"" . implode("\" = '{$atom->idEsc}', \"", $conceptColsB) . "\" = '{$atom->idEsc}'";
			
			$conceptTableInfoA = $atom->concept->getConceptTableInfo();
			$conceptTableA = $conceptTableInfoA->name;
			$anyConceptColForA = current($conceptTableInfoA->getCols());
			
			// Perform update
			$this->Exe("UPDATE \"$conceptTableB\" SET $queryString WHERE \"{$anyConceptColForA->name}\" = '{$atom->idEsc}'");
			
			// Check if query resulted in an affected row
			if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records updated in Database::atomSetConcept({$atom->__toString()}, {$conceptBName})", 500);
			
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
			$colNames = array();
			$conceptTableInfo = $atom->concept->getConceptTableInfo();
			$conceptTable = $conceptTableInfo->name;
			$conceptCol = reset($conceptTableInfo->getCols());
			
			$colNames[] = $conceptCol->name;
			foreach($atom->concept->getSpecializations() as $specConcept){
				$conceptTableInfo = $specConcept->getConceptTableInfo();
				$colNames[] = reset($conceptTableInfo->getColNames);
			}
			
			// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
			$queryString = "\"" . implode("\" = NULL, \"", $colNames) . "\" = NULL";
			
			$this->Exe("UPDATE \"$conceptTable\" SET $queryString WHERE \"{$conceptCol->name}\" = '{$atom->idEsc}'");
			
			// Check if query resulted in an affected row
			if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records updated in Database::atomClearConcept({$atom->__toString()})", 500);
			
			$this->addAffectedConcept($atom->concept); // add concept to affected concepts. Needed for conjunct evaluation.
			
			Notifications::addLog("Atom '{$atom->id}[{$atom->concept->name}]' removed as member from concept '$atom->concept->name'", 'DATABASE');
			
		}catch(Exception $e){
			throw $e;
		}
	}
	
	/**
	 * Add link (srcAtom,tgtAtom) into database table for relation r
	 * @param Relation $relation
	 * @param Atom $srcAtom
	 * @param Atom $tgtAtom
	 */
	public function addLink($relation, $srcAtom, $tgtAtom){
	    // This function is under control of transaction check!
	    if (!isset($this->transaction)) $this->startTransaction();
	    
	    $relTable = $relation->getMysqlTable();
	    
	    switch ($relTable->tableOf){
	        case null : // Relation is administrated in n-n table
	            $this->Exe("INSERT INTO `{$relTable->name}` (`{$relTable->srcCol()->name}`, `{$relTable->tgtCol()->name}`) VALUES ('{$srcAtom->idEsc}', '{$tgtAtom->idEsc}')");
	            break;
	        case 'src' : // Relation is administrated in concept table (wide) of source of relation
	            $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = '{$tgtAtom->idEsc}' WHERE `{$relTable->srcCol()->name}` = '{$srcAtom->idEsc}'");
	            break;
	        case 'tgt' : //  Relation is administrated in concept table (wide) of target of relation
	            $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = '{$srcAtom->idEsc}' WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtom->idEsc}'");
	            break;
	        default :
	            throw new Exception ("Unknown 'tableOf' option for relation '{$relation->name}'", 500);
	    }
	    // Check if query resulted in an affected row
	    if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records updated in Database::addLink({$relation->__toString()},{$srcAtom->__toString()},{$tgtAtom->__toString()})", 500);
	    
	    $this->addAffectedRelations($relation); // Add relation to affected relations. Needed for conjunct evaluation.
	}
	
	/**
	 * Delete link (srcAtom,tgtAtom) into database table for relation r
	 * @param Relation $relation
	 * @param Atom $srcAtom
	 * @param Atom $tgtAtom
	 */
	public function deleteLink($relation, $srcAtom, $tgtAtom){
	    // This function is under control of transaction check!
	    if (!isset($this->transaction)) $this->startTransaction();
	     
	    $relTable = $relation->getMysqlTable();
	     
	    switch ($relTable->tableOf){
	        case null : // Relation is administrated in n-n table
	            if(is_null($srcAtom->id) || is_null($tgtAtom->id)) throw new Exception ("Cannot delete from relation table '{$relTable->name}', because srcAtom or tgtAtom is null", 500);
	            $this->Exe("DELETE FROM `{$relTable->name}` WHERE `{$relTable->srcCol()->name}` = '{$srcAtom->idEsc}' AND `{$relTable->tgtCol()->name}` = '{$tgtAtom->idEsc}'");
	            break;
	        case 'src' : // Relation is administrated in concept table (wide) of source of relation
	            if(!$relTable->tgtCol()->null) throw new Exception("Cannot delete link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) from relation '{$relation->__toString()}' because target column '{$relTable->tgtCol()->name}' in table '{$relTable->name}' may not be set to null", 500);
	            
	            // Source atom can be used in WHERE statement
	            if(!is_null($srcAtom->id)) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = NULL WHERE `{$relTable->srcCol()->name}` = '{$srcAtom->idEsc}'");
	            // Target can be used in WHERE statement, because tgtCol is unique
	            elseif($relTable->tgtCol()->unique) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = NULL WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtom->idEsc}'");
	            // Else update cannot be performed, because of missing target
	            else throw new Exception ("Cannot set '{$relTable->tgtCol()->name}' to NULL in concept table '{$relTable->name}', because srcAtom is null", 500);
	            break;
	        case 'tgt' : //  Relation is administrated in concept table (wide) of target of relation
	            if(!$relTable->srcCol()->null) throw new Exception("Cannot delete link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) from relation '{$relation->__toString()}' because source column '{$relTable->srcCol()->name}' in table '{$relTable->name}' may not be set to null", 500);
	            
	            // Target atom can be used in WHERE statement
	            if(!is_null(($tgtAtom->id))) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = NULL WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtom->idEsc}'");
	            // Source can be used in WHERE statement, because srcCol is unique
	            elseif($relTable->srcCol()->unique) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = NULL WHERE `{$relTable->srcCol()->name}` = '{$srcAtom->idEsc}'");
	            // Else update cannot be performed, because of missing target
	            else throw new Exception ("Cannot set '{$relTable->srcCol()->name}' to NULL in concept table '{$relTable->name}', because tgtAtom is null", 500);
	            break;
	        default :
	            throw new Exception ("Unknown 'tableOf' option for relation '{$relation->name}'", 500);
	    }
	    // Check if query resulted in an affected row
	    if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records updated in Database::deleteLink({$relation->__toString()},{$srcAtom->__toString()},{$tgtAtom->__toString()})", 500);
	    
	    $this->addAffectedRelations($relation); // Add relation to affected relations. Needed for conjunct evaluation.
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
		    
			$concept = Concept::getConcept($atom->concept->name);
			
			// Delete atom from concept table
			$conceptTable = $concept->getConceptTableInfo();
			$query = "DELETE FROM `{$conceptTable->name}` WHERE `{$conceptTable->getFirstCol()->name}` = '{$atom->idEsc}' LIMIT 1";
			$this->Exe($query);
			
			// Check if query resulted in an affected row
			if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records deleted in Database::deleteAtom({$atom->__toString()})", 500);
			
			$this->addAffectedConcept($concept); // add concept to affected concepts. Needed for conjunct evaluation.
			
			// Delete atom from relation tables where atom is mentioned as src or tgt atom
			foreach (Relation::getAllRelations() as $relation){
			    if(($tableName = $relation->getMysqlTable()->name) == $conceptTable->name) continue; // Skip this relation, row in table is already deleted
			    
			    $cols = array();
			    if($relation->srcConcept->inSameClassificationTree($concept)) $cols[] = $relation->getMysqlTable()->srcCol();
			    if($relation->tgtConcept->inSameClassificationTree($concept)) $cols[] = $relation->getMysqlTable()->tgtCol();
			    
			    foreach($cols as $col){			        
			        // If column may be set to null, update
			        if($col->null) $query = "UPDATE `{$tableName}` SET `{$col->name}` = NULL WHERE `{$col->name}` = '{$atom->idEsc}'";
			        // Else, we remove the entire row (cascades delete for TOT and SUR relations)
			        else $query = "DELETE FROM `{$tableName}` WHERE `{$col->name}` = '{$atom->idEsc}'";
			        
			        $this->Exe($query);
			        $this->addAffectedRelations($relation);
			    }
			}
			
			Notifications::addLog("Atom '{$atom->__toString()}' (and all related links) deleted in database", 'DATABASE');
			
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
	 * @param boolean $databaseCommit specifies to commit (true) or rollback (false) when all invariants hold
	 * @param Atom $atomStoreNewContent specifies to store the new content for the updated/created atom
	 * @return boolean specifies if invariant rules hold (true) or not (false)
	 */
	public function closeTransaction($succesMessage = 'Updated', $databaseCommit = null, &$atomStoreNewContent = null){		
		Hooks::callHooks('preDatabaseCloseTransaction', get_defined_vars());
		
		Notifications::addLog('========================= CLOSING TRANSACTION =========================', 'DATABASE');
		
		Notifications::addLog("Check all affected conjuncts", 'DATABASE');
		
		
		// Check invariant rules (we only have to check the affected invariant rules)
		$affectedConjuncts = RuleEngine::getAffectedConjuncts($this->affectedConcepts, $this->affectedRelations, 'inv'); // Get affected invariant conjuncts
		$invariantRulesHold = RuleEngine::checkInvariantRules($affectedConjuncts, true);
		
		// Check all process rules that are relevant for the activate roles
		RuleEngine::checkProcessRules();
		
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

Class DatabaseTable {
    /**
     * 
     * @var string
     */
    public $name;
    
    /**
     * 
     * @var array
     */
    private $cols = array();
    
    /**
     * Constructor of Database table
     * @param string $name
     */
    public function __construct($name){
        if($name == '') throw new Exception ("Database table name is an empty string" ,500); 
        $this->name = $name;
    }
    
    /**
     * Add database table column object to this table
     * @param DatabaseTableCol $col
     * return void
     */
    public function addCol($col){
        $this->cols[$col->name] = $col;
    }
    
    /**
     * Get all col objects for this table
     * @throws Exception when no columns are defined for this table
     * @return DatabaseTableCol[]
     */
    public function getCols(){
        if (empty($this->cols)) throw new Exception("No column defined for table '{$this->name}'", 500);
        return $this->cols;
    }
    
    /**
     * Returns names of all table cols
     * @return string[]
     */
    public function getColNames(){
        $colNames = array();
        foreach($this->getCols() as $col) $colNames[] = $col->name;
        return $colNames; 
    }
    
    /**
     * Return col object with given column name
     * @param string $colName
     * @throws Exception when col does not exists
     * @return DatabaseTableCol[]
     */
    public function getCol($colName){
        if(!array_key_exists($colName, $this->getCols())) throw new Exception ("Col '{$colName}' does not exists in table '{$this->name}'", 500);
        return $this->getCols()[$colName];
    }
    
    /**
     * Return first registered col object
     * @return DatabaseTableCol
     */
    public function getFirstCol(){
        return current($this->getCols());
    }
}

Class RelationTable extends DatabaseTable {
    
    /**
     * 
     * @var DatabaseTableCol
     */
    private $srcCol = null;
    
    /**
     * 
     * @var DatabaseTableCol
     */
    private $tgtCol = null;
    
    /**
     * Specifies if this relation is administrated in the table of the src concept ('src'), the tgt concept ('tgt') or its own n-n table (null)
     * @var string
     */
    public $tableOf;
    
    /**
     * Constructor of RelationTable
     * @param string $name
     * @param string|null $tableOf ('src', 'tgt' or null)
     */
    public function __construct($name, $tableOf){
        parent::__construct($name);
        
        switch ($tableOf){
            case 'src':
            case 'tgt':
            case null :
                $this->tableOf = $tableOf;
                break;
            default :
                throw new Exception ("Unknown tableOf value '{$tableOf}' specified for RelationTable {$this->name}", 500);
        }
    }
    
    /**
     * 
     * @param DatabaseTableCol $col
     * @return void
     */
    public function addSrcCol($col){
        $this->srcCol = $col;
        $this->cols[$col->name] = $col;
    }
    
    /**
     *
     * @param DatabaseTableCol $col
     * @return void
     */
    public function addTgtCol($col){
        $this->tgtCol = $col;
        $this->cols[$col->name] = $col;
    }
    
    /**
     * 
     * @throws Exception when src column is not defined
     * @return DatabaseTableCol
     */
    public function srcCol(){
        if(is_null($this->srcCol)) throw new Exception ("Src column for RelationTable {$this->name} not defined", 500);
        return $this->srcCol;
    }
    
    /**
     * 
     * @throws Exception when tgt column is not defined
     * @return DatabaseTableCol
     */
    public function tgtCol(){
        if(is_null($this->tgtCol)) throw new Exception ("Tgt column for RelationTable {$this->name} not defined", 500);
        return $this->tgtCol;
    }
}

Class DatabaseTableCol {
    /**
     * Name/header of database column
     * @var string
     */
    public $name;
    
    /**
     * Specifies if value in this database column can be NULL
     * @var boolean|NULL
     */
    public $null;
    
    /**
     * Specifies if this database column has uniquness constraint (i.e. no duplicates may exist in all rows)
     * @var boolean|NULL
     */
    public $unique;
    
    /**
     * Constructor of Database table column
     * @param string $name
     * @param boolean $null
     * @param boolean $unique
     */
    public function __construct($name, $null = null, $unique = null){
        if($name == '') throw new Exception ("Database table column name is an empty string" ,500);
        $this->name = $name;
        $this->null = $null;
        $this->unique = $unique;
    }
}

?>
