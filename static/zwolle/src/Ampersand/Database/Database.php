<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Database;

use Exception;
use mysqli;
use Ampersand\Log\Logger;
use Ampersand\Config;
use Ampersand\Session;
use Ampersand\Hooks;
use Ampersand\Rule\Conjunct;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Rule\RuleEngine;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Database {
    /**
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
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
	    $this->logger = Logger::getLogger('DATABASE');
	    
	    try{
    	    $this->db_host = Config::get('dbHost', 'mysqlDatabase');
    		$this->db_user = Config::get('dbUser', 'mysqlDatabase');
    		$this->db_pass = Config::get('dbPassword', 'mysqlDatabase');
    		$this->db_name = Config::get('dbName', 'mysqlDatabase');
    		
    		// Enable mysqli errors to be thrown as Exceptions
    		mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);
    		
    		// Connect to MYSQL database
    		$this->db_link = mysqli_init();
    		$this->db_link->real_connect($this->db_host, $this->db_user, $this->db_pass, $this->db_name, null, null, MYSQLI_CLIENT_FOUND_ROWS);
    		
    		// Set sql_mode to ANSI
    		$this->db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
			
		}catch (Exception $e){
		    // Convert mysqli_sql_exceptions into 500 errors
		    if(!Config::get('productionEnv')){
    		    switch ($e->getCode()){
    		        case 1049 :
    		            Logger::getUserLogger()->error('Please <a href="#/admin/installer" class="alert-link">install database</a>');
    		        default : 
    		            throw new Exception("{$e->getCode()}: {$e->getMessage()}", 500);
    		    }
		    }else{
		        throw new Exception("Cannot connect to database", 500);
		    }
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
	        $logger = Logger::getLogger('DATABASE');
	        
    		$DB_host = Config::get('dbHost', 'mysqlDatabase');
    		$DB_user = Config::get('dbUser', 'mysqlDatabase');
    		$DB_pass = Config::get('dbPassword', 'mysqlDatabase');
    		$DB_name = Config::get('dbName', 'mysqlDatabase');
    		
    		// Enable mysqli errors to be thrown as Exceptions
    		mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);
    		
    		$db_link = mysqli_init();
    		
    		// Connect to MYSQL database
    		$logger->info("Connecting to host: '{$DB_host}'");
    		$db_link->real_connect($DB_host, $DB_user, $DB_pass);
    		
    		// Set sql_mode to ANSI
    		$logger->info("Setting session sql_mode to 'ANSI,TRADITIONAL'");
    		$db_link->query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'");
    		
    		// Drop database
    		$logger->info("Drop database if exists: '{$DB_name}'");
    		$db_link->query("DROP DATABASE IF EXISTS $DB_name");
    		
    		// Create new database
    		$logger->info("Create new database: '{$DB_name}'");
    		$db_link->query("CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8");
		
		}catch (Exception $e){
		    // Convert mysqli_sql_exceptions into 500 errors
		    throw new Exception($e->getMessage(), 500);
		}
	}
	
	/**
	 * Function to reinstall database structure and load default population
	 * @return void
	 */
	public function reinstallDB(){
		$queries = file_get_contents(Config::get('pathToGeneratedFiles') . 'mysql-installer.json');
		$queries = json_decode($queries, true);
		
		$this->logger->info("Start database reinstall");
		
		$this->logger->info("Execute database structure queries");
		foreach($queries['allDBstructQueries'] as $query){
			$this->Exe($query);
		}
		
		if(Config::get('checkDefaultPopulation', 'transactions')) $this->startTransaction();
		
		$this->logger->info("Execute database population queries");
		foreach($queries['allDefPopQueries'] as $query){
			$this->Exe($query);
		}
		
		// Ininiate new session
		Session::reInit();
		
		Hooks::callHooks('postDatabaseReinstallDB', get_defined_vars());
		
		$this->logger->info("Database reinstalled");
		
		// Initial conjunct evaluation
		Conjunct::evaluateConjuncts(null, true); // Evaluate, cache and store all conjuncts, not only those that are affected (done by closeTransaction() function)
		
		$this->closeTransaction('Database successfully reinstalled', true);
		
		if (version_compare(PHP_VERSION, '5.6', '<')) {
		   Logger::getUserLogger()->warning("Support for PHP version <= 5.5 will stop in the summer of 2016. Please upgrade to 5.6. Note! Ampersand framework does not support PHP 7 yet. You are on version: " . PHP_VERSION, 500);
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
		$this->logger->debug($query);

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
	    $this->logger->debug("addAtomToConcept({$atom->__toString()})");
	    
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
			
			$this->logger->debug("Atom '{$atom->__toString()}' added to database");
			
			Hooks::callHooks('postDatabaseAddAtomToConceptInsert', get_defined_vars());
		}else{
			$this->logger->debug("Atom '{$atom->__toString()}' already exists in database");
			
			Hooks::callHooks('postDatabaseAddAtomToConceptSkip', get_defined_vars());
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
	    $this->logger->debug("atomSetConcept({$atom->__toString()}, {$conceptBName})");
	    
	    $conceptB = Concept::getConcept($conceptBName);
	    
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
		$anyConceptColForA = current($conceptTableInfoA->getCols());
		
		// Perform update
		$this->Exe("UPDATE \"$conceptTableB\" SET $queryString WHERE \"{$anyConceptColForA->name}\" = '{$atom->idEsc}'");
		
		// Check if query resulted in an affected row
		if($this->db_link->affected_rows == 0) throw new Exception ("Oops.. something went wrong. No records updated in Database::atomSetConcept({$atom->__toString()}, {$conceptBName})", 500);
		
		$this->addAffectedConcept($conceptB); // add concept to affected concepts. Needed for conjunct evaluation.
		
		$this->logger->debug("Atom '{$atom->__toString()}' added as member to concept '{$conceptB->__toString()}'");
	}
	
	/**
	 * Removing an atom as member from a Concept set. 
	 * This can only be done when the concept is a specialization of another concept.
	 * @param Atom $atom
	 * @throws Exception
	 * @return void
	 */
	public function atomClearConcept($atom){
		$this->logger->debug("atomClearConcept({$atom->__toString()})");
		
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
		
		$this->logger->debug("Atom '{$atom->__toString()}' removed as member from concept '{$atom->concept->__toString()}'");
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
	 * @param \Ampersand\Core\Atom $atom
	 * @return void
	 */
	function deleteAtom($atom){
		$this->logger->debug("deleteAtom({$atom->__toString()})");
		
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
		        // If n-n table, remove row
		        if(is_null($relation->getMysqlTable()->tableOf)) $query = "DELETE FROM `{$tableName}` WHERE `{$col->name}` = '{$atom->idEsc}'";
		        // Elseif column may be set to null, update
		        elseif($col->null) $query = "UPDATE `{$tableName}` SET `{$col->name}` = NULL WHERE `{$col->name}` = '{$atom->idEsc}'";
		        // Else, we remove the entire row (cascades delete for TOT and SUR relations)
		        else $query = "DELETE FROM `{$tableName}` WHERE `{$col->name}` = '{$atom->idEsc}'";
		        
		        $this->Exe($query);
		        $this->addAffectedRelations($relation);
		    }
		}
		
		$this->logger->debug("Atom '{$atom->__toString()}' (and all related links) deleted in database");
		
		Hooks::callHooks('postDatabaseDeleteAtom', get_defined_vars());
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
		$this->logger->info("Starting database transaction");
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
		$this->logger->info("Commit database transaction");
		
		$this->Exe("COMMIT"); // commit database transaction
		unset($this->transaction);
		
		Hooks::callHooks('postDatabaseCommitTransaction', get_defined_vars());
	}
	
	/**
	 * Function to rollback changes made in the open database transaction
	 * @return void
	 */
	private function rollbackTransaction(){
		$this->logger->info("Rollback database transaction");
		
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
		
		$this->logger->info("Closing database transaction");
		
		$this->logger->info("Checking all affected conjuncts");
		
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
			Logger::getUserLogger()->notice($succesMessage);
		}elseif(Config::get('ignoreInvariantViolations', 'transactions') && $databaseCommit){
			$this->commitTransaction();
			Logger::getUserLogger()->warning("Transaction committed with invariant violations");
		}elseif($invariantRulesHold){
		    $this->logger->info("Invariant rules hold, but no database commit requested");
		    $this->rollbackTransaction(); // rollback database transaction			
		}else{
		    $this->logger->info("Invariant rules do not hold");
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
	 * @throws Exception when unknown request type specified (allowed: 'feedback' and 'promise')
	 * @return boolean (true for 'promise', false for 'feedback')
	 */
	private function processRequestType(){
		switch($this->requestType){
			case 'feedback' : return false;
			case 'promise' : return true;
			default : throw new Exception("Unkown request type '$this->requestType'. Supported are: 'feedback', 'promise'", 500);
		}
	}
    
	/**
	 * Mark a concept as affected within the open transaction
	 * @param Concept $concept
	 * @return void
	 */
	private function addAffectedConcept($concept){
		
		if($this->trackAffectedConjuncts && !in_array($concept, $this->affectedConcepts)){
			$this->logger->debug("Mark concept '{$concept->__toString()}' as affected concept");
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
			$this->logger->debug("Mark relation '{$relation->__toString()}' as affected relation");
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