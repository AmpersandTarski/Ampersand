<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Database;
use mysqli;
use DateTime;
use Exception;
use DateTimeZone;
use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Session;
use Ampersand\Core\Atom;
use Ampersand\Core\Link;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Interfacing\ViewSegment;
use Ampersand\Plugs\ConceptPlugInterface;
use Ampersand\Plugs\IfcPlugInterface;
use Ampersand\Plugs\RelationPlugInterface;
use Ampersand\Plugs\ViewPlugInterface;
use Ampersand\Storage\Transaction;
use Ampersand\Log\Logger;
use Ampersand\Rule\Conjunct;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Database implements ConceptPlugInterface, RelationPlugInterface, IfcPlugInterface, ViewPlugInterface {
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
     * Specifies if database transaction is active
     * @var boolean $dbTransactionActive
     */
    private $dbTransactionActive = false;
	
	/**
	 * Contains reference to database instance (singleton pattern)
	 * @var Database
	 */
	private static $_instance = null;
	
	/**
	 * Constructor of database class
	 * Singleton pattern: private function to prevent any outside instantiantion of this object. 
	 * Use Database::singleton() instead
	 */
	private function __construct(){
	    $this->logger = Logger::getLogger('DATABASE');
	    
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
	}
	
	/**
	 * Use Database::singleton() instead
	 * Singleton pattern: private function to prevent any copy/clone of database instance
	 */
	private function __clone(){}
    
	/**
	 * Function to return the database instance
     * Singleton pattern: use this static function to get the single instance of this class
	 * @return Database
	 */
	public static function singleton(){
        try {
            if(!is_object (self::$_instance)) self::$_instance = new Database();
        }catch (Exception $e){
            // Convert mysqli_sql_exceptions into 500 errors
            if(!Config::get('productionEnv')){
                switch ($e->getCode()){
                    case 1049 : // Error: 1049 SQLSTATE: 42000 (ER_BAD_DB_ERROR)
                        // throw new Exception("Please <a href=\"#/admin/installer\" class=\"alert-link\">install database</a>",500);
                        self::createDB();
                        self::$_instance = new Database();
                        self::$_instance->logger->info("Automatically installing database for the first time");
                        self::$_instance->reinstallDB();
                        break;
    		        default : 
    		            throw new Exception("{$e->getCode()}: {$e->getMessage()}", 500);
    		    }
		    }else{
		        throw new Exception("Cannot connect to database", 500);
		    }
		}
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
	 * @param boolean $loadDefaultPop specifies whether or not to install the default population
	 * @return void
	 */
	public function reinstallDB($installDefaultPop = true){
		$queries = file_get_contents(Config::get('pathToGeneratedFiles') . 'mysql-installer.json');
		$queries = json_decode($queries, true);
		
		$this->logger->info("Start database reinstall");
		
		$this->logger->info("Execute database structure queries");
		foreach($queries['allDBstructQueries'] as $query){
			$this->Exe($query);
		}
		
        if($installDefaultPop){
            $this->logger->info("Install default population");
		    if(Config::get('checkDefaultPopulation', 'transactions')) $this->startTransaction();
            
            foreach($queries['allDefPopQueries'] as $query){
                $this->Exe($query);
            }
        }else{
            $this->logger->info("Skip default population");
        }
		
		// Ininiate new session
		Session::singleton();
		
		Hooks::callHooks('postDatabaseReinstallDB', get_defined_vars());
		
		$this->logger->info("Database reinstalled");
		
		// Initial conjunct evaluation
		Conjunct::evaluateConjuncts(null, true); // Evaluate, cache and store all conjuncts
		
        $transaction = Transaction::getCurrentTransaction()->close(true);
        if($transaction->isCommitted()) Logger::getUserLogger()->notice("Database successfully reinstalled");
		
	}
    
    /**
     * Return escaped mysql representation of Atom (identifier) according to Ampersand technical types (TTypes)
     * @throws Exception when technical type is not (yet) supported
     * @return mixed
     */
    public function getDBRepresentation($atom){
        if(is_null($atom->id)) return null;
        
        switch($atom->concept->type){
            case "ALPHANUMERIC" :
            case "BIGALPHANUMERIC" :
            case "HUGEALPHANUMERIC" :
            case "PASSWORD" :
            case "TYPEOFONE" :
                return (string) $this->escape($atom->id);
            case "BOOLEAN" :
                return (int) $atom->id; // booleans are stored as tinyint(1) in the database. false = 0, true = 1
            case "DATE" :
                $datetime = new DateTime($atom->id);
                return $datetime->format('Y-m-d'); // format to store in database
            case "DATETIME" :
                $datetime = new DateTime($atom->id); // $atom->id can include timezone, e.g. 2005-08-15T15:52:01+00:00 (DATE_ATOM format)
                $datetime->setTimezone(new DateTimeZone('UTC')); // convert to UTC to store in database
                return $datetime->format('Y-m-d H:i:s'); // format to store in database (UTC)
            case "FLOAT" :
                return (float) $atom->id;
            case "INTEGER" :
                return (int) $atom->id;
            case "OBJECT" :
                return $this->escape($atom->id);
            default :
                throw new Exception("Unknown/unsupported representation type '{$atom->concept->type}' for concept '[{$atom->concept}]'", 501);
        }
    }
    
	/**
	 * Execute query on database. Function replaces reserved words by their corresponding value (e.g. _SESSION)
	 * @param string $query
	 * @return boolean|array
	 * 
	 * TODO:
	 * Create private equivalent that is used by addAtom(), addLink(), deleteLink() and deleteAtom() functions, to perform any INSERT, UPDATE, DELETE
	 * The public version should be allowed to only do SELECT queries.
	 * This is needed to prevent Extensions or ExecEngine functions to go around the functions in this class that keep track of the affectedConjuncts.
	 */
	public function Exe($query){
		$query = str_replace('_SESSION', session_id(), $query); // Replace _SESSION var with current session id.
		
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
            $this->logger->error($e->getMessage());
            if(!Config::get('productionEnv')){
                // Convert mysqli_sql_exceptions into 500 errors
                switch ($e->getCode()){
                    case 1146 : // Error: 1146 SQLSTATE: 42S02 (ER_NO_SUCH_TABLE)
                    case 1054 : // Error: 1054 SQLSTATE: 42S22 (ER_BAD_FIELD_ERROR)
                        throw new Exception("{$e->getMessage()}. Try <a href=\"#/admin/installer\" class=\"alert-link\">reinstalling database</a>",500);
                        break;
                    default:
                        throw new Exception("MYSQL error " . $e->getCode() . ": " . $e->getMessage() . " in query:" . $query, 500);
                        break;
                }
            }else{
                throw new Exception("Error in database query", 500);
            }
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

/**************************************************************************************************
 *
 * Implementation of StorageInterface methods (incl. database transaction handling)
 *
 *************************************************************************************************/
    
    /**
     * Returns name of storage implementation
     * @return string
     */
    public function getLabel(){
        return "MySQL database {$this->db_host} - {$this->db_name}";
    }
    
	/**
	 * Function to start/open a database transaction to track of all changes and be able to rollback
	 * @return void
	 */
	private function startTransaction(){
        Transaction::registerStorageTransaction($this);
        
        $this->Exe("START TRANSACTION"); // start database transaction
        $this->dbTransactionActive = true; // set flag dbTransactionActive
	}
	
	/**
	 * Function to commit the open database transaction
	 * @return void
	 */
	public function commitTransaction(){
		$this->logger->info("Commit database transaction");
		
		$this->Exe("COMMIT"); // commit database transaction
        $this->dbTransactionActive = false;
	}
	
	/**
	 * Function to rollback changes made in the open database transaction
	 * @return void
	 */
	public function rollbackTransaction(){
		$this->logger->info("Rollback database transaction");
		
		$this->Exe("ROLLBACK"); // rollback database transaction
        $this->dbTransactionActive = false;
	}
    
/**************************************************************************************************
 * 
 * Implementation of ConceptPlugInterface methods
 *
 *************************************************************************************************/

    /**
    * Check if atom exists in database
    * @param Atom $atom
    * @return boolean
    */
    public function atomExists(Atom $atom){
        $tableInfo = $atom->concept->getConceptTableInfo();
        $firstCol = current($tableInfo->getCols());
        $atomId = $this->getDBRepresentation($atom);
        
        $query = "/* Check if atom exists */ SELECT `$firstCol->name` FROM `{$tableInfo->name}` WHERE `$firstCol->name` = '{$atomId}'";
        $result = $this->Exe($query);
        
        if(empty($result)) return false;
        else return true;
    }
    
    /**
     * Get all atoms for given concept
     * @param Concept $concept
     * @return Atom[]
     */
    public function getAllAtoms(Concept $concept){
        $tableInfo = $concept->getConceptTableInfo();
        
        // Query all atoms in table
        if(isset($tableInfo->allAtomsQuery)) $query = $tableInfo->allAtomsQuery;
        else{
            $firstCol = current($tableInfo->getCols()); // We can query an arbitrary concept col for checking the existence of an atom
            $query = "SELECT DISTINCT `{$firstCol->name}` as `atomId` FROM `{$tableInfo->name}` WHERE `{$firstCol->name}` IS NOT NULL";
        }
        
        $arr = [];
        foreach ((array)$this->Exe($query) as $row){
            $tgtAtom = new Atom($row['atomId'], $concept);
            $tgtAtom->setQueryData($row);
            $arr[] = $tgtAtom;
        }
        return $arr;
    }
    
	/**
	 * Add atom to database
	 * @param Atom $atom
	 * @return void
	 */
	public function addAtom(Atom $atom){
	    $this->logger->debug("addAtom({$atom})");
	    
		// This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $atomId = $this->getDBRepresentation($atom);
	    			    
		// Get table properties
		$conceptTableInfo = $atom->concept->getConceptTableInfo();
		$conceptTable = $conceptTableInfo->name;
		$conceptCols = $conceptTableInfo->getCols(); // Concept are registered in multiple cols in case of specializations. We insert the new atom in every column.
		
		// Create query string: `<col1>`, `<col2>`, etc
		$allConceptCols = '`' . implode('`, `', $conceptTableInfo->getColNames()) . '`';
		
		
		// Create query string: '<newAtom>', '<newAtom', etc
		$atomIdsArray = array_fill(0, count($conceptCols), $atomId);
		$allValues = "'".implode("', '", $atomIdsArray)."'";
		
		foreach($conceptCols as $col) $str .= ", `$col->name` = '{$atomId}'";
		$duplicateStatement = substr($str, 1);
		
		$this->Exe("INSERT INTO `$conceptTable` ($allConceptCols) VALUES ($allValues)"
				  ." ON DUPLICATE KEY UPDATE $duplicateStatement");
		
		// Check if query resulted in an affected row
        $this->checkForAffectedRows();
	}
	
	/**
	 * Removing an atom as member from a concept set. 
	 * @param Atom $atom
	 * @throws Exception
	 * @return void
	 */
	public function removeAtom(Atom $atom){
        $this->logger->debug("removeAtom({$atom})");
	    
		// This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $atomId = $this->getDBRepresentation($atom);
        
		// Get col information for concept and its specializations
		$colNames = array();
		$conceptTableInfo = $concept->getConceptTableInfo();
		$conceptTable = $conceptTableInfo->name;
		$conceptCol = reset($conceptTableInfo->getCols());
		
		$colNames[] = $conceptCol->name;
		foreach($concept->getSpecializations() as $specConcept){
			$conceptTableInfo = $specConcept->getConceptTableInfo();
			$colNames[] = reset($conceptTableInfo->getColNames);
		}
		
		// Create query string: "<col1>" = '<atom>', "<col2>" = '<atom>', etc
		$queryString = "\"" . implode("\" = NULL, \"", $colNames) . "\" = NULL";
		
		$this->Exe("UPDATE \"$conceptTable\" SET $queryString WHERE \"{$conceptCol->name}\" = '{$atomId}'");
        
        // Check if query resulted in an affected row
        $this->checkForAffectedRows();
	}
    
    /**
	 * Delete atom from concept table in the database
	 * @param \Ampersand\Core\Atom $atom
	 * @return void
	 */
	public function deleteAtom(Atom $atom){
		$this->logger->debug("deleteAtom({$atom})");
		
	    // This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $atomId = $this->getDBRepresentation($atom);
        
        // Delete atom from concept table
        $conceptTable = $atom->concept->getConceptTableInfo();
        $query = "DELETE FROM `{$conceptTable->name}` WHERE `{$conceptTable->getFirstCol()->name}` = '{$atomId}' LIMIT 1";
        $this->Exe($query);
        
        // Check if query resulted in an affected row
        $this->checkForAffectedRows();
	}
    
/**************************************************************************************************
 *
 * Implementation of RelationPlugInterface methods
 *
 *************************************************************************************************/
    
    /**
    * Check if link exists in database
    * @param Link $link
    * @return boolean
    */
    public function linkExists(Link $link){
        $relTable = $link->relation()->getMysqlTable();
        $srcAtomId = $this->getDBRepresentation($link->src());
        $tgtAtomId = $this->getDBRepresentation($link->tgt());
        
        $result = $this->Exe("/* Check if link exists */ SELECT * FROM `{$relTable->name}` WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}' AND `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'");
        
        if(empty($result)) return false;
        else return true;
    }
    
    /**
    * Get all links given a relation
    * @param Relation $relation
    * @param Atom $srcAtom if specified get all links with $srcAtom as source
    * @param Atom $tgtAtom if specified get all links with $tgtAtom as tgt
    * @return Link[]
    */
    public function getAllLinks(Relation $relation, Atom $srcAtom = null, Atom $tgtAtom = null){
        $relTable = $relation->getMysqlTable();
        
        // Query all atoms in table
        $query = "SELECT `{$relTable->srcCol()->name}` as `src`, `{$relTable->tgtCol()->name}` as `tgt` FROM `{$relTable->name}`";
        
        // Construct WHERE-clause if applicable
        if(isset($srcAtom)){
            $srcAtomId = $this->getDBRepresentation($srcAtom);
            $query .= " WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}'";
        }
        if(isset($tgtAtom)){
            $tgtAtomId = $this->getDBRepresentation($tgtAtom);
            if(isset($srcAtom)) $query .= " AND `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'";
            else $query .= " WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'";
        }
        
        $links = [];
        foreach((array)$this->Exe($query) as $row){
            $links[] = new Link($relation, new Atom($row['src'], $relation->srcConcept), new Atom($row['tgt'], $relation->tgtConcept));
        }
        
        return $links;
    }
    
	/**
	 * Add link (srcAtom,tgtAtom) into database table for relation r
     * @param Link $link
     * @return void
	 */
	public function addLink(Link $link){
        $this->logger->debug("addLink({$link})");
        
	    // This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $relation = $link->relation();
        $srcAtomId = $this->getDBRepresentation($link->src());
        $tgtAtomId = $this->getDBRepresentation($link->tgt());
	    
	    $relTable = $relation->getMysqlTable();
	    
	    switch ($relTable->tableOf){
	        case null : // Relation is administrated in n-n table
	            $this->Exe("INSERT INTO `{$relTable->name}` (`{$relTable->srcCol()->name}`, `{$relTable->tgtCol()->name}`) VALUES ('{$srcAtomId}', '{$tgtAtomId}')");
	            break;
	        case 'src' : // Relation is administrated in concept table (wide) of source of relation
	            $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = '{$tgtAtomId}' WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}'");
	            break;
	        case 'tgt' : //  Relation is administrated in concept table (wide) of target of relation
	            $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = '{$srcAtomId}' WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'");
	            break;
	        default :
	            throw new Exception ("Unknown 'tableOf' option for relation '{$relation}'", 500);
	    }
        
	    // Check if query resulted in an affected row
	    $this->checkForAffectedRows();
	}
	
	/**
	 * Delete link (srcAtom,tgtAtom) into database table for relation r
     * @param Link $link
     * @return void
	 */
	public function deleteLink(Link $link){
        $this->logger->debug("deleteLink({$link})");
        
	    // This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $relation = $link->relation();
        $srcAtomId = $this->getDBRepresentation($link->src());
        $tgtAtomId = $this->getDBRepresentation($link->tgt());
	     
	    $relTable = $relation->getMysqlTable();
	     
	    switch ($relTable->tableOf){
	        case null : // Relation is administrated in n-n table
	            if(is_null($srcAtomId) || is_null($tgtAtomId)) throw new Exception ("Cannot delete from relation table '{$relTable->name}', because srcAtom or tgtAtom is null", 500);
	            $this->Exe("DELETE FROM `{$relTable->name}` WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}' AND `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'");
	            break;
	        case 'src' : // Relation is administrated in concept table (wide) of source of relation
	            if(!$relTable->tgtCol()->null) throw new Exception("Cannot delete link {$link} because target column '{$relTable->tgtCol()->name}' in table '{$relTable->name}' may not be set to null", 500);
	            
	            // Source atom can be used in WHERE statement
	            if(!is_null($srcAtomId)) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = NULL WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}'");
	            // Target can be used in WHERE statement, because tgtCol is unique
	            elseif($relTable->tgtCol()->unique) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->tgtCol()->name}` = NULL WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'");
	            // Else update cannot be performed, because of missing target
	            else throw new Exception ("Cannot set '{$relTable->tgtCol()->name}' to NULL in concept table '{$relTable->name}', because srcAtom is null", 500);
	            break;
	        case 'tgt' : //  Relation is administrated in concept table (wide) of target of relation
	            if(!$relTable->srcCol()->null) throw new Exception("Cannot delete link {$link} because source column '{$relTable->srcCol()->name}' in table '{$relTable->name}' may not be set to null", 500);
	            
	            // Target atom can be used in WHERE statement
	            if(!is_null(($tgtAtomId))) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = NULL WHERE `{$relTable->tgtCol()->name}` = '{$tgtAtomId}'");
	            // Source can be used in WHERE statement, because srcCol is unique
	            elseif($relTable->srcCol()->unique) $this->Exe("UPDATE `{$relTable->name}` SET `{$relTable->srcCol()->name}` = NULL WHERE `{$relTable->srcCol()->name}` = '{$srcAtomId}'");
	            // Else update cannot be performed, because of missing target
	            else throw new Exception ("Cannot set '{$relTable->srcCol()->name}' to NULL in concept table '{$relTable->name}', because tgtAtom is null", 500);
	            break;
	        default :
	            throw new Exception ("Unknown 'tableOf' option for relation '{$relation}'", 500);
	    }
	    
	    $this->checkForAffectedRows(); // Check if query resulted in an affected row
	}
    
    /**
     * @param Relation $relation relation from which to delete all links
     * @param Atom $atom atom for which to delete all links
     * @param string $srcOrTgt specifies to delete all link with $atom as src, tgt or both (null/not provided)
     * @return void
     */
    public function deleteAllLinks(Relation $relation, Atom $atom = null, $srcOrTgt = null){
        // This function is under control of transaction check!
        if (!$this->dbTransactionActive) $this->startTransaction();
        
        $relationTable = $relation->getMysqlTable();
        
        // Delete links for given atom
        if(isset($atom)){
            $atomId = $this->getDBRepresentation($atom);
            
            $cols = [];
            switch ($srcOrTgt) {
                case 'src':
                    $cols[] = $relationTable->srcCol();
                    break;
                case 'tgt':
                    $cols[] = $relationTable->tgtCol();
                    break;
                case null:
                    $cols[] = $relationTable->srcCol();
                    $cols[] = $relationTable->tgtCol();
                    breal;
                default:
                    throw new Exception("Unknown/unsupported param option '{$srcOrTgt}'. Supported options are 'src', 'tgt' or null", 500);
                    break;
            }
            
            foreach($cols as $col){
                // If n-n table, remove row
                if(is_null($relationTable->tableOf)) $query = "DELETE FROM `{$relationTable->name}` WHERE `{$col->name}` = '{$atomId}'";
                
                // Elseif column may be set to null, update
                elseif($col->null) $query = "UPDATE `{$relationTable->name}` SET `{$col->name}` = NULL WHERE `{$col->name}` = '{$atomId}'";
                
                // Else, we remove the entire row (cascades delete for TOT and SUR relations)
                else $query = "DELETE FROM `{$relationTable->name}` WHERE `{$col->name}` = '{$atomId}'";
                
                $this->Exe($query);
            }
            
        // Delete all links
        }else{
            // If n-n table, remove all rows
            if(is_null($relationTable->tableOf)){
                $query = "DELETE FROM `{$relationTable->name}`";
            
            // Else if in table of src concept, set tgt col to null
            }elseif($relationTable->tableOf == 'src'){ 
                $col = $relationTable->tgtCol();
                $query = "UPDATE `{$relationTable->name}` SET `{$col->name}` = NULL";
            
            // Else if in table of tgt concept, set src col to null
            }elseif($relationTable->tableOf == 'tgt'){ 
                $col = $relationTable->srcCol();
                $query = "UPDATE `{$relationTable->name}` SET `{$col->name}` = NULL";
            }
            
            $this->Exe($query);
        }
        
    }
    
/**************************************************************************************************
 *
 * Implementation of PlugInterface methods
 *
 *************************************************************************************************/
    
    /**
     * @param InterfaceObject $ifc
     * @param Atom $srcAtom
     * @return mixed
     */
    public function executeIfcExpression(InterfaceObject $ifc, Atom $srcAtom = null){
        $srcAtomId = $this->getDBRepresentation($srcAtom);
        if(strpos($ifc->query, '_SRCATOM') !== false){
            $query = str_replace('_SRCATOM', $srcAtomId, $ifc->query);
        }else{
            $query = "SELECT DISTINCT * FROM ({$ifc->query}) AS `results` WHERE `src` = '{$srcAtomId}' AND `tgt` IS NOT NULL";
        }
        return $this->Exe($query);
    }
    
    /**
     * @param ViewSegment $view
     * @param Atom $srcAtom
     * @return mixed
     */
    public function executeViewExpression(ViewSegment $view, Atom $srcAtom = null){
        $srcAtomId = $this->getDBRepresentation($srcAtom);
        $query = "SELECT DISTINCT `tgt` FROM ({$view->expSQL}) AS `results` WHERE `src` = '{$srcAtomId}' AND `tgt` IS NOT NULL";
        return array_column((array) $this->Exe($query), 'tgt');
    }
	
/**************************************************************************************************
 *
 * Helper functions
 *
 *************************************************************************************************/
    
    /**
     * Check if insert/update/delete function resulted in updated record(s). If not, report warning (or throw exception) to indicate that something is going wrong
     * @throws Exception when no records are affected and application is not in production mode
     * @return void
     */
    private function checkForAffectedRows(){
        if($this->db_link->affected_rows == 0){
            if(Config::get('productionEnv')){
                $this->logger->warning("Oops.. something went wrong: No recors affected in database");
            }else{
                throw new Exception ("Oops.. something went wrong: No records affected in database", 500);
            }
        } 
    }
}

?>