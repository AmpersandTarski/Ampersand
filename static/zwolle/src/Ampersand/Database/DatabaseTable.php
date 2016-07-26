<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Database;

use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class DatabaseTable
{
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

?>