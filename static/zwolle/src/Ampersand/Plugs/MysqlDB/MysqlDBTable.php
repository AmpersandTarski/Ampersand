<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs\MysqlDB;

use Exception;
use Ampersand\Plugs\MysqlDB\MysqlDBTableCol;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class MysqlDBTable
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
     *
     * @var string $allAtomsQuery
     */
    public $allAtomsQuery;

    /**
     * Constructor of Database table
     *
     * @param string $name
     */
    public function __construct(string $name)
    {
        if ($name == '') {
            throw new Exception("Database table name is an empty string", 500);
        }
        $this->name = $name;
    }

    /**
     * Add database table column object to this table
     *
     * @param \Ampersand\Plugs\MysqlDB\MysqlDBTableCol $col
     * @return void
     */
    public function addCol(MysqlDBTableCol $col)
    {
        $this->cols[$col->name] = $col;
    }

    /**
     * Get all col objects for this table
     *
     * @throws \Exception when no columns are defined for this table
     * @return \Ampersand\Plugs\MysqlDB\MysqlDBTableCol[]
     */
    public function getCols(): array
    {
        if (empty($this->cols)) {
            throw new Exception("No column defined for table '{$this->name}'", 500);
        }
        return $this->cols;
    }

    /**
     * Returns names of all table cols
     *
     * @return string[]
     */
    public function getColNames(): array
    {
        $colNames = [];
        foreach ($this->getCols() as $col) {
            $colNames[] = $col->name;
        }
        return $colNames;
    }

    /**
     * Return col object with given column name
     *
     * @param string $colName
     * @throws \Exception when col does not exist
     * @return \Ampersand\Plugs\MysqlDB\MysqlDBTableCol
     */
    public function getCol(string $colName): MysqlDBTableCol
    {
        if (!array_key_exists($colName, $this->getCols())) {
            throw new Exception("Col '{$colName}' does not exist in table '{$this->name}'", 500);
        }
        return $this->getCols()[$colName];
    }

    /**
     * Return first registered col object
     *
     * @return \Ampersand\Plugs\MysqlDB\MysqlDBTableCol
     */
    public function getFirstCol(): MysqlDBTableCol
    {
        $cols = $this->getCols();
        return reset($cols);
    }
}
