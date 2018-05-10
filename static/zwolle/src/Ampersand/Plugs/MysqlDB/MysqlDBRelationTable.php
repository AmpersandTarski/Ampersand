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

class MysqlDBRelationTable extends MysqlDBTable
{

    /**
     *
     * @var \Ampersand\Plugs\MysqlDB\MysqlDBTableCol
     */
    private $srcCol = null;

    /**
     *
     * @var \Ampersand\Plugs\MysqlDB\MysqlDBTableCol
     */
    private $tgtCol = null;

    /**
     * Specifies if this relation is administrated in the table of the src concept ('src'), the tgt concept ('tgt') or its own n-n table (null)
     *
     * @var string
     */
    public $tableOf;

    /**
     * Constructor of RelationTable
     *
     * @param string $name
     * @param string|null $tableOf ('src', 'tgt' or null)
     */
    public function __construct(string $name, string $tableOf = null)
    {
        parent::__construct($name);

        switch ($tableOf) {
            case 'src':
            case 'tgt':
            case null:
                $this->tableOf = $tableOf;
                break;
            default:
                throw new Exception("Unknown tableOf value '{$tableOf}' specified for RelationTable {$this->name}", 500);
        }
    }

    /**
     *
     * @param \Ampersand\Plugs\MysqlDB\MysqlDBTableCol $col
     * @return void
     */
    public function addSrcCol(MysqlDBTableCol $col)
    {
        $this->srcCol = $col;
        $this->cols[$col->name] = $col;
    }

    /**
     *
     * @param \Ampersand\Plugs\MysqlDB\MysqlDBTableCol $col
     * @return void
     */
    public function addTgtCol(MysqlDBTableCol $col)
    {
        $this->tgtCol = $col;
        $this->cols[$col->name] = $col;
    }

    /**
     *
     * @throws \Exception when src column is not defined
     * @return \Ampersand\Plugs\MysqlDB\MysqlDBTableCol
     */
    public function srcCol(): MysqlDBTableCol
    {
        if (is_null($this->srcCol)) {
            throw new Exception("Src column for RelationTable {$this->name} not defined", 500);
        }
        return $this->srcCol;
    }

    /**
     *
     * @throws \Exception when tgt column is not defined
     * @return MysqlDBTableCol
     */
    public function tgtCol(): MysqlDBTableCol
    {
        if (is_null($this->tgtCol)) {
            throw new Exception("Tgt column for RelationTable {$this->name} not defined", 500);
        }
        return $this->tgtCol;
    }
}
