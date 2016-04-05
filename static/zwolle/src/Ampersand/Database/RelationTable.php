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

class RelationTable extends DatabaseTable
{

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

?>