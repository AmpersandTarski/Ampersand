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
class DatabaseTableCol 
{
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