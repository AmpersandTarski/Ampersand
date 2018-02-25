<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs\MysqlDB;

use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class MysqlDBTableCol
{
    /**
     * Name/header of database column
     *
     * @var string
     */
    public $name;

    /**
     * Specifies if value in this database column can be NULL
     *
     * @var bool|NULL
     */
    public $null;

    /**
     * Specifies if this database column has uniquness constraint (i.e. no duplicates may exist in all rows)
     * @var bool|NULL
     */
    public $unique;

    /**
     * Constructor of Database table column
     *
     * @param string $name
     * @param bool $null
     * @param bool $unique
     */
    public function __construct(string $name, bool $null = null, bool $unique = null)
    {
        if ($name == '') {
            throw new Exception("Database table column name is an empty string", 500);
        }
        $this->name = $name;
        $this->null = $null;
        $this->unique = $unique;
    }
}
