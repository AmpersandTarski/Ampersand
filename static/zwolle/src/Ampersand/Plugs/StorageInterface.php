<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs;

use Ampersand\Transaction;

/**
 * Interface for storage implementations
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface StorageInterface {
    
    public function getLabel();

    public function startTransaction(Transaction $transaction);
    
    public function commitTransaction(Transaction $transaction);
    
    public function rollbackTransaction(Transaction $transaction);

    public function reinstallStorage();
}
