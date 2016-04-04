<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Monolog\Handler\AbstractProcessingHandler;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class NotificationHandler extends AbstractProcessingHandler
{
    protected function write(array $record){
        Notifications::addNotification($record['level'], $record['message']);
    }
}

?>