<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Log;

use Monolog\Handler\AbstractProcessingHandler;
use Ampersand\Log\Notifications;

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