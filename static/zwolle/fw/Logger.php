<?php

namespace Ampersand;

class Logger {
    
    private static $loggers = array();
    
    /**
     * 
     * @param string $channel
     * @return \Psr\Log\LoggerInterface
     */
    public static function getLogger($channel){
        
        if(isset(self::$loggers[$channel])) return self::$loggers[$channel];
        else { 
            $logger = new \Monolog\Logger($channel);
            
            // Add generic handlers (i.e. for all channels)
            foreach((array)\Config::get('genericHandlers', 'logger') as $handler) $logger->pushHandler($handler);
            
            // Add handlers for specific channels
            $channelHandlers = \Config::get('channelHandlers', 'logger');
            foreach((array)$channelHandlers[$channel] as $handler) $logger->pushHandler($handler);
            
            self::$loggers[$channel] = $logger;
            
            return $logger;
        
        }
    }
    
}

?>