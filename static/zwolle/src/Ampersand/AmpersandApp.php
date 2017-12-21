<?php

namespace Ampersand;

class AmpersandApp
{
    /**
     * Specifies the required version of the localsettings file that
     * @const float
     */
    const REQ_LOCALSETTINGS_VERSION = 1.5;

    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;
    /**
     * Contains reference to ampersand application (singleton pattern)
     * @var AmpersandApp
     */
    private static $_instance = null;
    
    private function __construct(){
        $this->logger = Logger::getLogger('APPLICATION');
    }

    /**
     * Use Database::singleton() instead
     * Singleton pattern: private function to prevent any copy/clone of database instance
     */
    private function __clone(){}
    
    /**
     * Function to return the ampersand application instance
     * Singleton pattern: use this static function to get the single instance of this class
     * @return AmpersandApp
     */
    public static function singleton(){
        if(!is_object (self::$_instance)) self::$_instance = new AmpersandApp();
        else return self::$_instance;
    }
    }
}