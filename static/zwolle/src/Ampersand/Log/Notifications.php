<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Log;

use Exception;
use Ampersand\Log\Logger;
use Ampersand\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Notifications {
	
	private static $errors = array();
	private static $invariants = array();
	private static $warnings = array();
	private static $signals = array();
	private static $infos = array();	
	private static $successes = array();

/**************************************************************************************************
 *
 * Notifications for: user logs (info, notice (success), warning and error)
 *
 *************************************************************************************************/
	
	/**
	 * Add notifications from userlog  user (e.g. catched exceptions)
	 * DON't use this function in code. Log via userlogger instead => Logger::getUserLogger()->...
	 * @param int $level
	 * @param string $message
	 * @return void
	 * @throws Exception when notification level is not supported
	 */
	public static function addNotification($level, $message){
	    switch($level){
	        case 200 : // Info
	            self::addInfo($message);
	            break;
	        case 250 : // Notice
	            self::addSuccess($message);
	            break;
	        case 300 : // Warning
	            self::addWarning($message);
	            break;
	        case 400 : // Error
	        case 500 : // Critical
	        case 550 : // Alert
	        case 600 : // Emergency
	            self::addError($message);
	            break;
	        default :
	            throw new Exception("Unsupported notification level: {$level}", 500);
	    }
	}
	
	/**
	 * Add error notification for user
	 * @param string $message
	 * @return void
	 */
	private static function addError($message){
		$errorHash = hash('md5', $message);
		
		self::$errors[$errorHash]['message'] = $message;
		self::$errors[$errorHash]['count']++;
		
	}
	
	/**
	 * Add warning notification for user
	 * @param string $message
	 * @return void
	 */
	private static function addWarning($message){
	    $hash = hash('md5', $message);
	    
	    self::$warnings[$hash]['message'] = $message;
	    self::$warnings[$hash]['count']++;
	}
	
	/**
	 * Add success notification for user
	 * @param string $message
	 * @return void
	 */
	private static function addSuccess($message){
	    self::$successes[] = array('message' => $message);
	}
	
	/**
	 * Add info notification for user 
	 * @param string $message
	 * @return void
	 */
	private static function addInfo($message){
	    self::$infos[] = array('message' => $message);
	}

/**************************************************************************************************
 * 
 * Notifications for: invariant and signal rule violations
 * 
 *************************************************************************************************/
	/**
	 * 
	 * @param Rule\Violation $violation
	 */
	public static function addInvariant($violation){
		$hash = hash('md5', $violation->rule->id);
			
		self::$invariants[$hash]['ruleMessage'] = $violation->rule->getViolationMessage();
		self::$invariants[$hash]['tuples'][] = array('violationMessage' => ($violationMessage = $violation->getViolationMessage()));
		
		Logger::getLogger('INVARIANT')->info("'{$violationMessage}' RULE: '{$violation->rule->__toString()}'");
	}
	
    /**
     * 
     * @param Rule\Violation $violation
     */
	public static function addSignal($violation){
		$ruleHash = hash('md5', $violation->rule->id);
		
		self::$signals[$ruleHash]['ruleMessage'] = $violation->rule->getViolationMessage();
		self::$signals[$ruleHash]['tuples'][] = array('violationMessage' => ($violationMessage = $violation->getViolationMessage())
		                                                ,'links' => $violation->getLinks());
		Logger::getLogger('SIGNAL')->info("'{$violationMessage}' RULE: '{$violation->rule->__toString()}'");
	}
	
/**************************************************************************************************
 *
 * Get notifications and default settings
 *
 *************************************************************************************************/
	
	public static function getAll(){
	    return array( 'errors' => array_values(self::$errors)
	                , 'warnings' => array_values(self::$warnings)
	                , 'infos' => array_values(self::$infos)
	                , 'successes' => array_values(self::$successes)
	                , 'invariants' => array_values(self::$invariants)
	                , 'signals' => array_values(self::$signals)
	                );
	}
	
	public static function getDefaultSettings(){
		return array('switchShowSignals' 	    => Config::get('defaultShowSignals', 'notifications')
					,'switchShowInfos'			=> Config::get('defaultShowInfos', 'notifications')
					,'switchShowSuccesses'		=> Config::get('defaultShowSuccesses', 'notifications')
					,'switchAutoHideSuccesses'	=> Config::get('defaultAutoHideSuccesses', 'notifications')
					,'switchShowErrors'			=> Config::get('defaultShowErrors', 'notifications')
		            ,'switchShowWarnings'       => Config::get('defaultShowWarnings', 'notifications')
					,'switchShowInvariants'		=> Config::get('defaultShowInvariants', 'notifications')
					);
	}
}
?>