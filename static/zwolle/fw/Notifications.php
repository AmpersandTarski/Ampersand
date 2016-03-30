<?php

// Default notification settings
Config::set('defaultShowViolations', 'notifications', true);
Config::set('defaultShowInfos', 'notifications', false);
Config::set('defaultShowSuccesses', 'notifications', true);
Config::set('defaultAutoHideSuccesses', 'notifications', true);
Config::set('defaultShowErrors', 'notifications', true);
Config::set('defaultShowInvariants', 'notifications', true);

// TODO: move notifications functions to new logger class
class Notifications {
	
	static $errors = array();
	static $invariants = array();
	static $warnings = array();
	static $violations = array();
	static $infos = array();	
	static $successes = array();
	
	/**
	 * Add notifications for user (e.g. catched exceptions)
	 * @param int $level
	 * @param string $message
	 */
	public static function addNotification($level, $message){
	    switch($level){
	        case 200 : // Info
	            self::addInfo($message);
	            break;
	        case 250 : // Notice
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
	 * DEPRECATED FUNCTION
	 * @param unknown $message
	 * @param string $code
	 */
	public static function addError($message, $code = null){
		$errorHash = hash('md5', $message);
		
		self::$errors[$errorHash]['message'] = $message;
		self::$errors[$errorHash]['code'] = $code;
		self::$errors[$errorHash]['count']++;
		// Provide backtrace only for the first time of this error occurrence to safe memory
		if(Config::get('debugMode') && self::$errors[$errorHash]['count'] <= 1) self::$errors[$errorHash]['details'] = '<pre>' . print_r(debug_backtrace(), true) . '</pre>';
		self::addLog($message, 'ERROR');
	}
	
	/**
	 * DEPRECATED FUNCTION
	 * @param Exception $e
	 */
	public static function addErrorException($e){
	    $errorHash = hash('md5', $e->getMessage());
	    
	    self::$errors[$errorHash]['message'] = $e->getMessage();
	    self::$errors[$errorHash]['code'] = $e->getCode();
	    self::$errors[$errorHash]['count']++;
	    if(Config::get('debugMode')) self::$errors[$errorHash]['details'] = nl2br($e->getTraceAsString());
	    self::addLog($e->getMessage(), 'ERROR');
	}
	
	/**
	 * Add warning message for user
	 * @param string $message
	 * @param int $code
	 */
	private static function addWarning($message){
	    $hash = hash('md5', $message);
	    
	    self::$warnings[$hash]['message'] = $message;
	    self::$warnings[$hash]['count']++;
	}
	
	/**
	 * 
	 * @param Violation $violation
	 */
	public static function addInvariant($violation){
		$hash = hash('md5', $violation->rule->id);
			
		self::$invariants[$hash]['ruleMessage'] = $violation->rule->getViolationMessage();
		self::$invariants[$hash]['tuples'][] = array('violationMessage' => ($violationMessage = $violation->getViolationMessage()));
		
		self::addLog($violationMessage, 'INVARIANT');
	}
	
    /**
     * 
     * @param Violation $violation
     */
	public static function addSignal($violation){
		$ruleHash = hash('md5', $violation->rule->id);
		
		self::$violations[$ruleHash]['ruleMessage'] = $violation->rule->getViolationMessage();
		self::$violations[$ruleHash]['tuples'][] = array('violationMessage' => ($violationMessage = $violation->getViolationMessage())
		                                                ,'links' => $violation->getLinks());
		self::addLog($violationMessage, 'VIOLATION');
	}
	
	public static function addInfo($message, $id = null, $aggregatedMessage = null){
		
		if(isset($id)){ // ID can be integer, but also string
			self::$infos[$id]['rows'][] = $message;
			self::$infos[$id]['count'] = count(self::$infos[$id]['rows']);
			if(!is_null($aggregatedMessage)) self::$infos[$id]['message'] = $aggregatedMessage;
			
			self::addLog(self::$infos[$id]['message'] .' - ' . $message, 'INFO');
			
			return $id;
		}else{
			self::$infos[]['message'] = $message;
			
			self::addLog($message, 'INFO');
			
			end(self::$infos); // pointer to end of array (i.e. new  inserted element)
			return key(self::$infos); // return key of current element
		}
		
	}
	
	public static function addSuccess($message, $id = null, $aggregatedMessage = null){
		
		if(isset($id)){ // ID can be integer, but also string
			self::$successes[$id]['rows'][] = $message;
			if(!is_null($aggregatedMessage)) self::$successes[$id]['message'] = $aggregatedMessage;
			
			self::addLog(self::$successes[$id]['message'] .' - ' . $message, 'SUCCESS');
			
			return $id;
		}else{
			self::$successes[]['message'] = $message;
			
			self::addLog($message, 'SUCCESS');
			
			end(self::$successes); // pointer to end of array (i.e. new  inserted element)
			return key(self::$successes); // return key of current element
		}
		
	}
	
	/**
	 * DEPRECATED FUNCTION
	 * @param string $message
	 * @param string $type
	 */
	public static function addLog($message, $type = 'LOG'){
		
	}
	
	public static function getErrors(){
		return array_values(self::$errors);
	}
	public static function getWarnings(){
	    return array_values(self::$warnings);
	}
	public static function getInvariants(){
		return array_values(self::$invariants);
	}
	public static function getViolations(){
		return array_values(self::$violations);
	}
	public static function getInfos(){
		return array_values(self::$infos);
	}
	public static function getSuccesses(){
		return array_values(self::$successes);
	}
	
	public static function getAll(){
		$all['errors'] = self::getErrors();
		$all['invariants'] = self::getInvariants();
		$all['warnings'] = self::getWarnings();
		$all['violations'] = self::getViolations();
		$all['infos'] = self::getInfos();
		$all['successes'] = self::getSuccesses();
		
		return $all;
	}
	
	public static function getDefaultSettings(){
		return array('switchShowViolations' 	=> Config::get('defaultShowViolations', 'notifications')
					,'switchShowInfos'			=> Config::get('defaultShowInfos', 'notifications')
					,'switchShowSuccesses'		=> Config::get('defaultShowSuccesses', 'notifications')
					,'switchAutoHideSuccesses'	=> Config::get('defaultAutoHideSuccesses', 'notifications')
					,'switchShowErrors'			=> Config::get('defaultShowErrors', 'notifications')
					,'switchShowInvariants'		=> Config::get('defaultShowInvariants', 'notifications')
					);
	}
}
?>