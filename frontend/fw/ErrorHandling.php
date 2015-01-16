<?php

class ErrorHandling { 	// TODO: rename to ErrorHandler? integrate with php error handling system
	
	static $errors = array();
	static $invariants = array();
	static $violations = array();
	static $infos = array();	
	static $successes = array();
	static $logs = array();
	
	public static function addError($message){
		self::$errors[]['message'] = $message;
		self::$logs[]['message'] = $message;
	}
	public static function addInvariant($message){
		self::$invariants[]['message'] = $message;
		self::$logs[]['message'] = $message;
	}
	public static function addViolation($rule, $srcAtom, $tgtAtom){
		$session = Session::singleton();
		
		$ruleHash = hash('md5', $rule['name']);
		
		$violationMessage = $rule['message'] ? $rule['message'] : "Violation of rule '".$rule['name']."'";
		$rowMessage = $srcAtom . ' - ' . $tgtAtom;
		
		self::$violations[$ruleHash]['violationMessage'] = $violationMessage;
		self::$violations[$ruleHash]['tuples'][] = $rowMessage;
		
		// self::$violations[] = array('violationMessage' => $violationMessage, 'tuples' => array($rowMessage)); //TODO: violations of the same rule in one array 
		self::$logs[]['message'] = $violationMessage . ' - ' . $rowMessage;
	}
	public static function addInfo($message){
		self::$infos[]['message'] = $message;
		self::$logs[]['message'] = $message;
	}
	public static function addSuccess($message, $id = null){
		if(isset($id)){ // ID can be integer, but also string
			self::$successes[$id]['rows'][] = $message;
		}else{
			self::$successes[]['message'] = $message;
		}
		self::$logs[]['message'] = $message;
		end(self::$successes);
		return key(self::$successes);
	}
	
	public static function addLog($message){
		self::$logs[] = $message;
	}
	
	public static function getErrors(){
		return self::$errors;
	}
	public static function getInvariants(){
		return self::$invariants;
	}
	public static function getViolations(){
		return array_values(self::$violations);
	}
	public static function getInfos(){
		return self::$infos;
	}
	public static function getSuccesses(){
		return self::$successes;
	}
	public static function getLogs(){
		return self::$logs;
	}
	
	public static function getAll(){
		$all['errors'] = self::getErrors();
		$all['invariants'] = self::getInvariants();
		$all['violations'] = self::getViolations();
		$all['infos'] = self::getInfos();
		$all['successes'] = self::getSuccesses();
		$all['logs'] = self::getLogs();
		
		return $all;
	}
	

}




?>