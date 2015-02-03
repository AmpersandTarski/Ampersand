<?php

class Notifications {
	
	static $errors = array();
	static $invariants = array();
	static $violations = array();
	static $infos = array();	
	static $successes = array();
	static $logs = array();
	
	public static function addError($message){
		$errorHash = hash('md5', $message);
		
		self::$errors[$errorHash]['message'] = $message;
		self::$errors[$errorHash]['count']++;
		self::addLog($message, 'ERROR');
	}
	
	public static function addInvariant($message){
		self::$invariants[]['message'] = $message;
		self::addLog($message, 'INVARIANT');
	}
	
	public static function addViolation($rule, $srcAtom, $tgtAtom){
		$session = Session::singleton();
		
		$ruleHash = hash('md5', $rule['name']);
		
		$ruleMessage = $rule['message'] ? $rule['message'] : "Violation of rule '".$rule['name']."'";
		
		$pairView = RuleEngine::getPairView($srcAtom, $rule['srcConcept'], $tgtAtom, $rule['tgtConcept'], $rule['pairView']); 
		
		self::$violations[$ruleHash]['ruleMessage'] = $ruleMessage;
		self::$violations[$ruleHash]['interfaceNames'] = $pairView['interfaceNames'];
		
		$violationMessage = empty($pairView['violationMessage']) ? $srcAtom . " - " . $tgtAtom : $pairView['violationMessage'];
		
		// Make links to interfaces
		$links = array();
		foreach ($session->role->getInterfaces(null, $rule['srcConcept']) as $interface){
			$links[] = '#/' . $interface->name . '/' . $srcAtom;
		}
		foreach ($session->role->getInterfaces(null, $rule['tgtConcept']) as $interface){
			$links[] = '#/' . $interface->name . '/' . $tgtAtom;
		}
		$links = array_unique($links);
		
		self::$violations[$ruleHash]['tuples'][] = array('violationMessage' => $violationMessage
														,'links' => $links);
		 
		self::addLog($violationMessage . ' - ' . $violationMessage, 'VIOLATION');
	}
	
	public static function addInfo($message, $id = null){
		
		if(isset($id)){
			$idHash = hash('md5', $id); // ID can be integer, but also string
			
			// Set message of info, in case this is not done yet (use: $id for this)
			if(empty(self::$infos[$idHash]['message'])) self::$infos[$idHash]['message'] = $id;
			
			// Set message of row (use: $message)
			self::$infos[$idHash]['rows'][] = $message;
			
			// Add INFO also to logging
			self::addLog(self::$infos[$idHash]['message'] .' - ' . $message, 'INFO');
			
			return $id;
		}else{
			self::addLog($message, 'INFO');
			self::$infos[]['message'] = $message;
			end(self::$infos); // pointer to end of array (i.e. new  inserted element)
			return key(self::$infos); // return key of current element
		}
		
	}
	
	public static function addSuccess($message, $id = null){
		
		if(isset($id)){ // ID can be integer, but also string
			self::$successes[$id]['rows'][] = $message;
			self::addLog(self::$successes[$id]['message'] .' - ' . $message, 'SUCCESS');;
			return $id;
		}else{
			self::addLog($message, 'SUCCESS');
			self::$successes[]['message'] = $message;
			end(self::$successes); // pointer to end of array (i.e. new  inserted element)
			return key(self::$successes); // return key of current element
		}
		
	}
	
	public static function addLog($message, $type = 'LOG'){
		self::$logs[] = $type . ': ' . $message;
	}
	
	public static function getErrors(){
		return array_values(self::$errors);
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