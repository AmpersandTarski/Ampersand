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
	
	public static function addInvariant($rule, $srcAtom, $tgtAtom){
		$session = Session::singleton();
		
		$ruleHash = hash('md5', $rule['name']);
		
		$ruleMessage = $rule['message'] ? $rule['message'] : "Violation of rule '".$rule['name']."'";
		
		$pairView = RuleEngine::getPairView($srcAtom, $rule['srcConcept'], $tgtAtom, $rule['tgtConcept'], $rule['pairView']);
		
		self::$invariants[$ruleHash]['ruleMessage'] = $ruleMessage;
		
		$violationMessage = empty($pairView['violationMessage']) ? $srcAtom . " - " . $tgtAtom : $pairView['violationMessage'];
		
		self::$invariants[$ruleHash]['tuples'][] = array('violationMessage' => $violationMessage);
			
		self::addLog($violationMessage . ' - ' . $violationMessage, 'INVARIANT');
	}
	
	public static function addViolation($rule, $srcAtom, $tgtAtom){
		$session = Session::singleton();
		
		$ruleHash = hash('md5', $rule['name']);
		
		$ruleMessage = $rule['message'] ? $rule['message'] : "Violation of rule '".$rule['name']."'";
		
		$pairView = RuleEngine::getPairView($srcAtom, $rule['srcConcept'], $tgtAtom, $rule['tgtConcept'], $rule['pairView']); 
		
		self::$violations[$ruleHash]['ruleMessage'] = $ruleMessage;
		self::$violations[$ruleHash]['interfaceIds'] = $pairView['interfaceIds'];
		
		$violationMessage = empty($pairView['violationMessage']) ? $srcAtom . " - " . $tgtAtom : $pairView['violationMessage'];
		
		// Make links to interfaces
		$links = array();
		if(isset($session->role)){
			foreach ($session->role->getInterfacesToReadConcept($rule['srcConcept']) as $interface){
				$links[] = '#/' . $interface->id . '/' . $srcAtom;
			}
			foreach ($session->role->getInterfacesToReadConcept($rule['tgtConcept']) as $interface){
				$links[] = '#/' . $interface->id . '/' . $tgtAtom;
			}
			$links = array_unique($links);
		}
		
		self::$violations[$ruleHash]['tuples'][] = array('violationMessage' => $violationMessage
														,'links' => $links);
		 
		self::addLog($violationMessage . ' - ' . $violationMessage, 'VIOLATION');
	}
	
	public static function addInfo($message, $id = null, $aggregatedMessage = null){
		
		if(isset($id)){ // ID can be integer, but also string
			self::addLog(self::$infos[$id]['message'] .' - ' . $message, 'INFO');
			self::$infos[$id]['rows'][] = $message;
			self::$infos[$id]['count'] = count(self::$infos[$id]['rows']);
			if(!is_null($aggregatedMessage)) self::$infos[$id]['message'] = $aggregatedMessage;
			
			return $id;
		}else{
			self::addLog($message, 'INFO');
			self::$infos[]['message'] = $message;
			
			end(self::$infos); // pointer to end of array (i.e. new  inserted element)
			return key(self::$infos); // return key of current element
		}
		
	}
	
	public static function addSuccess($message, $id = null, $aggregatedMessage = null){
		
		if(isset($id)){ // ID can be integer, but also string
			self::addLog(self::$successes[$id]['message'] .' - ' . $message, 'SUCCESS');
			self::$successes[$id]['rows'][] = $message;
			if(!is_null($aggregatedMessage)) self::$successes[$id]['message'] = $aggregatedMessage;
			
			return $id;
		}else{
			self::addLog($message, 'SUCCESS');
			self::$successes[]['message'] = $message;
			
			end(self::$successes); // pointer to end of array (i.e. new  inserted element)
			return key(self::$successes); // return key of current element
		}
		
	}
	
	public static function addLog($message, $type = 'LOG'){
		self::$logs[] = array('timestamp' => microtime(true), 'type' => $type, 'message' => $message);
	}
	
	public static function getErrors(){
		return array_values(self::$errors);
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
	public static function getLogs(){
		return self::$logs;
	}
	
	public static function getAll(){
		$all['errors'] = self::getErrors();
		$all['invariants'] = self::getInvariants();
		$all['violations'] = self::getViolations();
		$all['infos'] = self::getInfos();
		$all['successes'] = self::getSuccesses();
		$all['logs'] = Config::get('productionEnv') ? array(array('type' => 'LOG', 'message' => 'Log is disabled in production environment')) : self::getLogs();
		
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