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
		
		// TODO: move output related things to Viewer. Move ErrorHandling to Session??
		$violationMessage = $rule['message'] ? $rule['message'] : "Violation of rule '".$rule['name']."'";
		$rowMessage = '<a href="?interface='.current($session->role->getInterfaces(null, $rule['srcConcept']))->name.'&atom='.$srcAtom.'">' . Viewer::viewAtom($srcAtom, $rule['srcConcept']) . ' ('. $rule['srcConcept'] .')</a> - ' . Viewer::viewAtom($tgtAtom, $rule['tgtConcept']); // TODO: support for multiple interface. Now the first (using current()) is picked.
		
		self::$violations[$violationMessage][] = $rowMessage;
		self::$logs[]['message'] = $rowMessage;
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
		return self::$violations;
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
	
	// TODO: can be deleted when ErrorHandling is not static anymore but instantiated in Viewer class.
	public static function getAll(){
		$all['errors'] = self::$errors;
		$all['invariants'] = self::$invariants;
		$all['violations'] = self::$violations;
		$all['infos'] = self::$infos;
		$all['successes'] = self::$successes;
		$all['logs'] = self::$logs;
		
		return $all;
		
	}
	

}




?>