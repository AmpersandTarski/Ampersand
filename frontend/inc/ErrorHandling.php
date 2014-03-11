<?php

class ErrorHandling {
	
	static $errors = array();
	static $invariants = array();
	static $violations = array();
	static $notifications = array();	
	static $successes = array();
	
	public static function addError($message){
		self::$errors[] = $message;
	}
	public static function addInvariant($message){
		self::$invariants[] = $message;
	}
	public static function addViolation($rule, $srcAtom, $tgtAtom){
		$session = Session::singleton();
		
		// TODO: move output related things to Viewer. Move ErrorHandling to Session??
		$violationMessage = $rule['message'] ? $rule['messsage'] : "Violation of rule '".$rule['name']."'";
		$rowMessage = '<a href="?interface='.current($session->role->getInterfaces(null, $rule['srcConcept']))->name.'&atom='.$srcAtom.'">' . Viewer::viewAtom($srcAtom, $rule['srcConcept']) . ' ('. $rule['srcConcept'] .')</a> - ' . Viewer::viewAtom($tgtAtom, $rule['tgtConcept']); // TODO: support for multiple interface. Now the first (using current()) is picked.
		
		self::$violations[$violationMessage][] = $rowMessage;
	}
	public static function addNotification($message){
		self::$notifications[] = $message;
	}
	public static function addSuccess($message){
		self::$successes[] = $message;
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
	public static function getNotifications(){
		return self::$notifications;
	}
	public static function getSuccesses(){
		return self::$successes;
	}
	
	// TODO: can be deleted when ErrorHandling is not static anymore but instantiated in Session class.
	public static function getAll(){
		$all['errors'] = self::$errors;
		$all['invariants'] = self::$invariants;
		$all['violations'] = self::$violations;
		$all['notifications'] = self::$notifications;
		$all['successes'] = self::$successes;
		
		return $all;
		
	}
	

}




?>