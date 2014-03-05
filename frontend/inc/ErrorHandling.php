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
	public static function addViolation($message){
		self::$violations[] = $message;
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
	

}




?>