<?php
// Function is used by ExecEngine class. Not to be used in Ampersand scripts!!
function phpArgumentInterpreter($arg){
	// if php function is provided, evaluate this. Note! security hazard.
	// only 1 php statement can be executed, due to semicolon issue, see below
	// e.g. {php}date(DATE_ISO8601) returns the current datetime in ISO 8601 date format
	if(substr($arg, 0, 5) == '{php}') {
		$code = 'return('.substr($arg, 5).');'; // PHP statements must be properly terminated using a semicolon, but the semicolon is already used to seperate the parameters
		$arg = eval($code);
			
	}
	
	return $arg;
	
}

?>