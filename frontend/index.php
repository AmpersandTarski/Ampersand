<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);

require_once (__DIR__ . '/inc/includes.php');


// SESSION handling
$session = Session::singleton(); // initialize both a PHP session and an Ampersand session as soon as we can.
if(isset($_REQUEST['resetSession'])){
	$session->destroySession();	// unset $_SESSION variables and Ampersand SESSION atom
	$session = Session::singleton(); // initialize new session
}

// ROLE
if(isset($_REQUEST['role'])){	// new role selected
	 $roleId = $_REQUEST['role'];
}else{ // no change, or default role
	$roleId = null;
}		
$session->setRole($roleId);


$viewer = new Viewer();
print $viewer;

?>