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

/* INTERFACE
if(isset($_REQUEST['interface'])){ // new interface selected
	$interfaceName = $_REQUEST['interface'];
}else{ // no change, or default interface
	$interfaceName = null;
}
$session->setInterface($interfaceName);
*/

/* ATOM
if(isset($_REQUEST['atom'])){ // new atom selected
	$atomId = $_REQUEST['atom'];
	if(empty($atomId)) $atomId = null;
}else{ // no change, or default atom
	$atomId = null;
}	
$session->setAtom($atomId);
*/	

$viewer = new Viewer();
print $viewer;

?>