<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);

$debug = false; // TODO: waar wordt dit nog gebruikt. Verplaatsen naar config

require_once (__DIR__ . '/inc/includes.php');


// SESSION handling
$session = Session::singleton(); // initialize both a PHP session and an Ampersand session as soon as we can.
if(isset($_REQUEST['resetSession'])){ // TODO: reset working not working properly. Refresh of page needed before reset has effect.
	$session->destroySession();	// unset $_SESSION variables and Ampersand SESSION atom
	$session = Session::singleton(); // initialize new session
}

// ROLE
if(isset($_REQUEST['role'])){	// new role selected
	 $roleId = $_REQUEST['role'];
}else{ // no change, or default
	$roleId = null;
}		
$session->setRole($roleId);

// INTERFACE
if(isset($_REQUEST['interface'])){ // new interface selected
	$interfaceName = $_REQUEST['interface'];
}elseif(isset($_SESSION['interface'])){ // interface already selected
	$interfaceName = $_SESSION['interface'];
}else{ // default interface
	$interfaceName = null;
}
$session->setInterface($interfaceName);

// ATOM
if(isset($_REQUEST['atom'])){ // new atom selected
	$atomId = $_REQUEST['atom'];
	if(empty($atomId)) $atomId = null;
}elseif(isset($_SESSION['atom'])){ // atom already selected
	$atomId = $_SESSION['atom'];
}else{ // default atom
	$atomId = null;
}	
$session->setAtom($atomId);	

// VIEWER
if(isset($_REQUEST['viewer'])){ // new viewer selected
	$viewerName = $_REQUEST['viewer'];
}elseif(isset($_SESSION['viewer'])){ // viewer already selected
	$viewerName = $_SESSION['viewer'];
}else{
	$viewerName = null; 
}
$session->setViewer($viewerName);

print $session->viewer;

?>