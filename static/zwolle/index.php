<?php
try{
	require_once (__DIR__ . '/fw/includes.php');

	$viewer = new Viewer();
	print $viewer;
	
}catch(Exception $e){
	print $e->getMessage();
}
?>