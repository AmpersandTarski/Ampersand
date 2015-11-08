<?php
try{
	require_once (__DIR__ . '/fw/includes.php');

	print new AngularApp();
	
}catch(Exception $e){
	print $e->getMessage();
}
?>