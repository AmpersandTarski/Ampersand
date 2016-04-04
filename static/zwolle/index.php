<?php

use Ampersand\AngularApp;

require_once (__DIR__ . '/src/bootstrap.php');

try{
	print new AngularApp();
	
}catch(Exception $e){
	print $e->getMessage();
}
?>