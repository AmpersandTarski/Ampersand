<?php

use Ampersand\AngularApp;

try{
    require_once (__DIR__ . '/src/bootstrap.php');
	print new AngularApp();
	
}catch(Exception $e){
	print $e->getMessage();
}
?>