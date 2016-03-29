<?php

$app->get('/interfaces', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("List of all interfaces is not allowed in production environment", 403);

	$session = Session::singleton();
	$content = InterfaceObject::getAllInterfaces(); // Return all interfaces

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/interfaces/public', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("List of public interfaces is not allowed in production environment", 403);

	$session = Session::singleton();
	$content = InterfaceObject::getPublicInterfaces(); // Return all public interfaces
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});


?>