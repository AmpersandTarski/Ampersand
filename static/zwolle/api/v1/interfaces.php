<?php

use Ampersand\Config;
use Ampersand\Interfacing\InterfaceObject;

global $app;

$app->get('/interfaces', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("List of all interfaces is not allowed in production environment", 403);

	$content = InterfaceObject::getAllInterfaces(); // Return all interfaces

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/interfaces/public', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("List of public interfaces is not allowed in production environment", 403);

	$content = InterfaceObject::getPublicInterfaces(); // Return all public interfaces
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});


?>