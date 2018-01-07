<?php

use Ampersand\AngularApp;

try{
    require_once (__DIR__ . '/src/bootstrap.php');
    /** @var \Pimple\Container $container */
    global $container;
    print $container['angular_app']->buildHtml();
    
}catch(Exception $e){
    print $e->getMessage();
}
