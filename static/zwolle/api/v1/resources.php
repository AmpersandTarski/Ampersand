<?php

use Ampersand\Misc\Config;
use Ampersand\Core\Concept;
use Ampersand\Interfacing\Resource;
use Ampersand\Interfacing\Options;
use Ampersand\Interfacing\InterfaceController;
use Slim\Http\Request;
use Slim\Http\Response;

/**
 * @var \Slim\Slim $app
 */
global $app;

/** 
 * @var \Pimple\Container $container
 */
global $container;

/**************************************************************************************************
 *
 * resource calls WITHOUT interfaces
 *
 *************************************************************************************************/

$app->get('/resource', function(Request $request, Response $response, $args = []) use ($app, $container) {
    if(Config::get('productionEnv')) throw new Exception ("List of all resource types is not available in production environment", 403);
    
    $content = array_values(
        array_map(function($cpt){
            return $cpt->label; // only show label of resource types
        }, array_filter(Concept::getAllConcepts(), function($cpt){
            return $cpt->isObject(); // filter concepts without a representation (i.e. resource types)
    })));
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/resource/:resourceType', function (Request $request, Response $response, $args = []) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    $concept = Concept::getConcept($resourceType);
    
    // Checks
    if(!$concept->isObject()) throw new Exception ("Resource type not found", 404);
    if($concept->isSession()) throw new Exception ("Resource type not found", 404); // Prevent users to list other sessions
    if(!$ampersandApp->isEditableConcept($concept)) throw new Exception ("You do not have access for this call", 403);
    
    $resources = Resource::getAllResources($resourceType);
    
    print json_encode($resources, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->post('/resource/:resourceType', function (Request $request, Response $response, $args = []) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    $resource = Resource::makeNewResource($resourceType);

    $allowed = false;
    foreach ($ampersandApp->getAccessibleInterfaces() as $ifc) {
        if ($ifc->isRoot() && $ifc->crudC() && $ifc->tgtConcept == $resource->concept) {
            $allowed = true;
            break;
        }
    }
    if(!$allowed) throw new Exception ("You do not have access for this call", 403);
    
    // Don't save/commit new resource (yet)
    
    print json_encode($resource, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/resource/:resourceType/:resourceId', function (Request $request, Response $response, $args = []) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    $resource = Resource::makeResource($resourceId, $resourceType);
    
    // Checks
    if(!$ampersandApp->isEditableConcept($resource->concept)) throw new Exception ("You do not have access for this call", 403);
    if(!$resource->exists()) throw new Exception("Resource '{$resource}' not found", 404);

    print json_encode($resource, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


/**************************************************************************************************
 *
 * resource calls WITH interfaces
 *
 *************************************************************************************************/

// GET for interfaces with expr[SESSION*..]
$app->get('/session/:ifcPath+', function(Request $request, Response $response, $args = []) use ($app, $container) {
    // Input
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');

    // Prepare
    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);
    $resource = $container['ampersand_app']->getSession()->getSessionResource();

    // Output
    print json_encode($controller->get($resource, $ifcPath, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// GET for interfaces that start with other resource
$app->get('/resource/:resourceType/:resourceId/:ifcPath+', function (Request $request, Response $response, $args = []) use ($app, $container) {
    // Input
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    
    // Prepare
    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);
    $resource = Resource::makeResource($resourceId, $resourceType);

    // Output
    print json_encode($controller->get($resource, $ifcPath, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// PUT, PATCH, POST for interfaces with expr[SESSION*..]
$app->map('/session/:ifcPath+', function (Request $request, Response $response, $args = []) use ($app, $container) {
    // Input
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    $body = $app->request->getBody();
    
    // Prepare
    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);
    $resource = $container['ampersand_app']->getSession()->getSessionResource();

    // Output
    switch ($app->request()->getMethod()) {
        case 'PUT':
            print json_encode($controller->put($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        case 'PATCH':
            print json_encode($controller->patch($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        case 'POST':
            print json_encode($controller->post($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        default:
            throw new Exception("Unsupported HTTP method", 500);
            break;
    }
})->via('PUT', 'PATCH', 'POST');

// PUT, PATCH, POST for interfaces that start with other resource
$app->map('/resource/:resourceType/:resourceId/:ifcPath+', function (Request $request, Response $response, $args = []) use ($app, $container) {
    // Input
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    $body = $app->request->getBody();
    
    // Prepare
    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);
    $resource = Resource::makeResource($resourceId, $resourceType);

    // Output
    switch ($app->request()->getMethod()) {
        case 'PUT':
            print json_encode($controller->put($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        case 'PATCH':
            print json_encode($controller->patch($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        case 'POST':
            print json_encode($controller->post($resource, $ifcPath, $body, $options, $depth), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            break;
        default:
            throw new Exception("Unsupported HTTP method", 500);
            break;
    }
})->via('PUT', 'PATCH', 'POST');

$app->delete('/session/:ifcPath+', function (Request $request, Response $response, $args = []) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    $resource = $ampersandApp->getSession()->getSessionResource();

    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);

    print json_encode($controller->delete($resource, $ifcPath), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->delete('/resource/:resourceType/:resourceId/:ifcPath+', function (Request $request, Response $response, $args = []) use ($app, $container) {
    $resource = Resource::makeResource($resourceId, $resourceType);

    $controller = new InterfaceController($container['ampersand_app'], $container['angular_app']);

    print json_encode($controller->delete($resource, $ifcPath), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});
