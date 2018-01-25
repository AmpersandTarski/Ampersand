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

$middleWare1 = function (Request $request, Response $response, callable $next) {
    // Overwrite default media type parser for application/json
    $request->registerMediaTypeParser('application/json', function ($input) {
        return json_decode($input, false); // set accoc param to false, this will return php stdClass object instead of array for json objects {}
    });
    return $next($request, $response);
};

$app->group('/resource', function () use ($container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];

    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];

    $this->get('', function(Request $request, Response $response, $args = []) {
        if(Config::get('productionEnv')) throw new Exception ("List of all resource types is not available in production environment", 403);
        
        $content = array_values(
            array_map(function($cpt){
                return $cpt->label; // only show label of resource types
            }, array_filter(Concept::getAllConcepts(), function($cpt){
                return $cpt->isObject(); // filter concepts without a representation (i.e. resource types)
        })));
        
        return $response->withJson($content, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/{resourceType}', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $concept = Concept::getConcept($args['resourceType']);
        
        // Checks
        if(!$concept->isObject()) throw new Exception ("Resource type not found", 404);
        if($concept->isSession()) throw new Exception ("Resource type not found", 404); // Prevent users to list other sessions
        if(!$ampersandApp->isEditableConcept($concept)) throw new Exception ("You do not have access for this call", 403);
        
        $resources = Resource::getAllResources($args['resourceType']);
        
        return $response->withJson($resources, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->post('/{resourceType}', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $resource = Resource::makeNewResource($args['resourceType']);

        $allowed = false;
        foreach ($ampersandApp->getAccessibleInterfaces() as $ifc) {
            if ($ifc->isRoot() && $ifc->crudC() && $ifc->tgtConcept == $resource->concept) {
                $allowed = true;
                break;
            }
        }
        if(!$allowed) throw new Exception ("You do not have access for this call", 403);
        
        // Don't save/commit new resource (yet)
        return $response->withJson($resource, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/{resourceType}/{resourceId}', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);
        
        // Checks
        if(!$ampersandApp->isEditableConcept($resource->concept)) throw new Exception ("You do not have access for this call", 403);
        if(!$resource->exists()) throw new Exception("Resource '{$resource}' not found", 404);

        return $response->withJson($resource, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    // GET for interfaces that start with other resource
    $this->get('/{resourceType}/{resourceId}/{ifcPath:.*}', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');
        
        // Prepare
        $controller = new InterfaceController($ampersandApp, $angularApp);
        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);

        // Output
        return $response->withJson($controller->get($resource, $args['ifcPath'], $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    // PUT, PATCH, POST for interfaces that start with other resource
    $this->map(['PUT', 'PATCH', 'POST'], '/{resourceType}/{resourceId}/{ifcPath:.*}', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');
        $body = $request->getParsedBody();
        $ifcPath = $args['ifcPath'];
        
        // Prepare
        $controller = new InterfaceController($ampersandApp, $angularApp);
        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);

        // Output
        switch ($request->getMethod()) {
            case 'PUT':
                return $response->withJson($controller->put($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            case 'PATCH':
                return $response->withJson($controller->patch($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            case 'POST':
                return $response->withJson($controller->post($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            default:
                throw new Exception("Unsupported HTTP method", 500);
        }
    });

    $this->delete('/session/{ifcPath:.*}', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        $resource = $ampersandApp->getSession()->getSessionResource();

        $controller = new InterfaceController($ampersandApp, $angularApp);

        return $response->withJson($controller->delete($resource, $args['ifcPath']), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->delete('/{resourceType}/{resourceId}/{ifcPath:.*}', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);

        $controller = new InterfaceController($ampersandApp, $angularApp);

        return $response->withJson($controller->delete($resource, $args['ifcPath']), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

})->add($middleWare1);

$app->group('/session', function () use ($container, $middleWare1) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];

    // GET for interfaces with expr[SESSION*..]
    $this->get('/{ifcPath:.*}', function(Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');

        // Prepare
        $controller = new InterfaceController($ampersandApp, $angularApp);
        $resource = $ampersandApp->getSession()->getSessionResource();

        // Output
        return $response->withJson($controller->get($resource, $args['ifcPath'], $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    // PUT, PATCH, POST for interfaces with expr[SESSION*..]
    $this->map(['PUT', 'PATCH', 'POST'], '/{ifcPath:.*}', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');
        $body = $request->getParsedBody();
        $ifcPath = $args['ifcPath'];
        
        // Prepare
        $controller = new InterfaceController($ampersandApp, $angularApp);
        $resource = $ampersandApp->getSession()->getSessionResource();

        // Output
        switch ($request->getMethod()) {
            case 'PUT':
                return $response->withJson($controller->put($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            case 'PATCH':
                return $response->withJson($controller->patch($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            case 'POST':
                return $response->withJson($controller->post($resource, $ifcPath, $body, $options, $depth), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
            default:
                throw new Exception("Unsupported HTTP method", 500);
        }
    });
})->add($middleWare1);
