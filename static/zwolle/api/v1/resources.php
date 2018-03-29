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

/**************************************************************************************************
 *
 * resource calls WITHOUT interfaces
 *
 *************************************************************************************************/

/**
 * @phan-closure-scope \Slim\App
 */
$app->group('/resource', function () {
    // Inside group closure, $this is bound to the instance of Slim\App
    /** @var \Slim\App $this */

    $this->get('', function (Request $request, Response $response, $args = []) {
        if (Config::get('productionEnv')) {
            throw new Exception("List of all resource types is not available in production environment", 403);
        }
        
        $content = array_values(
            array_map(function ($cpt) {
                return $cpt->label; // only show label of resource types
            }, array_filter(Concept::getAllConcepts(), function ($cpt) {
                return $cpt->isObject(); // filter concepts without a representation (i.e. resource types)
            }))
        );
        
        return $response->withJson($content, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/{resourceType}', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];

        $concept = Concept::getConcept($args['resourceType']);
        
        // Checks
        if (!$concept->isObject()) {
            throw new Exception("Resource type not found", 404);
        }
        if ($concept->isSession()) {
            throw new Exception("Resource type not found", 404); // Prevent users to list other sessions
        }
        if (!$ampersandApp->isEditableConcept($concept)) {
            throw new Exception("You do not have access for this call", 403);
        }
        
        $resources = Resource::getAllResources($args['resourceType']);
        
        return $response->withJson($resources, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->post('/{resourceType}', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];

        $resource = Resource::makeNewResource($args['resourceType']);

        $allowed = false;
        foreach ($ampersandApp->getAccessibleInterfaces() as $ifc) {
            if ($ifc->isRoot() && $ifc->crudC() && $ifc->tgtConcept == $resource->concept) {
                $allowed = true;
                break;
            }
        }
        if (!$allowed) {
            throw new Exception("You do not have access for this call", 403);
        }
        
        // Don't save/commit new resource (yet)
        return $response->withJson($resource, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/{resourceType}/{resourceId}', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];

        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);
        
        // Checks
        if (!$ampersandApp->isEditableConcept($resource->concept)) {
            throw new Exception("You do not have access for this call", 403);
        }
        if (!$resource->exists()) {
            throw new Exception("Resource '{$resource}' not found", 404);
        }

        return $response->withJson($resource, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    // GET for interfaces that start with other resource
    $this->get('/{resourceType}/{resourceId}/{ifcPath:.*}', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

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
    $this->map(['PUT', 'PATCH', 'POST'], '/{resourceType}/{resourceId}[/{ifcPath:.*}]', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');
        $body = $request->reparseBody()->getParsedBody();
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

    $this->delete('/{resourceType}/{resourceId}[/{ifcPath:.*}]', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

        $resource = Resource::makeResource($args['resourceId'], $args['resourceType']);

        $controller = new InterfaceController($ampersandApp, $angularApp);

        return $response->withJson($controller->delete($resource, $args['ifcPath']), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });
})->add($middleWare1);

/**
 * @phan-closure-scope \Slim\App
 */
$app->group('/session', function () {
    // Inside group closure, $this is bound to the instance of Slim\App
    /** @var \Slim\App $this */

    // GET for interfaces with expr[SESSION*..]
    $this->get('[/{ifcPath:.*}]', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

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
    $this->map(['PUT', 'PATCH', 'POST'], '[/{ifcPath:.*}]', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

        // Input
        $options = Options::getFromRequestParams($request->getQueryParams());
        $depth = $request->getQueryParam('depth');
        $body = $request->reparseBody()->getParsedBody();
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

    $this->delete('[/{ifcPath:.*}]', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this->appContainer['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this->appContainer['angular_app'];

        $resource = $ampersandApp->getSession()->getSessionResource();

        $controller = new InterfaceController($ampersandApp, $angularApp);

        return $response->withJson($controller->delete($resource, $args['ifcPath']), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });
})->add($middleWare1);
