<?php

use Ampersand\Misc\Config;
use Ampersand\Core\Concept;
use Ampersand\Core\Atom;
use Ampersand\Interfacing\Resource;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Transaction;
use Ampersand\Interfacing\Options;

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

$app->get('/resources', function() use ($app, $container) {
    if(Config::get('productionEnv')) throw new Exception ("List of all resource types is not available in production environment", 403);
    
    $content = array_values(
        array_map(function($cpt){
            return $cpt->label; // only show label of resource types
        }, array_filter(Concept::getAllConcepts(), function($cpt){
            return $cpt->isObject(); // filter concepts without a representation (i.e. resource types)
    })));
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/resources/:resourceType', function ($resourceType) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    $concept = Concept::getConcept($resourceType);
    
    // Checks
    if(!$concept->isObject()) throw new Exception ("Resource type not found", 404);
    if($concept->isSession()) throw new Exception ("Resource type not found", 404); // Prevent users to list other session
    if(!$ampersandApp->isEditableConcept($concept)) throw new Exception ("You do not have access for this call", 403);
    
    $resources = Resource::getAllResources($resourceType);
    
    print json_encode($resources, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/resources/:resourceType/:resourceId', function ($resourceType, $resourceId) use ($app, $container) {
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

$app->get('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    // Options
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');

    // Get content
    $content = Resource::makeResource($resourceId, $resourceType)->walkPath($ifcPath)->get($options, $depth);

    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->put('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];
    $transaction = Transaction::getCurrentTransaction();
    
    // Options
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    
    // Perform put
    $obj = $app->request->getBody();
    $resource = Resource::makeResource($resourceId, $resourceType)->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->put($obj)->get($options, $depth);
    
    // Close transaction
    $transaction->close();
    if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " updated");
    
    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    // Return result
    $result =   [ 'content'             => $resource
                , 'notifications'       => Notifications::getAll()
                , 'invariantRulesHold'  => $transaction->invariantRulesHold()
                , 'sessionRefreshAdvice' => $angularApp->getSessionRefreshAdvice()
                , 'navTo'				=> $angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
                ];
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->patch('/resources/:resourceType/:resourceId(/:ifcPath+)', function ($resourceType, $resourceId, $ifcPath = array()) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];
    $transaction = Transaction::getCurrentTransaction();
    
    // Options
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    
    // Perform patch(es)
    $patches = $app->request->getBody();
    $resource = Resource::makeResource($resourceId, $resourceType)->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->patch($patches)->get($options, $depth);
    
    // Close transaction
    $transaction->close();
    if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " updated");
    
    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    // Return result
    $result = array ( 'patches'                => $app->request->getBody()
                    , 'content'             => $resource
                    , 'notifications'         => Notifications::getAll()
                    , 'invariantRulesHold'    => $transaction->invariantRulesHold()
                    , 'sessionRefreshAdvice' => $angularApp->getSessionRefreshAdvice()
					, 'navTo'				=> $angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
                    );
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    
});

$app->post('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];
    $transaction = Transaction::getCurrentTransaction();
    
    // Options
    $options = Options::getFromRequestParams($app->request()->params());
    $depth = $app->request->params('depth');
    
    // Perform create
    $obj = $app->request->getBody();
    $resource = Resource::makeResource($resourceId, $resourceType)->walkPath($ifcPath, 'Ampersand\Interfacing\ResourceList')->post($obj)->get($options, $depth);
    
    // Close transaction
    $transaction->close();
    if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " created");
    
    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    // Return result
    $result = array ( 'content'             => $resource
                    , 'notifications'         => Notifications::getAll()
                    , 'invariantRulesHold'    => $transaction->invariantRulesHold()
                    , 'sessionRefreshAdvice' => $angularApp->getSessionRefreshAdvice()
					, 'navTo'				=> $angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
                    );

    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->delete('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];
    $transaction = Transaction::getCurrentTransaction();
    
    // Perform delete
    $resource = Resource::makeResource($resourceId, $resourceType)->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->delete();
    
    // Close transaction
    $transaction->close();
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Resource deleted");
    
    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
    // Return result
    $result = array ( 'notifications'         => Notifications::getAll()
                    , 'invariantRulesHold'    => $transaction->invariantRulesHold()
                    , 'sessionRefreshAdvice'  => $angularApp->getSessionRefreshAdvice()
					, 'navTo'				  => $angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
                    );

    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});
