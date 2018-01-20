<?php

use Ampersand\Misc\Config;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Conjunct;
use Ampersand\Rule\Rule;
use Ampersand\Transaction;
use Ampersand\Rule\RuleEngine;
use Ampersand\IO\Exporter;
use Ampersand\IO\JSONWriter;
use Ampersand\IO\CSVWriter;
use Ampersand\IO\Importer;
use Ampersand\IO\JSONReader;
use Ampersand\IO\ExcelImporter;
use Ampersand\Misc\Reporter;
use Ampersand\Interfacing\Resource;

/**
 * @var \Slim\Slim $app
 */
global $app;

/** 
 * @var \Pimple\Container $container
 */
global $container;

$app->POST('/admin/resource/:resourceType/rename', function ($resourceType) use ($app, $container){
    $ampersandApp = $container['ampersand_app'];
    
    $list = $app->request->getBody();
    if(!is_array($list)) throw new Exception("Body must be array. Non-array provided", 500);

    $transaction = Transaction::getCurrentTransaction();

    foreach ($body as $item) {
        $resource = Resource::makeResource($item->oldId, $resourceType);
        $resource->rename($item->newId);
    }
    
    $transaction->close();

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/installer', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reinstallation of application not allowed in production environment", 403);
    
    $defaultPop = filter_var($app->request->params('defaultPop'), FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE); 
    if(is_null($defaultPop)) $defaultPop = true;

    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    $transaction = $ampersandApp->reinstall($defaultPop);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Application successfully reinstalled");

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    $content = Notifications::getAll(); // Return all notifications

    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/admin/execengine/run', function () use ($app, $container){
    $ampersandApp = $container['ampersand_app'];
    
    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForRunFunction','execEngine'))) throw new Exception("You do not have access to run the exec engine", 401);
        
    \Ampersand\Rule\ExecEngine::run(true);
    
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Run completed");
    else Logger::getUserLogger()->warning("Run completed but transaction not committed");

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
    print json_encode(Notifications::getAll(), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/ruleengine/evaluate/all', function() use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Evaluation of all rules not allowed in production environment", 403);
    
    foreach (RuleEngine::checkRules(Rule::getAllInvRules(), false) as $violation) Notifications::addInvariant($violation);
    foreach (RuleEngine::checkRules(Rule::getAllSigRules(), false) as $violation) Notifications::addSignal($violation);
    
    $content = Notifications::getAll(); // Return all notifications
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/export/all', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Export not allowed in production environment", 403);
    
    // Response header
    $filename = Config::get('contextName') . "_Population_" . date('Y-m-d\TH-i-s') . ".json";
    $app->response->headers->set('Content-Disposition', "attachment; filename={$filename}");
    $app->response->headers->set('Content-Type', 'application/json; charset=utf-8');

    // Output response
    $exporter = new Exporter(new JSONWriter(), Logger::getLogger('IO'));
    $exporter->exportAllPopulation();
});

$app->post('/admin/import', function () use ($app, $container){
    $ampersandApp = $container['ampersand_app'];

    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForImporter'))) throw new Exception("You do not have access to import population", 401);
    
    if (is_uploaded_file($_FILES['file']['tmp_name'])) {
        $extension = pathinfo($_FILES['file']['name'], PATHINFO_EXTENSION);
        switch ($extension) {
            case 'json':
                $reader = new JSONReader();
                $reader->loadFile($_FILES['file']['tmp_name']);
                $importer = new Importer($reader, Logger::getLogger('IO'));
                $importer->importPopulation();
                break;
            case 'xls':
            case 'xlsx':
            case 'ods':
                $importer = new ExcelImporter(Logger::getLogger('IO'));
                $importer->parseFile($_FILES['file']['tmp_name']);
                break;
            default:
                throw new Exception("Unsupported file extension", 400);
                break;
        }

        // Commit transaction
        $transaction = Transaction::getCurrentTransaction()->close(true);
        if($transaction->isCommitted()) Logger::getUserLogger()->notice("Imported {$_FILES['file']['name']} successfully");
        unlink($_FILES['file']['tmp_name']);
    } else {
        Logger::getUserLogger()->error("No file uploaded");
    }    
    
    // Check all process rules that are relevant for the activate roles
    $ampersandApp->checkProcessRules(); 
    $content = ['notifications' => Notifications::getAll(), 'files' => $_FILES];
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/report/relations', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);

    // Get report
    $reporter = new Reporter(new JSONWriter());
    $reporter->reportRelationDefinitions();

    // Set response headers
    $app->response->headers->set('Content-Type', 'application/json');

    // Output
    print $reporter;
});

$app->get('/admin/report/conjuncts/usage', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);
    
    // Get report
    $reporter = new Reporter(new JSONWriter());
    $reporter->reportConjunctUsage();

    // Set response headers
    $app->response->headers->set('Content-Type', 'application/json');
    
    // Output
    print $reporter;
});

$app->get('/admin/report/conjuncts/performance', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);

    // Get report
    $reporter = new Reporter(new CSVWriter());
    $reporter->reportConjunctPerformance(Conjunct::getAllConjuncts());
    
    // Set response headers
    $filename = Config::get('contextName') . "_Conjunct performance_" . date('Y-m-d\TH-i-s') . ".csv";
    $app->response->headers->set('Content-Type', 'text/csv; charset=utf-8');
    $app->response->headers->set('Content-Disposition', "attachment; filename={$filename}");

    // Output response
    print $reporter;
});

$app->get('/admin/report/interfaces', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);

    // Get report
    $reporter = new Reporter(new CSVWriter());
    $reporter->reportInterfaceDefinitions();

    // Set response headers
    $filename = Config::get('contextName') . "_Interface definitions_" . date('Y-m-d\TH-i-s') . ".csv";
    $app->response->headers->set('Content-Type', 'text/csv; charset=utf-8');
    $app->response->headers->set('Content-Disposition', "attachment; filename={$filename}");

    // Output
    print $reporter;
});

$app->get('/admin/report/interfaces/issues', function () use ($app, $container){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);

    // Get report
    $reporter = new Reporter(new CSVWriter());
    $reporter->reportInterfaceIssues();

    // Set response headers
    $filename = Config::get('contextName') . "_Interface issues_" . date('Y-m-d\TH-i-s') . ".csv";
    $app->response->headers->set('Content-Type', 'text/csv; charset=utf-8');
    $app->response->headers->set('Content-Disposition', "attachment; filename={$filename}");

    // Output
    print $reporter;
});
