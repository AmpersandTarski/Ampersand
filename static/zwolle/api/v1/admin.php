<?php

use Ampersand\Misc\Config;
use Ampersand\Database\Database;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Conjunct;
use Ampersand\Rule\Rule;
use Ampersand\Core\Relation;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Transaction;
use Ampersand\AmpersandApp;
use Ampersand\Rule\RuleEngine;
use Ampersand\IO\Exporter;
use Ampersand\IO\JSONWriter;
use Ampersand\IO\CSVWriter;
use Ampersand\IO\Importer;
use Ampersand\IO\JSONReader;
use Ampersand\Misc\Reporter;

global $app;

$app->get('/admin/installer', function () use ($app){
    /** @var \Slim\Slim $app */
    if(Config::get('productionEnv')) throw new Exception ("Reinstallation of application not allowed in production environment", 403);
    
    $defaultPop = filter_var($app->request->params('defaultPop'), FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE); 
    if(is_null($defaultPop)) $defaultPop = true;

    $ampersandApp = AmpersandApp::singleton();
    $ampersandApp->reinstall($defaultPop);

    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    // Checks
    $logger = Logger::getUserLogger();
    foreach (InterfaceObject::getAllInterfaces() as $key => $interface) {
        foreach($interface->getInterfaceFlattened() as $ifc){
            if($ifc->crudU() && !$ifc->isEditable()) $logger->warning("Update rights (crUd) specified while interface expression is not an editable relation! Ifc:". $ifc->getPath());
            if($ifc->crudC() && !$ifc->tgtConcept->isObject()) $logger->warning("Create rights (Crud) specified while target concept is a scalar. This has no affect! Ifc:". $ifc->getPath());
            if($ifc->crudD() && !$ifc->tgtConcept->isObject()) $logger->warning("Delete rights (cruD) specified while target concept is a scalar. This has no affect! Ifc:". $ifc->getPath());
            if(!$ifc->crudR()) $logger->warning("No read rights specified. Are you sure? Ifc:". $ifc->getPath());
            
            // Check for unsupported patchReplace functionality due to missing 'old value'. Related with issue #318. TODO: still needed??
            if($ifc->isEditable() && $ifc->crudU() && !$ifc->tgtConcept->isObject() && $ifc->isUni()){
                // Only applies to editable relations
                // Only applies to crudU, because issue is with patchReplace, not with add/remove
                // Only applies to scalar, because objects don't use patchReplace, but Remove and Add
                // Only if interface expression (not! the relation) is univalent, because else a add/remove option is used in the UI
                
                if((!$ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'tgt')
                        || ($ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'src'))
                    $logger->warning("Unsupported edit functionality due to combination of factors for (sub)interface: " . $ifc->getPath());
            }
        }
    }

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    $content = Notifications::getAll(); // Return all notifications

    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/admin/execengine/run', function () use ($app){
    /** @var \Slim\Slim $app */
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForRunFunction','execEngine'))) throw new Exception("You do not have access to run the exec engine", 401);
        
    \Ampersand\Rule\ExecEngine::run(true);
    
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Run completed");
    else Logger::getUserLogger()->warning("Run completed but transaction not committed");

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
        
    $result = array('notifications' => Notifications::getAll());
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/checks/rules/evaluate/all', function() use ($app){
    /** @var \Slim\Slim $app */
    if(Config::get('productionEnv')) throw new Exception ("Evaluation of all rules not allowed in production environment", 403);
    
    foreach (RuleEngine::checkRules(Rule::getAllInvRules(), true) as $violation) Notifications::addInvariant($violation);
    foreach (RuleEngine::checkRules(Rule::getAllSigRules(), true) as $violation) Notifications::addSignal($violation);
    
    $content = Notifications::getAll(); // Return all notifications
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/export/all', function () use ($app){
    /** @var \Slim\Slim $app */
    if(Config::get('productionEnv')) throw new Exception ("Export not allowed in production environment", 403);
    
    // Response header
    $filename = Config::get('contextName') . "_Population_" . date('Y-m-d\TH-i-s') . ".json";
    $app->response->headers->set('Content-Disposition', "attachment; filename={$filename}");
    $app->response->headers->set('Content-Type', 'application/json; charset=utf-8');

    // Output response
    $exporter = new Exporter(new JSONWriter());
    $exporter->exportAllPopulation();
});

$app->get('/admin/import', function () use ($app){
    /** @var \Slim\Slim $app */
    if(Config::get('productionEnv')) throw new Exception ("Import not allowed in production environment", 403);

    if (is_uploaded_file($_FILES['file']['tmp_name'])) {
        $reader = new JSONReader();
        $reader->loadFile(Config::get('pathToGeneratedFiles') . 'populations.json');
        $importer = new Importer($reader);
        $importer->importPopulation();
        
        unlink($_FILES['file']['tmp_name']);
    } else {
        throw new Exception("Import file not specified", 500);
    }
    
    // Commit transaction
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Imported {$_FILES['file']['name']} successfully");
    
    // Check all process rules that are relevant for the activate roles
    AmpersandApp::singleton()->checkProcessRules(); 
    $content = Notifications::getAll(); // Return all notifications
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/admin/performance/conjuncts', function () use ($app){
    /** @var \Slim\Slim $app */

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

$app->get('/admin/report/relations', function () use ($app){
    /** @var \Slim\Slim $app */

    // Get report
    $reporter = new Reporter(new JSONWriter());
    $reporter->reportRelationDefinitions();

    // Set response headers
    $app->response->headers->set('Content-Type', 'application/json');

    // Output
    print $reporter;
});


$app->get('/admin/report/conjuncts', function () use ($app){
    /** @var \Slim\Slim $app */
    
    // Get report
    $reporter = new Reporter(new JSONWriter());
    $reporter->reportConjunctUsage();

    // Set response headers
    $app->response->headers->set('Content-Type', 'application/json');
    
    // Output
    print $reporter;
});

$app->get('/admin/report/interfaces', function () use ($app){
    /** @var \Slim\Slim $app */

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
