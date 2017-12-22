<?php

use Ampersand\Config;
use Ampersand\Extension\ExecEngine\ExecEngine;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Interfacing\Transaction;
use Ampersand\Rule\RuleEngine;
use Ampersand\AmpersandApp;

global $app;

// Path to API is 'api/v1/execengine/import'
$app->get('/execengine/run', function () use ($app){
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForRunFunction','execEngine'))) throw new Exception("You do not have access to run the exec engine", 401);
        
    ExecEngine::run(true);
    
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Run completed");
    else Logger::getUserLogger()->warning("Run completed but transaction not committed");

    RuleEngine::checkProcessRules(); // Check all process rules that are relevant for the activate roles
        
    $result = array('notifications' => Notifications::getAll());
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>