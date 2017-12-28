<?php

use Ampersand\Misc\Config;
use Ampersand\Log\Logger;
use Ampersand\Extension\ExcelImport\ExcelImport;
use Ampersand\Log\Notifications;
use Ampersand\Transaction;
use Ampersand\AmpersandApp;

global $app;

// Path to API is 'api/v1/excelimport/import'
$app->post('/excelimport/import', function () use ($app){
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
            
    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForExcelImport','excelImport'))) throw new Exception("You do not have access to import excel files", 401);
    
    if (is_uploaded_file($_FILES['file']['tmp_name'])){
        // Parse:
        $parser = new ExcelImport();
        $parser->ParseFile($_FILES['file']['tmp_name']);
        
        $transaction = Transaction::getCurrentTransaction()->close(true);
        if($transaction->isCommitted()) Logger::getUserLogger()->notice("File {$_FILES['file']['tmp_name']} imported successfully");
        
        $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

        unlink($_FILES['file']['tmp_name']);
    }else{
        Logger::getUserLogger()->error("No file uploaded");
    }
    
    $result = array('notifications' => Notifications::getAll(), 'files' => $_FILES);
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>