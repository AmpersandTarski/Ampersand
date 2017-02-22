<?php

use Ampersand\Session;
use Ampersand\Config;
use Ampersand\Log\Logger;
use Ampersand\Extension\ExcelImport\ExcelImport;
use Ampersand\Log\Notifications;
use Ampersand\Database\Database;

global $app;

// Path to API is 'api/v1/excelimport/import'
$app->post('/excelimport/import', function () use ($app){
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
			
	// Check sessionRoles if allowedRolesForExcelImport is specified
    if(!$session->hasAccess(Config::get('allowedRolesForExcelImport','excelImport'))) throw new Exception("You do not have access to import excel files", 401);
	
	if (is_uploaded_file($_FILES['file']['tmp_name'])){
		// Parse:
		$parser = new ExcelImport();
		$parser->ParseFile($_FILES['file']['tmp_name']);
		
		Database::singleton()->closeTransaction("File {$_FILES['file']['tmp_name']} imported successfully", true);
		
		unlink($_FILES['file']['tmp_name']);
	}else{
	    Logger::getUserLogger()->error("No file uploaded");
	}
	
	$result = array('notifications' => Notifications::getAll(), 'files' => $_FILES);
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>