<?php

require_once (__DIR__ . '/../../fw/includes.php');
require_once (__DIR__ . '/ExcelImport.php');

if (is_uploaded_file($_FILES['file']['tmp_name'])){
	// Parse:
	$parser = new ImportExcel($_FILES['file']['tmp_name']);
	$result = $parser->ParseFile();
	unlink($_FILES['file']['tmp_name']);
}else{
    Notifications::addError('No file uploaded');
}

$result = array('notifications' => $result, 'files' => $_FILES);
print json_encode($result);

?>