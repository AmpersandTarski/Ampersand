<?php
error_reporting(E_ALL);
require_once (__DIR__ . '/../../inc/includes.php');
require_once (__DIR__ . '/ExcelImport.php');

if (is_uploaded_file($_FILES['userfile']['tmp_name'])){
	// Parse:
	$exparser = new ImportExcel($_FILES['userfile']['tmp_name']);
	$result = $exparser->ParseFile();
	print "File parsed!\n";	
	unlink($_FILES['userfile']['tmp_name']);
}else{
    print "Error code 80085.4711\n";
}


print 'Here is some more debugging info:';
print "<pre>";
print_r($_FILES);
print_r($result);
print_r(ErrorHandling::getLogs());
print "</pre>";

?>

