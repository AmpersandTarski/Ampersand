<?php
error_reporting(E_ALL);
require_once 'class.importexcel.php';
print "<pre>";

if (is_uploaded_file($_FILES['userfile']['tmp_name'])) 
{
	// Parse:
	$exparser = new ImportExcel($_FILES['userfile']['tmp_name']);
	$exparser->ParseFile();
	echo "File parsed!\n";	
	unlink($_FILES['userfile']['tmp_name']);
}else{
    echo "Error code 80085.4711\n";
}

/*
echo 'Here is some more debugging info:';
print_r($_FILES);
print "</pre>";
*/
?>

