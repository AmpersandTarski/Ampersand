<?php

require_once (__DIR__ . '/../../../fw/includes.php');
require_once (__DIR__ . '/../ExcelImport.php');
require_once (__DIR__ . '/../../../api/vendor/restler.php');
use Luracast\Restler\Restler;
use Luracast\Restler\Format\UploadFormat;

$r = new Restler();
UploadFormat::$allowedMimeTypes = array('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/excel');
$r->setSupportedFormats('JsonFormat', 'UploadFormat'); // some strange error in Restler when UploadFormat is mentioned as first parameter
$r->addAPIClass('ExcelImportApi','');
$r->handle();

?>