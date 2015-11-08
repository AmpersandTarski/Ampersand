<?php

require_once (__DIR__ . '/../../../fw/includes.php');
require_once (__DIR__ . '/../OAuthLogin.php');
require_once (__DIR__ . '/../../../api/vendor/restler.php');
use Luracast\Restler\Restler;
use Luracast\Restler\Format\UploadFormat;

$r = new Restler();
$r->setSupportedFormats('JsonFormat', 'XmlFormat', 'HtmlFormat'); // some strange error in Restler when UploadFormat is mentioned as first parameter
$r->addAPIClass('OAuthLoginApi','');
$r->handle();

?>