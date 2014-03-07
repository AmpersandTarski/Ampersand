<?php
require_once __DIR__.'/../dbSettings.php';

// let PHP also report undefined variable references
function terminate_missing_variables($errno, $errstr, $errfile, $errline)
{ if (($errno == E_NOTICE) and (strstr($errstr, "Undefined variable")))
  echo ("$errstr in $errfile line $errline");

  return false; // Let the PHP error handler handle all the rest
}
set_error_handler("terminate_missing_variables");



function emit(&$lines,$line) {
  $lines.=$line."\n";
}


function escapeHtmlAttrStr($str) {
  return str_replace(array('"', '&'), array('&quot;', '%26'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
}

?>
