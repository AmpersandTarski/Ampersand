<?php
$dbName = "ARM20";
require_once 'DatabaseUtils.php';
$query = "CALL AllProcedures";

$t0 = microtime(TRUE);

DB_doquer($query);

$t1 = microtime(TRUE);

$duration = $t1 - $t0;
print $duration . " sec";


?>