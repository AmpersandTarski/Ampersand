<?php
error_reporting(E_ALL); 
ini_set("display_errors", 1);
require "Interfaces.php";
require "php/DatabaseUtils.php";

echo '<html>';
echo '<head>';
echo '<link href="css/Experimental.css" rel="stylesheet" type="text/css"/>';
echo '<link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/>';
echo '<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>';
echo '<script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script>';
echo '<script src="js/Experimental.js"></script>';
echo '<script type="text/javascript">';
echo 'function init() {';
echo '  initializeLinks(getInterfacesMap());';
echo '}';
echo '';
echo generateInterfaceMap($allInterfaceObjects);
echo '</script>';
echo '</head>';
echo '<body onload="init()">';
echo '<button class="EditButton" onclick="startEditing()">Edit</button>';
echo '<button class="CancelButton" onclick="stopEditing(getInterfacesMap())">Cancel</button>';


if (isset($_REQUEST['interface']) && isset($_REQUEST['atom'])) {
    $interface=$_REQUEST['interface'];
    $atom=$_REQUEST['atom'];
    echo '<h3>Interface \''.htmlSpecialChars($interface).'\' for atom \''.htmlSpecialChars($atom).'\'</h3>';
    echo generateInterface($dbName, $allInterfaceObjects[$interface], $atom); 
} else {
echo '<h3>Top-level interfaces</h3>';
echo topLevelInterfaceLinks($allInterfaceObjects);
}
echo '</body>';
echo '</html>';
?>
