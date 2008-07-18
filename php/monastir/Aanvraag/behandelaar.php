<?

require "localsettings.inc.php";
require "behandelaar.inc.php";

$view = new view(parseRequest($behandelaar_object),$behandelaar_object);
$view->display();

?>