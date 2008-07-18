<?

require "localsettings.inc.php";
require "behandelaar.inc.php";

$view = new view(parseRequest(getObject_behandelaar()),getObject_behandelaar());
$view->display();

?>