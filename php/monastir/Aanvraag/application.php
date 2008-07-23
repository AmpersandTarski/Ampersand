<?

require "localsettings.inc.php";
require "application.inc.php";

$view = new view(parseRequest(getObject_application()),getObject_application());
$view->display();

?>