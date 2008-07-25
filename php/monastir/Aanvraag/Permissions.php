<?php // generated with ADL vs. 0.8.09
  
  require "localsettings.inc.php";
  require "Permissions.inc.php";
  
  $view = new view(parseRequest(getObject_Permissions()),getObject_Permissions());
  $view->display();
  ?>