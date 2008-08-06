<?php // generated with ADL vs. 0.8.10
  
  require "localsettings.inc.php";
  require "Application.inc.php";
  
  $view = new view(parseRequest(getObject_Application()),getObject_Application());
  $view->display();
  ?>