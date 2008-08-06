<?php // generated with ADL vs. 0.8.10
  
  require "localsettings.inc.php";
  require "Person.inc.php";
  
  $view = new view(parseRequest(getObject_Person()),getObject_Person());
  $view->display();
  ?>