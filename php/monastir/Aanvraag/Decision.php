<?php // generated with ADL vs. 0.8.10
  
  require "localsettings.inc.php";
  require "Decision.inc.php";
  
  $view = new view(parseRequest(getObject_Decision()),getObject_Decision());
  $view->display();
  ?>