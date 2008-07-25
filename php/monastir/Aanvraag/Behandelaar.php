<?php // generated with ADL vs. 0.8.09
  
  require "localsettings.inc.php";
  require "Behandelaar.inc.php";
  
  $view = new view(parseRequest(getObject_Behandelaar()),getObject_Behandelaar());
  $view->display();
  ?>