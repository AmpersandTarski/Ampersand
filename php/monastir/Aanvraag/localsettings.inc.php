<?php
  // Select the monastir view
  $incPath = "../inc/";
  $dbName  = "Aanvraag"; // mysql internal name
  $appName = "Aanvraag"; // full text name
  require $incPath."monastir.inc.php";
  require $incPath."globalsettings.inc.php";
  require "connectToDataBase.inc.php";
  
  class view Extends monastir {
    function view($obj,$object){
      global $action;
      $this->action=$action;
      $menu = array();
      $menu[] = array
        (new menuItem('Behandelaar.php','Show all Behandelaar objects','menuItem','Behandelaar')
        ,new menuItem('Permissions.php','Show all Permissions objects','menuItem','Permissions')
        ,new menuItem('Person.php','Show all Person objects','menuItem','Person')
        ,new menuItem('Application.php','Show all Application objects','menuItem','Application')
        ,new menuItem('Decision.php','Show all Decision objects','menuItem','Decision')
        );
      parent::monastir($menu,$obj,$object,ucfirst($object->name));
    }
  }
?>