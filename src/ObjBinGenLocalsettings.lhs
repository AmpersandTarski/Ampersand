> module ObjBinGenLocalsettings where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux (ObjectDef(Obj), ObjDefs, KeyDef(Kd), KeyDefs, Object(concept, attributes, ctx))
>  import CommonClasses (Identified(name))
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics

This module generates the PHP-code for "localsettings.inc.php", which creates all view-services.
For each view, it creates one menu item. The following (generated) example illustrates this principle.

<?php
  // Select the monastir view
  $incPath = "../inc/";
  $appName = "Deliveries"; // full text name
  require $incPath."globalsettings.inc.php";
  require "connectToDataBase.inc.php";
  require $incPath."monastir.inc.php";
  
  class view Extends monastir {
    function view($obj,$object){
      global $action;
      $this->action=$action;
      $menu = array();
      $menu[] = array
        (new menuItem('Customer.php','Show all Customer objects','menuItem','Customer')
        ,new menuItem('Provider.php','Show all Provider objects','menuItem','Provider')
        ,new menuItem('Bestellingen.php','Show all Bestellingen objects','menuItem','Bestellingen')
        ,new menuItem('Rekeningen.php','Show all Rekeningen objects','menuItem','Rekeningen')
        ,new menuItem('Levering.php','Show all Levering objects','menuItem','Levering')
        ,new menuItem('Orderitem.php','Show all Orderitem objects','menuItem','Orderitem')
        ,new menuItem('Bestellijst.php','Show all Bestellijst objects','menuItem','Bestellijst')
        );
      parent::monastir($menu,$obj,$object,ucfirst($object->name));
    }
  }
?>


>  localsettings context dbName = chain "\n"
>   (["<?php"
>    ] ++ (map ((++) "  ") (
>      ["// Select the monastir view"
>      ,"$incPath = \"../inc/\";"
>      ,"$appName = \"" ++ (name context) ++ "\"; // full text name"
>      ,"require $incPath.\"globalsettings.inc.php\";"
>      ,"require \"connectToDataBase.inc.php\";"
>      ,"require $incPath.\"monastir.inc.php\";"
>      ,""
>      ,"class view Extends monastir {"
>      ] ++ (map ((++) "  ") (
>        ["function view($obj,$object){"
>        ] ++ (map ((++) "  ") (
>          ["global $action;"
>          ,"$this->action=$action;"
>          ,"$menu = array();"
>          ,"$menu[] = array"
>          ,"  ("++ (chain "\n        ,"
>           [ "new menuItem('"++objname++".php','Show all "++objname++" objects','menuItem','"++objname++"')"
>           | o<-attributes context, objname <- [addslashes (name o)]
>           ])
>          ,"  );"
>          ,"parent::monastir($menu,$obj,$object,ucfirst($object->name));"
>          ])) ++
>        ["}"
>        ])) ++
>       ["}"
>       ])) ++
>      ["?>"
>    ])
