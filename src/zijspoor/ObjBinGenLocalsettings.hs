  module ObjBinGenLocalsettings ()
  where
   import Strings(chain)
   import Adl (Object(attributes))
   import CommonClasses (Identified(name))
   import RelBinGenBasics(addSlashes)

   localsettings context serviceObjects dbName = chain "\n"
    (["<?php"
     ] ++ (map ((++) "  ") (
       ["// Select the monastir view"
       ,"$incPath = \"../inc/\";"
       ,"$appName = \"" ++ (name context) ++ "\"; // full text name"
       ,"require $incPath.\"globalsettings.inc.php\";"
       ,"require \"connectToDataBase.inc.php\";"
       ,"require $incPath.\"monastir.inc.php\";"
       ,""
       ,"class view Extends monastir {"
       ] ++ (map ((++) "  ") (
         ["function view($obj,$object){"
         ] ++ (map ((++) "  ") (
           ["global $action;"
           ,"$this->action=$action;"
           ,"$menu = array();"
           ,"$menu[] = array"
           ,"  ("++ (chain "\n        ,"
            [ "new menuItem('"++objname++".php','Show all "++objname++" objects','menuItem','"++objname++"')"
            | o<-serviceObjects, objname <- [addSlashes (name o)]
            ])
           ,"  );"
           ,"parent::monastir($menu,$obj,$object,ucfirst($object->name));"
           ])) ++
         ["}"
         ])) ++
        ["}"
        ])) ++
       ["?>"
     ])
