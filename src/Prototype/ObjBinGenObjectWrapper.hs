  module Prototype.ObjBinGenObjectWrapper where
   import Strings(chain)
   import Prototype.RelBinGenBasics (phpIdentifier)
  

   objectWrapper objectName
    = (chain "\n  "
      ([ "<?php // generated with ADL"
       , ""
       , "require \"localsettings.inc.php\";"
       , "require \""++objectName++".inc.php\";"
       , ""
       , "$view = new view(parseRequest(getObject_"++phpIdentifier objectName++"()),getObject_"++phpIdentifier objectName++"());"
       , "$view->display();"
       , ""
       ]
      )) ++ "?>"
