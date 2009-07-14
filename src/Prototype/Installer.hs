 module Prototype.Installer where
  import Adl
  import Strings (chain)
  import Data.Plug
  import Data.Fspec
--  import Prototype.Mappings
  
  installer fSpec dbName = "<?php\n  "
     ++ ((chain "\n  ").concat) (map plugCode (plugs fSpec))
     ++ "?>\n"
    where plugCode plug
           = [ ""
             , "plug: "++(plname plug)
             , "---"
             , show plug
             , "---"
             ]
  
  