{-# OPTIONS_GHC -Wall #-}
 module Prototype.ContextGen where
  import Adl
  import Strings    (chain)
  import Data.Fspec
  import Version (versionbanner)
   
  contextGen :: Fspc -> String
  contextGen fspc = "<?php\n  " ++ chain "\n  "
     (
        [ "// "++name fspc++".php"
        , "// Generated with "++ versionbanner
        , "// Prototype interface design by Sebastiaan JC Joosten (c) Aug 2009"
        , ""
        , ""
        , "error_reporting(E_ALL); "
        , "ini_set(\"display_errors\", 1);"
        , ""
        , "$content = $_REQUEST['content'];"
        , "$ctxenv = array(" ++ 
           (if name fspc=="Atlas" then "'User'=>$_REQUEST['User'], 'Script'=>$_REQUEST['Script']" else [])
           ++ ");"
        , "include \"$content.php\"; "
        , ""
        , "?>"])
