{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.Index(htmlindex) where
   import Strings(chain)
   import Prototype.RelBinGenBasics(indentBlock,isOne)
   import Version (versionbanner)
--   import System.FilePath (addExtension)
   import Adl (name,ObjectDef)
   import Data.Fspec
   import Options
   
   
   htmlindex :: Fspc -> [ObjectDef] -> Options -> [Char]
   htmlindex fSpec serviceObjects flags
    = chain "\n  "
      ( [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
        , "<html><head>"
        , "  <title>"++appname++"- ADL Prototype</title>"
        , "  <link href=\"css/reset.css\"  rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "  <link href=\"css/screen.css\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "</head><body style=\"height:100%;width:100%;\">"
        , "  <h1>"++appname++"</h1>"
        , "  <ol>"
        , "    <li><a href=\"Installer.php\">Click here to reset the database</a></li>"
        , "    <li class=\"buttons\">Use services:"
        , "      <ul>"
        ] ++ indentBlock 8 (concat
                     [ ["<li><a href=\""++name fSpec++".php?content="++name o++(if isOne o then "" else "&new=1")++"\">"
                             ,(if isOne o then "  " else "  New ")++name o
                             ,"</a></li>"]
                           | o <- serviceObjects
                           ]
                           ) ++
        [ "      </ul>"
        , "    </li>"
        , "  </ol>"
        , "  <!--"
        , "  <h2>Some data:</h2>"
        , "  <ul>"
        , "    <li>Compiled with: "++versionbanner++"</li>"
        , "    <li>Application name: "++appname++"</li>"
        , "    <li>Database host: "++sqlHost flags++"</li>"
        , "    <li>Database Usename / password set: "++(if sqlLogPwdDefd flags then "YES" else "NO")++"</li>"
        , "    <li>Database name: "++dbName flags++"</li>"
        , "  </ul>"
        , "  -->"
        , "</body></html>"]
      )
    where
     appname   = name fSpec
