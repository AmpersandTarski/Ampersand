{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables#-}
module DatabaseDesign.Ampersand_Prototype.Index(htmlindex) where
   import Data.List
   import Data.Maybe
   import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(indentBlock)
   import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL(isOne)
--   import System.FilePath (addExtension)
   import DatabaseDesign.Ampersand_Prototype.CoreImporter
   import DatabaseDesign.Ampersand_Prototype.Version 
   
   
   htmlindex :: Fspc -> [Interface] -> Options -> [Char]
   htmlindex fSpec ifcs flags
    = intercalate "\n  "
      ( [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
        , "<html><head>"
        , "  <title>"++appname++"- Ampersand Prototype</title>"
        , "  <link href=\"css/reset.css\"  rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "  <link href=\"css/screen.css\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "</head><body style=\"height:100%;width:100%;\">"
        , "  <h1>"++appname++"</h1>"
        , "  <ol>"
        , "    <li><a href=\"Installer.php\">Click here to reset the database</a></li>"
        , "    <li class=\"buttons\">Use interfaces:"
        , "      <ul>"
        ] ++ indentBlock 8 (concat
                           [ if isOne (ifcObj ifc)
                             then ["<li><a href=\""++name fSpec++".php?content="++name ifc++"\">"
                                  ,"  "++name ifc
                                  ,"</a></li>"]
                             else ["<li><a href=\""++name fSpec++".php?content="++name ifc++"&new=1\">"
                                  ,"  New "++name ifc
                                  ,"</a></li>"]
                           | ifc <- ifcs
                           ]
                           ) ++
        [ "      </ul>"
        , "    </li>"
        , "  </ol>"
        , "  <!--"
        , "  <h2>Some data:</h2>"
        , "  <ul>"
        , "    <li>Compiled with: "++prototypeVersionStr++"</li>"
        , "    <li>Application name: "++appname++"</li>"
        , "    <li>Database host: "++(fromMaybe "localhost" $ sqlHost flags)++"</li>"
        , "    <li>Database Usename / password set: "++ (if (isJust $ sqlLogin flags) || (isJust $ sqlPwd flags) then "YES" else "NO")++"</li>"
        , "    <li>Database name: "++dbName flags++"</li>"
        , "  </ul>"
        , "  -->"
        , "</body></html>"]
      )
    where
     appname   = name fSpec
