{-# LANGUAGE ScopedTypeVariables#-}
  module Prototype.Index(htmlindex) where
   import Strings(chain)
   import Prototype.RelBinGenBasics(indentBlock,isOne)
   import Version (versionbanner)
   import System.FilePath (addExtension)
   import Adl (name)
   import Data.Fspec
   import Options


   htmlindex fSpec serviceObjects flags
    = chain "\n  "
      ( [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
        , "<HTML><HEAD>"
        , "  <TITLE>"++appname++"- ADL Prototype</TITLE>"
        , "  <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
        , "</HEAD><BODY STYLE=\"height:100%;width:100%;\">"
        , "  <H1>"++appname++"</H1>"
        , "  <OL>"
        , "    <LI><a href=\"Installer.php\">Click here to reset the database</a></LI>"
        , "    <LI class=\"buttons\">Use services:"
        , "      <UL>"
        ] ++ indentBlock 8 (concat
                           [ ["<LI><a href=\""++addExtension (name o) ".php"++(if isOne o then "" else "?new=1")++"\">"
                             ,(if isOne o then "  " else "  New ")++name o
                             ,"</LI>"]
                           | o <- serviceObjects
                           ]
                           ) ++
        [ "      </UL>"
        , "    </LI>"
        , "  </OL>"
        , "  <!--"
        , "  <H2>Some data:</H2>"
        , "  <UL>"
        , "    <LI>Compiled with: "++versionbanner++"</LI>"
        , "    <LI>Application name: "++appname++"</LI>"
        , "    <LI>Database host: "++sqlHost flags++"</LI>"
        , "    <LI>Database Usename / password set: "++(if sqlLogPwdDefd flags then "YES" else "NO")++"</LI>"
        , "    <LI>Database name: "++dbName flags++"</LI>"
        , "  </UL>"
        , "  -->"
        , "</BODY></HTML>"]
      )
    where
     FS_id appname   = fsfsid fSpec
