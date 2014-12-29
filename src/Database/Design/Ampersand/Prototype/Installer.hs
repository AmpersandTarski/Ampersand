module Database.Design.Ampersand.Prototype.Installer
  (installerDBstruct,installerDefPop,dumpPopulationToADL,
   createTablesPHP,populateTablesPHP) where

import Database.Design.Ampersand
import Database.Design.Ampersand.Prototype.RelBinGenBasics
import Database.Design.Ampersand.Prototype.RelBinGenSQL
import Database.Design.Ampersand.Prototype.PHP

fatal :: Int -> String -> a
fatal = fatalMsg "Installer"

installerDBstruct :: FSpec -> String
installerDBstruct fSpec = unlines $
      ["<?php"
      , "// Try to connect to the database"
      , ""
      , "include \"dbSettings.php\";"
      , ""
      , "$DB_name='"++addSlashes (dbName (getOpts fSpec))++"';"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
      , "}"
      , ""
      , "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
      , "mysqli_query($DB_link,$sql);"
      , "// Don't bother about the error if the database didn't exist..."
      , ""
      , "// Create the database"
      , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8\";"
      , "if (!mysqli_query($DB_link,$sql)) {"
      , "  die(\"Error creating the database: \" . mysqli_error($DB_link));"
      , "  }"
      , ""
      , "// Connect to the freshly created database"
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to the database: \" . mysqli_connect_error());"
      , "  }"
      , ""
      ] ++
      createTablesPHP fSpec ++
      [ "mysqli_query($DB_link,'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"
      , "?>"
      ]

--installerTriggers :: FSpec -> String
--installerTriggers _ = unlines $
--      [ "<?php"
--      , ""
--      , ""
--      , "// Array for trigger queries that need to be installed"
--      , "$queries = array();"
--      , ""
--      , ""
--      ] ++ [] -- something like:     map unlines [ trigger tablename query | ]
--       ++
--      [ "$queries[] = \"CREATE TRIGGER etc\";"
--      , ""
--      , ""
--      , ""
--      , ""
--      , "// Execute queries"
--      , "foreach ($queries as $query){"
--      , " print $query.\"<br/>\";"
--      , " // $db->Exe($query); "
--      , " // print($db->error());"
--      , ""
--      , "}"
--      , "?>"
--      ]
--  where
--      trigger tablename query
--       = [ "// Trigger for DELETE Atom or Pair in function in Concept table"
--         , "$queries['delete_"++tablename++"']"
--         , " = \"CREATE TRIGGER `delete_"++tablename++"` BEFORE DELETE ON `"++tablename++"`"
--         , "    FOR EACH ROW"
--         , "    BEGIN "
--         , "        DELETE FROM <other table> WHERE <other table>.<column name> = OLD.<column name>; "
--         , "    END\";"
--         ]

installerDefPop :: FSpec -> String
installerDefPop fSpec = unlines $
      ["<?php"
      , "// Connect to the database"
      , "include \"dbSettings.php\";"
      , ""
      , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
      , "// Check connection"
      , "if (mysqli_connect_errno()) {"
      , "  die(\"Failed to connect to the database: \" . mysqli_connect_error());"
      , "  }"
      , "$error=false;"
      ] ++
      populateTablesPHP fSpec ++
      ["?>"
      ]

dumpPopulationToADL :: FSpec -> String
dumpPopulationToADL fSpec = unlines $
      ["<?php"
      ,"  $content = '"
      ,"  <?php"
      ,"  require \"Generics.php\";"
      ,"  require \"php/DatabaseUtils.php\";"
      ,"  $dumpfile = fopen(\"dbdump.adl\",\"w\");"
      ,"  fwrite($dumpfile, \"CONTEXT "++name fSpec++" IN DUTCH\\n\");"
      ]
      ++
      ["  fwrite($dumpfile, dumprel(\""++name d++showSign (sign d)++"\",\""++qry++"\"));"
      | d<-relsDefdIn fSpec, decusr d
      , let dbrel = case sqlRelPlugs fSpec (EDcD d) of
                      [] -> fatal 82 "null dbrel"
                      x  -> x
      , let (_,srcField,trgField) = head dbrel
      , let qry = selectExprRelation fSpec (-1) (fldname srcField) (fldname trgField) d]
      ++
      ["  fwrite($dumpfile, \"ENDCONTEXT\");"
      ,"  fclose($dumpfile);"
      ,"  "
      ,"  function dumprel ($rel,$quer)"
      ,"  {"
      ,"    $rows = DB_doquer($quer);"
      ,"    $pop = \"\";"
      ,"    foreach ($rows as $row)"
      ,"      $pop = $pop.\";(\\\"\".escapedoublequotes($row[0]).\"\\\",\\\"\".escapedoublequotes($row[1]).\"\\\")\\n  \";"
      ,"    return \"POPULATION \".$rel.\" CONTAINS\\n  [\".substr($pop,1).\"]\\n\";"
      ,"  }"
      ,"  function escapedoublequotes($str) { return str_replace(\"\\\"\",\"\\\\\\\\\\\\\"\",$str); }"
      ,"  ?>';"
      ,"  @$res=file_put_contents(\"dbdump.php.\",$content);"
      ,"  if($res===FALSE)"
      ,"    echo '(population dump file not created, due to insufficient server permissions)<br/><br/>';"
      , "?>"
      ]

