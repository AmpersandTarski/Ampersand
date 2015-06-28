module Database.Design.Ampersand.Prototype.Installer
  (installerDBstruct,installerDefPop,
   createTablesPHP,populateTablesPHP) where

import Database.Design.Ampersand
import Database.Design.Ampersand.Prototype.ProtoUtil
import Database.Design.Ampersand.Prototype.PHP

--fatal :: Int -> String -> a
--fatal = fatalMsg "Installer"

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
      ]++setSqlModePHP++
      [ "// Drop the database if it exists"
      , "$sql=\"DROP DATABASE $DB_name\";"
      , "mysqli_query($DB_link,$sql);"
      , "// Don't bother about the error if the database didn't exist..."
      , ""
      , "// Create the database"
      , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8 DEFAULT COLLATE UTF8_BIN\";"
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
      ]++setSqlModePHP++
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
      ] ++setSqlModePHP++
      populateTablesPHP fSpec ++
      ["?>"
      ]
