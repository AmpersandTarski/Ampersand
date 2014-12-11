module Database.Design.Ampersand.Prototype.Installer
  (installerDBstruct,installerTriggers,installerDefPop,dumpPopulationToADL,
   createTablesPHP,populateTablesPHP,plug2tbl,dropplug,historytbl,sessiontbl,CreateTable) where

import Data.List
import Database.Design.Ampersand
import Database.Design.Ampersand.Prototype.RelBinGenBasics(indentBlock,commentBlock,addSlashes,phpIndent,showPhpStr, quote, sqlAtomQuote)
import Database.Design.Ampersand.Prototype.RelBinGenSQL(selectExprRelation,sqlRelPlugs)

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

installerTriggers :: FSpec -> String
installerTriggers fSpec = unlines $
      [ "<?php"
      , ""
      , ""
      , "// Array for trigger queries that need to be installed"
      , "$queries = array();"
      , ""
      , ""
      ] ++ [] -- something like:     map unlines [ trigger tablename query | ]
       ++
      [ "$queries[] = \"CREATE TRIGGER etc\";"
      , ""
      , ""
      , ""
      , ""
      , "// Execute queries"
      , "foreach ($queries as $query){"
      , " print $query.\"<br/>\";"
      , " // $db->Exe($query); "
      , " // print($db->error());"
      , ""
      , "}"
      , "?>"
      ]
  where
      trigger tablename query
       = [ "// Trigger for DELETE Atom or Pair in function in Concept table"
         , "$queries['delete_"++tablename++"']"
         , " = \"CREATE TRIGGER `delete_"++tablename++"` BEFORE DELETE ON `"++tablename++"`"
         , "    FOR EACH ROW"
         , "    BEGIN "
         , "        DELETE FROM <other table> WHERE <other table>.<column name> = OLD.<column name>; "
         , "    END\";"
         ]

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
      ,"  file_put_contents(\"dbdump.php.\",$content);"
      , ""
      , "?>"
      ]

createTablesPHP :: FSpec ->[String]
createTablesPHP fSpec =
        [ "/*** Create new SQL tables ***/"
        , ""
        , "// Session timeout table"
        , "if($columns = mysqli_query($DB_link, "++showPhpStr "SHOW COLUMNS FROM `__SessionTimeout__`"++")){"
        , "    mysqli_query($DB_link, "++showPhpStr "DROP TABLE `__SessionTimeout__`"++");"
        , "}"
        ] ++ createTablePHP 21 sessiontbl ++
        [ "if($err=mysqli_error($DB_link)) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , ""
        , "// Timestamp table"
        , "if($columns = mysqli_query($DB_link, "++showPhpStr "SHOW COLUMNS FROM `__History__`"++")){"
        , "    mysqli_query($DB_link, "++showPhpStr "DROP TABLE `__History__`"++");"
        , "}"
        ] ++ createTablePHP 21 historytbl ++
        [ "if($err=mysqli_error($DB_link)) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , "$time = explode(' ', microTime()); // copied from DatabaseUtils setTimestamp"
        , "$microseconds = substr($time[0], 2,6);"
        , "$seconds =$time[1].$microseconds;"
        , "date_default_timezone_set(\"Europe/Amsterdam\");"
        -- to prevent a php warning TODO: check if this is ok when Ampersand is used in different timezones
        , "$date = date(\"j-M-Y, H:i:s.\").$microseconds;"
        , "mysqli_query($DB_link, \"INSERT INTO `__History__` (`Seconds`,`Date`) VALUES ('$seconds','$date')\");"
        , "if($err=mysqli_error($DB_link)) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , ""
        , "//// Number of plugs: " ++ show (length (plugInfos fSpec))

        -- The next statements will drop each table if exists:
        , "if(true){"
        ] ++ indentBlock 2 (concatMap checkPlugexists (plugInfos fSpec))
        ++ ["}"]

        -- The next statements will create all plugs
        ++ concatMap createPlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    checkPlugexists (ExternalPlug _) = []
    checkPlugexists (InternalPlug plug)
         = [ "if($columns = mysqli_query($DB_link, "++showPhpStr ("SHOW COLUMNS FROM "++quote (name plug)++"")++")){"
           , "  mysqli_query($DB_link, "++showPhpStr (dropplug plug)++");" --todo: incremental behaviour
           , "}" ]
    createPlugPHP plug
         = commentBlock (["Plug "++name plug,"","fields:"]++map (\x->showADL (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (plugFields plug))
           ++ createTablePHP 17 (plug2tbl plug)
           ++ ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]

populateTablesPHP :: FSpec -> [String]
populateTablesPHP fSpec =
    concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    populatePlugPHP plug
         = case tblcontents (gens fSpec) (initialPops fSpec) plug of
               [] -> []
               tblRecords -> ( "mysqli_query($DB_link, "++showPhpStr ("INSERT IGNORE INTO "++quote (name plug)
                                                           ++" ("++intercalate "," [quote (fldname f) |f<-plugFields plug]++")"
                                                           ++phpIndent 17++"VALUES " ++ intercalate (phpIndent 22++", ") [ "(" ++valuechain md++ ")" | md<-tblRecords]
                                                           ++phpIndent 16 )
                                        ++");"
                             ):
                             ["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
     where
        valuechain record = intercalate ", " [case fld of Nothing -> "NULL" ; Just str -> sqlAtomQuote str | fld<-record]

-- (CREATE TABLE name, fields, engine)
type CreateTable = (String,[String],String)

createTablePHP :: Int -> CreateTable -> [String]
createTablePHP i (crtbl,crflds,crengine)
         = [ "mysqli_query($DB_link,\""++ crtbl]
           ++ indentBlock i crflds
           ++ [replicate i ' ' ++ crengine ++ "\");"]

plug2tbl :: PlugSQL -> CreateTable
plug2tbl plug
 = ( "CREATE TABLE "++quote (name plug)
   , [ comma: " "++quote (fldname f)++" " ++ showSQL (fldtype f) ++ (if fldauto f then " AUTO_INCREMENT" else " DEFAULT NULL")
     | (f,comma)<-zip (plugFields plug) ('(':repeat ',') ]++
      case (plug, (head.plugFields) plug) of
           (BinSQL{}, _)   -> []
           (_,    primFld) ->
                case flduse primFld of
                   TableKey isPrim _ -> [ ", "++ (if isPrim then "PRIMARY " else "")
                                          ++ "KEY (`"++fldname primFld++"`)"
                                        ]
                   ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                   PlainAttr     -> []
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")

dropplug :: PlugSQL -> String
dropplug plug = "DROP TABLE "++quote (name plug)++""

historytbl :: CreateTable
historytbl
 = ( "CREATE TABLE `__History__`"
   , [ "( `Seconds` VARCHAR(255) DEFAULT NULL"
     , ", `Date` VARCHAR(255) DEFAULT NULL"]
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")

sessiontbl :: CreateTable
sessiontbl
 = ( "CREATE TABLE `__SessionTimeout__`"
   , [ "( `SESSION` VARCHAR(255) UNIQUE NOT NULL"
     , ", `lastAccess` BIGINT NOT NULL"]
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")
