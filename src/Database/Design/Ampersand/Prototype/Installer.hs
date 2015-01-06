module Database.Design.Ampersand.Prototype.Installer
  (installerDBstruct,installerDefPop,dumpPopulationToADL,
   createTablesPHP,populateTablesPHP,plug2TableSpec,dropplug,historyTableSpec,sessionTableSpec,mkSignalTableSpec,getTableName,TableSpec) where

import Data.List
import Database.Design.Ampersand
import Database.Design.Ampersand.Prototype.RelBinGenBasics
import Database.Design.Ampersand.Prototype.RelBinGenSQL

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


createTablesPHP :: FSpec -> [String]
createTablesPHP fSpec =
        [ "/*** Create new SQL tables ***/"
        , ""
        ] ++
        concatMap createTablePHP [sessionTableSpec, historyTableSpec] ++
        [ "$time = explode(' ', microTime()); // copied from DatabaseUtils setTimestamp"
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
        ] ++
        concatMap (createTablePHP . mkSignalTableSpec) (vconjs fSpec) ++
        [ ""
        , "//// Number of plugs: " ++ show (length (plugInfos fSpec))
        ]
        -- Create all plugs
        ++ concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]

--                 (headerCmmnt,tableName,crflds,engineOpts)
type TableSpec = (String,String,[String],String)

getTableName :: TableSpec -> String
getTableName (_,nm,_,_) = nm

createTablePHP :: TableSpec -> [String]
createTablePHP (headerCmmnt,tableName,crflds,engineOpts) =
  [ headerCmmnt
  -- Drop table if it already exists
  , "if($columns = mysqli_query($DB_link, "++showPhpStr ("SHOW COLUMNS FROM `"++tableName++"`")++")){"
  , "    mysqli_query($DB_link, "++showPhpStr ("DROP TABLE `"++tableName++"`")++");"
  , "}"
  ] ++
  [ "mysqli_query($DB_link,\"CREATE TABLE `"++tableName++"`"] ++
  [ replicate 23 ' ' ++ [pref] ++ " " ++ fld | (pref, fld) <- zip ('(' : repeat ',') crflds ] ++
  [replicate 23 ' ' ++ ") ENGINE=" ++engineOpts ++ "\");"]++
  [ "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , "" ]

plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug
 = ( unlines $ commentBlock (["Plug "++name plug,"","fields:"]++map (\x->showADL (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (plugFields plug))
   , name plug
   , [ quote (fldname f)++" " ++ showSQL (fldtype f) ++ (if fldauto f then " AUTO_INCREMENT" else " DEFAULT NULL")
     | f <- plugFields plug ]++
      case (plug, (head.plugFields) plug) of
           (BinSQL{}, _)   -> []
           (_,    primFld) ->
                case flduse primFld of
                   TableKey isPrim _ -> [ (if isPrim then "PRIMARY " else "")
                                          ++ "KEY (`"++fldname primFld++"`)"
                                        ]
                   ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                   PlainAttr     -> []
   , "InnoDB DEFAULT CHARACTER SET UTF8")

mkSignalTableSpec :: Conjunct -> TableSpec
mkSignalTableSpec conj =
  ( "// Signal table for conjunct " ++ rc_id conj
  , "signals_" ++ rc_id conj
  , [ "`src` VARCHAR(255) NOT NULL"
    , "`tgt` VARCHAR(255) NOT NULL" ]
  , "InnoDB DEFAULT CHARACTER SET UTF8"
  )

sessionTableSpec :: TableSpec
sessionTableSpec
 = ( "// Session timeout table"
   , "__SessionTimeout__"
   , [ "`SESSION` VARCHAR(255) UNIQUE NOT NULL"
     , "`lastAccess` BIGINT NOT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8" )

historyTableSpec :: TableSpec
historyTableSpec
 = ( "// Timestamp table"
   , "__History__"
   , [ "`Seconds` VARCHAR(255) DEFAULT NULL"
     , "`Date` VARCHAR(255) DEFAULT NULL" ]
   , "InnoDB DEFAULT CHARACTER SET UTF8" )

populateTablesPHP :: FSpec -> [String]
populateTablesPHP fSpec =
  concatMap fillSignalTable (initialConjunctSignals fSpec) ++
  concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  where
    fillSignalTable (conj, viols) =
      [ "mysqli_query($DB_link, "++showPhpStr ("INSERT IGNORE INTO "++ quote (getTableName $ mkSignalTableSpec conj)
                                                                    ++" (`src`, `tgt`)"
                                              ++phpIndent 24++"VALUES " ++ 
                                              intercalate (phpIndent 29++", ") 
                                                [ "(" ++sqlAtomQuote src++", "++sqlAtomQuote tgt++")" 
                                                | (src, tgt) <- viols
                                                ])++"\n"++
        "            );"
      , "if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"
      ]
    
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


dropplug :: PlugSQL -> String
dropplug plug = "DROP TABLE "++quote (name plug)++""
