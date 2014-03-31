{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Installer
  (installer,createDatabasePHP,createTablesPHP,plug2tbl,dropplug,historytbl,sessiontbl,CreateTable) where

import Data.List
import Data.Maybe
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(indentBlock,commentBlock,addSlashes,sqlEscIdentifier,sqlEscString)
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL(selectExprRelation,sqlRelPlugs)
import DatabaseDesign.Ampersand_Prototype.Version 

fatal :: Int -> String -> a
fatal = fatalMsg "Installer"


--  import Debug.Trace

installer :: Fspc -> Options -> String
installer fSpec flags = intercalate "\n  "
   (
      [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Strict//EN\">"
      , "<html>"
      , "<head>"
      , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
      , ""
      , "<meta http-equiv=\"Pragma\" content=\"no-cache\">"
      , "<meta http-equiv=\"no-cache\">"
      , "<meta http-equiv=\"Expires\" content=\"-1\">"
      , "<meta http-equiv=\"cache-Control\" content=\"no-cache\">"
      , ""
      , "</html>"
      , "<body>"
      ,"<?php"
      , "// Try to connect to the database\n"
      , "if(isset($DB_host)&&!isset($_REQUEST['DB_host'])){"
      , "  $included = true; // this means user/pass are probably correct"
      , "  $DB_link = @mysql_connect(@$DB_host,@$DB_user,@$DB_pass);"
      , "}else{"
      , "  $included = false; // get user/pass from some place else"
      , "  if(file_exists(\"dbSettings.php\")) include \"dbSettings.php\";"
      , "  else { // no settings found.. try some default settings"
      , "    if(!( $DB_link=@mysql_connect($DB_host='"++addSlashes (fromMaybe "localhost" $ sqlHost flags)++"',$DB_user='"++addSlashes (fromMaybe "root" $ sqlLogin flags)++"',$DB_pass='"++addSlashes (fromMaybe "" $ sqlPwd flags)++"')))"
      , "    { // we still have no working settings.. ask the user!"
      , "      die(\"Install failed: cannot connect to MySQL\"); // todo" --todo
      , "    }"
      , "  } "
      , "}"
      , "if($DB_slct = @mysql_select_db('"++dbName flags++"')){"
      , "  $existing=true;"
      , "}else{"
      , "  $existing = false; // db does not exist, so try to create it" ] ++
      indentBlock 2 (createDatabasePHP (dbName flags))  ++
      [ "  $DB_slct = @mysql_select_db('"++dbName flags++"');"
      , "}"
      , "if(!$DB_slct){"
      , "  echo die(\"Install failed: cannot connect to MySQL or error selecting database '"++dbName flags++"'\");" --todo: full error report
      , "}else{"
      ] ++ indentBlock 2
      (
        [ "if(!$included && !file_exists(\"dbSettings.php\")){ // we have a link now; try to write the dbSettings.php file"
        , "   if($fh = @fopen(\"dbSettings.php\", 'w')){"
        , "     fwrite($fh, '<'.'?php $DB_link=mysql_connect($DB_host=\"'.$DB_host.'\", $DB_user=\"'.$DB_user.'\", $DB_pass=\"'.$DB_pass.'\"); $DB_debug = 3; ?'.'>');"
        , "     fclose($fh);"
        , "   }else die('<P>Error: could not write dbSettings.php, make sure that the directory of Installer.php is writable"
        , "              or create dbSettings.php in the same directory as Installer.php"
        , "              and paste the following code into it:</P><code>'."
        , "             '&lt;'.'?php $DB_link=mysql_connect($DB_host=\"'.$DB_host.'\", $DB_user=\"'.$DB_user.'\", $DB_pass=\"'.$DB_pass.'\"); $DB_debug = 3; ?'.'&gt;</code>');"
        , "}\n"
        , "$error=false;" ] ++
        ["mysql_query('SET GLOBAL SQL_MODE=ANSI_QUOTES');" ] ++
        createTablesPHP fSpec ++
        ["mysql_query('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');" ] ++
        -- TODO: why set this only for the next transaction? (no SESSION or GLOBAL parameter is provided)
    
        ["if ($err=='') {"
        ,"  echo '<div id=\"ResetSuccess\"/>The database has been reset to its initial population.<br/><br/><button onclick=\"window.location.href = document.referrer;\">Ok</button>';"
        ,"  $content = '"
        ,"  <?php"
        ,"  require \"Generics.php\";"
        ,"  require \"php/DatabaseUtils.php\";"
        ,"  $dumpfile = fopen(\"dbdump.adl\",\"w\");"
        ,"  fwrite($dumpfile, \"CONTEXT "++name fSpec++"\\n\");"
        ]
        ++
        ["  fwrite($dumpfile, dumprel(\""++name d++showSign (sign d)++"\",\""++qry++"\"));" 
        | d<-relsDefdIn fSpec, decusr d
        , let dbrel = sqlRelPlugs fSpec (EDcD d)
        , if null dbrel then fatal 82 "null dbrel" else True
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
        ,"}"]
       ) ++
       [ "}"
       , "\n?></body></html>\n" ]
    ) 

createDatabasePHP :: String ->[String]
createDatabasePHP nm = ["@mysql_query('CREATE DATABASE "++sqlEscIdentifier nm++" DEFAULT CHARACTER SET UTF8');"]

createTablesPHP :: Fspc ->[String]
createTablesPHP fSpec =
        [ "/*** Create new SQL tables ***/"
        , ""
        , "// Session timeout table"
        , "if($columns = mysql_query('SHOW COLUMNS FROM \"__SessionTimeout__\"')){"
        , "    mysql_query('DROP TABLE \"__SessionTimeout__\"');"
        , "}"
        ] ++ createTablePHP 21 sessiontbl ++
        [ "if($err=mysql_error()) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , "" 
        , "// Timestamp table"
        , "if($columns = mysql_query('SHOW COLUMNS FROM \"__History__\"')){"
        , "    mysql_query('DROP TABLE \"__History__\"');"
        , "}"
        ] ++ createTablePHP 21 historytbl ++
        [ "if($err=mysql_error()) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , "$time = explode(' ', microTime()); // copied from DatabaseUtils setTimestamp"
        , "$microseconds = substr($time[0], 2,6);"
        , "$seconds =$time[1].$microseconds;"
        , "date_default_timezone_set(\"Europe/Amsterdam\");" 
        -- to prevent a php warning TODO: check if this is ok when Ampersand is used in different timezones 
        , "$date = date(\"j-M-Y, H:i:s.\").$microseconds;" 
        , "mysql_query('INSERT INTO \"__History__\" (\"Seconds\",\"Date\") VALUES ("++addSlashes (sqlEscString "$seconds")++","++addSlashes (sqlEscString "$date")++")');"
        , "if($err=mysql_error()) {"
        , "  $error=true; echo $err.'<br />';"
        , "}"
        , ""
        , "//// Number of plugs: " ++ show (length (plugInfos fSpec))
        , "if($existing==true){"
        ] ++ indentBlock 2 (concatMap checkPlugexists (plugInfos fSpec))
        ++ ["}"]
        ++ concatMap plugCode [p | InternalPlug p <- plugInfos fSpec]
  where plugCode plug
         = commentBlock (["Plugje "++name plug,"","fields:"]++map (\x->showADL (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (plugFields plug))
           ++ createTablePHP 17 (plug2tbl plug)
           ++ ["if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
           ++ if null table then [] else
               [ "else"
                               , "mysql_query('INSERT IGNORE INTO "++sqlEscIdentifier (name plug)++" ("++intercalate "," [sqlEscIdentifier (fldname f)++" " |f<-plugFields plug]++")"
                               ]++ indentBlock 13
                                                 [ comma++ " (" ++valuechain md++ ")"
                                                 | (md,comma)<-zip table ("VALUES":repeat "      ,")
                                                 ]
                                               
                               ++ ["            ');" 
                               , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
           where table = tblcontents (gens fSpec) (initialPops fSpec) plug
                 valuechain record = intercalate ", " [case fld of Nothing -> "NULL" ; Just val -> addSlashes (sqlEscString val) |fld<-record]
        checkPlugexists (ExternalPlug _) = []
        checkPlugexists (InternalPlug plug)
         = [ "if($columns = mysql_query('SHOW COLUMNS FROM "++sqlEscIdentifier (name plug)++"')){"
           , "  mysql_query('"++ dropplug plug ++"');" --todo: incremental behaviour
           , "}" ]



-- (CREATE TABLE name, fields, engine)
type CreateTable = (String,[String],String)

createTablePHP :: Int -> CreateTable -> [String] 
createTablePHP i (crtbl,crflds,crengine)
         = [ "mysql_query('"++ crtbl]
           ++ indentBlock i crflds
           ++ [replicate i ' ' ++ crengine ++ "');"]
           
plug2tbl :: PlugSQL -> CreateTable
plug2tbl plug
 = ( "CREATE TABLE "++sqlEscIdentifier (name plug)
   , [ comma: " " ++ sqlEscIdentifier (fldname f) ++ " " ++ showSQL (fldtype f) ++ (if fldauto f then " AUTO_INCREMENT" else " DEFAULT NULL") 
     | (f,comma)<-zip (plugFields plug) ('(':repeat ',') ]++
      case (plug, plugFields plug) of
           (BinSQL{}, _) -> []
           (_,    plg:_) -> [", PRIMARY KEY ("++sqlEscIdentifier (fldname plg)++")"]
           _             -> []
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")

dropplug :: PlugSQL -> String
dropplug plug = "DROP TABLE "++sqlEscIdentifier (name plug)

historytbl :: CreateTable
historytbl
 = ( "CREATE TABLE \"__History__\""
   , [ "( \"Seconds\" VARCHAR(255) DEFAULT NULL"
     , ", \"Date\" VARCHAR(255) DEFAULT NULL"]
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")

sessiontbl :: CreateTable
sessiontbl
 = ( "CREATE TABLE \"__SessionTimeout__\""
   , [ "( \"SESSION\" VARCHAR(255) UNIQUE NOT NULL"
     , ", \"lastAccess\" BIGINT NOT NULL"]
   , ") ENGINE=InnoDB DEFAULT CHARACTER SET UTF8")
