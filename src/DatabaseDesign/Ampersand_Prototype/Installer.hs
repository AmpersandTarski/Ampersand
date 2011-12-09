{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Installer
  (installer)
where
  import Data.List
  import DatabaseDesign.Ampersand_Prototype.CoreImporter
  import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(phpShow,indentBlock,commentBlock,addSlashes)
  
--  import Debug.Trace

  installer :: Fspc -> Options -> String
  installer fSpec flags = "<?php\n  " ++ intercalate "\n  "
     (
        [ "// Try to connect to the database\n"
        , "if(isset($DB_host)&&!isset($_REQUEST['DB_host'])){"
        , "  $included = true; // this means user/pass are probably correct"
        , "  $DB_link = @mysql_connect(@$DB_host,@$DB_user,@$DB_pass);"
        , "}else{"
        , "  $included = false; // get user/pass elsewhere"
        , "  if(file_exists(\"dbsettings.php\")) include \"dbsettings.php\";"
        , "  else { // no settings found.. try some default settings"
        , "    if(!( $DB_link=@mysql_connect($DB_host='"++addSlashes (sqlHost flags)++"',$DB_user='"++addSlashes (sqlLogin flags)++"',$DB_pass='"++addSlashes (sqlPwd flags)++"')))"
        , "    { // we still have no working settings.. ask the user!"
        , "      die(\"Install failed: cannot connect to MySQL\"); // todo" --todo
        , "    }"
        , "  } "
        , "}"
        , "if($DB_slct = @mysql_select_db('"++dbName flags++"')){"
        , "  $existing=true;"
        , "}else{"
        , "  $existing = false; // db does not exist, so try to create it"
        , "  @mysql_query(\"CREATE DATABASE `"++dbName flags++"` DEFAULT CHARACTER SET UTF8\");"
        , "  $DB_slct = @mysql_select_db('"++dbName flags++"');"
        , "}"
        , "if(!$DB_slct){"
        , "  echo die(\"Install failed: cannot connect to MySQL or error selecting database '"++dbName flags++"'\");" --todo: full error report
        , "}else{"
        ] ++ indentBlock 2
        (
          [ "if(!$included && !file_exists(\"dbsettings.php\")){ // we have a link now; try to write the dbsettings.php file"
          , "   if($fh = @fopen(\"dbsettings.php\", 'w')){"
          , "     fwrite($fh, '<'.'?php $DB_link=mysql_connect($DB_host=\"'.$DB_host.'\", $DB_user=\"'.$DB_user.'\", $DB_pass=\"'.$DB_pass.'\"); $DB_debug = 3; ?'.'>');"
          , "     fclose($fh);"
          , "   }else die('<P>Error: could not write dbsettings.php, make sure that the directory of Installer.php is writable"
          , "              or create dbsettings.php in the same directory as Installer.php"
          , "              and paste the following code into it:</P><code>'."
          , "             '&lt;'.'?php $DB_link=mysql_connect($DB_host=\"'.$DB_host.'\", $DB_user=\"'.$DB_user.'\", $DB_pass=\"'.$DB_pass.'\"); $DB_debug = 3; ?'.'&gt;</code>');"
          , "}\n"
          , "$error=false;"
          , "/*** Create new SQL tables ***/"
          , "//// Number of plugs: "++(show (length (plugInfos fSpec)))
          , "if($existing==true){"
          ] ++ indentBlock 2 (concat (map checkPlugexists (plugInfos fSpec)))
          ++ ["}"]
          ++ concat (map plugCode [p | InternalPlug p<-plugInfos fSpec])
          ++ ["mysql_query('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"
             ,"if ($err=='')"
             ,"  echo 'The database has been reset to its initial population.<br/><br/><button onclick=\"window.location.href = document.referrer;\">Ok</button>';"]
        ) ++
        [ "}" ]
     ) ++ "\n?>\n"
    where plugCode plug
           = commentBlock (["Plug "++name plug,"","fields:"]++(map (\x->show (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (tblfields plug)))
             ++
             [ "mysql_query(\"CREATE TABLE `"++name plug++"`"]
             ++ indentBlock 17
                    [ comma: " `" ++ fldname f ++ "` " ++ showSQL (fldtype f) ++ " " ++ autoIncr ++ "DEFAULT NULL"
                    | (f,comma)<-zip (tblfields plug) ('(':repeat ',')
                    , let autoIncr = if fldauto f
                                     then " AUTO_INCREMENT" else ""
                    ]
             ++ ["                  ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8\");"
             , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             ++ (if (null $ tblcontents plug) then [] else
                 [ "else"
                                 , "mysql_query(\"INSERT IGNORE INTO `"++name plug++"` ("++intercalate "," ["`"++fldname f++"` " |f<-tblfields plug]++")"
                                 ]++ indentBlock 12
                                                 ( [ comma++ " (" ++valuechain md++ ")"
                                                   | (md,comma)<-zip (tblcontents plug) ("VALUES":repeat "      ,")
                                                   ]
                                                 )
                                 ++ ["            \");"
                                 , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             )
          valuechain record = intercalate ", " [if null fld then "NULL" else phpShow fld |fld<-record]
          checkPlugexists plug
           = [ "if($columns = mysql_query(\"SHOW COLUMNS FROM `"++(name plug)++"`\")){"
             , "  mysql_query(\"DROP TABLE `"++(name plug)++"`\");" --todo: incremental behaviour
             , "}" ]
   
