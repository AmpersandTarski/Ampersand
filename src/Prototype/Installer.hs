 module Prototype.Installer where
  import Adl
  import Auxiliaries
  import Strings    (chain)
  import Data.Plug
  import Data.Fspec
  import Collection (rd,(>-))
  import NormalForms(conjNF)
  import Prototype.RelBinGenBasics(phpShow,plugs)
  
  installer :: Fspc -> String -> String
  installer fSpec dbName = "<?php\n  " ++ chain "\n  "
     (
        [ "// Try to connect to the database\n"
        , "if(isset($DB_host)&&!isset($_REQUEST['DB_host'])){"
        , "  $included = true; // this means user/pass are probably correct"
        , "  $DB_link = @mysql_connect(@$DB_host,@$DB_user,@$DB_pass);"
        , "}else{"
        , "  $included = false; // get user/pass elsewhere"
        , "  if(file_exists(\"dbsettings.php\")) include \"dbsettings.php\";"
        , "  else { // no settings found.. try some default settings"
        , "    if(!( $DB_link=@mysql_connect($DB_host='localhost',$DB_user='root',$DB_pass='')"
        , "       or $DB_link=@mysql_connect($DB_host='localhost',$DB_user='ADL',$DB_pass='ADL')))"
        , "    { // we still have no working settings.. ask the user!"
        , "      die(\"Install failed: cannot connect to MySQL\"); // todo" --todo
        , "    }"
        , "  } "
        , "}"
        , "if($DB_slct = @mysql_select_db('"++dbName++"')){"
        , "  $existing=true;"
        , "}else{"
        , "  $existing = false; // db does not exist, so try to create it"
        , "  @mysql_query(\"CREATE DATABASE `"++dbName++"` DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
        , "  $DB_slct = @mysql_select_db('"++dbName++"');"
        , "}"
        , "if(!$DB_slct){"
        , "  echo die(\"Install failed: cannot not connect to MySQL or error selecting database\");" --todo: full error report
        , "}else{"
        ] ++ map ((++) "  ")
        (
          [ "if(!$included && !file_exists(\"dbsettings.php\")){ // we have a link now; try to write the dbsettings.php file"
          , "   if($fh = @fopen(\"dbsettings.php\", 'w')){"
          , "     fwrite($fh, '<'.'?php $DB_link=mysql_connect($DB_host=\"'.$DB_host.'\", $DB_user=\"'.$DB_user.'\", $DB_pass=\"'.$DB_pass.'\"); ?'.'>');"
          , "     fclose($fh);"
          , "   }"
          , "}\n"
          , "$error=false;"
          , "/*** Create new SQL tables ***/"
          , "//// Number of plugs: "++(show (length (plugs fSpec)))
          , "if($existing==true){"
          ] ++ map ((++) "  ") (concat (map checkPlugexists (plugs fSpec)))
          ++ ["}"]
          ++ concat (map plugCode (plugs fSpec))
-- onderstaande regel werkt niet omdat het verwijderen van dit bestand niet tijdens het uitvoeren ervan kan
--          ++ ["if(!$error) unlink(__FILE__); // this script should self-destruct to avoid resetting the database"]
          ++ ["DB_doquer('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"]
        ) ++
        [ "}" ]
     ) ++ "\n?>\n"
    where plugCode plug
           = [ "/* Plug "++plname plug++", fields: "++(show $ map fldexpr $fields plug)++" */"
             , "mysql_query(\"CREATE TABLE `"++plname plug++"`"]
             ++ map ((++) "                  ")
                    ( [ comma: " `" ++ fldname f ++ "` " ++ showSQL (fldtype f) ++ "" ++ nul
                      | (f,comma)<-zip (fields plug) ('(':repeat ','), let nul = if fldnull f then "" else " NOT NULL"
                      ] ++
                      [", UNIQUE KEY (`"++fldname key++"`)"
                      | key <- fields plug, flduniq key, not (fldnull key)]
                    )
             ++ ["                  ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
             , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             ++ (if (null $ mdata plug) then [] else
                 [ "else"
				 , "mysql_query(\"INSERT INTO `"++plname plug++"` ("++chain "," ["`"++fldname f++"` "|f<-fields plug]++")"
				 ]++ map ((++)   "          ")
						 ( [ comma++ " (" ++md++ ")"
						   | (md,comma)<-zip (mdata plug) ("VALUES":repeat "      ,")
						   ]
						 )
				 ++ ["          \");"
				 , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             )
          checkPlugexists plug
           = [ "if($columns = mysql_query(\"SHOW COLUMNS FROM `"++(plname plug)++"`\")){"
             , "  mysql_query(\"DROP TABLE `"++(plname plug)++"`\");" --todo: incremental behaviour
             , "}" ]
          mdata plug
           = [ chain ", " [ head ([phpShow b
                                  | [a',b]<-contents$fldexpr f,a==a' -- we know by the definition of these tables that this results in max 1 value for b
                                  ]++["NULL"])
                          | f<-fields plug]
             | [a,b]<-contents (fldexpr$head$fields plug) -- be sure that the concepts return their respective populations
             ]
  

