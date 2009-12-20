 module Prototype.Installer where
  import Adl
  import Strings    (chain)
  import Data.Plug
  import Data.Fspec
  import Collection (rd)
  import Options
--  import NormalForms(conjNF)
  import Prototype.RelBinGenBasics(phpShow,indentBlock,commentBlock,addSlashes)
  import Debug.Trace

  installer :: Fspc -> Options -> String
  installer fSpec flags = "<?php\n  " ++ chain "\n  "
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
        , "  @mysql_query(\"CREATE DATABASE `"++dbName flags++"` DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
        , "  $DB_slct = @mysql_select_db('"++dbName flags++"');"
        , "}"
        , "if(!$DB_slct){"
        , "  echo die(\"Install failed: cannot connect to MySQL or error selecting database\");" --todo: full error report
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
          , "//// Number of plugs: "++(show (length (plugs fSpec)))
          , "if($existing==true){"
          ] ++ indentBlock 2 (concat (map checkPlugexists (plugs fSpec)))
          ++ ["}"]
          ++ concat (map plugCode (plugs fSpec))
          ++ ["mysql_query('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"]
        ) ++
        [ "}" ]
     ) ++ "\n?>\n"
    where plugCode plug
           = commentBlock (["Plug "++plname plug,"","fields:"]++(map (\x->show (fldexpr x)++"  "++show (multiplicities $ fldexpr x)) (fields plug)))
             ++
             [ "mysql_query(\"CREATE TABLE `"++plname plug++"`"]
             ++ indentBlock 17
                    ( [ comma: " `" ++ fldname f ++ "` " ++ showSQL (fldtype f) ++ autoIncr ++ nul
                      | (f,comma)<-zip (fields plug) ('(':repeat ',')
                      , let nul = if fldnull f then "" else " NOT NULL"
                      , let autoIncr = if fldauto f
                                       then " AUTO_INCREMENT" else ""
                      ] ++
                      [", UNIQUE KEY (`"++fldname key++"`)"
                      | key <- fields plug, flduniq key, not (fldnull key)]
                    )
             ++ ["                  ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
             , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             ++ (if (null $ mdata plug) then [] else
                 [ "else"
                                 , "mysql_query(\"INSERT IGNORE INTO `"++plname plug++"` ("++chain "," ["`"++fldname f++"` "|f<-fields plug]++")"
                                 ]++ indentBlock 12
                                                 ( [ comma++ " (" ++md++ ")"
                                                   | (md,comma)<-zip (mdata plug) ("VALUES":repeat "      ,")
                                                   ]
                                                 )
                                 ++ ["            \");"
                                 , "if($err=mysql_error()) { $error=true; echo $err.'<br />'; }"]
             )
          checkPlugexists plug
           = [ "if($columns = mysql_query(\"SHOW COLUMNS FROM `"++(plname plug)++"`\")){"
             , "  mysql_query(\"DROP TABLE `"++(plname plug)++"`\");" --todo: incremental behaviour
             , "}" ]
          mdata :: Plug -> [String]
          mdata plug
           = if name plug==name S then [ "1" ] else
             if length (fields plug)==2 -- treat binary tables differently
             then
             --DESCR -> the first field contains an expression::[A*A] which can be the expression I[A]
             --         the second field contains an expression Mph[A*B]
             --         we need the contents of the relation morphism only
             [ phpShow (srcPaire p) ++", "++ phpShow (trgPaire p)
             | Tm (m'@Mph{}) <- map fldexpr (fields plug), p<-contents m']
             else
             [ chain ", " [ head ([phpShow (trgPaire p)
                                  | p<-contents$fldexpr f,a==srcPaire p
                                  ]++
                                  [phpShow a
                                  | isIdent (fldexpr f) -- this should go automatically, but does not
                                  ]++["NULL"])
                          | f<-fields plug]
             | a<- rd $ map srcPaire (concat (map (contents.fldexpr) (fields plug))) -- be sure that the concepts return their respective populations
             ]
   
