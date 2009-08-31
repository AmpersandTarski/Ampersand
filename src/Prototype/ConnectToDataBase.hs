{-# OPTIONS_GHC -Wall #-}
  module Prototype.ConnectToDataBase (connectToDataBase) 
  where
   --import Auxiliaries(eqClass,eqCl)
   import Strings (chain) --TODO -> is this correct instead of chain from Auxiliaries?
   --import Collection(rd)
   --import Calc(informalRule, disjNF, computeOrder, ComputeRule, triggers)
   import Adl (runum,nr,isFalse,normExpr,Rule,Expression(..))
   import ShowADL(showADL)
   import CommonClasses(explain,{- name -})
   import NormalForms (conjNF) --TODO -> correct replacement of Calc (conjNF)?
   import Prototype.RelBinGenBasics(selectExpr,sqlExprTrg,sqlExprSrc,phpShow,pDebug,noCollide)
   import Version (versionbanner)
   import Data.Fspec

   connectToDataBase :: Fspc -> String -> String
   connectToDataBase fSpec dbName
    = (chain "\n  " 
      ([ "<?php // generated with "++versionbanner
       , "require \"dbsettings.php\";"
       , ""
       , "function stripslashes_deep(&$value) "
       , "{ $value = is_array($value) ? "
       , "           array_map('stripslashes_deep', $value) : "
       , "           stripslashes($value); "
       , "    return $value; "
       , "} "
       , "if((function_exists(\"get_magic_quotes_gpc\") && get_magic_quotes_gpc()) "
       , "    || (ini_get('magic_quotes_sybase') && (strtolower(ini_get('magic_quotes_sybase'))!=\"off\")) ){ "
       , "    stripslashes_deep($_GET); "
       , "    stripslashes_deep($_POST); "
       , "    stripslashes_deep($_REQUEST); "
       , "    stripslashes_deep($_COOKIE); "
       , "} "
       , "$DB_slct = mysql_select_db('"++dbName++"',$DB_link);"
       , "function firstRow($rows){ return $rows[0]; }"
       , "function firstCol($rows){ foreach ($rows as $i=>&$v) $v=$v[0]; return $rows; }"
       , "function DB_debug($txt,$lvl=0){"
       , "  global $DB_debug;"
       , "  if($lvl<=$DB_debug) {"
       , "    echo \"<i title=\\\"debug level $lvl\\\">$txt</i>\\n<P />\\n\";"
       , "    return true;"
       , "  }else return false;"
       , "}"
       , ""
       , "$DB_errs = array();"
       , "// wrapper function for MySQL"
       , "function DB_doquer($quer,$debug=5)"
       , "{"
       , "  global $DB_link,$DB_errs;"
       , "  DB_debug($quer,$debug);"
       , "  $result=mysql_query($quer,$DB_link);"
       , "  if(!$result){"
       , "    DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query \"'.$quer.'\": '.mysql_error(),2);"
       , "    $DB_errs[]='Error '.($ernr=mysql_errno($DB_link)).' in query \"'.$quer.'\"';"
       , "    return false;"
       , "  }"
       , "  if($result===true) return true; // succes.. but no contents.."
       , "  $rows=Array();"
       , "  while (($row = @mysql_fetch_array($result))!==false) {"
       , "    $rows[]=$row;"
       , "    unset($row);"
       , "  }"
       , "  return $rows;"
       , "}"
       , "function DB_plainquer($quer,&$errno,$debug=5)"
       , "{"
       , "  global $DB_link,$DB_errs,$DB_lastquer;"
       , "  $DB_lastquer=$quer;"
       , "  DB_debug($quer,$debug);"
       , "  $result=mysql_query($quer,$DB_link);"
       , "  if(!$result){"
       , "    $errno=mysql_errno($DB_link);"
       , "    return false;"
       , "  }else{"
       , "    if(($p=stripos($quer,'INSERT'))!==false"
       , "       && (($q=stripos($quer,'UPDATE'))==false || $p<$q)"
       , "       && (($q=stripos($quer,'DELETE'))==false || $p<$q)"
       , "      )"
       , "    {"
       , "      return mysql_insert_id();"
       , "    } else return mysql_affected_rows();"
       , "  }"
       , "}"
       , ""
       ] ++ (ruleFunctions fSpec)
       ++
       [ ""
       , "if($DB_debug>=3){"
       ] ++
          [ "  checkRule"++show (runum r)++"();"
          | r<-vrules fSpec ] ++
       [ "}"
       ]
      )) ++ "\n?>"

   ruleFunctions :: Fspc -> [String]
   ruleFunctions fSpec
    = [ "\n  function checkRule"++show (nr rule)++"(){\n    "++
           (if isFalse rule'
            then "// Tautology:  "++showADL rule++"\n     "
            else "// No violations should occur in ("++showADL rule++")\n    "++
                 concat [ "//            rule':: "++(showADL rule') ++"\n    " | pDebug] ++
                 concat [ "// sqlExprSrc fSpec rule':: "++src++"\n     " | pDebug] ++
                 "$v=DB_doquer('"++selectExpr fSpec 19 src trg rule'++"');\n     "++
                 "if(count($v)) {\n    "++
                 "  DB_debug("++ phpShow (dbError rule ("'.$v[0]['"++src++"'].'") ("'.$v[0]['"++trg++"'].'")) ++",3);\n    "++
                 "  return false;\n    }"
           ) ++ "return true;\n  }"
         | rule<-vrules fSpec, rule'<-[(conjNF . Cp . normExpr) rule], src<-[sqlExprSrc fSpec rule'], trg<-[noCollide [src] (sqlExprTrg fSpec rule')] ]

   dbError :: Rule -> String -> String -> String
   dbError rule _ _
    = "Overtreding van de regel: \""++(if null (explain rule) then "Artificial explanation: "++showADL rule else explain rule)++"\"<BR>"