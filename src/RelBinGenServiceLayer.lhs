> module RelBinGenServiceLayer where
>  import Char(toLower)
>  import Auxiliaries(commaNL,chain,eqCl,adlVersion,sord,eqClass)
>  import Calc(conjNF, triggers)
>  import CC_aux
>  import CommonClasses
>  import Collection(Collection(rd))
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import RelBinGenBasics
>--  import MultRules


dbError generates the text to be printed when 'rule' is violated. Parameters x and y are instances that participate in the violation.

>  dbError :: Rule -> String -> String -> String
>  dbError rule x y
>   = "Overtreding van de regel: \""++(if null (explain rule) then "Artificial explanation: "++showADL rule else explain rule)++"\"<BR>"
>      {-++
>      (charshow (dbCorrect ( Fu [ disjNF (Fu [f| f<-fr, f/=t])
>                                | Fi frs<-[conjNF (normExpr rule)], Fu fr<-frs, t<-fr]) x y))
>      -}
>     where
>      charshow (Forall vars restr)
>       = charVars "For all" vars ++ charshow restr
>      charshow (Not (Exists vars (Conj restr)))
>       = charVars "Verwijder " vars  ++ " opdat de zinnen " ++
>         commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" "++(if length restr==2 then "allebei" else "allemaal")++" onwaar worden"
>      charshow (Not (Exists vars (Disj restr)))
>       = charVars "Verwijder " vars  ++ " opdat &eacute;&eacute;n van de zinnen " ++
>         commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" onwaar wordt"
>      charshow (Exists vars (Conj restr))
>       = charVars "Voeg " vars  ++ " toe, zodat de zinnen " ++
>         commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" "++(if length restr==2 then "allebei" else "allemaal")++" waar worden"
>      charshow (Exists vars (Disj restr))
>       = charVars "Voeg " vars  ++ " toe, zodat &eacute;&eacute;n van de zinnen " ++
>         commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" waar wordt"
>      charshow (Implies antc conseq)
>       = charshow antc++" ==> "  ++ charshow conseq
>      charshow (Equiv lhs rhs)
>       = charshow lhs++" <=> "   ++ charshow rhs
>      charshow (Conj rs)
>       = if null rs then "" else
>         chain "<BR>&Eacute;N<BR>" (map charshow rs)
>      charshow (Disj rs)
>       = if null rs then "" else
>         chain "<BR>OF<BR>" (map charshow rs)
>      charshow (Not (Rel lhs m rhs))
>       = "Doet het even niet"
>         {- if isIdent e
>         then "Zorg dat "++
>              charshow lhs++" ongelijk wordt aan "++charshow rhs
>                     ++ ", door (\\'"++d++"\\',\\'"++c++"\\') te wijzigen of te verwijderen"
>         else "Maak de zin \\'"++
>              ( if inline m
>                then applyM (declaration m) (charshow lhs) (charshow rhs)
>                else applyM (declaration m) (charshow rhs) (charshow lhs)
>              )++ "\\' onwaar, door (\\'"++d++"\\',\\'"++c++"\\') te verwijderen uit "++name m
>         -}
>      charshow (Rel lhs m rhs)
>       = "Doet het even niet"
>         {- if isIdent m
>         then "Zorg dat "++
>              charshow (Funs d [m| t<-lhs, m<-mors t])++" gelijk wordt aan "++charshow rhs
>         else "Maak de zin \\'"++
>              ( if inline m
>                then applyM (declaration m) (charshow lhs) (charshow rhs)
>                else applyM (declaration m) (charshow rhs) (charshow lhs)
>              )++ "\\' waar door (\\'"++c++"\\',\\'"++d++"\\') toe te voegen aan "++name m
>          -}
>      charshow (Not rs) = "niet ("++charshow rs++")"
>      charshow (Funs x [])     = x
>      charshow (Funs x (m:ms)) = if isIdent m then charshow (Funs x ms) else charshow (Funs (x++"."++name m) ms)
>      charVars q vs
>       = if null vs then "" else
>         q++" "++chain "; " (map zet vss)
>         where
>          vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]
>          zet ([v], dType) = dType++", "++v++", "
>          zet (vs , dType) = commaNL "en" vs++"("++dType++")"
>      chrShw (Rel lhs m rhs)
>       = if False -- nog corrigeren. was:   isIdent e
>         then chrShw lhs++" is gelijk aan "++chrShw rhs
>         else if inline m
>              then applyM (declaration m) (chrShw lhs) (chrShw rhs)
>              else applyM (declaration m) (chrShw rhs) (chrShw lhs)
>      chrShw (Not (Rel lhs m rhs))
>       = if False -- nog corrigeren. was:   isIdent e
>         then chrShw lhs++" is ongelijk aan "++chrShw rhs
>         else "niet ("++(if inline m
>                         then applyM (declaration m) (chrShw lhs) (chrShw rhs)
>                         else applyM (declaration m) (chrShw rhs) (chrShw lhs)
>                        )++")"
>      chrShw r = charshow r


>  serviceLayer context noTrans beeper dbName -- komt in het bestand <context>.php
>   = chain "\n  " 
>     [ "<? // generated with "++adlVersion
>     , "$DB_daba = '"++ dbName {- was: name context -}++"';"
>     , "$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');"
>     , "$DB_slct = mysql_select_db("++dbName++",$DB_link);"
>     , ""
>     , "// DB_display is used for the entity headers"
>     , (let xs = sord ["\""++phpRelName context s++"\" => Array (\"Name\"=>"++phpShow (name s)++", \"Src\"=>"++phpShow (name (source s))++", \"Trg\"=>"++phpShow (name (target s))++")"
>                                          | o@(Obj nm pos c ats)<-attributes context, a<-ats, s<-declarations a   {-, not (isSignal s) -} ]++
>                                     ["\""++phpConcept context (concept o)++"\" => Array (\"Name\"=>"++phpShow (name o)++", \"Src\"=>"++phpShow (name o)++", \"Trg\"=>"++phpShow (name o)++")"
>                                     | o<-attributes context]
>        in if null xs then "$DB_display = Array();" else "$DB_display = Array\n      ( "++chain "\n      , " xs++"\n      );")
>     , ""
>     , "// DB_mult_cc is used by isOkRelation (entities) and by adlExport"
>     , "$DB_mult_cc = Array"
>     , "    ( "++chain "\n      , " [ "\""++phpRelName context s++"\"=>Array("++chain "," ["'"++show m++"'"| m<-multiplicities s, m/=Aut]++")"
>                                    | s<-declarations context, not (isSignal s)]++"\n      );"
>     , ""
>     , "//  DB_tbls is required for ADL export."
>     , "$DB_tbls = Array"
>     , "    ( "++chain "\n      , " [ "Array(\""++phpRelName context s++"\" , Array (\""++sqlRelSrc s++"\",\""++sqlRelTrg s++"\"))"
>                                    | s<-declarations context, not (isSignal s)]++"\n      );"
>     , ""
>     , "if($DB_debug>0){"
>     , "   function DB_debug($txt,$lvl){"
>     , "     global $DB_debug;"
>     , "     if($lvl>=$DB_debug || $lvl==null) {"
>     , "             if($DB_debug>=3) {echo \"<i>$txt</i>\\n<P />\\n\";}"
>     , "             else {echo \"<i>$txt</i>\\n<br />(debug level $lvl)<br />\\n\";}"
>     , "             return true;"
>     , "     }else return false;"
>     , "   }"
>     , "}else{"
>     , "        function DB_debug($txt,$lvl){return false;}"
>     , "}"
>     , chain "\n" (ruleFunctions context)
>     , ""
>     , chain "\n  " (createAndSelectDB context dbName noTrans)
>     , if noTrans then "  DB_doquer('SET AUTOCOMMIT=1');" else "  DB_doquer('SET AUTOCOMMIT=0');"
>     , ""
>     , "if($DB_debug<=3){"
>     , chain "\n  "
>        [ "  checkRule"++show (nr r)++"();"
>        | r<-rules context ]
>     , "}"
>     , ""
>     , "function DB_doquer($quer,$debug=false){"
>     , "  global $DB_link,$DB_errs;"
>     , "  if($debug) DB_debug($quer,5); else DB_debug($quer,1);"
>     , "  $result=mysql_query($quer,$DB_link);"
>     , "  if(!$result){"
>     , "    DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query \"'.$quer.'\"',4);"
>     , "    $DB_errs=true;"
>     , "    return false;"
>     , "  }"
>     , "  $rows=Array();"
>     , "  while (($row = @mysql_fetch_array($result))!==false) {"
>     , "    $rows[]=$row;"
>     , "    //print_r($row);"
>     , "    //echo '<br />';"
>     , "    unset($row);"
>     , "  }"
>     , "  return $rows;"
>     , "}"
>     , ""
>     , "function DB_drop(){"
>     , "  global $DB_daba;"
>     , "  DB_doquer('DROP DATABASE "++ dbName {- was: $DB_daba -}++"');"
>     , "}"
>     , ""
>     , "function closure0($table,$source,$target){"
>     , "  $cs=Array();"
>     , "  $cs=DB_doquer('SELECT t1.'.$source.' AS '.$source.',t2.'.$target.' AS '.$target.' FROM '.$table.' AS t1,'.$table.' AS t2 WHERE t1.'.$source.'=t2.'.$target);"
>     , "  for($i=0;$i<count($cs);$i++){"
>     , "    DB_doquer(\"INSERT IGNORE INTO $table SELECT t1.$source AS $source,t2.$target AS $target FROM $table AS t1, $table AS t2 WHERE t1.$target='\".addslashes($cs[$i][1]).\"' AND t2.$source='\".addslashes($cs[$i][0]).\"'\");"
>     , "  }"
>     , "  for($i=0;$i<count($cs);$i++){"
>     , "    DB_doquer(\"INSERT IGNORE INTO $table VALUES ( '\".addslashes($cs[$i][1]).\"' , '\".addslashes($cs[$i][0]).\"' )\");"
>     , "  }"
>     , "  return Array();"
>     , "}"
>     , ""
>     , "function closure1($table,$source,$target){"
>     , "  $cs=Array();"
>     , "  $cs=DB_doquer('SELECT t1.'.$source.' AS '.$source.',t2.'.$target.' AS '.$target.' FROM '.$table.' AS t1,'.$table.' AS t2 WHERE t1.'.$source.'=t2.'.$target);"
>     , "  for($i=0;$i<count($cs);$i++){"
>     , "    DB_doquer(\"INSERT IGNORE INTO $table SELECT t1.$source AS $source,t2.$target AS $target FROM $table AS t1, $table AS t2 WHERE t1.$target='\".addslashes($cs[$i][1]).\"' AND t2.$source='\".addslashes($cs[$i][0]).\"'\");"
>     , "  }"
>     , "  return Array();"
>     , "}"
>     , ""
>     , "function DB_affected(){"
>     , "   global $DB_link;"
>     , "   return mysql_affected_rows($DB_link);"
>     , "}"
>     , "?>"
>     ]
>    where
>     (entities, relations, ruls') = erAnalysis context
>     hcs = [hc| rule<-rules context, hc<-triggers rule ]


>  dbDelRelation context i s srcVar trgVar
>   = [' '| x<-[1..i-3]]++"// ON deleteEvent ("++show (source s)++","++show (target s)++") ON \""++name s++"\"["++name (source s)++"*"++name (target s)++"] DO"
>     ++"\n"++[' '| x<-[1..i]]++"DB_doquer('DELETE FROM "++sqlRelName context s++" WHERE "++sqlRelSrc s++"=\\''.addslashes("++srcVar++").'\\' AND "++sqlRelTrg s++"=\\''.addslashes("++trgVar++").'\\'');"
>     ++"\n"++[' '| x<-[1..i]]++"if(DB_affected()){"
>     ++"\n"++dbDelConcept context (i+2) (source s) srcVar
>     ++"\n"++dbDelConcept context (i+2) (target s) trgVar
>     ++"\n"++[' '| x<-[1..i]] ++"}"

>  createAndSelectDB context dbName noTrans
>   =  [ ""
>      , "if(!$DB_slct){"
>      , "      DB_debug( \"Warning: error connecting to database, building database\",3 );"
>      , "      mysql_query(\"CREATE DATABASE "++ dbName {- was: $DB_daba -}++"\",$DB_link) or die('Could not create DB "++dbName++"');"
>      , "      $DB_slct = mysql_select_db('"++ dbName {- was: $DB_daba -} ++"',$DB_link) or die ('Could not select DB "++dbName++"');"
>      , "      $DB_errs = false;"
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlClosName context e++" ("++sqlExprSrc e++" varchar(380) NOT NULL default '', "++sqlExprTrg e++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlExprSrc e++","++sqlExprTrg e++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1\");"
>                                     | e<-closE context, error ("clos: "++showADL e)]
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlRelName context s++" ("++sqlRelSrc s++" varchar(380) NOT NULL default '', "++sqlRelTrg s++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlRelSrc s++","++sqlRelTrg s++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1\");"++
>                                       if null chn then "" else
>                                       "\n        DB_doquer(\"INSERT INTO "++sqlRelName context s++" ("++sqlRelSrc s++","++sqlRelTrg s++
>                                       ") VALUES "++chn++"\");"
>                                     | s<-rd (declarations context), chn<-let truncate xs = if length xs>380 then take (380-if xs!!(380-1)=='\\' then 2 else 1) xs++"'" else xs
>                                                                          in [chain ", " ["("++truncate (phpShow a)++","++truncate (phpShow b)++")" | [a,b]<-contents s, not (null a), not (null b)]]]
>        ++if rd (declarations context)==declarations context then "" else
>          error ("(module RelBinGenServiceLayer) Fatal: Some declarations are not unique."++concat ["\n"++chain "\n" [showHS "" s|s<-cl]|cl<-eqClass (==) (declarations context), length cl>1])
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlConcept context c++" ("++sqlAttConcept context c++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlAttConcept context c++")) TYPE=InnoDB DEFAULT CHARACTER SET latin1\");"
>                                     | c<-concs context, ss<-[[s| s<-declarations context, not (null (contents s)), c <= source s || c <= target s]]]
>      , "      "++chain "\n        " [ if null ss then "" else
>                                       insConcept context c
>                                       ( chain " UNION " (["SELECT DISTINCT "++sqlRelSrc s++" FROM "++sqlRelName context s | s<-declarations context, not (null (contents s)), c <= source s]++
>                                                          ["SELECT DISTINCT "++sqlRelTrg s++" FROM "++sqlRelName context s | s<-declarations context, not (null (contents s)), c <= target s])
>                                       )
>                                     | c<-concs context, ss<-[[s| s<-declarations context, not (null (contents s)), c <= source s || c <= target s]]]
>      , "      "++chain "\n        " [ "\n        DB_doquer(\"INSERT IGNORE INTO "++sqlClosName context e++" "++selectNormFiExpr "$attrs" context 15 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"++
>                                       "\n        "++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName context e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');"
>                                     | e<-closE context]
>      , let checkers = [ "checkRule"++show (nr r)++"()" | r<-rules context ]
>        in "      if($DB_errs"++ (if noTrans || null checkers then "" else " || !("++chain " && " checkers++")")++")"
>      , "      {  DB_debug( \"DB errors, removing database\",5);"
>      , "         mysql_query(\"DROP DATABASE "++ dbName {- was: $DB_daba -}++"\",$DB_link) or die('Could not delete DB "++dbName++"');"
>      , "         die ('Errors creating database');"
>      , "        } else {"
>      , "           DB_doquer('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"
>      , "        }"
>      , "  }else{"
>      , "    DB_debug( \"Connected to database\",3 );"
>      , "  }"
>      ]

>  ruleFunctions context
>   = [ "\n  function checkRule"++show (nr rule)++"(){\n    "++
>          (if isFalse rule'
>           then "// Tautology:  "++showADL rule++"\n     "
>           else "// No violations should occur in ("++showADL rule++")\n    "++
>                concat [ "//            rule':: "++(showADL rule') ++"\n    " | pDebug] ++
>                concat [ "// sqlExprSrc rule':: "++src++"\n     " | pDebug] ++
>                "$v=DB_doquer('"++selectExpr context 19 src trg rule'++"');\n     "++
>                "if(count($v)) {\n    "++
>                "  DB_debug("++ phpShow (dbError rule ("'.$v[0]['"++src++"'].'") ("'.$v[0]['"++trg++"'].'")) ++",3);\n    "++
>                "  return false;\n    }")++
>                "return true;\n  }"
>        | rule<-rules context, rule'<-[(conjNF . Cp . normExpr) rule], src<-[sqlExprSrc rule'], trg<-[noCollide [src] (sqlExprTrg rule')] ]

Translation of rules to SQL

>  makeVar ex cs = mknew ex [[(toLower.head.(++"x").name) c]|c<-cs]
>   where
>    mknew ex [] = []
>    mknew ex (x:xs) | x `elem` ex = mknew ex ((x++"'"):xs)
>                    | otherwise = x: mknew (ex++[x]) xs

TODO: Uitsluitend genormaliseerde expressies (zowel Fu- als Fi-normaal) afdwingen in datastructuur
? waarom ?
