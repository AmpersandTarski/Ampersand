> module ObjBinGenConnectToDataBase where
>  import Char
>  import Auxiliaries
>  import Collection
>  import Calc(informalRule, disjNF, computeOrder, ComputeRule, triggers)
>  import ADLdef
>  import CC_aux( showHS
>               , applyM
>               )
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import Calc (conjNF)
>  import RelBinGenBasics

>  type PHPcode = String

This module generates component "connectToDataBase.inc.php", which is the foundation upon which alle
database service modules are built.
It contains all code to create the database, i.e. the table definitions and the initial population.
It also contains the checker-routines for all rules in the Compliant Service Layer (CSL).
Upon success, it yields a CSL with no violation of any of the rules.

>  connectToDataBase context dbName
>   = (chain "\n  " 
>     ([ "<?php // generated with "++adlVersion
>      , "$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');"
>      , "$DB_slct = mysql_select_db('"++dbName++"',$DB_link);"
>      , ""
>      ] ++ (ruleFunctions context)
>       ++
>      (createAndSelectDB context dbName False) ++
>      [""
>      , "if($DB_debug>=3){"
>      ] ++
>         [ "  checkRule"++show (nr r)++"();"
>         | r<-rules context ] ++
>      [ "}"
>      ]
>     )) ++ "?>"

>  createAndSelectDB :: Context -> String -> Bool -> [String] 
>  createAndSelectDB context dbName noTrans
>   =  [ ""
>      , "if(!$DB_slct){"
>      , "      DB_debug( \"Warning: error connecting to database, building database\",3 );"
>      , "      mysql_query(\"CREATE DATABASE "++ dbName {- was: $DB_daba -}++" DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\",$DB_link) or die('Could not create DB "++dbName++"');"
>      , "      $DB_slct = mysql_select_db('"++ dbName {- was: $DB_daba -} ++"',$DB_link) or die ('Could not select DB "++dbName++"');"
>      , "      $DB_errs = false;"
>      , "      set_time_limit(0);"
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlClosName context e++" ("++sqlExprSrc e++" varchar(380) NOT NULL default '', "++sqlExprTrg e++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlExprSrc e++","++sqlExprTrg e++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
>                                     | e<-closE context, error ("clos: "++showADL e)]
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlRelName context s++" ("++sqlRelSrc s++" varchar(380) NOT NULL default '', "++sqlRelTrg s++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlRelSrc s++","++sqlRelTrg s++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"++
>                                       if null chn then "" else
>                                       "\n        DB_doquer(\"INSERT INTO "++sqlRelName context s++" ("++sqlRelSrc s++","++sqlRelTrg s++
>                                       ") VALUES "++chn++"\");"
>                                     | s<-rd (declarations context), not (isIdent s)
>                                     , chn<-let truncate xs = if length xs>380 then take (380-if xs!!(380-1)=='\\' then 2 else 1) xs++"'" else xs
>                                            in [ chain ", " (rd ["("++truncate (phpShow a)++","++truncate (phpShow b)++")"
>                                                                | [a,b]<-contents s, not (null a), not (null b)])
>                                               ]
>                                     ]
>        ++if rd (declarations context)==declarations context then "" else
>          error ("(module RelBinGenServiceLayer) Fatal: Some declarations are not unique."++concat ["\n"++chain "\n" [showHS "" s|s<-cl]|cl<-eqClass (==) (declarations context), length cl>1])
>      , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlConcept context c++" ("++sqlAttConcept context c++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlAttConcept context c++")) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
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

>  ruleFunctions :: Context -> [PHPcode]
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
>                then applyM (makeDeclaration m) (charshow lhs) (charshow rhs)
>                else applyM (makeDeclaration m) (charshow rhs) (charshow lhs)
>              )++ "\\' onwaar, door (\\'"++d++"\\',\\'"++c++"\\') te verwijderen uit "++name m
>         -}
>      charshow (Rel lhs m rhs)
>       = "Doet het even niet"
>         {- if isIdent m
>         then "Zorg dat "++
>              charshow (Funs d [m| t<-lhs, m<-mors t])++" gelijk wordt aan "++charshow rhs
>         else "Maak de zin \\'"++
>              ( if inline m
>                then applyM (makeDeclaration m) (charshow lhs) (charshow rhs)
>                else applyM (makeDeclaration m) (charshow rhs) (charshow lhs)
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
>              then applyM (makeDeclaration m) (chrShw lhs) (chrShw rhs)
>              else applyM (makeDeclaration m) (chrShw rhs) (chrShw lhs)
>      chrShw (Not (Rel lhs m rhs))
>       = if False -- nog corrigeren. was:   isIdent e
>         then chrShw lhs++" is ongelijk aan "++chrShw rhs
>         else "niet ("++(if inline m
>                         then applyM (makeDeclaration m) (chrShw lhs) (chrShw rhs)
>                         else applyM (makeDeclaration m) (chrShw rhs) (chrShw lhs)
>                        )++")"
>      chrShw r = charshow r
