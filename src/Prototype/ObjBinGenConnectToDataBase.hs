  module Prototype.ObjBinGenConnectToDataBase where
 --  import Char
   import Auxiliaries(eqClass,eqCl)
   import Strings (chain) --TODO -> is this correct instead of chain from Auxiliaries?
   import Collection(rd)
 --  import Calc(informalRule, disjNF, computeOrder, ComputeRule, triggers)
   import Adl
   import ShowADL(showADL)
   import ShowHS(showHS)
   import CC_aux( applyM )
   import CommonClasses(explain,name)
   import PredLogic -- (for error messages by dbCorrect)
 --  import Hatml     -- (for converting error messages to HTML)
 --  import Atlas     -- (for converting error messages to HTML)
   import NormalForms (conjNF) --TODO -> correct replacement of Calc (conjNF)?

   import Prototype.RelBinGenBasics(selectExpr,sqlExprTrg,sqlExprSrc,addSlashes,sqlMorName
                        ,sqlConcept,sqlAttConcept,sqlMorSrc
                        ,sqlClosName,closE,sqlRelName,sqlRelSrc,sqlRelTrg
                        ,phpShow,insConcept
                        ,selectNormFiExpr,clos0,pDebug,noCollide)
   import Version (versionbanner)
   import Data.Fspec
   import Prototype.Garbage --TODO -> clean up Garbage



   type PHPcode = String


   connectToDataBase fSpec dbName
    = (chain "\n  " 
      ([ "<?php // generated with "++versionbanner
       , "$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');"
       , "$DB_slct = mysql_select_db('"++dbName++"',$DB_link);"
       , ""
       ] ++ (ruleFunctions fSpec)
        ++
       (createAndSelectDB fSpec dbName False) ++
       [""
       , "if($DB_debug>=3){"
       ] ++
          [ "  checkRule"++show (runum r)++"();"
          | r<-vrules fSpec ] ++
       [ "}"
       ]
      )) ++ "?>"

   createAndSelectDB :: Fspc -> String -> Bool -> [String] 
   createAndSelectDB fSpec dbName noTrans
    =  [ ""
       , "if(!$DB_slct){"
       , "      DB_debug( \"Warning: error connecting to database, building database\",3 );"
       , "      mysql_query(\"CREATE DATABASE "++ dbName {- was: $DB_daba -}++" DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\",$DB_link) or die('Could not create DB "++dbName++"');"
       , "      $DB_slct = mysql_select_db('"++ dbName {- was: $DB_daba -} ++"',$DB_link) or die ('Could not select DB "++dbName++"');"
       , "      $DB_errs = false;"
       , "      set_time_limit(0);"
       , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlClosName fSpec e++" ("++sqlExprSrc e++" varchar(380) NOT NULL default '', "++sqlExprTrg e++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlExprSrc e++","++sqlExprTrg e++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
                                      | e<-closE fSpec, error ("clos: "++showADL e)]
       , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlRelName fSpec s++" ("++sqlRelSrc s++" varchar(380) NOT NULL default '', "++sqlRelTrg s++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlRelSrc s++","++sqlRelTrg s++") ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"++
                                        if null chn then "" else
                                        "\n        DB_doquer(\"INSERT INTO "++sqlRelName fSpec s++" ("++sqlRelSrc s++","++sqlRelTrg s++
                                        ") VALUES "++chn++"\");"
                                      | s<-vrels {-was declarations-} fSpec, not (isIdent s)
                                      , chn<-let truncate xs = if length xs>380 then take (380-if xs!!(380-1)=='\\' then 2 else 1) xs++"'" else xs
                                             in [ chain ", " [ "("++truncate (phpShow a)++","++truncate (phpShow b)++")"
                                                             | [a,b]<-contents s, not (null a), not (null b)]
                                                ]
                                      ]
         ++if rd (vrels fSpec)==vrels fSpec then "" else
           error ("(module RelBinGenServiceLayer) Fatal: Some declarations are not unique."++concat ["\n"++chain "\n" [showHS "" s|s<-cl]|cl<-eqClass (==) (vrels fSpec), length cl>1])
       , "      "++chain "\n        " [ "DB_doquer(\"CREATE TABLE "++sqlConcept fSpec c++" ("++sqlAttConcept fSpec c++" varchar(380) NOT NULL default '', UNIQUE  ("++sqlAttConcept fSpec c++")) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
                                      | c<-concs fSpec, ss<-[[s| s<-vrels fSpec, not (null (contents s)), c <= source s || c <= target s]]]
       , "      "++chain "\n        " [ if null ss then "" else
                                        insConcept fSpec c
                                        ( chain " UNION " (["SELECT DISTINCT "++sqlRelSrc s++" FROM "++sqlRelName fSpec s | s<-vrels fSpec, not (null (contents s)), c <= source s]++
                                                           ["SELECT DISTINCT "++sqlRelTrg s++" FROM "++sqlRelName fSpec s | s<-vrels fSpec, not (null (contents s)), c <= target s])
                                        )
                                      | c<-concs fSpec, ss<-[[s| s<-vrels fSpec, not (null (contents s)), c <= source s || c <= target s]]]
       , "      "++chain "\n        " [ "\n        DB_doquer(\"INSERT IGNORE INTO "++sqlClosName fSpec e++" "++selectNormFiExpr "$attrs" fSpec 15 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"++
                                        "\n        "++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName fSpec e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');"
                                      | e<-closE fSpec]
       , let checkers = [ "checkRule"++show (runum r)++"()" | r<-vrules fSpec ]
         in "      if($DB_errs"++ (if noTrans || null checkers then "" else " || !("++chain " && " checkers++")")++")"
       , "      {  DB_debug( \"DB errors, removing database\",5);"
       , "         mysql_query(\"DROP DATABASE "++ dbName {- was: $DB_daba -}++"\",$DB_link) or die('Could not delete DB "++dbName++"');"
       , "         set_time_limit(30);"
       , "         die ('Errors creating database');"
       , "        } else {"
       , "           DB_doquer('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');"
       , "        }"
       , "  }else{"
       , "    set_time_limit(30);"
       , "    DB_debug( \"Connected to database\",3 );"
       , "  }"
       ]

   ruleFunctions :: Fspc -> [PHPcode]
   ruleFunctions fSpec
    = [ "\n  function checkRule"++show (nr rule)++"(){\n    "++
           (if isFalse rule'
            then "// Tautology:  "++showADL rule++"\n     "
            else "// No violations should occur in ("++showADL rule++")\n    "++
                 concat [ "//            rule':: "++(showADL rule') ++"\n    " | pDebug] ++
                 concat [ "// sqlExprSrc rule':: "++src++"\n     " | pDebug] ++
                 "$v=DB_doquer('"++selectExpr fSpec 19 src trg rule'++"');\n     "++
                 "if(count($v)) {\n    "++
                 "  DB_debug("++ phpShow (dbError rule ("'.$v[0]['"++src++"'].'") ("'.$v[0]['"++trg++"'].'")) ++",3);\n    "++
                 "  return false;\n    }")++
                 "return true;\n  }"
         | rule<-vrules fSpec, rule'<-[(conjNF . Cp . normExpr) rule], src<-[sqlExprSrc rule'], trg<-[noCollide [src] (sqlExprTrg rule')] ]



   dbError :: Rule -> String -> String -> String
   dbError rule x y
    = "Overtreding van de regel: \""++(if null (explain rule) then "Artificial explanation: "++showADL rule else explain rule)++"\"<BR>"
       {-++
       (charshow (dbCorrect ( Fu [ disjNF (Fu [f| f<-fr, f/=t])
                                 | Fi frs<-[conjNF (normExpr rule)], Fu fr<-frs, t<-fr]) x y))
       -}
      where
       charshow (Forall vars restr)
        = charVars "For all" vars ++ charshow restr
       charshow (Not (Exists vars (Conj restr)))
        = charVars "Verwijder " vars  ++ " opdat de zinnen " ++
          commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" "++(if length restr==2 then "allebei" else "allemaal")++" onwaar worden"
       charshow (Not (Exists vars (Disj restr)))
        = charVars "Verwijder " vars  ++ " opdat &eacute;&eacute;n van de zinnen " ++
          commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" onwaar wordt"
       charshow (Exists vars (Conj restr))
        = charVars "Voeg " vars  ++ " toe, zodat de zinnen " ++
          commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" "++(if length restr==2 then "allebei" else "allemaal")++" waar worden"
       charshow (Exists vars (Disj restr))
        = charVars "Voeg " vars  ++ " toe, zodat &eacute;&eacute;n van de zinnen " ++
          commaNL "en" [ "\\'"++chrShw r++"\\'" | r<-restr]++" waar wordt"
       charshow (Implies antc conseq)
        = charshow antc++" ==> "  ++ charshow conseq
       charshow (Equiv lhs rhs)
        = charshow lhs++" <=> "   ++ charshow rhs
       charshow (Conj rs)
        = if null rs then "" else
          chain "<BR>&Eacute;N<BR>" (map charshow rs)
       charshow (Disj rs)
        = if null rs then "" else
          chain "<BR>OF<BR>" (map charshow rs)
       charshow (Not (Rel lhs m rhs))
        = "Doet het even niet"
          {- if isIdent e
          then "Zorg dat "++
               charshow lhs++" ongelijk wordt aan "++charshow rhs
                      ++ ", door (\\'"++d++"\\',\\'"++c++"\\') te wijzigen of te verwijderen"
          else "Maak de zin \\'"++
               ( if inline m
                 then applyM (makeDeclaration m) (charshow lhs) (charshow rhs)
                 else applyM (makeDeclaration m) (charshow rhs) (charshow lhs)
               )++ "\\' onwaar, door (\\'"++d++"\\',\\'"++c++"\\') te verwijderen uit "++name m
          -}
       charshow (Rel lhs m rhs)
        = "Doet het even niet"
          {- if isIdent m
          then "Zorg dat "++
               charshow (Funs d [m| t<-lhs, m<-mors t])++" gelijk wordt aan "++charshow rhs
          else "Maak de zin \\'"++
               ( if inline m
                 then applyM (makeDeclaration m) (charshow lhs) (charshow rhs)
                 else applyM (makeDeclaration m) (charshow rhs) (charshow lhs)
               )++ "\\' waar door (\\'"++c++"\\',\\'"++d++"\\') toe te voegen aan "++name m
           -}
       charshow (Not rs) = "niet ("++charshow rs++")"
       charshow (Funs x [])     = x
       charshow (Funs x (m:ms)) = if isIdent m then charshow (Funs x ms) else charshow (Funs (x++"."++name m) ms)
       charVars q vs
        = if null vs then "" else
          q++" "++chain "; " (map zet vss)
          where
           vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]
           zet ([v], dType) = dType++", "++v++", "
           zet (vs , dType) = commaNL "en" vs++"("++dType++")"
       chrShw (Rel lhs m rhs)
        = if False -- nog corrigeren. was:   isIdent e
          then chrShw lhs++" is gelijk aan "++chrShw rhs
          else if inline m
               then applyM (makeDeclaration m) (chrShw lhs) (chrShw rhs)
               else applyM (makeDeclaration m) (chrShw rhs) (chrShw lhs)
       chrShw (Not (Rel lhs m rhs))
        = if False -- nog corrigeren. was:   isIdent e
          then chrShw lhs++" is ongelijk aan "++chrShw rhs
          else "niet ("++(if inline m
                          then applyM (makeDeclaration m) (chrShw lhs) (chrShw rhs)
                          else applyM (makeDeclaration m) (chrShw rhs) (chrShw lhs)
                         )++")"
       chrShw r = charshow r
