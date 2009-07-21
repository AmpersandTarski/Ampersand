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
       , "$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or require \"dbsettings.php\";"
       , "$DB_slct = mysql_select_db('"++dbName++"',$DB_link);"
       , ""
       ] ++ (ruleFunctions fSpec)
       ++
       [""
       , "if($DB_debug>=3){"
       ] ++
          [ "  checkRule"++show (runum r)++"();"
          | r<-vrules fSpec ] ++
       [ "}"
       ]
      )) ++ "?>"

   ruleFunctions :: Fspc -> [PHPcode]
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
   dbError rule x y
    = "Overtreding van de regel: \""++(if null (explain rule) then "Artificial explanation: "++showADL rule else explain rule)++"\"<BR>"
      where
       charVars q vs
        = if null vs then "" else
          q++" "++chain "; " (map zet vss)
          where
           vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]
           zet ([v], dType) = dType++", "++v++", "
           zet (vs , dType) = commaNL "en" vs++"("++dType++")"
