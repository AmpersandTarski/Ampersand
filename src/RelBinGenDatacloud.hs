{-# LINE 1 "RelBinGenDatacloud.lhs" #-}
#line 1 "RelBinGenDatacloud.lhs"
  module RelBinGenDatacloud where
   import Auxiliaries(chain,adlVersion)
   import Calc(conjNF, triggers)
   import ADLdef
   import CommonClasses
   import Collection (Collection(rd))
   import ERmodel
   import PredLogic -- (for error messages by dbCorrect)
   import RelBinGenBasics
 --  import MultRules

   dataCloud context filename -- komt in het bestand <context>.cloud.php
    = chain "\n" 
      [ "<? // generated with "++adlVersion
      , "  require_once \""++filename++".php\";"
      , ""
      , "  // DB_titles is used for the entity headers with keys (Bas, klopt dit commentaar?)"
      , if null (attributes context) then "  $DB_conceptTypes   = Array();" else
        "  $DB_conceptTypes = Array\n"++
        "      ( "++chain "\n      , " [ "\""++phpConcept context c++"\"\n        => Array ( "++chain ", " ["\""++phpRelName context s++"\""|s<-declarations context, source s==c || target s==c]++" )"
                                       | c<-concs context]
                                       ++"\n      );"




















      , ""
      , "  // DB_tbls2 is used for ... (Bas, uitleggen...)"
      , if null relations then "  $DB_tbls2 = Array();" else
        "  $DB_tbls2 = Array\n      ( "++chain "\n      , "
        [ "\""++phpRelName context s++"\" => Array (\""++sqlConcept context (source s)++"\",\""++sqlConcept context (target s)++"\")"
        | s<-declarations context ]++"\n      );"















      , "  // DB_tbls3 is used for ... (Bas, uitleggen...)"
      , if null relations then "  $DB_tbls3 = Array();" else
        "  $DB_tbls3 = Array\n      ( "++chain "\n      , "
        [ "\""++phpRelName context s++"\" => Array (\""++sqlRelSrc s++"\",\""++sqlRelTrg s++"\")"
        | s<-declarations context ]++"\n      );"















      , ""
      , "  // DB_concepts is used for ... (Bas, uitleggen...)"
      , "  $DB_concepts = Array( "++chain "\n                      , "
        ([ "'"++phpConcept context c++"'=>"++phpShow (name c)| c<-concs context ]++
         [ "'"++p++"'=>"++phpShow (name c)
         | (p,c)<-rd ([(p,c)| r<-relations, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]] ++
                      [(p,c)| r<-rules context, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]])
         ])
      , "                    );"































      , "" ] ++ "\n?>"
    where
        (datasets, viewEsts, relations, erruls) = erAnalysis context
