> module RelBinGenDatacloud where
>  import Auxiliaries(chain,adlVersion)
>  import Calc(conjNF, triggers)
>  import CC_aux
>  import CommonClasses
>  import Collection (Collection(rd))
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import RelBinGenBasics
>--  import MultRules

>  dataCloud context filename -- komt in het bestand <context>.cloud.php
>   = chain "\n" 
>     [ "<? // generated with "++adlVersion
>     , "  require_once \""++filename++".php\";"
>     , ""
>     , "  // DB_titles is used for the entity headers with keys (Bas, klopt dit commentaar?)"
>     , if null (attributes context) then "  $DB_conceptTypes   = Array();" else
>       "  $DB_conceptTypes = Array\n"++
>       "      ( "++chain "\n      , " [ "\""++phpConcept context c++"\"\n        => Array ( "++chain ", " ["\""++phpRelName context s++"\""|s<-declarations context, source s==c || target s==c]++" )"
>                                      | c<-concs context]
>                                      ++"\n      );"

Voorbeeld:
  // DB_conceptTypes is used for the entity headers with keys (Bas, klopt dit commentaar?)
  $DB_conceptTypes = Array
      ( "C1_P_erson"
        => Array ( "T1_authentic", "T2_applicant", "T8_inhabitant" )
      , "C2_ID_document"
        => Array ( "T1_authentic", "T3_checked" )
      , "C3_A_pplication"
        => Array ( "T2_applicant", "T3_checked", "T4_assigned", "T6_kind", "T10_leadsto" )
      , "C4_E_mployee"
        => Array ( "T4_assigned", "T5_auth", "T9_area" )
      , "C5_P_roduct"
        => Array ( "T5_auth", "T6_kind", "T7_kind" )
      , "C6_D_ecision"
        => Array ( "T7_kind", "T10_leadsto" )
      , "C7_A_rea"
        => Array ( "T8_inhabitant", "T9_area" )
      );
  
>     , ""
>     , "  // DB_tbls2 is used for ... (Bas, uitleggen...)"
>     , if null relations then "  $DB_tbls2 = Array();" else
>       "  $DB_tbls2 = Array\n      ( "++chain "\n      , "
>       [ "\""++phpRelName context s++"\" => Array (\""++sqlConcept context (source s)++"\",\""++sqlConcept context (target s)++"\")"
>       | s<-declarations context ]++"\n      );"

--Let op: de volgende informatie zit ook al in $DB_typs
  $DB_tbls2 = Array
      ( "T1_authentic" => Array ("C1_P_erson","C2_ID_document")
      , "T2_applicant" => Array ("C3_A_pplication","C1_P_erson")
      , "T3_checked" => Array ("C3_A_pplication","C2_ID_document")
      , "T4_assigned" => Array ("C3_A_pplication","C4_E_mployee")
      , "T5_auth" => Array ("C4_E_mployee","C5_P_roduct")
      , "T6_kind" => Array ("C3_A_pplication","C5_P_roduct")
      , "T7_kind" => Array ("C6_D_ecision","C5_P_roduct")
      , "T8_inhabitant" => Array ("C1_P_erson","C7_A_rea")
      , "T9_area" => Array ("C4_E_mployee","C7_A_rea")
      , "T10_leadsto" => Array ("C3_A_pplication","C6_D_ecision")
      );

>     , "  // DB_tbls3 is used for ... (Bas, uitleggen...)"
>     , if null relations then "  $DB_tbls3 = Array();" else
>       "  $DB_tbls3 = Array\n      ( "++chain "\n      , "
>       [ "\""++phpRelName context s++"\" => Array (\""++sqlRelSrc s++"\",\""++sqlRelTrg s++"\")"
>       | s<-declarations context ]++"\n      );"

--Let op: de volgende informatie zit ook al in $DB_typs
    $DB_tbls3 = Array
      ( "T1_authentic" => Array ("AttP_erson","AttID_document")
      , "T2_applicant" => Array ("AttA_pplication","AttP_erson")
      , "T3_checked" => Array ("AttA_pplication","AttID_document")
      , "T4_assigned" => Array ("AttA_pplication","AttE_mployee")
      , "T5_auth" => Array ("AttE_mployee","AttP_roduct")
      , "T6_kind" => Array ("AttA_pplication","AttP_roduct")
      , "T7_kind" => Array ("AttD_ecision","AttP_roduct")
      , "T8_inhabitant" => Array ("AttP_erson","AttA_rea")
      , "T9_area" => Array ("AttE_mployee","AttA_rea")
      , "T10_leadsto" => Array ("AttA_pplication","AttD_ecision")
      );

>     , ""
>     , "  // DB_concepts is used for ... (Bas, uitleggen...)"
>     , "  $DB_concepts = Array( "++chain "\n                      , "
>       ([ "'"++phpConcept context c++"'=>"++phpShow (name c)| c<-concs context ]++
>        [ "'"++p++"'=>"++phpShow (name c)
>        | (p,c)<-rd ([(p,c)| r<-relations, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]] ++
>                     [(p,c)| r<-rules context, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]])
>        ])
>     , "                    );"

  $DB_concepts = Array( 'C1_P_erson'=>'Person'
                      , 'C2_ID_document'=>'IDdocument'
                      , 'C3_A_pplication'=>'Application'
                      , 'C4_E_mployee'=>'Employee'
                      , 'C5_P_roduct'=>'Product'
                      , 'C6_D_ecision'=>'Decision'
                      , 'C7_A_rea'=>'Area'
                      , 'AttA_pplication'=>'Application'
                      , 'AttID_document'=>'IDdocument'
                      , 'AttE_mployee'=>'Employee'
                      , 'AttP_roduct'=>'Product'
                      , 'AttA_rea'=>'Area'
                      , 'AttD_ecision'=>'Decision'
                      , 'AttP_erson'=>'Person'
                      , 'SrcP_erson'=>'Person'
                      , 'TrgP_erson'=>'Person'
                      , 'SrcID_document'=>'IDdocument'
                      , 'TrgID_document'=>'IDdocument'
                      , 'SrcA_pplication'=>'Application'
                      , 'TrgA_pplication'=>'Application'
                      , 'SrcE_mployee'=>'Employee'
                      , 'TrgE_mployee'=>'Employee'
                      , 'SrcP_roduct'=>'Product'
                      , 'TrgP_roduct'=>'Product'
                      , 'SrcD_ecision'=>'Decision'
                      , 'TrgD_ecision'=>'Decision'
                      , 'SrcA_rea'=>'Area'
                      , 'TrgA_rea'=>'Area'
                      );

>     , "" ] ++ "\n?>"
>   where
>       (entities, relations, erruls) = erAnalysis context
