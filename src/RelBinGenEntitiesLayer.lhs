> module RelBinGenEntitiesLayer where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, shrink, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics
>--  import MultRules

>  entitiesLayer context filename noTransactions beeper
>   = chain "\n  "
>     [ "<? // generated with "++adlVersion
>     , "require_once \""++filename++".php\";"
>     , ""
>     , "// DB_titles is used for the entity headers with keys"
>     , if null entities then "$DB_titles   = Array();" else
>       "$DB_titles = Array\n  "++
>       "    ( "++chain "\n      , " [ "\""++phpConcept context c++"\"\n        => Array ( "++chain "\n                 , "
>                                      (if c `elem` [k| (k,lbl,ms)<-keys context]
>                                       then [ "\"label\"=>"++phpShow (labelname (nm,lbl,ks))++", \"titles\"=>Array('"++
>                                              chain "','" [phpMorName context k| k<-ks++[a|(a,_)<-as,not (a `elem` ks)]]++"')"
>                                            |(nm,lbl,ks)<-[(k,lbl,ms)| (k,lbl,ms)<-keys context, k==c]]
>                                       else ["\"label\"=>"++phpShow "Unique ID"++
>                                             ", \"titles\"=>Array('"++phpConcept context c++"', '"++chain "', '" [phpMorName context a|(a,_)<-as]++"')"])++")"
>                                    | (c,as)<-entities]
>                                    ++"\n      );"
>     , if null entities then "$DB_entities = Array();" else "\n  "++
>       "$DB_entities= Array\n     ( "++chain "\n     , "
>       [ "Array\n        ( 'Name'=>"++phpShow (name c)++
>                            "\n        , 'ID'=>'"++phpConcept context c++
>                           "'\n        , 'Attr'=>Array('"++phpConcept context c++"'=>'"++{-addslashes-} (name c)++"','"++
>                           chain "','" [ phpMorName context a++"'=>'"++{-addslashes-}(name (target a)) | (a,_)<-as]++"')"++
>                           "\n        , 'Disabled'=>Array('"++phpConcept context c++"'=>'1','"++
>                           chain "','" [ phpMorName context a++"'=>'"++(if declaration a `elem` comp then "1"  else "0") | (a,_)<-as]++"')\n        )"
>       | (c,as)<-entities ]++"\n     );"
>     , ""
>     , "$DB_concepts = Array( "++chain "\n                      , "
>       ([ "'"++phpConcept context c++"'=>"++phpShow (name c)| c<-concs context ]++
>        [ "'"++p++"'=>"++phpShow (name c)
>        | (p,c)<-rd ([(p,c)| r<-relations, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]] ++
>                     [(p,c)| r<-rules context, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]])
>        ])
>     , "                    );"
>     , ""
>     , if null (filter (not.isSgnl) relations) then "$DB_nonEntRels=Array();" else
>       "$DB_nonEntRels=Array( "++chain "\n                      , "
>       [ "Array('ID'=>'"++phpRelName context r++"','Name'=>"++phpShow (name r)++",'Src'=>'"++phpRelSrc context r++"','Trg'=>'"++phpRelTrg context r++"')"
>       | r<-relations, not (isSgnl r) ]++"\n                      );"
>     , ""
>     , if null (signals context) then "$DB_rules=Array();" else
>       "$DB_rules=Array( "++chain "\n                 , "
>       [ "Array('ID'=>'"++sqlRuleName context r++
>               "','Name'=>'"++addslashes (name signal)++
>               "','Src'=>'"++phpRelSrc context r++
>               "','Trg'=>'"++phpRelTrg context r++
>               "','Fix'=>Array ('"++chain "','" ([ {- if name c=="Opdracht" then error ("(module RelBinGen) "++chain "\n" ([showHS a++"\n  "++showHS (declaration a)|(a,_)<-as])) else -} phpConcept context c | (c,as)<-entities, not (null (mors r `isc` mors [a|(a,_)<-as]))]++
>                                                 [ phpRelName context s | s<-relations,     s `elem` [declaration m| m<-mors r]])++
>               "'),'Desc'=>"++phpShow (if null (explain r) then "Artificial explanation: "++(lang English .assemble.normRule) r else explain r)++")"
>       | r@(Sg p rule expla sgn nr pn signal)<-signals context]++"\n                 );"
>     , ""
>     , "// DB_typs is used by isOkEntity (only)"
>     , "$DB_typs = Array"
>     , "    ( "++chain "\n      , " [ "\""++phpRelName context s++"\" => Array (\""++sqlRelSrc s++"\",\""++sqlRelTrg s++"\",\""++sqlConcept context (source s)++"\",\""++sqlConcept context (target s)++"\",\""++sqlAttConcept context (source s)++"\",\""++sqlAttConcept context (target s)++"\")" | s<-declarations context]++"\n      );"
>     , ""
>     , "// these functions are used by isOkRelation (only, through DB_get_violations_table)"
>     , "function DB_vioAll_RFX ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   $c=$DB_typs[$rel][2];"
>     , "   $cAtt=$DB_typs[$rel][4];"
>     , "   return DB_doquer(\"SELECT $cAtt,$cAtt FROM $c WHERE NOT EXISTS (SELECT * FROM $rel WHERE $rel.$src=$c.$cAtt AND $rel.$trg=$c.$cAtt)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_TRN ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   return DB_doquer(\"SELECT t0.$src,t1.$trg FROM $rel AS t0, $rel AS t1 WHERE t0.$trg=t1.$src AND NOT EXISTS (SELECT * FROM $rel AS t2 WHERE t2.$src=t0.$src AND t2.$trg=t1.$trg)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_ASY ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   $c=$DB_typs[$rel][2];"
>     , "   $cAtt=$DB_typs[$rel][4];"
>     , "   return DB_doquer(\"SELECT antc.$src, antc.$trg \"."
>     , "                    \"FROM (SELECT f0.$src, f0.$trg \"."
>     , "                          \"FROM $rel AS f0 \"."
>     , "                             \", $rel AS f1 \"."
>     , "                          \"WHERE f0.$src=f1.$trg AND f0.$trg=f1.$src \"."
>     , "                          \") AS antc \"."
>     , "                    \"WHERE NOT EXISTS (SELECT * FROM $c \"."
>     , "                                      \"WHERE antc.$src=$c.$cAtt AND antc.$trg=$c.$cAtt)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_SYM ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   return DB_doquer(\"SELECT antc.$src, antc.$trg FROM $rel AS antc WHERE NOT EXISTS (SELECT * FROM $rel AS cons WHERE antc.$src=cons.$trg AND antc.$trg=cons.$src) \"."
>     , "                    \"UNION \"."
>     , "                    \"SELECT antc.$trg AS $src, antc.$src AS $trg FROM $rel AS antc WHERE NOT EXISTS (SELECT * FROM $rel AS cons WHERE antc.$trg=cons.$src AND antc.$src=cons.$trg)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_INJ ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   return DB_doquer(\"SELECT $src,$trg FROM $rel WHERE EXISTS (SELECT * FROM $rel AS Flp_$rel WHERE $rel.$src<>Flp_$rel.$src AND $rel.$trg=Flp_$rel.$trg)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_SUR ($rel){"
>     , "   global $DB_typs;"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   $c=$DB_typs[$rel][3];"
>     , "   $cAtt=$DB_typs[$rel][5];"
>     , "   return DB_doquer(\"SELECT $cAtt FROM $c WHERE NOT EXISTS (SELECT $trg FROM $rel WHERE $rel.$trg = $c.$cAtt)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_TOT ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $c=$DB_typs[$rel][2];"
>     , "   $cAtt=$DB_typs[$rel][4];"
>     , "   return DB_doquer(\"SELECT $cAtt FROM $c WHERE NOT EXISTS (SELECT $src FROM $rel  WHERE $rel.$src = $c.$cAtt)\");"
>     , "}"
>     , ""
>     , "function DB_vioAll_UNI ($rel){"
>     , "   global $DB_typs;"
>     , "   $src=$DB_typs[$rel][0];"
>     , "   $trg=$DB_typs[$rel][1];"
>     , "   return DB_doquer(\"SELECT $src,$trg FROM $rel WHERE EXISTS (SELECT * FROM $rel AS Flp_$rel WHERE $rel.$src=Flp_$rel.$src AND $rel.$trg<>Flp_$rel.$trg)\");"
>     , "}"
>     , ""
>     , "// used by isOkRelation"
>     , "function DB_get_violations_table($tabl){"
>     , "   global $DB_mult_cc;"
>     , "   $viols = Array();"
>     , "   "
>     , "   for($i=0;$i<count($DB_mult_cc[$tabl]);$i++){"
>     , "     if($viol=call_user_func('DB_vioAll_'.$DB_mult_cc[$tabl][$i],$tabl)) $viols[$DB_mult_cc[$tabl][$i]] = $viol;"
>     , "   }"
>     , "   "
>     , "   if(count($viols)) return $viols;"
>     , "   return false;"
>     , "}"
>     , "" 
>     , "function isOkEntity($ent){"
>     , " switch($ent){"
>     , "   "++chain "\n     "
>       [ "case '"++phpConcept context c++"':\n       return "++
>         chain "&&" [ "isOkRelation('"++phpMorName context a++"')" | (a,_)<-as]++";"| (c,as)<-entities ]
>     , " }"
>     , " return DB_debug('Entity '.$ent.' unknown',4);"
>     , "}"
>     , "function isOkRelation($rel){"
>     , "  if($mv=DB_get_violations_table($rel)){"
>     , "    foreach($mv as $k=>$v){"
>     , "      if(count($v[0])) return false;"
>     , "    }"
>     , "  }"
>     , "  return true;"
>     , "}"
>     , ""
>     , "function quoteSplit($txt){"
>     , "  // split a string like:"
>     , "  // 'word 1','2\'nd word','Old\' bevelsborg'"
>     , "  // into array('word 1','2\'nd word','Old\' bevelsborg')"
>     , "  // note that the delimiter (i.e. ',' in the above example) can be anything, or even nothing."
>     , "  if(preg_match_all('/(?:\\')(((?'.'>(\\\\\\\\.)*)(.*?)(?'.'>(\\\\\\\\.)*))*?)(?:\\')/si',$txt,$res))"
>     , "    return array_map('stripslashes',$res[1]); else return array();"
>     , "}"
>     , "function quoteSplitArray($arr){"
>     , "  // function made for DB_getEnt to make good use of quoteSplit"
>     , "  for($i=0;$i<count($arr);$i++){"
>     , "    foreach($arr[$i] as $j=>$val){"
>     , "      $arr[$i][$j]=quoteSplit($val);"
>     , "    }"
>     , "  }"
>     , "  return $arr;"
>     , "}"
>     , ""
>     , "function DB_getEntOmmissions($ent){"
>     , "  switch($ent){"
>     , chain "\n  "
>       [ "    case '"++phpConcept context c++"':\n        "++
>         if or [Sur `elem` multiplicities a| (a,_)<-as]
>         then "return DB_doquer('"++
>              chain (phpIndent 26++"UNION"++phpIndent 26)
>                [ "SELECT NULL AS "++sqlConcept context c++", "++
>                  chain ", " [(if s==t
>                               then sqlConcept context (target a')++"."++sqlAttConcept context (target a')
>                               else "NULL"
>                              )++" AS "++sqlRelName context s
>                             |(a',_)<-as, s<-declarations a']++
>                  phpIndent 26++
>                  "FROM "++sqlConcept context (target a)++
>                  phpIndent 26++
>                  "WHERE NOT EXISTS (SELECT * FROM "++sqlRelName context t++
>                                   " WHERE "++sqlConcept context (target a)++"."++sqlAttConcept context (target a)
>                                            ++"="++sqlRelName context t++"."++sqlMorTrg context a++")"
>                | (a,_)<-as, t<-declarations a, Sur `elem` multiplicities a]++
>              "');"
>         else "return Array();"
>       | (c,as)<-entities]
>     , "  }"
>     , "  return Array();"
>     , "}"
>     , ""
>     , "function DB_getEnt($ent,$sort=''){"
>     , "  switch($ent){\n      "++
>       chain "\n      "
>       [ "case '"++phpConcept context c++"':\n        return quoteSplitArray(DB_doquer('SELECT "++
>         chain (","++phpIndent 49) 
>               ["group_concat( DISTINCT quote( "++sqlMorName context a++"."++sqlMorTrg context a++" ) ) AS "++sqlMorName context a
>               | (a,_)<-as ]++
>         ","++phpIndent 49++"group_concat( DISTINCT quote( "++sqlConcept context c++"."++sqlAttConcept context c++" ) ) AS "++sqlConcept context c++phpIndent 42++
>         chain (phpIndent 47++"LEFT JOIN ")
>               (["FROM "++sqlConcept context c]++
>                [ sqlRelName context a++" ON "++sqlRelName context a++"."++sqlMorSrc context a++"="++sqlConcept context c++"."++sqlAttConcept context c
>                  | (a,_)<-as ])++
>         phpIndent 42++"GROUP BY "++sqlConcept context c++"."++sqlAttConcept context c++" '.$sort));"
>       | (c,as)<-entities ]
>     , "  }"
>     , "  return DB_debug('Entity '.$ent.' unknown',4);"
>     , "}  "
>     , ""
>     , "function DB_getRelOmmissions($rel){"
>     , "  switch($rel){\n      "++
>       chain "\n      "
>       [ "case '"++phpRelName context r++"':\n        "++if Sur `elem` multiplicities r || Tot `elem` multiplicities r
>         then "return DB_doquer('"++
>              chain (" UNION "++phpIndent 26)
>              (["SELECT NULL AS "++sqlRelSrc r++", "++ifAs (sqlRelTrg r) (sqlAttConcept context (target r))++" FROM "++sqlConcept context (target r)++" WHERE NOT EXISTS (SELECT * FROM "++sqlRelName context r++" WHERE "++sqlRelName context r++"."++sqlRelTrg r++"="++sqlConcept context (target r)++"."++sqlAttConcept context (target r)++")"
>               | Sur `elem` multiplicities r]++
>               ["SELECT "++ifAs (sqlRelSrc r) (sqlAttConcept context (source r))++", NULL AS "++sqlRelTrg r++" FROM "++sqlConcept context (source r)++" WHERE NOT EXISTS (SELECT * FROM "++sqlRelName context r++" WHERE "++sqlConcept context (source r)++"."++sqlAttConcept context (source r)++"="++sqlRelName context r++"."++sqlRelSrc r++")"
>               | Tot `elem` multiplicities r]) ++ "');"
>         else "return Array();"
>       | r<-relations ]
>     , "  }"
>     , "  return DB_debug('Entity '.$ent.' unknown',4);"
>     , "}"
>     , ""
>     , "function DB_getRel($rel,$sort=''){"
>     , "  switch($rel){\n      "++
>       chain "\n      "
>       [ "case '"++phpRelName context r++"':\n        return DB_doquer("++
>         (if Tot `elem` multiplicities r && Sur `elem` multiplicities r
>             then "\"SELECT "++sqlConcept context (source r)++"."++ifAs (sqlRelSrc r) (sqlAttConcept context (source r))++", "++sqlRelTrg r++" FROM "++sqlConcept context (source r)++" LEFT JOIN "++sqlRelName context r++" ON "++sqlRelName context r++"."++sqlRelSrc r++"="++sqlConcept context (source r)++"."++sqlAttConcept context (source r)++" UNION "++phpIndent 26++
>                  "SELECT "++sqlRelSrc r++", "++sqlConcept context (target r)++"."++ifAs (sqlRelTrg r) (sqlAttConcept context (target r))++" FROM "++sqlConcept context (target r)++" LEFT JOIN "++sqlRelName context r++" ON "++sqlRelName context r++"."++sqlRelTrg r++"="++sqlConcept context (target r)++"."++sqlAttConcept context (target r)++" \".$sort"
>             else
>          if Tot `elem` multiplicities r
>             then "\"SELECT "++sqlConcept context (source r)++"."++ifAs (sqlRelSrc r) (sqlAttConcept context (source r))++", "++sqlRelTrg r++" FROM "++sqlConcept context (source r)++ " LEFT JOIN "++sqlRelName context r++" ON "++sqlRelName context r++"."++sqlRelSrc r++"="++sqlConcept context (source r)++"."++sqlAttConcept context (source r)++" \".$sort"
>             else
>          if Sur `elem` multiplicities r
>             then "\"SELECT "++sqlRelSrc r++", "++sqlConcept context (target r)++"."++ifAs (sqlRelTrg r) (sqlAttConcept context (target r))++" FROM "++sqlConcept context (target r)++" LEFT JOIN "++sqlRelName context r++" ON "++sqlRelName context r++"."++sqlRelTrg r++"="++sqlConcept context (target r)++"."++sqlAttConcept context (target r)++" \".$sort"
>             else
>          "\"SELECT "++sqlRelSrc r++", "++sqlRelTrg r++" FROM "++sqlRelName context r++" \".$sort")++
>         ");"
>       | r<-relations ]
>     , "  }"
>     , "  return DB_debug('Relation '.$rel.' unknown',4);"
>     , "}"
>     , ""
>     , "function DB_getRul($rul,$sort=''){"
>     , (let xs = [ "case '"++sqlRuleName context r++"':"++
>                 "\n          //  "++showADL r++", so compute"++
>                 "\n          //  "++showADL r'++
>--                 "\n        return DB_doquer('"++selectExpr context 10 (phpRelSrc context signal) (phpRelTrg context signal) signal ++" '.$sort);"
>--  should be equivalent to:
>                 "\n        return DB_doquer('"++selectExpr context 10 (phpRelSrc context r) (phpRelTrg context r) r' ++" '.$sort);"
>                 | r@(Sg p rule expla sgn nr pn signal)<-signals context
>                 , r'<-[(shrink . disjNF . Cp . normExpr) rule] ]
>        in (if null xs
>            then ""
>            else "  switch($rul){\n      "++chain "\n      " xs ++"\n  }\n")++
>           "  return DB_debug('Rule '.$rul.' unknown',4);")
>     , "}"
>     , ""
>     , "function DB_createRelation($rel,$attrs){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeRels phpCodeRelCreate (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "true"
>     , "}"
>     , ""
>     , "function DB_updateRelation($rel,$attrsOld,$attrs){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeRels phpCodeRelUpdate (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "true"
>     , "}"
>     , ""
>     , "function DB_deleteRelation($rel,$attrs){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeRels phpCodeRelDelete (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "true"
>     , "}"
>     , ""
>     , "function DB_createEntity($ent,$attrs){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeEnts phpCodeEntCreate (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "$attrs"
>     , "}"
>     , ""
>     , "function DB_updateEntity($ent,$attrs){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeEnts phpCodeEntUpdate (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "$attrs"
>     , "}"
>     , ""
>     , "function DB_deleteEntity($ent,$handle){"
>     , phpCodeTransactionStart context noTransactions
>     , phpCodeEnts phpCodeEntDelete (context,entities,relations,hcs)
>     , phpCodeTransactionClose context noTransactions "true"
>     , "}"
>     , ""

Obsolete?
     , chain "\n\n  "
       [ chain "\n  "
         [ "function DB_get"++phpConcept context c++"($atom){"
         , "  return Array( "++chain "\n                , "
           [ "\""++phpRelName context s++"\"=>DB_doquer(\"SELECT "++sqlRelName context s++"."++sqlRelSrc a++" FROM "++sqlRelName context s++" WHERE "++sqlRelName context s++"."++sqlRelTrg a++" = '\".addslashes($atom).\"'\")"
           | (a,_)<-as, s<-declarations a]
         , "              );"
         , "}"
         ]
       | (c,as)<-entities ]
     , ""

>     , "// checks whether multiplicities are OK...."
>     , "function DB_is_multi($rel,$source){"
>     , "    switch($rel){"
>     , chain "\n  "
>       [ "        case "++phpShow (name r)++": return "++
>         if inj ps && fun ps then "false; // only one "++name (source r)++" matches only one "++name (target r) else
>         if inj ps           then "($source=="++phpShow (name (source r))++"); // multiple "++plural English (name (target r))++" to one "++name (source r) else
>         if           fun ps then "($source=="++phpShow (name (target r))++"); // multiple "++plural English (name (source r))++" to one "++name (target r) else
>                                  "true; // multiples are allowed both ways" 
>       | r<-relations, ps<-[multiplicities r] ]
>     , "        default: DB_debug('Relation '.$rel.' unknown',3);"
>     , "    }"
>     , "}"
>     , "" ] ++ "\n?>"
>   where
>       (entities, relations, erruls) = erAnalysis context
>       hcs = [hc| rule<-rules context++multRules context, hc<-triggers rule ]
>       labelname (nm,"",k:ks) = if length labels <=1
>                                then name k
>                                else name k++"["++name (target k)++"]"
>                                where labels = [name a++"["++name (target a)++"]"|(c,as)<-entities, nm==c, (a,_)<-as, name a==name k]
>       labelname (nm,lbl,ks)  = lbl
>       comp :: [Declaration]      -- all computed relations
>       comp = rd [s| rule<-rules context, toExpr<-cpu rule, s<-declarations toExpr]


>  phpCodeRelCreate (context,entities,relations,hcs) r
>   = (chain "\n".filter (not.null))
>     [ "        if(isset($attrs['"++phpRelSrc context r++"']) && isset($attrs['"++phpRelTrg context r++"'])) {"
>     , insConcepts context hcs 8 (source r) (sqlRelSrc r) [r]
>     , insConcepts context hcs 8 (target r) (sqlRelTrg r) [r]
>     , "        DB_doquer('INSERT IGNORE INTO "++sqlRelName context r++" ("++sqlRelSrc r++","++sqlRelTrg r++")"++phpIndent 19++"VALUES (\\''.addslashes($attrs['"++sqlRelSrc r++"']).'\\', \\''.addslashes($attrs['"++sqlRelTrg r++"']).'\\')');"
>     , concat [ "\n     // "++informalRule hc++(if isSgnl (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>                phpCodeIncrHornClauseRel "$attrs" 8 context r hc
>              | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-computeOrder hcs "INSERT INTO" [r]
>              , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>              ]
>     , "        }" ]

>  phpCodeRelUpdate (context,entities,relations,hcs) r
>   = (chain "\n".filter (not.null)) (
>     [ "        if(isset($attrs['"++phpRelSrc context r++"']) && isset($attrs['"++phpRelTrg context r++"'])) {"
>     , "           $deleted=0;"
>     , "           if(isset($attrsOld['"++phpRelSrc context r++"']) && isset($attrsOld['"++phpRelTrg context r++"'])) {"
>     , "              if($attrsOld['"++phpRelSrc context r++"']!=$attrs['"++phpRelSrc context r++"'] ||"
>     , "                 $attrsOld['"++phpRelTrg context r++"']!=$attrs['"++phpRelTrg context r++"']) {"
>     , "                DB_doquer('DELETE FROM "++sqlRelName context r++" WHERE "++sqlRelSrc r++"=\\''.addslashes($attrsOld['"++phpRelSrc context r++"']).'\\' AND "++sqlRelTrg r++"=\\''.addslashes($attrsOld['"++phpRelTrg context r++"']).'\\'');"
>     , "                $deleted=DB_affected();"
>     , "              }"
>     , "           }"
>     , "           DB_doquer('INSERT IGNORE INTO "++sqlRelName context r++" ("++sqlRelSrc r++","++sqlRelTrg r++")"++phpIndent 19++"VALUES (\\''.addslashes($attrs['"++sqlRelSrc r++"']).'\\', \\''.addslashes($attrs['"++sqlRelTrg r++"']).'\\')');"
>     , "           $inserted=DB_affected();"
>     ] ++ (let closs=[e| e<-closE context, r `elem` declarations e] in
>           if null closs then [] else
>     [ "           if($inserted || $deleted){"
>     , chain "\n"
>       [ "         DB_doquer(\"DELETE FROM "++sqlClosName context e++" WHERE True\");"++
>         "\n         DB_doquer(\"INSERT IGNORE INTO "++sqlClosName context e++" "++selectNormFiExpr "$attrs" context 14 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"++
>         "\n         if(DB_affected()){"++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName context e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');}"
>       | e<-closs]
>     , "              }" ]) ++
>     [ "     // "++informalRule ([("UPDATE",r)], e, bOp, toExpr, frExpr, rule)++(if isSgnl (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>       phpCodeIncrHornClauseRel attrs 15 context r hc
>     | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-computeOrder hcs "UPDATE" [r]
>     , attrs<-[if fst (head fOps)=="DELETE FROM" then "$attrsOld" else "$attrs"]
>     , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>     ] ++ 
>     [ "           if($deleted){"
>     , dbDelConcept context 11 (source r) ("$attrsOld['"++phpRelSrc context r++"']")
>     , dbDelConcept context 11 (target r) ("$attrsOld['"++phpRelTrg context r++"']")
>     , "           }"
>     , "           if($inserted){"
>     , insConcepts context hcs 11 (source r) (sqlRelSrc r) [r]
>     , insConcepts context hcs 11 (target r) (sqlRelTrg r) [r]
>     , "           }"
>     , "        }" ] )

>  phpCodeRelDelete (context,entities,relations,hcs) r
>   = (chain "\n".filter (not.null)) (
>     [ "        if(isset($attrs['"++phpRelSrc context r++"']) && isset($attrs['"++phpRelTrg context r++"'])) {"
>     , "           DB_doquer('DELETE FROM "++sqlRelName context r++" WHERE "++sqlRelSrc r++"=\\''.addslashes($attrs['"++phpRelSrc context r++"']).'\\' AND "++sqlRelTrg r++"=\\''.addslashes($attrs['"++phpRelTrg context r++"']).'\\'');"
>     , "           if(DB_affected()){"
>     ] ++ (let closs=[e| e<-closE context, r `elem` declarations e] in
>           if null closs then [] else
>     [ chain "\n"
>       [ "            DB_doquer(\"DELETE FROM "++sqlClosName context e++" WHERE True\");"++
>         "\n            DB_doquer(\"INSERT IGNORE INTO "++sqlClosName context e++" "++selectNormFiExpr "$attrs" context 14 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"++
>         "\n            if(DB_affected()){"++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName context e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');}"
>       | e<-closs ]]) ++
>     [ "           // "++informalRule ([("DELETE FROM",r)], e, bOp, toExpr, frExpr, rule)++(if isSgnl (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>       phpCodeIncrHornClauseRel "$attrs" 14 context r hc
>     | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-computeOrder hcs "DELETE FROM" [r]
>     , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>     ] ++ 
>     [ dbDelConcept context 14 (source r) ("$attrs['"++phpRelSrc context r++"']")
>     , dbDelConcept context 14 (target r) ("$attrs['"++phpRelTrg context r++"']")
>     , "           }"
>     , "        }" ] )

>  phpCodeEntCreate (context,entities,relations,hcs) (c,as)
>   = (chain "\n".filter (not.null))
>     [ "        if(!isset($attrs['"++phpConcept context c++"'])) $attrs['"++phpConcept context c++"']=rand(); // random.."
>-- insert c into the concept relation
>     , insConcepts context hcs 8 c (phpConcept context c) [s| (a,_)<-as, s<-declarations a]
>-- insert attribute values
>     , chain "\n"
>       [ chain "\n"
>         [ "        if(isset($attrs['"++phpMorName context a++"'])){"
>         , "           DB_doquer('INSERT IGNORE INTO "++sqlMorName context a++"("++sqlMorSrc context a++","++sqlMorTrg context a++") VALUES (\\''.addslashes($attrs['"++phpConcept context c++"']).'\\',\\''.addslashes($attrs['"++phpMorName context a++"']).'\\')');"
>         , insConcepts context hcs 11 (target a) (phpMorName context a) [s| (a,_)<-as, s<-declarations a]
>         , "        }" ]
>       | (a,_)<-as, s<-declarations a]++
>-- insert derived values
>       phpCodeIncrHornClauseEnt 8 "$attrs" context (c,as) []
>        [ hc
>        | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-( computeOrder hcs "INSERT INTO" . map declaration . map fst) as
>        , suitable toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>        ]
>     ]
>  suitable (F ts) = True
>  suitable e      = oneMorphism e

Approach for updates:
$qa contains the current state of the entity.
It is compared to the desired state,

>  phpCodeEntUpdate (context,entities,relations,hcs) (c,as)
>   = (chain "\n".filter (not.null))
>     [ "        $qa=quoteSplitArray(DB_doquer('SELECT "++
>         chain (","++phpIndent (i+22)) 
>               (["group_concat( DISTINCT quote( "++sqlRelName context s++"."++trgAtt++" ) ) AS "++sname
>                | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]++
>                ["group_concat( DISTINCT quote( "++sqlConcept context c++"."++sqlAttConcept context c++" ) ) AS "++sqlConcept context c]
>               )++
>         phpIndent (i+15)++
>         chain (phpIndent (i+21)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context c]++
>                [ sqlRelName context s++" ON "++
>                  sqlRelName context s++"."++sqlRelSrc s++"="++sqlConcept context c++"."++sqlAttConcept context c
>                | (a,_)<-as, s<-declarations a {-, not (s `elem` declarations dms)-}]
>               )++
>         phpIndent (i+15)++"WHERE "++sqlConcept context c++"."++sqlAttConcept context c++"=\\''.addslashes($attrs['"++phpConcept context c++"']).'\\''));"
>     , phpCodeIncrHornClauseEnt 8 "$qa[0]" context (c,as) []
>        [ hc
>        | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-( computeOrder hcs "DELETE FROM" . map declaration . map fst) as
>        , suitable toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>        , source toExpr/=c && target toExpr/=c
>        ]
>     , chain "\n"
>       [ [' '| x<-[1..i]]++"$attr=isset($attrs['"++sqlRelName context s++"'])?Array($attrs['"++sqlRelName context s++"']):Array();\n"++
>         [' '| x<-[1..i]]++"if($attr!=$qa[0]['"++sqlRelName context s++"']){\n"++
>         [' '| x<-[1..i]]++
>         "  DB_doquer(\"DELETE FROM "++sqlRelName context s++
>                    " WHERE "++srcAtt++"='\".addslashes($attrs['"++phpConcept context c++"']).\"'"++
>                    (if src==trg then " AND "++trgAtt++"<>'\".addslashes($attrs['"++phpConcept context c++"']).\"'" else "")++"\");\n"++
>         chain "\n"
>          [ chain "\n"
>            [ [' '| x<-[1..i+2]]++"if(isset($attrs['"++phpMorName context a++"'])){"
>            , [' '| x<-[1..i+2]]++"   DB_doquer('INSERT IGNORE INTO "++sqlMorName context a++"("++sqlMorSrc context a++","++sqlMorTrg context a++") VALUES (\\''.addslashes($attrs['"++phpConcept context c++"']).'\\',\\''.addslashes($attrs['"++phpMorName context a++"']).'\\')');"
>            , insConcepts context hcs (i+5) (target a) (phpMorName context a) [s| (a,_)<-as, s<-declarations a]
>            , [' '| x<-[1..i+2]]++"}" ]
>          | (a,_)<-as, [s]==declarations a]++
>         " }"
>       | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings, not (s `elem` comp)]++
>-- insert derived values
>       phpCodeUpdHornClauseEnt 8 context (c,as) []
>        [ (fOps, e, bOp, toExpr, frExpr, rule)
>        | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-( computeOrder hcs "UPDATE" . map declaration . map fst) as
>        , suitable toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>        ]
>     ] where relStrings
>              = [if target s==source s
>                 then (s,"T"++sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)
>                 else if source s==c
>                      then (s,sqlRelName context s,source s,target s,sqlRelSrc s,sqlRelTrg s)
>                      else (s,sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)
>                | (a,_)<-as, s<-declarations a {-, not (s `elem` declarations dms)-}]
>             dms = []::[Morphism] -- delMors context c
>             i=8
>             comp :: [Declaration]      -- all computed relations
>             comp = rd [s| rule<-rules context, toExpr<-cpu rule, s<-declarations toExpr]

>  phpCodeEntDelete (context,entities,relations,hcs) (c,as)
>   = (chain "\n".filter (not.null))
>     [ if null dms then "" else
>       "        $core=DB_doquer('SELECT "++
>         chain (","++phpIndent (i+24)) 
>               ["group_concat( DISTINCT quote( "++sqlMorName context m++"."++sqlMorTrg context m++" ) ) AS "++sqlMorName context m
>               | m<-dms {- delMors context c -}]++
>         ","++phpIndent (i+24)++"group_concat( DISTINCT quote( "++sqlConcept context c++"."++sqlAttConcept context c++" ) ) AS "++sqlConcept context c++
>         phpIndent (i+17)++
>         chain (phpIndent (i+23)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context c]++
>                [ sqlMorName context m++" ON "++
>                  sqlMorName context m++"."++sqlMorSrc context m++"="++sqlConcept context c++"."++sqlAttConcept context c
>                | m<-dms {- delMors context c -}]
>               )++
>         phpIndent (i+17)++"WHERE "++sqlConcept context c++"."++sqlAttConcept context c++"=\\''.addslashes($handle).'\\'');"
>     , "        $qa=DB_doquer('SELECT "++
>         chain (","++phpIndent (i+22)) 
>               (["group_concat( DISTINCT quote( "++sqlRelName context s++"."++trgAtt++" ) ) AS "++sname
>                | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]++
>                ["group_concat( DISTINCT quote( "++sqlConcept context c++"."++sqlAttConcept context c++" ) ) AS "++sqlConcept context c]
>               )++
>         phpIndent (i+15)++
>         chain (phpIndent (i+21)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context c]++
>                [ sqlRelName context s++" ON "++
>                  chain " OR " ([sqlRelName context s++"."++sqlRelSrc s++"="++sqlConcept context c++"."++sqlAttConcept context c
>                                | source s==c ]++
>                                [sqlRelName context s++"."++sqlRelTrg s++"="++sqlConcept context c++"."++sqlAttConcept context c
>                                | target s==c ]
>                               )
>                | s<-declarations context, source s==c || target s==c, not (s `elem` declarations dms)]
>               )++
>         phpIndent (i+15)++"WHERE "++sqlConcept context c++"."++sqlAttConcept context c++"=\\''.addslashes($handle).'\\'');"
>     , phpCodeIncrHornClauseEnt 8 "$qa[0]" context (c,as) []
>        [ hc
>        | hc@(fOps, e, bOp, toExpr, frExpr, rule)<-( computeOrder hcs "DELETE FROM" . map declaration . map fst) as
>        , suitable toExpr, null [e| e<-mors frExpr, isSgnl (declaration e)]
>        , source toExpr/=c && target toExpr/=c
>        ]
>     , if null dms then "" else phpIndent i++
>       chain (phpIndent i)
>             [chain (phpIndent i) 
>              [ "$v=quoteSplit($core[0]['"++sqlMorName context m++"']);"
>              , "for($i=0;$i<count($v);$i++){"
>              , "  DB_deleteEntity('"++sqlConcept context (target m)++"',$v[$i]);"
>              , "}"
>              ]
>             | m<-dms {- delMors context c -}]
>     , "        DB_doquer(\"DELETE FROM "++sqlConcept context c++" WHERE "++sqlConcept context c++"."++sqlAttConcept context c++"='\".addslashes($handle).\"'\");"
>     , chain "\n"
>       [ dbDelEntAtt context i c "$handle" (s, sname, src, trg, srcAtt, trgAtt)
>       | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]
>     ] where relStrings
>              = [(s,sqlRelName context s,source s,target s,sqlRelSrc s,sqlRelTrg s)| s<-declarations context, source s==c, not (s `elem` declarations dms)]++
>                [(s,(if target s==source s then "T" else "")++sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)| s<-declarations context, target s==c, not (s `elem` declarations dms)]
>             dms = []::[Morphism] -- delMors context c
>             i=8

>  phpCodeRels f (context,entities,relations,hcs)
>   = if null rels then "  DB_debug('Relation '.$rel.' unknown',4);" else
>     chain "\n"
>     [ "  switch($rel){"
>     , "      "++chain "\n      "
>         [ "case '"++phpRelName context r++"':\n"++
>           f (context,entities,relations,hcs) r
>           ++"\n        break;"
>         | r<-rels]
>     , "    DB_debug('Relation '.$rel.' unknown',4);"
>     , "    }"
>     ]
>     where rels= [r| r<-relations, not (isSgnl r)]

>  phpCodeEnts f (context,entities,relations,hcs)
>   = if null entities then "  DB_debug('Entity '.$ent.' unknown',4);" else
>     chain "\n"
>     [ "  switch($ent){"
>     , "      "++chain "\n      "
>       [ "case '"++phpConcept context c++"':\n"++
>         f (context,entities,relations,hcs) (c,as)++
>         "\n        break;"
>       | (c,as)<-entities ]
>     , "    DB_debug('Entity '.$ent.' unknown',4);"
>     , "    }"
>     ]


>  phpCodeIncrHornClauseEnt :: Int -> String -> Context -> (Concept,[(Morphism,r)]) -> [Morphism] -> [ComputeRule] -> String
>  phpCodeIncrHornClauseEnt i var context (c,as) filled [] = ""
>  phpCodeIncrHornClauseEnt i var context (c,as) filled (hc@(fOps, e, "INSERT INTO", toExpr, frExpr, rule): rest)
>   = ( phpIndent (i-3)++"// "++informalRule {-(declarations (map fst as))-} hc++(if isSgnl (declaration m) then " (SIGNAL)" else "")++
>       phpIndent (i-3)++"// ("++showADL rule++".  "++explain rule++")"++
>     (if pNoDebug then "" else
>      phpIndent (i-3)++"// inside phpCodeIncrHornClauseEnt i {- var: -} "++show var++" context ("++name c++",as) {- filled: -} "++show filled++
>      phpIndent (i-3)++"//          (hc@("++chain ", " (map fst fOps)++", "++showADL e++", \"INSERT INTO\", "++showADL toExpr++", "++showADL frExpr++", "++showADL rule++"): rest):"
>     )++
>     ( if null issetsM then "" else phpIndent i++ "if("++chain " && " (map (phpIsset var context) issetsM)++")" )++
>       if oneMorphism toExpr
>       then phpIndent i++"/* case 1 */ DB_doquer("++
>            sqlCodeComputeRule "$attrs" (i+11) context [] hc++");"
>       else (if null sub || null issetsM then "" else "{ /* 2 */")++
>            phpIndent (i+3)++"$v=array(); /* case 1b */"++
>            phpIndent (i+3)++"$v=DB_doquer('"++selectExpr context (17+i) (sqlExprSrc frExpr) (sqlExprTrg frExpr) (doSubsExpr context var [(a,sqlConcept context c,sqlRelName context a) |a<-issetsM] frExpr)
>                                                           ++"');"++
>            phpIndent (i+3)++"for($i=0;$i<count($v);$i++) { /* 3 */"++
>            concat
>            [ phpIndent (i+6)++"$v[$i]["++show j++"]=rand();"++
>              concat
>              [ phpIndent (i+6)++"DB_doquer('INSERT IGNORE INTO "++sqlConcept context c++" ("++sqlAttConcept context c++") VALUES (\\''.addslashes($v[$i]["++show j++"]).'\\')');"
>              | c<-rd [target l,source r]
>              ]
>            | F tos<-[toExpr]
>            , (l,j,r)<-zip3 (init tos) [2..length tos] (tail tos)]++
>            concat
>            [ phpIndent (i+6)++"DB_doquer('INSERT IGNORE INTO "++sqlMorName context m++" ("++sqlMorSrc context m++","++sqlMorTrg context m++") VALUES (\\''.addslashes($v[$i]["++l++"]).'\\',\\''.addslashes($v[$i]["++r++"]).'\\')');"
>            | F tos<-[toExpr]
>            , vs<-[["'"++sqlExprSrc frExpr++"'"]++map show [2..length tos]++["'"++sqlExprTrg frExpr++"'" ]]
>            , (l,t,r)<-zip3 (init vs) tos (tail vs), m<-mors t ] ++phpIndent (i+3)++"} /* 3 */"++
>            (if null sub || null issetsM then "" else phpIndent i++"} /* 2 */")
>     )++phpCodeIncrHornClauseEnt i var context (c,as) (filled ++ mors toExpr) rest
>     where issetsM = sub >- flipper (mors toExpr)
>           attributes = map fst as
>           m = head (mors toExpr)
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           flipper ms = ms `uni` map flp ms
>           sub = (attributes `isc` flipper (mors frExpr))>-filled
>  phpCodeIncrHornClauseEnt i var context (c,as) filled (hc@(fOps, e, "DELETE FROM", toExpr, frExpr, rule):rest)
>   = "\n     // "++informalRule {-(declarations (map fst as))-} hc++(if null (morlist toExpr) then "null (morlist toExpr)" else if isSgnl (declaration m) then " (SIGNAL)" else "")++
>     phpIndent i++
>     (if oneMorphism toExpr
>      then phpIndent i++"/* case 2 */ DB_doquer("++
>           sqlCodeComputeRule "$attrs" (i+11) context []
>                              hc++");"
>       else "DB_doquer('not yet implemented: DELETE FROM "++showADL toExpr)++
>     phpCodeIncrHornClauseEnt i var context (c,as) filled rest
>     where issetsM = sub >- flipper (mors toExpr)
>           attributes = map fst as
>           m = head (morlist toExpr) -- ; m' = last (morlist toExpr)
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           flipper ms = ms `uni` map flp ms
>           sub = attributes `isc` flipper (mors frExpr)
>           nub = if fst (head fOps)=="INSERT INTO" then [] else attributes `isc` flipper (mors toExpr)

TODO:
In de cascade van regels worden incrementele inserts of delets nog niet incrementeel doorgegeven.
Dat gebeurt nu waarschijnlijk fout, of op z'n best erg inefficient.
Voorbeeld:
     // ON INSERT INTO sessie DO INSERT INTO van SELECTFROM sessie;actief
     // (sessie;actief -: van COMPUTING [van].  Een formulier is ingevuld door de actor in wiens sessie het formulier wordt gemaakt. U kunt alleen bij uw B-dossier nadat u bent ingelogd. Hierdoor weet de computer wie er achter het scherm zit. Alle gegevens en formulieren, die in deze sessie worden gemaakt, worden op uw naam geregistreerd.)
        if($attrs['T36_sessie']!=$qa[0]['T36_sessie']){
           if(isset($attrs['T36_sessie'])){
             DB_doquer('INSERT IGNORE INTO T8_van
                       SELECT \''.addslashes($attrs['C2_F_ormulier']).'\' AS AttF_ormulier, T34_actief.AttA_ctor
                         FROM T34_actief
                         WHERE \''.addslashes($attrs['T36_sessie']).'\'=T34_actief.AttS_essie');
           }
        }
     // ON INSERT INTO van DO INSERT INTO in SELECTFROM van;eigenaar~
     // (van = in;eigenaar COMPUTING [in,van].  Een formulier ingevuld door een actor zit in het dossier van diezelfde actor.)
             DB_doquer('INSERT IGNORE INTO T6_in
                       SELECT T8_van.AttF_ormulier, T7_eigenaar.AttD_ossier
                         FROM T8_van
                            , T7_eigenaar
                         WHERE T8_van.AttA_ctor=T7_eigenaar.AttA_ctor');
        }
In dit voorbeeld wordt T8_van netjes incrementeel geupdated, maar vervolgens gebeurt T6_in niet incrementeel! (voorbeeld uit Bdossier)

>  phpCodeUpdHornClauseEnt :: Int -> Context -> (Concept,[(Morphism,r)]) -> [Morphism] -> [ComputeRule] -> String
>  phpCodeUpdHornClauseEnt i context (c,as) filled [] = ""
>  phpCodeUpdHornClauseEnt i context (c,as) filled ((hc@(fOps, e, bOp, toExpr, frExpr, rule)): rest)
>   = ( phpIndent (i-3)++"// "++informalRule {-(declarations (map fst as))-} hc++(if isSgnl (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>       phpIndent (i-3)++"// ("++showADL rule++".  "++explain rule++")"++
>       (if pNoDebug then "" else
>        phpIndent (i-3)++"// inside phpCodeUpdHornClauseEnt i context ("++name c++",as) {- filled: -} "++show filled++
>        phpIndent (i-3)++"//          (hc@("++chain ", " (map fst fOps)++", "++showADL e++", "++show bOp++", "++showADL toExpr++", "++showADL frExpr++", "++showADL rule++"): rest):"
>       )++
>       ( if null issetsM then "" else
>         phpIndent i++ "if("++chain " && " [ (if bOp=="INSERT INTO" then ((phpIsset "$attrs" context m) ++ " && ") else "") ++
>                                         ("$attrs['"++sqlMorName context m++"']!=$qa[0]['"++sqlMorName context m++"']")
>                                       | m<-issetsM]++
>                     "){" )++
>       ( if oneMorphism toExpr
>         then phpIndent ind++"/* case 3 */ DB_doquer("++
>              sqlCodeComputeRule "$attrs" (ind+11) context
>                                 []
>                                 hc++");"
>         else phpIndent ind++"$v=array(); /* case 3b */"++
>              phpIndent ind++"$v=DB_doquer('"++selectExpr context (17+ind) (sqlExprSrc frExpr) (sqlExprTrg frExpr) (doSubsExpr context "$attrs" [(a,sqlConcept context c,sqlMorName context a) |a<-issetsM] frExpr)
>                                                           ++"');"++
>              phpIndent ind++"for($i=0;$i<count($v);$i++) {"++
>              concat
>              [ phpIndent (ind+3)++"$v[$i]["++show j++"]=rand();"++
>                concat
>                [ phpIndent (ind+3)++"DB_doquer('INSERT IGNORE INTO "++sqlConcept context c++" ("++sqlAttConcept context c++") VALUES (\\''.addslashes($v[$i]["++show j++"]).'\\')');"
>                | c<-rd [target l,source r]
>                ]
>              | F tos<-[toExpr]
>              , (l,j,r)<-zip3 (init tos) [2..length tos] (tail tos)]++
>              concat
>              [ phpIndent (ind+3)++"DB_doquer('INSERT IGNORE INTO "++sqlMorName context m++" ("++sqlMorSrc context m++","++sqlMorTrg context m++") VALUES (\\''.addslashes($v[$i]["++l++"]).'\\',\\''.addslashes($v[$i]["++r++"]).'\\')');"
>              | F tos<-[toExpr]
>              , vs<-[["'"++sqlExprSrc frExpr++"'"]++map show [2..length tos]++["'"++sqlExprTrg frExpr++"'" ]]
>              , (l,t,r)<-zip3 (init vs) tos (tail vs), m<-mors t ]
>       )++
>       ( if      null issetsM  &&      oneMorphism toExpr  then "" else
>         if not (null issetsM) && not (oneMorphism toExpr) then phpIndent i++"}  }" else
>         phpIndent i++"}")
>     )++phpCodeUpdHornClauseEnt i context (c,as) (filled ++ mors toExpr) rest
>     where issetsM = sub >- ms toExpr
>           issetsS = attributes `isc` ms toExpr
>           attributes = map fst as
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           ms expr = mors expr `uni` map flp (mors expr)
>           sub = (attributes `isc` ms frExpr)>-filled
>           nub = attributes `isc` ms toExpr
>           ind = if null issetsM then i else i+3

>  phpCodeIncrHornClauseRel :: String -> Int -> Context -> Declaration -> ComputeRule -> String
>  phpCodeIncrHornClauseRel attrs i context r hc@(fOps, e, "INSERT INTO", toExpr, frExpr, rule) 
>   = if isTrue frExpr
>     then phpIndent i++"DB_doquer('INSERT IGNORE INTO T1_r"++
>          phpIndent (i+11)++      "SELECT "++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++", \\''.addslashes("++attrs++"['"++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"']).'\\' FROM C1_A WHERE NOT EXISTS (SELECT * FROM T2_s WHERE T2_s."++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"=C1_A."++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++" AND T2_s."++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"=\\''.addslashes("++attrs++"['"++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"']).'\\')');"++
>          phpIndent i++"DB_doquer('INSERT IGNORE INTO T1_r"++
>          phpIndent (i+11)++      "SELECT \\''.addslashes("++attrs++"['"++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"']).'\\', "++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++" FROM C2_B WHERE NOT EXISTS (SELECT * FROM T2_s WHERE T2_s."++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"=\\''.addslashes("++attrs++"['"++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"']).'\\' AND T2_s."++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"=C2_B."++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++")');"
>     else (if pNoDebug then "" else
>           phpIndent (i-3)++"// inside phpCodeIncrHornClauseRel i context:"++
>       --    phpIndent (i-3)++"// r="++show (name r)++"; substitutions="++show substitutions++";"++
>           phpIndent (i-3)++"// fOps="++show fOps++"; e="++showADL e++"; bOp=INSERT INTO; toExpr="++showADL toExpr++"; frExpr="++showADL frExpr++
>           phpIndent (i-3)++"// m = "++showADL m++", m' = "++showADL m'
>       --  ++phpIndent (i-3)++"// replacement: "++(showADL (doSubsExpr context "$" substitutions frExpr))
>          )++phpIndent i++"/* case 4 */"++phpIndent i
>           ++"DB_doquer("++sqlCodeComputeRule attrs (i+11) context [] hc++");"
>     where m = head (mors toExpr)
>           m' = head (mors frExpr)

>  phpCodeIncrHornClauseRel attrs i context r hc@(fOps, e, "DELETE FROM", toExpr, frExpr, rule)
>   = (if pNoDebug then "" else
>      phpIndent (i-3)++"// inside phpCodeIncrHornClauseRel:"++
>   -- phpIndent (i-3)++"// r="++show (name r)++"; substitutions="++show [(m,phpMorSrc context m,phpMorTrg context m)| m<-mors e, declaration m==r]++";"++
>      phpIndent (i-3)++"// fOps="++show fOps++"; e="++showADL e++"; bOp=INSERT INTO; toExpr="++showADL toExpr++"; frExpr="++showADL frExpr++
>      phpIndent (i-3)++"// m = "++showADL m++", m' = "++showADL m')++
>     if oneMorphism frExpr
>     then phpIndent i++"DB_doquer('DELETE FROM "++sqlMorName context m++
>          phpIndent (i+11)++      "WHERE "++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"=\\''.addslashes("++attrs++"['"++(if inline m' then sqlMorSrc context m else sqlMorTrg context m)++"']).'\\' AND "++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"=\\''.addslashes("++attrs++"['"++(if inline m' then sqlMorTrg context m else sqlMorSrc context m)++"']).'\\'');"
>     else (if pNoDebug then "" else
>           phpIndent (i-3)++"// calling selectNormFiExpr \""++attrs++"\" context "++show (10+i)++" frExpr "++
>           phpIndent i++       "//            "++show (sqlMorSrc context m,sqlMorTrg context m)++
>     --      phpIndent i++       "// substitutions: "++show substitutions++
>           phpIndent i++       "//   frExpr:  "++take 2(showHS frExpr)++":  ("++showADL frExpr++")"
>     --    ++phpIndent (i-3)++"// replacement: "++(showADL (doSubsExpr context "$" substitutions frExpr))
>          )++
>           phpIndent i++"/* case 5 */"++phpIndent i
>      ++"DB_doquer("++sqlCodeComputeRule attrs (i+11) context [] hc++");"
>     where m = head (mors toExpr)
>           m' = head (mors frExpr)

>  dbDelEntAtt context i c entVar (s, sname, src, trg, srcAtt, trgAtt)
>   = [' '| x<-[1..i]]++
>     "DB_doquer(\"DELETE FROM "++sqlRelName context s++
>                " WHERE "++srcAtt++"='\".addslashes("++entVar++").\"'"++
>                (if src==trg then " AND "++trgAtt++"<>'\".addslashes("++entVar++").\"'" else "")++"\");"
>     ++"\n"++[' '| x<-[1..i]]++
>     concat
>      [chain ("\n"++[' '| x<-[1..i+4]])
>        [ "if(DB_affected()){"
>        , "     DB_doquer(\"DELETE FROM "++sqlClosName context e++" WHERE True\");"
>        , "     DB_doquer(\"INSERT IGNORE INTO "++sqlClosName context e++" "++selectNormFiExpr "$attrs" context 15 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"
>        , "     "++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName context e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');"
>        , "}\n"++[' '| x<-[1..i+4]]]
>      | e<-closE context, s `elem` declarations e]
>     ++"if(count($qa[0]['"++sname++"'])) DB_doquer(\"DELETE FROM "++sqlConcept context trg++
>                         " WHERE "++sqlConcept context trg++"."++sqlAttConcept context trg++
>                         " IN (\".$qa[0]['"++sname++"'].\") AND NOT EXISTS ("++
>                            chain " UNION "
>                            (["SELECT "++sqlRelSrc s'++" FROM "++sqlRelName context s'++
>                             " WHERE "++sqlRelName context s'++"."++sqlRelSrc s'++"="++sqlConcept context trg++"."++sqlAttConcept context trg
>                             | s'<-declarations context, source s'==trg]++
>                             ["SELECT "++sqlRelTrg s'++" FROM "++sqlRelName context s'++
>                             " WHERE "++sqlRelName context s'++"."++sqlRelTrg s'++"="++sqlConcept context trg++"."++sqlAttConcept context trg
>                             | s'<-declarations context, target s'==trg])++
>                         ")\");"
