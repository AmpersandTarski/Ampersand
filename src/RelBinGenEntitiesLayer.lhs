> module RelBinGenEntitiesLayer where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, disjNF, computeOrder, ComputeRule(CR), triggers)
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
>     , if null (attributes context) then "$DB_titles   = Array();" else
>       "$DB_titles = Array\n  "++
>       "    ( "++chain "\n      , " [ "\" <attributes> \"\n        => Array ( "++chain "\n                 , "
>                                      ["\"label\"=>"++phpShow "Unique ID"++
>                                        ", \"titles\"=>Array('"++phpShow (name o)++"', '"++chain "', '" [name a|a<-attributes o]++"')"]
>                                      ++")"
>                                    | o<-attributes context]
>                                    ++"\n      );"
>     , if null (attributes context) then "$DB_entities = Array();" else "\n  "++
>       "$DB_entities= Array\n     ( "++chain "\n     , "
>       [ "Array\n        ( 'Name'=>"++phpShow (name o)++
>                            "\n        , 'ID'=>'"++phpConcept context (concept o)++
>                           "'\n        , 'Attr'=>Array('"++phpConcept context (concept o)++"'=>'"++{-addslashes-} (phpShow (name o))++"','"++
>                           chain "','" [ name a++"'=>'"++{-addslashes-}phpShow (name a) | a<-attributes o]++"')"++
>                           "\n        , 'Disabled'=>Array('"++phpConcept context (concept o)++"'=>'1','"++
>                           chain "','" [ name a++"'=>'"++(if null (declarations a `isc` comp) then "0"  else "1") | a<-attributes o]++"')\n        )"
>       | o<-attributes context ]++"\n     );"
>     , ""
>     , "$DB_concepts = Array( "++chain "\n                      , "
>       ([ "'"++phpConcept context c++"'=>"++phpShow (name c)| c<-concs context ]++
>        [ "'"++p++"'=>"++phpShow (name c)
>        | (p,c)<-rd ([(p,c)| r<-relations, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]] ++
>                     [(p,c)| r<-rules context, (p,c)<-[(phpRelSrc context r,source r), (phpRelTrg context r,target r)]])
>        ])
>     , "                    );"
>     , ""
>     , if null (filter (not.isSignal) relations) then "$DB_nonEntRels=Array();" else
>       "$DB_nonEntRels=Array( "++chain "\n                      , "
>       [ "Array('ID'=>'"++phpRelName context r++"','Name'=>"++phpShow (name r)++",'Src'=>'"++phpRelSrc context r++"','Trg'=>'"++phpRelTrg context r++"')"
>       | r<-relations, not (isSignal r) ]++"\n                      );"
>     , ""
>     , if null (signals context) then "$DB_rules=Array();" else
>       "$DB_rules=Array( "++chain "\n                 , "
>       [ "Array('ID'=>'"++sqlRuleName context r++
>               "','Name'=>'"++addslashes (name signal)++
>               "','Src'=>'"++phpRelSrc context r++
>               "','Trg'=>'"++phpRelTrg context r++
>               "','Fix'=>Array ('"++chain "','" ([ phpConcept context (concept o) | o<-attributes context, not (null (mors r `isc` mors o))]++
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
>       [ "case '"++phpShow (name o)++"':\n       return "++
>         chain "&&" [ "isOkRelation('"++name a++"')" | a<-attributes o]++";"
>       | o<-attributes context ]
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
>       [ "    case '"++phpConcept context (concept o)++"':\n        "++
>         if or [Sur `elem` multiplicities (ctx a)| a<-attributes o]
>         then "return DB_doquer('"++
>              chain (phpIndent 26++"UNION"++phpIndent 26)
>                [ "SELECT NULL AS "++sqlConcept context (concept o)++", "++
>                  chain ", " [(if s==t
>                               then sqlConcept context (target (ctx a'))++"."++sqlAttConcept context (target (ctx a'))
>                               else "NULL"
>                              )++" AS "++sqlRelName context s
>                             | a'<-attributes o, s<-declarations a']++
>                  phpIndent 26++
>                  "FROM "++sqlConcept context (target (ctx a))++
>                  phpIndent 26++
>                  "WHERE NOT EXISTS (SELECT * FROM "++sqlRelName context t++
>                                   " WHERE <sqlConcept context (target (ctx a))> . <sqlAttConcept context (target (ctx a))> "
>                                            ++"= "++sqlRelName context t++".<sqlMorTrg context (ctx a)> )"
>                | a<-attributes o, t<-declarations a, Sur `elem` multiplicities (ctx a)]++
>              "');"
>         else "return Array();"
>       | o<-attributes context]
>     , "  }"
>     , "  return Array();"
>     , "}"
>     , ""
>     , "function DB_getEnt($ent,$sort=''){"
>     , "  switch($ent){\n      "++
>       chain "\n      "
>       [ "case '"++phpConcept context (concept o)++"':\n        return quoteSplitArray(DB_doquer('SELECT "++
>         chain (","++phpIndent 49) 
>               ["group_concat( DISTINCT quote( <sqlMorName context a> . <sqlMorTrg context a> ) ) AS <sqlMorName context a>"
>               | a<-attributes o ]++
>         ","++phpIndent 49++"group_concat( DISTINCT quote( "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++" ) ) AS "++sqlConcept context (concept o)++phpIndent 42++
>         chain (phpIndent 47++"LEFT JOIN ")
>               (["FROM "++sqlConcept context (concept o)]++
>                [ "<sqlRelName context a> ON <sqlRelName context a>.<sqlMorSrc context a> ="++sqlConcept context (target (ctx a))++"."++sqlAttConcept context (target (ctx a))
>                  | a<-attributes o ])++
>         phpIndent 42++"GROUP BY "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++" '.$sort));"
>       | o<-attributes context ]
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
>                 (if isFalse r' then "\n        return(Array());" else 
>                  "\n        return DB_doquer('"++selectExpr context 10 (phpRelSrc context r) (phpRelTrg context r) r' ++" '.$sort);")
>                 | r@(Sg p rule expla sgn nr pn signal)<-signals context
>                 , r'<-[(disjNF . Cp . normExpr) rule] ]
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
           | a<-attributes o, s<-declarations a]
         , "              );"
         , "}"
         ]
       | o<-attributes context ]
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
>       hcs = [hc| rule<-rules context, hc<-triggers rule ]
>       labelname (nm,"",k:ks) = if length labels <=1
>                                then name k
>                                else name k++"["++name (target k)++"]"
>                                where labels = [name a++"["++name (target (ctx a))++"]"|o<-attributes context, name o==name (concept o), a<-attributes o, name a==name k]
>       labelname (nm,lbl,ks)  = lbl
>       comp :: [Declaration]      -- all computed relations
>       comp = rd [s| rule<-rules context, toExpr<-cpu rule, s<-declarations toExpr]


>  phpCodeRelCreate (context,entities,relations,hcs) r
>   = (chain "\n".filter (not.null)) (
>     [ "        if(isset($attrs['"++phpRelSrc context r++"']) && isset($attrs['"++phpRelTrg context r++"'])) {"
>     , insConcepts context hcs 8 (source r) (sqlRelSrc r) [r]
>     , insConcepts context hcs 8 (target r) (sqlRelTrg r) [r]
>     , "        DB_doquer('INSERT IGNORE INTO "++sqlRelName context r++" ("++sqlRelSrc r++","++sqlRelTrg r++")"++phpIndent 19++"VALUES (\\''.addslashes($attrs['"++sqlRelSrc r++"']).'\\', \\''.addslashes($attrs['"++sqlRelTrg r++"']).'\\')');"
>     ] ++ (if null triggers then ["        }"] else
>            [ "        $someHornclausesActive=True;"
>            , "        while($someHornclausesActive){"
>            , "             $someHornclausesActive=False;"
>            ] ++       triggers ++
>            [ "        }}" ] )
>     )
>     where triggers
>            = [ "          // "++informalRule hc++(if isSignal (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>                phpCodeIncrHornClauseRel "$attrs" 13 context r hc++
>                "\n             $someHornclausesActive=$someHornclausesActive || DB_affected();"
>              | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-computeOrder hcs "INSERT INTO" [r]
>              , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>              ]

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
>     [ "     // "++informalRule (CR([("UPDATE",r)], e, bOp, toExpr, frExpr, rule))++(if isSignal (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>       phpCodeIncrHornClauseRel attrs 15 context r hc
>     | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-computeOrder hcs "UPDATE" [r]
>     , attrs<-[if fst (head fOps)=="DELETE FROM" then "$attrsOld" else "$attrs"]
>     , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
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
>           [ chain "\n"
>             [ "            DB_doquer(\"DELETE FROM "++sqlClosName context e++" WHERE True\");"++
>               "\n            DB_doquer(\"INSERT IGNORE INTO "++sqlClosName context e++" "++selectNormFiExpr "$attrs" context 14 e (sqlExprSrc e,sqlExprTrg e) [] e++"\");"++
>               "\n            if(DB_affected()){"++(if clos0 e then "closure0" else "closure1")++"('"++sqlClosName context e++"', '"++sqlExprSrc e++"', '"++sqlExprTrg e++"');}"
>             | e<-closs ]]) ++
>     (if null triggers then [] else
>       [ "              $someHornclausesActive=True;"
>       , "              while($someHornclausesActive){"
>       , "                 $someHornclausesActive=False;"
>       ] ++ triggers ++ 
>       [ "              };" ])++
>     [ dbDelConcept context 14 (source r) ("$attrs['"++phpRelSrc context r++"']")
>     , dbDelConcept context 14 (target r) ("$attrs['"++phpRelTrg context r++"']")
>     , "           }"
>     , "        }" ] )
>     where triggers
>            = [ "              // "++informalRule (CR([("DELETE FROM",r)], e, bOp, toExpr, frExpr, rule))++(if isSignal (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>                phpCodeIncrHornClauseRel "$attrs" 17 context r hc++
>                "\n              $someHornclausesActive=$someHornclausesActive || DB_affected();"
>              | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-computeOrder hcs "DELETE FROM" [r]
>              , suitable toExpr, m<-mors toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>              ]

>  phpCodeEntCreate (context,entities,relations,hcs) o
>   = (chain "\n".filter (not.null))
>     [ "        if(!isset($attrs['"++phpCname++"'])) $attrs['"++phpCname++"']=rand(); // random.."
>-- insert c into the concept relation
>     , insConcepts context hcs 8 (concept o) (phpCname) [s| a<-attributes o, s<-declarations a]
>-- insert attribute values
>     , chain "\n"
>       [ chain "\n"
>         [ "        if(isset($attrs['"++name a++"'])){"
>         , "           DB_doquer('INSERT IGNORE INTO <sqlMorName context a> ( <sqlMorSrc context a> , <sqlMorTrg context a> ) VALUES (\\''.addslashes($attrs['"++phpCname++"']).'\\',\\''.addslashes($attrs['"++name a++"']).'\\')');"
>         , insConcepts context hcs 11 (target (ctx a)) (name a) [s| a<-attributes o, s<-declarations a]
>         , "        }" ]
>       | a<-attributes o, s<-declarations a]++
>-- insert derived values
>       phpCodeIncrHornClauseEnt 8 "$attrs" context o [] hcs'
>     ]
>    where
>     phpCname = phpConcept context (concept o)
>     hcs' = [ hc
>            | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-computeOrder hcs "INSERT INTO" (Isn (concept o) (concept o):declarations o)
>            , suitable toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>            ]
>  suitable (F ts) = True
>  suitable e      = oneMorphism e

Approach for updates:
$qa contains the current state of the entity.
It is compared to the desired state,

>  phpCodeEntUpdate (context,entities,relations,hcs) o
>   = (chain "\n".filter (not.null))
>     [ "        $qa=quoteSplitArray(DB_doquer('SELECT "++
>         chain (","++phpIndent (i+22)) 
>               (["group_concat( DISTINCT quote( "++sqlRelName context s++"."++trgAtt++" ) ) AS "++sname
>                | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]++
>                ["group_concat( DISTINCT quote( "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++" ) ) AS "++sqlConcept context (concept o)]
>               )++
>         phpIndent (i+15)++
>         chain (phpIndent (i+21)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context (concept o)]++
>                [ sqlRelName context s++" ON "++
>                  sqlRelName context s++"."++sqlRelSrc s++"="++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)
>                | a<-attributes o, s<-declarations a {-, not (s `elem` declarations dms)-}]
>               )++
>         phpIndent (i+15)++"WHERE "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++"=\\''.addslashes($attrs['"++phpCname++"']).'\\''));"
>     , phpCodeIncrHornClauseEnt 8 "$qa[0]" context o []
>        [ hc
>        | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-( computeOrder hcs "DELETE FROM" . declarations) o
>        , suitable toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>        , source toExpr/=concept o && target toExpr/=concept o
>        ]
>     , chain "\n"
>       [ [' '| x<-[1..i]]++"$attr=isset($attrs['"++sqlRelName context s++"'])?Array($attrs['"++sqlRelName context s++"']):Array();\n"++
>         [' '| x<-[1..i]]++"if($attr!=$qa[0]['"++sqlRelName context s++"']){\n"++
>         [' '| x<-[1..i]]++
>         "  DB_doquer(\"DELETE FROM "++sqlRelName context s++
>                    " WHERE "++srcAtt++"='\".addslashes($attrs['"++phpCname++"']).\"'"++
>                    (if src==trg then " AND "++trgAtt++"<>'\".addslashes($attrs['"++phpCname++"']).\"'" else "")++"\");\n"++
>         chain "\n"
>          [ chain "\n"
>            [ [' '| x<-[1..i+2]]++"if(isset($attrs['"++name a++"'])){"
>            , [' '| x<-[1..i+2]]++"   DB_doquer('INSERT IGNORE INTO <sqlMorName context a> ( <sqlMorSrc context a>, <sqlMorTrg context a> ) VALUES (\\''.addslashes($attrs['"++phpCname++"']).'\\',\\''.addslashes($attrs['"++name a++"']).'\\')');"
>            , insConcepts context hcs (i+5) (target (ctx a)) (name a) [s| a<-attributes o, s<-declarations a]
>            , [' '| x<-[1..i+2]]++"}" ]
>          | a<-attributes o, [s]==declarations a]++
>         " }"
>       | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings, not (s `elem` comp)]++
>-- insert derived values
>       phpCodeUpdHornClauseEnt 8 context o []
>        [ (CR(fOps, e, bOp, toExpr, frExpr, rule))
>        | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-( computeOrder hcs "UPDATE" . declarations) o
>        , suitable toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>        ]
>     ] where relStrings
>              = [if target s==source s
>                 then (s,"T"++sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)
>                 else if source s==concept o
>                      then (s,sqlRelName context s,source s,target s,sqlRelSrc s,sqlRelTrg s)
>                      else (s,sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)
>                | a<-attributes o, s<-declarations a {-, not (s `elem` declarations dms)-}]
>             phpCname = phpConcept context (concept o)
>             dms = []::[Morphism] -- delMors context (concept o)
>             i=8
>             comp :: [Declaration]      -- all computed relations
>             comp = rd [s| rule<-rules context, toExpr<-cpu rule, s<-declarations toExpr]

>  phpCodeEntDelete (context,entities,relations,hcs) o
>   = (chain "\n".filter (not.null))
>     [ if null dms then "" else
>       "        $core=DB_doquer('SELECT "++
>         chain (","++phpIndent (i+24)) 
>               ["group_concat( DISTINCT quote( "++sqlMorName context m++"."++sqlMorTrg context m++" ) ) AS "++sqlMorName context m
>               | m<-dms {- delMors context (concept o) -}]++
>         ","++phpIndent (i+24)++"group_concat( DISTINCT quote( "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++" ) ) AS "++sqlConcept context (concept o)++
>         phpIndent (i+17)++
>         chain (phpIndent (i+23)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context (concept o)]++
>                [ sqlMorName context m++" ON "++
>                  sqlMorName context m++"."++sqlMorSrc context m++"="++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)
>                | m<-dms {- delMors context (concept o) -}]
>               )++
>         phpIndent (i+17)++"WHERE "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++"=\\''.addslashes($handle).'\\'');"
>     , "        $qa=DB_doquer('SELECT "++
>         chain (","++phpIndent (i+22)) 
>               (["group_concat( DISTINCT quote( "++sqlRelName context s++"."++trgAtt++" ) ) AS "++sname
>                | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]++
>                ["group_concat( DISTINCT quote( "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++" ) ) AS "++sqlConcept context (concept o)]
>               )++
>         phpIndent (i+15)++
>         chain (phpIndent (i+21)++"LEFT JOIN ")
>               (["FROM "++sqlConcept context (concept o)]++
>                [ sqlRelName context s++" ON "++
>                  chain " OR " ([sqlRelName context s++"."++sqlRelSrc s++"="++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)
>                                | source s == concept o ]++
>                                [sqlRelName context s++"."++sqlRelTrg s++"="++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)
>                                | target s==concept o ]
>                               )
>                | s<-declarations context, source s==concept o || target s==concept o, not (s `elem` declarations dms)]
>               )++
>         phpIndent (i+15)++"WHERE "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++"=\\''.addslashes($handle).'\\'');"
>     , phpCodeIncrHornClauseEnt 8 "$qa[0]" context o []
>        [ hc
>        | hc@(CR(fOps, e, bOp, toExpr, frExpr, rule))<-( computeOrder hcs "DELETE FROM" . declarations) o
>        , suitable toExpr, null [e| e<-mors frExpr, isSignal (declaration e)]
>        , source toExpr/=concept o && target toExpr/=concept o
>        ]
>     , if null dms then "" else phpIndent i++
>       chain (phpIndent i)
>             [chain (phpIndent i) 
>              [ "$v=quoteSplit($core[0]['"++sqlMorName context m++"']);"
>              , "for($i=0;$i<count($v);$i++){"
>              , "  DB_deleteEntity('"++sqlConcept context (target m)++"',$v[$i]);"
>              , "}"
>              ]
>             | m<-dms {- delMors context (concept o) -}]
>     , "        DB_doquer(\"DELETE FROM "++sqlConcept context (concept o)++" WHERE "++sqlConcept context (concept o)++"."++sqlAttConcept context (concept o)++"='\".addslashes($handle).\"'\");"
>     , chain "\n"
>       [ dbDelEntAtt context i (concept o) "$handle" (s, sname, src, trg, srcAtt, trgAtt)
>       | (s,sname,src,trg,srcAtt,trgAtt)<-relStrings]
>     ] where relStrings
>              = [(s,sqlRelName context s,source s,target s,sqlRelSrc s,sqlRelTrg s)| s<-declarations context, source s==concept o, not (s `elem` declarations dms)]++
>                [(s,(if target s==source s then "T" else "")++sqlRelName context s,target s,source s,sqlRelTrg s,sqlRelSrc s)| s<-declarations context, target s==concept o, not (s `elem` declarations dms)]
>             dms = []::[Morphism] -- delMors context (concept o)
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
>     where rels= [r| r<-relations, not (isSignal r)]

>  phpCodeEnts f (context,entities,relations,hcs)
>   = if null (attributes context) then "  DB_debug('Entity '.$ent.' unknown',4);" else
>     chain "\n"
>     [ "  switch($ent){"
>     , "      "++chain "\n      "
>       [ "case '"++phpConcept context (concept o)++"':\n"++
>         f (context,entities,relations,hcs) o++
>         "\n        break;"
>       | o<-attributes context ]
>     , "    DB_debug('Entity '.$ent.' unknown',4);"
>     , "    }"
>     ]


>  phpCodeIncrHornClauseEnt :: Int -> String -> Context -> ObjectDef -> [Morphism] -> [ComputeRule] -> String
>  phpCodeIncrHornClauseEnt i var context o filled [] = ""
>  phpCodeIncrHornClauseEnt i var context o filled (hc@(CR(fOps, e, "INSERT INTO", toExpr, frExpr, rule)): rest)
>   = ( phpIndent (i-3)++"// "++informalRule hc++(if isSignal (declaration m) then " (SIGNAL)" else "")++
>       phpIndent (i-3)++"// ("++showADL rule++".  "++explain rule++")"++
>     (if pNoDebug then "" else
>      phpIndent (i-3)++"// inside phpCodeIncrHornClauseEnt i {- var: -} "++show var++" context "++name (concept o)++" {- filled: -} "++show filled++
>      phpIndent (i-3)++"//          (hc@("++chain ", " (map fst fOps)++", "++showADL e++", \"INSERT INTO\", "++showADL toExpr++", "++showADL frExpr++", "++showADL rule++"): rest):"
>     )++
>     ( if null issetsM then "" else phpIndent i++ "if("++chain " && " (map (phpIsset var context) issetsM)++")" )++
>       if oneMorphism toExpr
>       then phpIndent i++"/* case 1 */"++phpIndent i++"DB_doquer("++
>            sqlCodeComputeRule "$attrs" (i+11) context [] hc++");"
>       else (if null sub || null issetsM then "" else "{ /* 2 */")++
>            phpIndent (i+3)++"$v=array(); /* case 1b */"++
>            phpIndent (i+3)++"$v=DB_doquer('"++selectExpr context (17+i) (sqlExprSrc frExpr) (sqlExprTrg frExpr) (doSubsExpr context var [(a,sqlConcept context (concept o),sqlRelName context a) |a<-issetsM] frExpr)
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
>     )++phpCodeIncrHornClauseEnt i var context o (filled ++ mors toExpr) rest
>     where issetsM = sub >- flipper (mors toExpr)
>           m = head (mors toExpr)
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           flipper ms = ms `uni` map flp ms
>           sub = (mors o `isc` flipper (mors frExpr))>-filled
>  phpCodeIncrHornClauseEnt i var context o filled (hc@(CR(fOps, e, "DELETE FROM", toExpr, frExpr, rule)):rest)
>   = "\n     // "++informalRule hc++(if null (morlist toExpr) then "null (morlist toExpr)" else if isSignal (declaration m) then " (SIGNAL)" else "")++
>     phpIndent i++
>     (if oneMorphism toExpr
>      then phpIndent i++"/* case 2 */ DB_doquer("++
>           sqlCodeComputeRule "$attrs" (i+11) context [] hc++");"
>       else "DB_doquer('not yet implemented: DELETE FROM "++showADL toExpr)++
>     phpCodeIncrHornClauseEnt i var context o filled rest
>     where issetsM = sub >- flipper (mors toExpr)
>           m = head (morlist toExpr) -- ; m' = last (morlist toExpr)
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           flipper ms = ms `uni` map flp ms
>           sub = mors o `isc` flipper (mors frExpr)
>           nub = if fst (head fOps)=="INSERT INTO" then [] else mors o `isc` flipper (mors toExpr)

>  phpCodeUpdHornClauseEnt :: Int -> Context -> ObjectDef -> [Morphism] -> [ComputeRule] -> String
>  phpCodeUpdHornClauseEnt i context o filled [] = ""
>  phpCodeUpdHornClauseEnt i context o filled (hc@(CR(fOps, e, bOp, toExpr, frExpr, rule)): rest)
>   = ( phpIndent (i-3)++"// "++informalRule hc++(if isSignal (declaration (head (mors toExpr))) then " (SIGNAL)" else "")++
>       phpIndent (i-3)++"// ("++showADL rule++".  "++explain rule++")"++
>       (if pNoDebug then "" else
>        phpIndent (i-3)++"// inside phpCodeUpdHornClauseEnt i context "++name (concept o)++" {- filled: -} "++show filled++
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
>                                    []
>                                    hc++");"
>         else phpIndent ind++"$v=array(); /* case 3b */"++
>              phpIndent ind++"$v=DB_doquer('"++selectExpr context (17+ind) (sqlExprSrc frExpr) (sqlExprTrg frExpr) (doSubsExpr context "$attrs" [(a,sqlConcept context (concept o),sqlMorName context a) |a<-issetsM] frExpr)
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
>     )++phpCodeUpdHornClauseEnt i context o (filled ++ mors toExpr) rest
>     where issetsM = sub >- ms toExpr
>           issetsS = mors o `isc` ms toExpr
>       -- bereken welke invulvelden er zijn. Die kunnen immers mogelijk leeggelaten worden door de gebruiker
>           ms expr = mors expr `uni` map flp (mors expr)
>           sub = (mors o `isc` ms frExpr)>-filled
>           nub = mors o `isc` ms toExpr
>           ind = if null issetsM then i else i+3

>  phpCodeIncrHornClauseRel :: String -> Int -> Context -> Declaration -> ComputeRule -> String
>  phpCodeIncrHornClauseRel attrs i context r hc@(CR(fOps, e, "INSERT INTO", toExpr, frExpr, rule)) 
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

>  phpCodeIncrHornClauseRel attrs i context r hc@(CR(fOps, e, "DELETE FROM", toExpr, frExpr, rule))
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
>           phpIndent i++       "//   frExpr:  "++take 2(showHS "" frExpr)++":  ("++showADL frExpr++")"
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
