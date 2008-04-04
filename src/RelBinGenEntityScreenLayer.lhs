> module RelBinGenEntityScreenLayer where
>  import Char
>  import Auxiliaries
>  import Calc
>  import CC_aux
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics


currently obsolete code: entityScreen layer (FILENAME.entscr.php) is not used for the moment

>  entityScreenLayer context filename noTransactions
>   = chain "\n  "
>     [ "<? // generated with "++adlVersion
>     , "require_once \""++filename++".ent.php\";"
>     , ""
>     , "// DB_keys is used for the entity headers with keys"
>     , "$DB_keys = Array"
>     , "    ( "++chain "\n      , " [ "\""++phpConcept context c'++"\"\n        => Array ( "++chain "\n                 , "
>                                      (if c' `elem` [k| (k,lbl,ms)<-keys context]
>                                       then [ "Array(\"label\"=>"++phpShow (labelname (nm,lbl,ks))++", \"Attr\"=>Array('"++
>                                              chain "', '" [phpMorName context k| k<-ks]++"'))"
>                                            |(nm,lbl,ks)<-[(k,lbl,ks)| (k,lbl,ks)<-keys context, k==c']]
>                                       else ["Array(\"label\"=>"++phpShow "Unique ID"++
>                                             ", 'Attr'=>Array('"++phpConcept context c'++"'))"])++")"
>                                    | (c',as)<-entities++[(c,[])| c<-concs context, not (c `elem` map fst entities)]
>                                    ]
>                                    ++"\n      );"
>     , ""
>     , "$DB_requiredForNew = Array"
>     , "    ( "++chain "\n      , "
>                 [ "\""++phpConcept context e++"\" => Array"++
>                   "\n           ( "++
>                   "'"++phpConcept context e++"' => array('Disp'=>'I', 'Type'=>'"++phpConcept context e++"'), "++
>                  (if null as
>                    then "'"++phpConcept context e++"' => array('Disp'=>'I', 'Type'=>'"++phpConcept context e++"')"
>                    else chain "\n           , "
>                          [ "'"++phpMorName context a++"' => array('Disp'=>'"++name (target a)++"', 'Type'=>'"++phpConcept context (target a)++"')"
>                          | (a,_)<-as])++
>                   "\n           ) "
>                 | (e,as)<-entities++[(c,[])|c<-concs context, not (c `elem` map fst entities)]]
>     , "    );"
>     , ""
>     , "$DB_entities= Array"++
>          "\n      ( "++chain "\n      , "
>        [ chain "\n"
>          [ "'"++phpConcept context c++"' =>"
>          , "        Array('Name'=>"++phpShow (name c)++",'ID'=>'"++phpConcept context c++"'"
>          , "             ,'Fields'=>Array"
>          , "               ( '"++phpConcept context c++"'=>array('Behavior'=>'"++(if displayInternalCode context c then "-" else "Disp")++"','Disp'=>'"++name c++"',>     , "            if($i!=0)$s.=', ';"
>     , "            $s.='(\\'';"
>     , "            if(is_array($v1)) $s.=addslashes($v1[$i]); else $s.=addslashes($v1);"
>     , "            $s.='\\',\\'';"
>     , "            if(is_array($v2)) $s.=addslashes($v2[$i]); else $s.=addslashes($v2);"
>     , "            $s.='\\')';"
>     , "          }"
>     , "          return $s;"
>     , "      }"
>     , "    }"
>     , "}"
>     , ""
>     , "function DB_createNewFromRequired($conc,$new){"
>     , phpCodeTransactionStart context noTransactions
>     , "    switch($conc){"
>     , chain "\n  "
>       [ "    case '"++sqlConcept context e++"':$id=rand();\n"++
>         ( if null as
>           then "        DB_doquer('INSERT IGNORE INTO "++sqlConcept context e++" (\\'"++sqlAttConcept context e++"\\') VALUES '.mergeForMysql($id,$new['"++sqlAttConcept context e++"']));"
>           else chain "\n"
>                 [ "        if(isset($new['"++phpMorName context a++"'])) DB_doquer('INSERT IGNORE INTO "++sqlMorName context a++" (\\'"++sqlMorSrc context a++"\\', \\'"++sqlMorTrg context a++"\\') VALUES '.mergeForMysql($id,$new['"++phpMorName context a++"']));"
>                 | (a,_)<-as] )++
>         "\n        break;"
>       | (e,as)<-entities++[(c,[])|c<-concs context, not (c `elem` map fst entities)]]
>     , "      default: DB_debug('Concept '.$conc.' unknown',4);"
>     , "    }"
>     , phpCodeTransactionClose context noTransactions>     , "            if($i!=0)$s.=', ';"
>     , "            $s.='(\\'';"
>     , "            if(is_array($v1)) $s.=addslashes($v1[$i]); else $s.=addslashes($v1);"
>     , "            $s.='\\',\\'';"
>     , "            if(is_array($v2)) $s.=addslashes($v2[$i]); else $s.=addslashes($v2);"
>     , "            $s.='\\')';"
>     , "          }"
>     , "          return $s;"
>     , "      }"
>     , "    }"
>     , "}"
>     , ""
>     , "function DB_createNewFromRequired($conc,$new){"
>     , phpCodeTransactionStart context noTransactions
>     , "    switch($conc){"
>     , chain "\n  "
>       [ "    case '"++sqlConcept context e++"':$id=rand();\n"++
>         ( if null as
>           then "        DB_doquer('INSERT IGNORE INTO "++sqlConcept context e++" (\\'"++sqlAttConcept context e++"\\') VALUES '.mergeForMysql($id,$new['"++sqlAttConcept context e++"']));"
>           else chain "\n"
>                 [ "        if(isset($new['"++phpMorName context a++"'])) DB_doquer('INSERT IGNORE INTO "++sqlMorName context a++" (\\'"++sqlMorSrc context a++"\\', \\'"++sqlMorTrg context a++"\\') VALUES '.mergeForMysql($id,$new['"++phpMorName context a++"']));"
>                 | (a,_)<-as] )++
>         "\n        break;"
>       | (e,as)<-entities++[(c,[])|c<-concs context, not (c `elem` map fst entities)]]
>     , "      default: DB_debug('Concept '.$conc.' unknown',4);"
>     , "    }"
>     , phpCodeTransactionClose context noTransactions "$id" "$new"
>     , "}"
>     , ""
>     , "function DB_setAllValues($ent,$id,$new,$old){"
>     , phpCodeTransactionStart context noTransactions
>     , "    switch($ent){"
>     , chain "\n"
>       [ "      case '"++phpConcept context c++"':\n"++
>         chain "\n"
>           ([  "          DB_doquer('DELETE FROM "++phpRelName context s++" WHERE "++phpRelSrc context s++" = \\''.addslashes($id).'\\'');"++
>             "\n          if(isset($new['"++phpRelName context s++"'])) DB_doquer('INSERT IGNORE INTO "++phpRelName context s++" (\\'"++phpRelSrc context s++"\\', \\'"++phpRelTrg context s++"\\') VALUES '.mergeForMysql($id,$new['"++phpRelName context s++"']));"
>            | s<-declarations context, source s==c]++
>            [  "          DB_doquer('DELETE FROM "++phpRelName context s++" WHERE "++phpRelTrg context s++" = \\''.addslashes($id).'\\'');"++
>             "\n          if(isset($new['"++phpRelName context s++"'])) DB_doquer('INSERT IGNORE INTO "++phpRelName context s++" (\\'"++phpRelTrg context s++"\\', \\'"++phpRelSrc context s++"\\') VALUES '.mergeForMysql($id,$new['"++phpRelName context s++"']));"
>            | s<-declarations context, target s==c])++
>         "\n      break;"
>       | (c,as)<-entities ]
>     , "        default:"
>     , "        DB_debug('Entity '.$ent.' unknown',4);"
>     , "    }"
>     , phpCodeTransactionClose context noTransactions "true" "$new"
>     , "}"
>     , ""
>     , "function DB_getAllValues($ent,$id){"
>     , "  switch($ent){"
>     , chain "\n"
>       [ "     case '"++phpConcept context c++"':"++
>         "\n       return array"++
>         "\n          ( '"++phpConcept context c++"'=>$id"++
>                       concat ( ["\n          , '"++phpRelName context s++"'=>MapFst(DB_doquer(\"SELECT "++phpRelTrg context s++" FROM "++phpRelName context s++" WHERE "++phpRelSrc context s++"='\".addslashes($id).\"'\"))"
>                                | s<-declarations context, source s==c]++
>                                ["\n          , '"++phpRelName context s++"'=>MapFst(DB_doquer(\"SELECT "++phpRelSrc context s++" FROM "++phpRelName context s++" WHERE "++phpRelTrg context s++"='\".addslashes($id).\"'\"))"
>                                | s<-declarations context, target s==c]
>                              )++
>         "\n          );"
>       | (c,as)<-entities ]
>     , "    }"
>     , "    return DB_debug('Entity '.$ent.' unknown',4);"
>     , "  }"
>     , ""

>     , "function DB_getEntKeyList($ent,$key){"
>     , "  switch($ent){\n      "
>     , chain "\n      "
>         [ "case \""++phpConcept context c++"\":$x=0;\n      switch($key){\n          "
>                 ++chain "\n          "
>                 ( if c `elem` [k| (k,lbl,ms)<-keys context]
>                   then [ "case $x++:return DB_doquer('SELECT "++
>                          (chain ", ".rd) ([sqlMorName context k ++"."++ifAs (phpMorName context k) (sqlMorTrg context k)
>                                           | k<-ks]++[sqlConcept context c ++"."++ifAs (phpConcept context c) (sqlAttConcept context c)])++
>                          " FROM "++ sqlConcept context c ++
>                          chain " " [" LEFT JOIN " ++ sqlMorName context k ++ " ON "++ sqlConcept context c ++ "."++ sqlAttConcept context c ++"=" ++ sqlMorName context k++"."++sqlMorSrc context k
>                                    | k<-ks, not (isIdent k)
>                                    ] ++
>                          "');"
>                        | (nm,lbl,ks)<-keys context, nm==c
>                        ]
>                   else ["case $x++:return DB_doquer('SELECT "++
>                         ""++ifAs (phpConcept context c) (sqlAttConcept context c)++" FROM "++ sqlConcept context c ++"');"
>                        ]
>                 )++
>                 "\n      }"++
>                 "\n      return DB_debug(\"Key not in scope\",4);"
>         | c<-concs context]
>     , "  }"
>     , "  return DB_debug('Concept '.$ent.' unknown',4);"
>     , "}"
>     , "?>"
>     ]
>    where
>     (entities, relations, ruls) = erAnalysis context
>     labelname (nm,"",k:ks) = if length labels <=1
>                              then name k
>                              else name k++"["++name (target k)++"]"
>                              where labels = [name a++"["++name (target a)++"]"|(c,as)<-entities, nm==c, (a,_)<-as, name a==name k]
>     labelname (nm,lbl,ks)  = lbl
