 module Prototype.Installer where
  import Adl
  import Auxiliaries
  import Strings    (chain)
  import Data.Plug
  import Data.Fspec
  import Collection (rd,(>-))
  import NormalForms(conjNF)
  
  installer :: Fspc -> String -> String
  installer fSpec dbName = "<?php\n  " ++ chain "\n  "
     (
        [ "// Try to connect to the database\n"
        , "if(isset($DB_host)&&!isset($_REQUEST['DB_host'])){"
        , "  $included = true; // this means user/pass are probably correct"
        , "  $DB_link = @mysql_connect(@$DB_host,@$DB_user,@$DB_pass);"
        , "}else{"
        , "  $included = false; // get user/pass elsewhere"
        , "  if(file_exists(\"dbsettings.php\")) include \"dbsettings.php\";"
        , "  else { // no settings found.. try some default settings"
        , "    if(!( $DB_link=@mysql_connect($DB_host='localhost',$DB_user='root',$DB_pass='')"
        , "       or $DB_link=@mysql_connect($DB_host='localhost',$DB_user='ADL',$DB_pass='ADL')))"
        , "    { // we still have no working settings.. ask the user!"
        , "      die(\"Install failed: cannot connect to MySQL\"); // todo" --todo
        , "    }"
        , "  } "
        , "}"
        , "if($DB_slct = @mysql_select_db('"++dbName++"')){"
        , "  $existing=true;"
        , "}else{"
        , "  $existing = false; // db does not exist, so try to create it"
        , "  @mysql_query(\"CREATE DATABASE `"++dbName++"` DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"
        , "  $DB_slct = @mysql_select_db('"++dbName++"');"
        , "}"
        , "if(!$DB_slct){"
        , "  echo die(\"Install failed: cannot not connect to MySQL or error selecting database\");" --todo: full error report
        , "}else{"
        ] ++ map ((++) "  ")
        (
          [ "if(!$included && !file_exists(\"dbsettings.php\"){ // we have a link now; try to write the dbsettings.php file"
          , "   if($fh = @fopen(\"dbsettings.php\", 'w')){"
          , "     fwrite($fh, '<'.'?php $DB_host=\"'.$DB_host.'\"; $DB_user=\"'.$DB_user.'\"; $DB_pass=\"'.$DB_pass.'\"; ?'.'>');"
          , "     fclose($fh);"
          , "   }"
          , "}\n"
          , "/*** Create new SQL tables ***/"
          , "//// Number of plugs: "++(show (length (plugs fSpec)))
          , "if($existing==true){"
          ] ++ map ((++) "  ") (concat (map checkPlugexists (plugs fSpec)))
          ++ ["}"]
          ++ concat (map plugCode (plugs fSpec))
        ) ++
        [ "}" ]
     ) ++ "\n?>\n"
    where plugCode plug
           = [ "mysql_query(\"CREATE TABLE `"++(plname plug)++"`"]
             ++ map ((++) "                  ")
                    ( [ comma: " " ++ fldname f ++ " " ++ showSQL (fldtype f) ++ "" ++ nul ++ " # "++show (fldexpr f)
                      | (f,comma)<-zip (fields plug) ('(':repeat ','), let nul = if fldnull f then "" else " NOT NULL"
                      ] ++
                      [", UNIQUE KEY ("++fldname key++")"
                      | key <- fields plug, flduniq key, not (fldnull key)]
                    )
             ++ ["                  ) TYPE=InnoDB DEFAULT CHARACTER SET latin1 COLLATE latin1_bin\");"]
          checkPlugexists plug
           = [ "if($columns = mysql_query(\"SHOW COLUMNS FROM `"++(plname plug)++"`\")){"
             , "  mysql_query(\"DROP TABLE `"++(plname plug)++"`\");" --todo: incremental behaviour
             , "}" ]
  
  plugs :: Fspc -> [Plug]
  plugs spc = neededPlugs given spc ++ given
    where given = []
  
  neededPlugs :: [Plug] -> Fspc -> [Plug] -- given [Plug], what plugs are still needed to implement Fspc?
  neededPlugs given spec
   = theplugs
     where
      otherRels      = looseRels >- mors (given ++ complexPlugs)
      looseRels      = map makeMph (vrels spec) >- mors given
      looseConcs     = concs (vrels spec) -- todo: we can make this less, since V[conc] isn't allways asked for..
                       >- concs (given ++ relPlugs ++ complexPlugs)
      -- complexPlugs are the plugs that consist of multiple relations
      uniSurRels :: Morphisms
      uniSurRels     = [r | r <- rd (looseRels++map flp looseRels) , null ([Uni,Sur,Inj] >- multiplicities r)]
      complexPlugs   = [] {-
      complexPlugs   = error$show [ show rclos
                       | rclos <- [ foldl1 (\x y -> if length x <= length y then y else x) cl -- longest list in cl
                                  | cl <- eqClass (\x y -> length x < length (x >- y)) -- there is overlap between x and y
                                                  (clos source target uniSurRels)
                                  ]
                       ] -}
      mkdataset rclos= error "mkdataset function not done yet"
                       
      relPlugs       = map morplug otherRels
      theplugs       = (uniqueNames . joinPlugs) (complexPlugs ++ relPlugs ++ map concplug looseConcs)
      
  
  -- todo, this algorithm is supposidly O(n^3), but this has never been measured
  -- the orders O(..) are mere estimations
  -- if the this algorithm is slower than n^3, it should be made faster
  joinPlugs :: [Plug] -> [Plug]
  joinPlugs ps = doJoin ps (joins inCols outCols) -- O(n^3)
    where
    outCols  = sort [(c,(p,f))|p<-ps,f<-fields p,flduniq f,Sur `elem` (multiplicities (fldexpr f)),let c=target(fldexpr f)]
    inCols   = sort [(c,(p,f))|p<-ps,f<-fields p,not (fldnull f),flduniq f,let c=source (fldexpr f),c==target (fldexpr f)]
    joins [] _ = []
    joins _ [] = []
    joins (i@(ci,pi):ins) (o@(co,po):outs) = -- joins is O(n)
      if ci<co then joins ins (o:outs) else if ci>co then joins (i:ins) outs
      else if fst pi == fst po then joins ins (o:outs) else (pi,po):joins ins (o:outs)
  
  doJoin :: [Plug] -> [((Plug, SqlField), (Plug, SqlField))] -> [Plug]
  doJoin ps [] = ps -- O(n^3)
  doJoin ps (j:js) = pair : doJoin rest js'
    where
    myIns  p = [fst i|(i,o)<-(j:js),fst o==p] -- O(n)
    ins      = myIns (fst addTo) -- O(n)
    addTo    = snd j --O(1)
    pair     = (fst addTo){fields = (fields (fst addTo)) ++ [joinFields f|o<-ins,f<-tail (fields o)] --O(n^2)
                          ,plname = (name . source) expr }
    expr     = fldexpr (snd addTo) --O(1)
    joinFields fld = fld{fldexpr = conjNF (F [expr,fldexpr fld]) -- O(1) ?
                        ,fldnull = fldnull (snd addTo) || fldnull fld}
    js'      = [(i',o')|(i,o)<-js,o /= addTo
                       ,let i'=if (fst i) `elem` ins then (pair,joinFields (snd i)) else i
                       ,let o'=if (fst o) `elem` ins then (pair,joinFields (snd o)) else o] -- O(n^2)
    rest     = [p|p<-ps, p `notElem` (fst addTo:ins)] -- O(n^2)
    --newFields = addFields (fields p) ps
  
  {-
  addFields fs (p:ps) = (first newFields,unused ++ second newFields ns)
    where
    newFields    = addFields fs' ns
    concexprs    = [e|e<-(map fldexpr (fs)),Sur `elem` (multiplicities e)]
    matchCols    = [c|
  -}
  
  uniqueNames :: [Plug]->[Plug]
  uniqueNames plgs = uN [] plgs -- todo: de velden in elke plug moeten ook unieke namen hebben!
   where uN :: [String] -> [Plug] -> [Plug]
         uN _ [] = []
         uN lst (p:ps) | (name p) `notElem` lst = p:(uN (name p:lst) ps)
         uN lst (p:ps) | (n1 p)   `notElem` lst = p{plname=n1 p}:(uN (n1 p:lst) ps)
         uN lst (p:ps) | (n2 p)   `notElem` lst = p{plname=n2 p}:(uN (n2 p:lst) ps)
         uN lst ps     | otherwise              = (uNn lst ps 1)
         uNn :: [String] -> [Plug] -> Integer -> [Plug]
         uNn lst (p:ps) n | name p ++ show n `notElem` lst = p{plname=name p ++ show n}:(uN (name p:lst) ps)
         uNn lst ps n | otherwise = uNn lst ps (n+1)
         n1 p = name p ++ plsource p
         n2 p = name p ++ pltarget p
         plsource p = name (source (fldexpr (head (fields (p)))))
         pltarget p = name (target (fldexpr (last (fields (p)))))
-- uniqueNames p:ps | ((name p++(name source p)) `elem` (names ps)) = p:(uniqueNames ps)
  
  concplug :: Concept -> Plug
  concplug c = plugsql (name c) [field (name c) (Tm (mIs c)) Nothing False True]
  
  morplug :: Morphism -> Plug
  morplug  m
   = {- If the morphism is UNI, INJ and SUR
      we can identify the target by its source alone
      we shoud, however do this afterwards, transforming ALL plugs
     if ( isUni && isInj && isSur && fldtyp (target m) == SQLId ) || (isIdent m)
     then plugsql (name (source m)) [field (name (source m)) (Tm (mIs (source m))) Nothing False True
                                    ,field (name (target m)) (Tm m) (Just SQLBool) (not isTot) True]
     else -}
     if isInj && not isUni then morplug (flp m)
     else if isUni || isTot
     then plugsql (name m) [field (name (source m)) (Tm (mIs (source m))) Nothing False isUni
                           ,field (name (target m)) (Tm m) Nothing (not isTot) isInj]
     else if isInj || isSur then morplug (flp m)
     else plugsql (name m) [field (name (source m)) (Fi {es=[Tm (mIs (source m)),F {es=[Tm m,flp (Tm m)]}]}
                                                    )      Nothing False False
                           ,field (name (target m)) (Tm m) Nothing False False]
     where
       mults = multiplicities m
       isTot = Tot `elem` mults
       isUni = Uni `elem` mults
       isSur = Sur `elem` mults
       isInj = Inj `elem` mults
  
  plugsql :: String -> [SqlField] -> Plug
  plugsql nm fld = PlugSql {plname=nm,database=CurrentDb,fields=fld}
  field nm expr Nothing   nul uniq = Fld {fldname = nm, fldexpr=expr, fldtype=fldtyp (target expr),fldnull=nul,flduniq=uniq}
  field nm expr (Just tp) nul uniq = Fld {fldname = nm, fldexpr=expr, fldtype=tp,fldnull=nul,flduniq=uniq}

  
  fldtyp :: (Identified a) => a -> SqlType
  fldtyp nm = case name nm of { "BLOB"   -> SQLBlob;
                                "PASS"   -> SQLPass;
                                "STRING" -> SQLVarchar 255;
                                _        -> SQLId
                              }

