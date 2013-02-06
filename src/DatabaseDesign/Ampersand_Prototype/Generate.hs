{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where


import DatabaseDesign.Ampersand_Prototype.CoreImporter 
import DatabaseDesign.Ampersand.Fspec(showPrf,cfProof,lookupCpt,getSpecializations,getGeneralizations)
import Prelude hiding (writeFile,readFile,getContents,catch,exp)
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Directory               
import DatabaseDesign.Ampersand_Prototype.Version 
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
import Control.Exception

fatal :: Int -> String -> a
fatal = fatalMsg "Generate"

customCssPath :: String
customCssPath = "css/Custom.css"

generateAll :: Fspc -> Options -> IO ()
generateAll fSpec flags =
 do { writePrototypeFile "Generics.php" . genPhp "Generate.hs" "Generics.php" . intercalate [""] $ 
        [ generateConstants flags
        , generateSpecializations fSpec
        , generateTableInfos fSpec
        , generateRules fSpec flags
        , generateRoles fSpec
        , generateKeys fSpec flags
        , generateInterfaces fSpec flags ]
    
    ; case customCssFile flags of
        Just customCssFilePath ->
         do { customCssContents <- readCustomCssFile customCssFilePath
            ; writePrototypeFile customCssPath customCssContents
            }
        Nothing -> -- If no css file is specified, we use <filename>.css, if it exists.
         do { let dedicatedCSSPath = replaceExtension (fileName flags) "css"
            ; dedicatedCSSExists <- doesFileExist dedicatedCSSPath
            ; if dedicatedCSSExists then
               do { putStrLn $ "  Found " ++ dedicatedCSSPath ++ ", which will be used as Custom.css."
                  ; customCssContents <- readCustomCssFile dedicatedCSSPath
                  ; writePrototypeFile customCssPath customCssContents
                  }
              else -- If not, we check whether there is a css/Custom.css in the prototype directory and create a default one if there isn't.
               do { customExists <- doesFileExist (combine (dirPrototype flags) customCssPath)
                  ; if customExists
                    then verboseLn flags $ "  File " ++ customCssPath ++ " already exists."
                    else do { verboseLn flags $ "  File " ++ customCssPath ++ " does not exist, creating default for Oblomilan style."
                            ; writePrototypeFile customCssPath "@import url(\"Oblomilan.css\");"
                            }
                  }
            }
            
    ; when (development flags) $ 
       do { verboseLn flags "Generated tables\n"
          ; verboseLn flags ( unlines ( concatMap showPlug [ plug | InternalPlug plug <- plugInfos fSpec]))
          }
    }
  where
    readCustomCssFile f =
      catch (readFile f)
            (\e -> do let err = show (e :: IOException)
                      _ <- error ("ERROR: Cannot open custom css file ' " ++ f ++ "': " ++ err)
                      return "")
    writePrototypeFile fname content =
     do { verboseLn flags ("  Generating "++fname)
        --; verboseLn flags $ content
        ; writeFile (combine (dirPrototype flags) fname) content
        }

generateConstants :: Options -> [String]
generateConstants flags = 
  [ "$versionInfo = "++showPhpStr prototypeVersionStr++";" -- so we can show the version in the php-generated html
  , ""
  , "$dbName = "++showPhpStr (dbName flags)++";"
  , ""
  , "$isDev = "++showPhpBool (development flags)++";"
  , ""
  , "$autoRefreshInterval = "++showPhpStr (show $ fromMaybe 0 $ autoRefresh flags)++";"
  ]
  
generateSpecializations :: Fspc -> [String]
generateSpecializations fSpec =
  [ "$allSpecializations ="
  , "  array" ] ++
  addToLastLine ";" 
    (indent 4 (blockParenthesize "(" ")" ","
         [ [ showPhpStr (name cpt)++" => array ("++ intercalate ", " (map (showPhpStr . name) specializations) ++")" ] 
         | cpt <- concs fSpec, let specializations = getSpecializations fSpec cpt,  not ( null specializations) ])
    )        

generateTableInfos :: Fspc -> [String]
generateTableInfos fSpec =
  [ "$relationTableInfo ="
  , "  array" ] ++
  addToLastLine ";" 
    (indent 4 (blockParenthesize "(" ")" ","
         [ [showPhpStr rnm++" => array ('srcConcept' => "++showPhpStr (name (source rel))++", 'tgtConcept' => "++showPhpStr (name (target rel))++
                                          ", 'table' => "++showPhpStr table++", 'srcCol' => "++showPhpStr srcCol++", 'tgtCol' => "++showPhpStr tgtCol++")"] 
         | rel@(Rel {relnm = rnm}) <- mors fSpec
         , let (table,srcCol,tgtCol) = fromMaybe (fatal 61 $ "No table info for relation " ++ show rel)
                                         (getRelationTableInfo fSpec rel)
         ])) ++
  [ ""
  , "$conceptTableInfo ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4 
       (blockParenthesize "(" ")" ","
         [ ( (showPhpStr.name) c++" => array "
           ) :
           indent 4 
              (blockParenthesize "(" ")" ","
                [ [ "array ( 'table' => "++(showPhpStr.name) table
                  , "      , 'cols' => array ("++ intercalate ", " (map (showPhpStr . fldname) conceptFields) ++")"
                  , "      )"
                  ]
                -- get the concept tables (pairs of table and column names) for the concept and its generalizations and group them per table name
                | (table,conceptFields) <- groupOnTable . concatMap (lookupCpt fSpec) $ c : getGeneralizations fSpec c 
                ]
              )
         | c <- concs fSpec
         ]
    )  ) ++        
  [ ""
  , "$tableColumnInfo ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4 
       (blockParenthesize "(" ")" ","
         [ [ (showPhpStr.name) plug++" =>"
           , "  array" 
           ] ++
           indent 4 
              (blockParenthesize "(" ")" ","
                [ [ (showPhpStr.fldname) field++ " => array ( 'concept' => "++(showPhpStr.name.target.fldexpr) field++
                                                           ", 'unique' => " ++(showPhpBool.flduniq)            field++
                                                           ", 'null' => "  ++ (showPhpBool.fldnull)            field++
                                                           ")"
                  ] 
                | field <- getPlugFields plug]
              ) 
         | InternalPlug plug <- plugInfos fSpec
         ]
     )  )
 where groupOnTable :: [(PlugSQL,SqlField)] -> [(PlugSQL,[SqlField])]       
       groupOnTable tablesFields = [(t,fs) | (t:_, fs) <- map unzip . groupBy ((==) `on` fst) $ sortBy (\(x,_) (y,_) -> name x `compare` name y) tablesFields ]
 
generateRules :: Fspc -> Options -> [String]
generateRules fSpec flags =
  [ "$allRulesSql ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4
      (blockParenthesize  "(" ")" ","
         [ [ (showPhpStr.rrnm) rule ++ " =>"
           , "  array ( 'name' => "      ++(showPhpStr.rrnm)              rule
           , "        , 'ruleAdl' => "   ++(showPhpStr.show.rrexp)        rule
           , "        , 'origin' => "    ++(showPhpStr.show.rrfps)        rule
           , "        , 'meaning' => "   ++(showPhpStr.showMeaning)       rule
           , "        , 'message' => "   ++(showPhpStr.showMessage)       rule
           , "        , 'srcConcept' => "++(showPhpStr.name.source.rrexp) rule
           , "        , 'tgtConcept' => "++(showPhpStr.name.target.rrexp) rule
           ] ++
           ( if violExpr /= violationsExpr && verboseP flags
             then   ["        // Normalization steps:"]
                  ++["        // "++escapePhpStr ls | ls<-(showPrf showADL . cfProof showADL) violExpr]
                  ++["        // "]
             else   []
           ) ++
           [ "        // Normalized complement (== violationsSQL): "++ (escapePhpStr.show) violationsExpr
           , "        , 'violationsSQL' => '"++ fromMaybe (fatal 100 ( "No sql generated for "++showHS flags "" violationsExpr))
                                                          (selectExpr fSpec 26 "src" "tgt" violationsExpr)
                                             ++"'" 
           ] ++
           [ "        , 'contentsSQL' => '" ++
             let contentsExpr = conjNF rExpr in
             fromMaybe
               ("/*ERROR: no sql generated for " ++
                  escapePhpStr (showHS flags "" contentsExpr) ++ "*/")
               (selectExpr fSpec 26 "src" "tgt" contentsExpr)
               ++ "'"
           | development flags -- with --dev, also generate sql for the rule itself (without negation) so it can be tested with
                                      -- php/Database.php?testRule=RULENAME
           ] ++
           [ "        , 'pairView' =>" -- a list of sql queries for the pair-view segments 
           , "            array" 
           ] ++
           indent 14 
             (blockParenthesize "(" ")" ","
               ((genMPairView.rrviol) rule
             ) ) ++  
           [ "        )" ]
         | rule <- vrules fSpec ++ grules fSpec
         , let rExpr=rrexp rule
         , let violExpr = notCpl (sign rExpr) rExpr
         , let violationsExpr = conjNF violExpr
         ]
    ) ) ++
  [ ""
  , "$invariantRuleNames = array ("++ intercalate ", " (map (showPhpStr . name) invRules) ++");" ]
 where showMeaning rule = maybe "" aMarkup2String (meaning (language flags) rule)
       showMessage rule = case [ markup | markup <- rrmsg rule, amLang markup == language flags ] of
                            []    -> ""
                            markup:_ -> aMarkup2String markup
       rulesPerRole = [ (role, [rule | (rl, rule) <- fRoleRuls fSpec, rl == role ]) | role <- nub $ map fst $ fRoleRuls fSpec ]
       processRuleNames = nub $ concatMap snd rulesPerRole
       invRules = grules fSpec ++ filter (`notElem` processRuleNames) (vrules fSpec)
       
       genMPairView Nothing                  = []
       genMPairView (Just (PairView pvsegs)) = map genPairViewSeg pvsegs
       
       genPairViewSeg (PairViewText str)   = [ "array ( 'segmentType' => 'Text', 'Text' => " ++ showPhpStr str ++ ")" ] 
       genPairViewSeg (PairViewExp srcOrTgt exp) =
         [ "array ( 'segmentType' => 'Exp'"
         , "      , 'srcOrTgt' => "++showPhpStr (show srcOrTgt)
         , "      , 'expTgt' => "++showPhpStr (show $ target exp)
         , "      , 'expSQL' =>"
         , "          '" ++ fromMaybe (fatal 100 $ "No sql generated for "++showHS flags "" exp)
                                      (selectExpr fSpec 33 "src" "tgt" exp)++"'"
         , "      )"
         ] 
       
   
generateRoles :: Fspc -> [String]
generateRoles fSpec =
  [ "$allRoles ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4 
      (blockParenthesize  "(" ")" "," 
         [ [ "array ( 'name' => "++showPhpStr role
           , "      , 'ruleNames' => array ("++ intercalate ", " (map (showPhpStr . name) rulez) ++")"
           , "      )" ]
         | (role,rulez) <- rulesPerRole ]
    ) )
        
 where rulesPerRole = [ (role, [rule | (rl, rule) <- fRoleRuls fSpec, rl == role ]) | role <- nub $ map fst $ fRoleRuls fSpec ]
       
generateKeys :: Fspc -> Options -> [String]
generateKeys fSpec flags =
  [ "//$allKeys is sorted from spec to gen such that the first match for a concept will be the most specific (e.g. see DatabaseUtils.getKey())."
  , "$allKeys ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4 
      (blockParenthesize  "(" ")" ","
         [ [ "  array ( 'label' => "++showPhpStr label
           , "        , 'concept' => "++showPhpStr (name cpt)
           , "        , 'segments' =>" -- a labeled list of sql queries for the key expressions 
           , "            array" 
           ] ++
           indent 14 (blockParenthesize "(" ")" "," (map genKeySeg keySegs)) ++  
           [ "      )" ]           
         | Kd _ label cpt keySegs <- [ k | c<-conceptsFromSpecificToGeneric, k <- vkeys fSpec, kdcpt k==c ] --sort from spec to gen
         ]
    ) )
 where genKeySeg (KeyText str)   = [ "array ( 'segmentType' => 'Text', 'Text' => " ++ showPhpStr str ++ ")" ] 
       genKeySeg (KeyHtml str)   = [ "array ( 'segmentType' => 'Html', 'Html' => " ++ showPhpStr str ++ ")" ] 
       genKeySeg (KeyExp objDef) = [ "array ( 'segmentType' => 'Exp'"
                                   , "      , 'label' => "++ showPhpStr (objnm objDef) ++ " // key exp: " ++ escapePhpStr (show $ objctx objDef) -- note: unlabeled exps are labeled by (index + 1)
                                   , "      , 'expSQL' =>"
                                   , "          '" ++ fromMaybe (fatal 100 $ "No sql generated for "++showHS flags "" (objctx objDef))
                                                                (selectExpr fSpec 33 "src" "tgt" $ objctx objDef)
                                                   ++"' )"
                                   ]
       (_,islands,_,_,_) = case concs fSpec of
                              []  -> fatal 276 "No concepts in fSpec"
                              c:_ -> cptgE c
       conceptsFromSpecificToGeneric = concat (map reverse islands)
                
generateInterfaces :: Fspc -> Options -> [String]
generateInterfaces fSpec flags =
  [ "$allInterfaceObjects ="
  , "  array"
  ] ++
  addToLastLine ";" 
     (indent 4 
       (blockParenthesize  "(" ")" ","
         (map (generateInterface fSpec flags) (interfaceS fSpec ++ interfaceG fSpec))
     ) )
 

generateInterface :: Fspc -> Options -> Interface -> [String]
generateInterface fSpec flags interface =
  [ let roleStr = case ifcRoles interface of []    -> " for all roles"
                                             rolez -> " for role"++ (if length rolez == 1 then "" else "s") ++" " ++ intercalate ", " (ifcRoles interface)
    in  "// Top-level interface " ++ name interface ++ roleStr  ++ ":" 
  , showPhpStr (name interface) ++ " => " ] ++
  genInterfaceObjects fSpec flags (ifcParams interface) (Just $ ifcRoles interface) 1 (ifcObj interface) 
  
-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> Options -> [Relation] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec flags editableRels mInterfaceRoles depth object =
  [ "array ( 'name' => "++showPhpStr (name object)]
  ++ (if objctx object /= normalizedInterfaceExp && verboseP flags
      then   ["      // Normalization steps:"]
           ++["      // "++escapePhpStr ls | ls<-showPrf showADL (cfProof showADL (objctx object))] -- escape for the pathological case that one of the names in the relation contains a newline
           ++["      //"]      
      else   []
     )
  ++["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (show normalizedInterfaceExp) ] -- escape for the pathological case that one of the names in the relation contains a newline
  ++ case mInterfaceRoles of -- interfaceRoles is present iff this is a top-level interface
       Just interfaceRoles -> [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map showPhpStr interfaceRoles) ++")" 
                              , "      , 'editableConcepts' => array (" ++ intercalate ", " (map (showPhpStr . name) $ getEditableConcepts object) ++")" ]
                                       -- editableConcepts is not used in the interface itself, only globally (maybe we should put it in a separate array) 
       Nothing             -> [] 
  ++ case objctx object of
         ERel r _          | isEditable r -> [ "      , 'relation' => "++showPhpStr (name r) -- only support editing on user-specified relations (no expressions, and no I or V)
                                             , "      , 'relationIsFlipped' => false" 
                                             , "      , 'min' => "++ if isTot r then "'One'" else "'Zero'"
                                             , "      , 'max' => "++ if isUni r then "'One'" else "'Many'"
                                             ]
         EFlp (ERel r _) _ | isEditable r -> [ "      , 'relation' => "++showPhpStr (name r) -- and on flipped versions of those relations. NOTE: same cases appear in getEditableConcepts
                                             , "      , 'relationIsFlipped' => true" 
                                             , "      , 'min' => "++ if isSur r then "'One'" else "'Zero'"
                                             , "      , 'max' => "++ if isInj r then "'One'" else "'Many'"
                                             ]          
         _                                -> [ "      , 'relation' => ''" 
                                             , "      , 'relationIsFlipped' => ''" 
                                             ]          
  ++     
  [ "      , 'srcConcept' => "++showPhpStr (name (source normalizedInterfaceExp))
  , "      , 'tgtConcept' => "++showPhpStr (name (target normalizedInterfaceExp))
  , "      , 'expressionSQL' => '" ++ fromMaybe (fatal 151 ("No sql generated for "++showHS flags "" normalizedInterfaceExp))
                                                (selectExpr fSpec (20+14*depth) "src" "tgt" normalizedInterfaceExp) 
                                   ++ "'"                                                  
  ] 
  ++ generateMSubInterface fSpec flags editableRels depth (objmsub object) ++
  [ "      )"
  ]
 where isEditable rel = rel `elem` editableRels
       normalizedInterfaceExp = conjNF $ objctx object
       getEditableConcepts obj = (case objctx obj of
                                   ERel r _          | isEditable r -> [target r]
                                   EFlp (ERel r _) _ | isEditable r -> [source r]
                                   _                                -> [])
                                 ++ concatMap getEditableConcepts (objAts obj)
  
generateMSubInterface :: Fspc -> Options -> [Relation] -> Int -> Maybe SubInterface -> [String] 
generateMSubInterface fSpec flags editableRels depth subIntf =
  case subIntf of
    Nothing                -> [ "      // No subinterfaces" ] 
    Just (InterfaceRef nm) -> [ "      // InterfaceRef" 
                              , "      , 'refSubInterface' => "++ showPhpStr nm
                              ]
    Just (Box objects)     -> [ "      // Box" 
                              , "      , 'boxSubInterfaces' =>"
                              , "          array"
                              ] ++ 
                              indent 12 
                                (blockParenthesize "(" ")" "," 
                                  (map (genInterfaceObjects fSpec flags editableRels Nothing (depth + 1)) objects))
  

-- utils

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp :: String -> String -> [String] -> String
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++prototypeVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
 
blockParenthesize :: String -> String -> String -> [[String]] -> [String]
blockParenthesize open close sep liness = 
  case liness of 
    [] -> [open ++ close]
    _  -> concat [ zipWith (++) (pre:repeat "  ") linez 
                 | (pre, linez) <- zip ((open++" "): repeat (sep++" ")) liness ] ++ [close]
-- [["line"], ["line1", "line2", "line3"],["linea", "lineb"] ->
-- ( line
-- , line1
--   line2
--   line3
-- , linea
--   lineb
-- )

addToLastLine :: String -> [String] -> [String]
addToLastLine str [] = [str] 
addToLastLine str liness = init liness ++ [last liness ++ str] 
  
showPhpStr :: String -> String
showPhpStr str = "'"++escapePhpStr str++"'"

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr :: String -> String
escapePhpStr cs = concat [fromMaybe [c] $ lookup c [('\'', "\\'"),('\\', "\\\\"),('\n', "\\n")] | c<-cs ] -- escape '   \   \n
-- todo: escape everything else (unicode, etc)

showPhpBool :: Bool -> String
showPhpBool b = if b then "true" else "false"
  
indent :: Int -> [String] -> [String]
indent n liness = [ replicate n ' ' ++ line | line <- liness ]

-- FSpec utils


showPlug :: PlugSQL -> [String]
showPlug plug =  
 ("Table: '"++sqlname plug++"'")
 : 
 indent 4 
   (blockParenthesize "[" "]" "," (map showField (getPlugFields plug)))

-- TODO: maybe this one exists already
getPlugFields :: PlugSQL -> [SqlField]
getPlugFields (TblSQL  {fields = flds}) = flds
getPlugFields BinSQL  { columns = (fld1,fld2)} = [fld1, fld2]
getPlugFields ScalarSQL { sqlColumn = fld} = [fld]

showField :: SqlField -> [String]
showField fld = ["{" ++ (if fldnull fld then "+" else "-") ++ "NUL," ++ (if flduniq fld then "+" else "-") ++ "UNQ} " ++ 
                 "'"++fldname fld ++ "':"++show (target $ fldexpr fld)]

