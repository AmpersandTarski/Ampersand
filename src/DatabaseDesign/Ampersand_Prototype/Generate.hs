{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where


import DatabaseDesign.Ampersand_Prototype.CoreImporter 
-- import DatabaseDesign.Ampersand.Fspec (showPrf,cfProof,lookupCpt,getSpecializations,getGeneralizations)
import Prelude hiding (writeFile,readFile,getContents,exp)
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
 do { writePrototypeFile "Generics.php" . genPhp "Generate.hs" "Generics.php" $ genericsPhpContent
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
    genericsPhpContent :: [String]
    genericsPhpContent =  
      intercalate [""] 
        [ generateConstants flags
        , generateSpecializations fSpec
        , generateTableInfos fSpec
        , generateRules fSpec flags
        , generateRoles fSpec
        , generateViews fSpec flags
        , generateInterfaces fSpec flags ]
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
         | cpt <- concs fSpec, let specializations = smallerConcepts (gens fSpec) cpt,  not ( null specializations) ])
    )        

generateTableInfos :: Fspc -> [String]
generateTableInfos fSpec =
  [ "$relationTableInfo ="
  , "  array" ] ++
  addToLastLine ";" 
    (indent 4 (blockParenthesize "(" ")" ","
         [ [showPhpStr (name decl)++" => array ('srcConcept' => "++showPhpStr (name (source decl))++", 'tgtConcept' => "++showPhpStr (name (target decl))++
                                          ", 'table' => "++showPhpStr (name table)
                                        ++", 'srcCol' => "++showPhpStr (fldname srcCol)
                                        ++", 'tgtCol' => "++showPhpStr (fldname tgtCol)++")"] 
         | decl@Sgn{} <- allDecls fSpec  -- SJ 13 nov 2013: changed to generate all relations instead of just the ones used.
         , let (table,srcCol,tgtCol) = fromMaybe (fatal 105 $ "No table info for declaration " ++ show decl)
                                                 (getDeclarationTableInfo fSpec decl)
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
                | (table,conceptFields) <- groupOnTable . concatMap (lookupCpt fSpec) $ c : largerConcepts (gens fSpec) c 
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
                | field <- plugFields plug]
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
           , "  array ( 'name'          => "++(showPhpStr.rrnm)              rule
           , "        , 'ruleAdl'       => "++(showPhpStr.showADL.rrexp)     rule
           , "        , 'origin'        => "++(showPhpStr.show.rrfps)        rule
           , "        , 'meaning'       => "++(showPhpStr.showMeaning)       rule
           , "        , 'message'       => "++(showPhpStr.showMessage)       rule
           , "        , 'srcConcept'    => "++(showPhpStr.name.source.rrexp) rule
           , "        , 'tgtConcept'    => "++(showPhpStr.name.target.rrexp) rule
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
           [ "        , 'contentsSQL'   => '" ++
             let contentsExpr = conjNF rExpr in
             fromMaybe
               ("/*ERROR: no sql generated for " ++
                  escapePhpStr (showHS flags "" contentsExpr) ++ "*/")
               (selectExpr fSpec 26 "src" "tgt" contentsExpr)
               ++ "'"
           | development flags -- with --dev, also generate sql for the rule itself (without negation) so it can be tested with
                                      -- php/Database.php?testRule=RULENAME
           ] ++
           [ "        , 'pairView'      =>" -- a list of sql queries for the pair-view segments 
           , "            array" 
           ] ++
           indent 14 
             (blockParenthesize "(" ")" ","
               ((genMPairView.rrviol) rule
             ) ) ++  
           [ "        )" ]
         | rule <- vrules fSpec ++ grules fSpec
         , let rExpr=rrexp rule
         , let violExpr = notCpl rExpr
         , let violationsExpr = conjNF violExpr
         ]
    ) ) ++
  [ ""
  , "$invariantRuleNames = array ("++ intercalate ", " (map (showPhpStr . name) (invars fSpec)) ++");" ]
 where showMeaning rule = maybe "" aMarkup2String (meaning (fsLang fSpec) rule)
       showMessage rule = case [ markup | markup <- rrmsg rule, amLang markup == fsLang fSpec ] of
                            []    -> ""
                            markup:_ -> aMarkup2String markup
       
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
        
 where rulesPerRole = [ (role, [rule | (rl, rule) <- fRoleRuls fSpec, rl == role ]) | role <- fRoles fSpec ]
       
generateViews :: Fspc -> Options -> [String]
generateViews fSpec flags =
  [ "//$allViews is sorted from spec to gen such that the first match for a concept will be the most specific (e.g. see DatabaseUtils.getView())."
  , "$allViews ="
  , "  array" 
  ] ++
  addToLastLine ";" 
    (indent 4 
      (blockParenthesize  "(" ")" ","
         [ [ "  array ( 'label' => "++showPhpStr label
           , "        , 'concept' => "++showPhpStr (name cpt)
           , "        , 'segments' =>" -- a labeled list of sql queries for the view expressions 
           , "            array" 
           ] ++
           indent 14 (blockParenthesize "(" ")" "," (map genViewSeg viewSegs)) ++  
           [ "        )" ]           
         | Vd _ label cpt viewSegs <- [ v | c<-conceptsFromSpecificToGeneric, v <- vviews fSpec, vdcpt v==c ] --sort from spec to gen
         ]
    ) )
 where genViewSeg (ViewText str)   = [ "array ( 'segmentType' => 'Text', 'Text' => " ++ showPhpStr str ++ ")" ] 
       genViewSeg (ViewHtml str)   = [ "array ( 'segmentType' => 'Html', 'Html' => " ++ showPhpStr str ++ ")" ] 
       genViewSeg (ViewExp objDef) = [ "array ( 'segmentType' => 'Exp'"
                                     , "      , 'label' => "++ showPhpStr (objnm objDef) ++ " // view exp: " ++ escapePhpStr (show $ objctx objDef) -- note: unlabeled exps are labeled by (index + 1)
                                     , "      , 'expSQL' =>"
                                     , "          '" ++ fromMaybe (fatal 100 $ "No sql generated for "++showHS flags "" (objctx objDef))
                                                                (selectExpr fSpec 33 "src" "tgt" $ objctx objDef)++"'"
                                     , "      )"
                                   ]
       conceptsFromSpecificToGeneric = concatMap reverse (kernels fSpec)

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
genInterfaceObjects :: Fspc -> Options -> [Expression] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec flags editableRels mInterfaceRoles depth object =
  [ "array ( 'name' => "++showPhpStr (name object)]
  ++ (if objctx object /= normalizedInterfaceExp && verboseP flags
      then   ["      // Normalization steps:"]
           ++["      // "++escapePhpStr ls | ls<-(showPrf showADL.cfProof showADL.objctx) object] -- escapePhpStr for the pathological case that one of the names in the relation contains a newline
           ++["      //"]      
      else   []
     )
  ++["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (show normalizedInterfaceExp) ] -- escape for the pathological case that one of the names in the relation contains a newline
  ++ case mInterfaceRoles of -- interfaceRoles is present iff this is a top-level interface
       Just interfaceRoles -> [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map showPhpStr interfaceRoles) ++")" 
                              , "      , 'editableConcepts' => array (" ++ intercalate ", " (map (showPhpStr . name) $ getEditableConcepts object) ++")" ]
                                       -- editableConcepts is not used in the interface itself, only globally (maybe we should put it in a separate array) 
       Nothing             -> [] 
  ++ let (flipped, unflippedExpr) = case objctx object of
                                       EFlp e -> (True,e)
                                       e      -> (False,e)
         d = case unflippedExpr of 
             EDcD d'  -> d'
             EDcI c   -> Isn c
             EDcV sgn -> Vs sgn
             _        -> fatal 325 $ "only primitive expressions should be found here.\nHere we see: " ++ show unflippedExpr
     in (if isEditable unflippedExpr
         then [ "      , 'relation' => "++showPhpStr (name d) 
              , "      , 'relationIsFlipped' => "++show flipped ]++ 
              [ "      , 'min' => "++ if isSur d then "'One'" else "'Zero'" | flipped] ++
              [ "      , 'max' => "++ if isInj d then "'One'" else "'Many'" | flipped] ++
              [ "      , 'min' => "++ if isTot d then "'One'" else "'Zero'" | not flipped] ++
              [ "      , 'max' => "++ if isUni d then "'One'" else "'Many'" | not flipped]
         else [ "      , 'relation' => ''" 
              , "      , 'relationIsFlipped' => ''" 
              ])       
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
 where isEditable (EDcD d) = d `elem` [d' | EDcD d' <- editableRels]
       isEditable (EFlp e) = isEditable e
       isEditable _                   = False
       normalizedInterfaceExp = conjNF $ objctx object
       getEditableConcepts obj = (let e = objctx obj in
                                  case e of
                                   EDcD d        | isEditable e       -> [target d]
                                   EFlp (EDcD d) | isEditable (flp e) -> [source d]
                                   _                                  -> []
                                 )
                                 ++ concatMap getEditableConcepts (attributes obj)
  
generateMSubInterface :: Fspc -> Options -> [Expression] -> Int -> Maybe SubInterface -> [String] 
generateMSubInterface fSpec flags editableRels depth subIntf =
  case subIntf of
    Nothing                -> [ "      // No subinterfaces" ] 
    Just (InterfaceRef nm) -> [ "      // InterfaceRef" 
                              , "      , 'refSubInterface' => "++ showPhpStr nm
                              ]
    Just (Box _ objects)     -> [ "      // Box" 
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
   (blockParenthesize "[" "]" "," (map showField (plugFields plug)))


showField :: SqlField -> [String]
showField fld = ["{" ++ (if fldnull fld then "+" else "-") ++ "NUL," ++ (if flduniq fld then "+" else "-") ++ "UNQ} " ++ 
                 "'"++fldname fld ++ "':"++show (target $ fldexpr fld)]

