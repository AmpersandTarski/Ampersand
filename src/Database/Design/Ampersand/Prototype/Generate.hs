module Database.Design.Ampersand.Prototype.Generate (generateAll) where

import Database.Design.Ampersand
-- import Database.Design.Ampersand.FSpec (showPrf,cfProof,lookupCpt,getSpecializations,getGeneralizations)
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Directory
import Database.Design.Ampersand.Prototype.RelBinGenBasics(showPhpStr,escapePhpStr,showPhpBool)
import Database.Design.Ampersand.Prototype.RelBinGenSQL
import Control.Exception
import Database.Design.Ampersand.Prototype.Installer (mkSignalTableSpec, getTableName)

fatal :: Int -> String -> a
fatal = fatalMsg "Generate"

customCssPath :: String
customCssPath = "css/Custom.css"

generateAll :: FSpec -> IO ()
generateAll fSpec =
 do { let filecontent = genPhp "Generate.hs" "Generics.php" genericsPhpContent
--  ; verboseLn (getOpts fSpec) filecontent
    ; writePrototypeFile "Generics.php" filecontent
    ; when (genStaticFiles (getOpts fSpec))(
       case customCssFile (getOpts fSpec) of
        Just customCssFilePath ->
         do { customCssContents <- readCustomCssFile customCssFilePath
            ; writePrototypeFile customCssPath customCssContents
            }
        Nothing -> -- If no css file is specified, we use <filename>.css, if it exists.
         do { let dedicatedCSSPath = replaceExtension (fileName (getOpts fSpec)) "css"
            ; dedicatedCSSExists <- doesFileExist dedicatedCSSPath
            ; if dedicatedCSSExists then
               do { putStrLn $ "  Found " ++ dedicatedCSSPath ++ ", which will be used as Custom.css."
                  ; customCssContents <- readCustomCssFile dedicatedCSSPath
                  ; writePrototypeFile customCssPath customCssContents
                  }
              else -- If not, we check whether there is a css/Custom.css in the prototype directory and create a default one if there isn't.
               do { customExists <- doesFileExist (combine (dirPrototype (getOpts fSpec)) customCssPath)
                  ; if customExists
                    then verboseLn (getOpts fSpec) $ "  File " ++ customCssPath ++ " already exists."
                    else do { verboseLn (getOpts fSpec) $ "  File " ++ customCssPath ++ " does not exist, creating default for Oblomilan style."
                            ; writePrototypeFile customCssPath "@import url(\"Oblomilan.css\");"
                            }
                  }
            }
      )
    }
  where
    genericsPhpContent :: [String]
    genericsPhpContent =
      intercalate [""]
        [ generateConstants (getOpts fSpec)
        , generateSpecializations fSpec
        , generateTableInfos fSpec
        , generateRules fSpec
        , generateConjuncts fSpec
        , generateRoles fSpec
        , generateViews fSpec
        , generateInterfaces fSpec
        ]
    readCustomCssFile f =
      catch (readFile f)
            (\e -> do let err = show (e :: IOException)
                      _ <- fatal 75 ("ERROR: Cannot open custom css file ' " ++ f ++ "': " ++ err)
                      return "")
    writePrototypeFile fname content =
     do { verboseLn (getOpts fSpec) ("  Generating "++fname)
        ; writeFile (combine (dirPrototype (getOpts fSpec)) fname) content
        }

generateConstants :: Options -> [String]
generateConstants opts =
  [ "$versionInfo = "++showPhpStr ampersandVersionStr++";" -- so we can show the version in the php-generated html
  , ""
  , "$dbName = "++showPhpStr (dbName opts)++";"
  , ""
  , "$isDev = "++showPhpBool (development opts)++";"
  , ""
  , "$autoRefreshInterval = "++showPhpStr (show $ fromMaybe 0 $ autoRefresh opts)++";"
  ]

generateSpecializations :: FSpec -> [String]
generateSpecializations fSpec =
  [ "$allSpecializations = // transitive, so including specializations of specializations"
  , "  array" ] ++
  addToLastLine ";"
    (indent 4 (blockParenthesize "(" ")" ","
         [ [ showPhpStr (name cpt)++" => array ("++ intercalate ", " (map (showPhpStr . name) specializations) ++")" ]
         | cpt <- concs fSpec, let specializations = smallerConcepts (gens fSpec) cpt,  not ( null specializations) ])
    )

generateTableInfos :: FSpec -> [String]
generateTableInfos fSpec =
  [ "$relationTableInfo ="
  , "  array" ] ++
  addToLastLine ";"
    (indent 4 (blockParenthesize "(" ")" ","
         [ [showPhpStr (showHSName decl)++" => array ( 'srcConcept' => "++showPhpStr (name (source decl))
                                                 ++ ", 'tgtConcept' => "++showPhpStr (name (target decl))
                                                 ++ ", 'table'      => "++showPhpStr (name table)
                                                 ++ ", 'srcCol'     => "++showPhpStr (fldname srcCol)
                                                 ++ ", 'tgtCol'     => "++showPhpStr (fldname tgtCol)++")"]
         | decl@Sgn{} <- allDecls fSpec  -- SJ 13 nov 2013: changed to generate all relations instead of just the ones used.
         , let (table,srcCol,tgtCol) = getDeclarationTableInfo fSpec decl
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

generateRules :: FSpec -> [String]
generateRules fSpec =
  [ "$allRules ="
  , "  array"
  ] ++
  addToLastLine ";"
    (indent 4
      (blockParenthesize  "(" ")" ","
         [ [ (showPhpStr.rrnm) rule ++ " =>"
           , "  array ( 'name'          => "++(showPhpStr.rrnm)              rule
           , "        , 'signalTable'   => "++(showPhpStr.getTableName.mkSignalTableSpec) rule
           , "        , 'ruleAdl'       => "++(showPhpStr.showADL.rrexp)     rule
           , "        , 'origin'        => "++(showPhpStr.show.rrfps)        rule
           , "        , 'meaning'       => "++(showPhpStr.showMeaning)       rule
           , "        , 'message'       => "++(showPhpStr.showMessage)       rule
           , "        , 'srcConcept'    => "++(showPhpStr.name.source.rrexp) rule
           , "        , 'tgtConcept'    => "++(showPhpStr.name.target.rrexp) rule
           ] ++
           ( if verboseP (getOpts fSpec)
             then   ["        // Normalization steps:"]
                  ++["        // "++ls | ls<-(showPrf showADL . cfProof (getOpts fSpec)) violExpr]
                  ++["        // "]
             else   []
           ) ++
           ( if development (getOpts fSpec)
             then [ "        // Rule Ampersand: "++escapePhpStr (showADL rExpr) ] ++
                  [ "        // Normalized complement (== violationsSQL): " ] ++
                  (lines ( "        // "++(showHS (getOpts fSpec) "\n        // ") violationsExpr))
             else [] ) ++
           [ "        , 'violationsSQL' => "++ showPhpStr (selectExpr fSpec 26 "src" "tgt" violationsExpr)
           ] ++
           [ "        , 'contentsSQL'   => " ++
             let contentsExpr = conjNF (getOpts fSpec) rExpr in
              showPhpStr (selectExpr fSpec 26 "src" "tgt" contentsExpr)
           | development (getOpts fSpec) -- with --dev, also generate sql for the rule itself (without negation) so it can be tested with
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
         , let violationsExpr = conjNF (getOpts fSpec) violExpr
         ]
    ) )
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
         , "          " ++ showPhpStr (selectExpr fSpec 33 "src" "tgt" exp)
         , "      )"
         ]

generateConjuncts :: FSpec -> [String]
generateConjuncts fSpec =
  [ "$allConjuncts ="
  , "  array"
  ] ++
  addToLastLine ";"
     (indent 4
       (blockParenthesize  "(" ")" ","
         [ [ showPhpStr (rc_id conj) ++ " =>"
           , "  array ( 'ruleName'   => "++(showPhpStr . rrnm . rc_orgRule)   conj -- the name of the rule that gave rise to this conjunct 
           ] ++
           ( if verboseP (getOpts fSpec)
             then   ["        // Normalization steps:"]
                  ++["        // "++ls | ls<-(showPrf showADL . cfProof (getOpts fSpec)) violExpr]
                  ++["        // "]
             else   [] ) ++
           ( if development (getOpts fSpec)
             then [ "        // Conjunct Ampersand: "++escapePhpStr (showADL rExpr) ] ++
                  [ "        // Normalized complement (== violationsSQL): " ] ++
                  (lines ( "        // "++(showHS (getOpts fSpec) "\n        // ") violationsExpr))
             else [] ) ++
           [ "        , 'violationsSQL' => "++ showPhpStr (selectExpr fSpec 36 "src" "tgt" violationsExpr)
           , "        )"
           ]
         | conj<-vconjs fSpec
         , let rExpr=rc_conjunct conj
         , not (rrnm (rc_orgRule conj) `elem` uniRuleNames fSpec && not (isSignal $ rc_orgRule conj)) -- don't include UNI conjuncts for invariants 
         , let violExpr = notCpl rExpr
         , let violationsExpr = conjNF (getOpts fSpec) violExpr
         ]
     ) )
    
uniRuleNames :: FSpec -> [String]
uniRuleNames fSpec = [ name rule | Just rule <- map (rulefromProp Uni) $ declsInScope fSpec ]

generateRoles :: FSpec -> [String]
generateRoles fSpec =
  [ "$allRoles ="
  , "  array"
  ] ++
  addToLastLine ";"
    (indent 4
      (blockParenthesize  "(" ")" ","
         [ [ "array ( 'name' => "++showPhpStr role
           , "      , 'ruleNames' => array ("++ intercalate ", " ((map (showPhpStr . name . snd) . filter (maintainedByRole role) . fRoleRuls) fSpec) ++")"
           , "      )" ]
         | role <- fRoles fSpec ]
    ) )
  where maintainedByRole role (role',_) = role == role'

generateViews :: FSpec -> [String]
generateViews fSpec =
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
                                     , "      , 'label' => " ++ showPhpStr (objnm objDef) ++ " // view exp: " ++ escapePhpStr (showADL $ objctx objDef) -- note: unlabeled exps are labeled by (index + 1)
                                     , "      , 'expSQL' =>"
                                     , "          " ++ showPhpStr (selectExpr fSpec 33 "src" "tgt" (objctx objDef))
                                     , "      )"
                                   ]
       conceptsFromSpecificToGeneric = concatMap reverse (kernels fSpec)

generateInterfaces :: FSpec -> [String]
generateInterfaces fSpec =
  [ "$allInterfaceObjects ="
  , "  array"
  ] ++
  addToLastLine ";"
     (indent 4
       (blockParenthesize  "(" ")" ","
         (map (generateInterface fSpec) (interfaceS fSpec ++ interfaceG fSpec))
     ) )

generateInterface :: FSpec -> Interface -> [String]
generateInterface fSpec interface =
  [ let roleStr = case ifcRoles interface of []    -> " for all roles"
                                             rolez -> " for role"++ (if length rolez == 1 then "" else "s") ++" " ++ intercalate ", " (ifcRoles interface)
    in  "// Top-level interface " ++ name interface ++ roleStr  ++ ":"
  , showPhpStr (name interface) ++ " => " ] ++
  indent 2 (genInterfaceObjects fSpec(ifcParams interface) (Just $ topLevelFields) 1 (ifcObj interface))
  where topLevelFields = -- for the top-level interface object we add the following fields (saves us from adding an extra interface node to the php data structure)
          [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map showPhpStr $ ifcRoles interface) ++")" 
          , "      , 'interfaceInvariantConjunctIds' => array ("++intercalate ", " (map (showPhpStr . rc_id) invConjs)++")"
          , "      , 'interfaceSignalConjunctIds' => array ("++intercalate ", " (map (showPhpStr . rc_id) sgnlConjs)++")"
          ]
          where -- Currently, conjuncts that appear in several rules will be evaluated multiple times, to be able to report
                -- failure of each rule (and for signals update each rule signal database.) This can be optimized by having
                -- only one conjunct for each unique conjunct expression, and keep a list of originating rules in that conjunct.
                (sgnlConjs, invConjs) = partition (isSignal . rc_orgRule) [ conj | conj <- ifcControls interface ] 

genInterfaceObjects :: FSpec -> [Expression] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec editableRels mTopLevelFields depth object =
  [ "array ( 'name' => "++showPhpStr (name object)]
  ++ (if verboseP (getOpts fSpec)  -- previously, this included the condition        objctx object /= normalizedInterfaceExp
      then   ["      // Normalization steps:"]
           ++["      // "++ls | ls<-(showPrf showADL.cfProof (getOpts fSpec).objctx) object] -- let's hope that none of the names in the relation contains a newline
           ++["      //"]
      else   []
     )
  ++ ["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (showADL normalizedInterfaceExp) ]
  ++ ["      // normalizedInterfaceExp = " ++ show normalizedInterfaceExp | development (getOpts fSpec) ]
             -- escape for the pathological case that one of the names in the relation contains a newline
  ++ fromMaybe [] mTopLevelFields -- declare extra fields if this is a top level interface object
  ++ case getEditableRelation editableRels normalizedInterfaceExp of 
       Just (srcConcept, d, tgtConcept, isFlipped) ->
         [ "      , 'relation' => "++showPhpStr (showHSName d) ++ " // this interface expression is editable"
         , "      , 'relationIsFlipped' => "++show isFlipped ] ++
         (if isFlipped 
          then [ "      , 'min' => "++ if isSur d then "'One'" else "'Zero'"
               , "      , 'max' => "++ if isInj d then "'One'" else "'Many'" ]
          else [ "      , 'min' => "++ if isTot d then "'One'" else "'Zero'" 
               , "      , 'max' => "++ if isUni d then "'One'" else "'Many'" ]) ++
         [ "      , 'srcConcept' => "++showPhpStr (name srcConcept)
         , "      , 'tgtConcept' => "++showPhpStr (name tgtConcept)
         ]
       _ ->
         [ "      , 'relation' => '' // this interface expression is not editable"
         , "      , 'relationIsFlipped' => ''"
         , "      , 'srcConcept' => "++showPhpStr (name (source normalizedInterfaceExp)) -- fall back to typechecker type, as we don't want
         , "      , 'tgtConcept' => "++showPhpStr (name (target normalizedInterfaceExp)) -- to copy its functionality here
         ]
  ++
  [ "      , 'expressionSQL' => " ++ showPhpStr (selectExpr fSpec (22+14*depth) "src" "tgt" normalizedInterfaceExp)
  ]
  ++ generateMSubInterface fSpec editableRels depth (objmsub object) ++
  [ "      )"
  ]
 where normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object

generateMSubInterface :: FSpec -> [Expression] -> Int -> Maybe SubInterface -> [String]
generateMSubInterface fSpec editableRels depth subIntf =
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
                                  (map (genInterfaceObjects fSpec editableRels Nothing (depth + 1)) objects))

-- utils

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp :: String -> String -> [String] -> String
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++ampersandVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
