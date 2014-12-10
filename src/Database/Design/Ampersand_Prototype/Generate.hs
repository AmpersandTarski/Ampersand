{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand_Prototype.Generate (generateAll) where

import Database.Design.Ampersand
-- import Database.Design.Ampersand.Fspec (showPrf,cfProof,lookupCpt,getSpecializations,getGeneralizations)
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Directory
import Database.Design.Ampersand_Prototype.RelBinGenBasics(showPhpStr,escapePhpStr,showPhpBool)
import Database.Design.Ampersand_Prototype.RelBinGenSQL
import Control.Exception

fatal :: Int -> String -> a
fatal = fatalMsg "Generate"

customCssPath :: String
customCssPath = "css/Custom.css"

generateAll :: Fspc -> IO ()
generateAll fSpec =
 do { let filecontent = genPhp "Generate.hs" "Generics.php" genericsPhpContent
--  ; verboseLn (flags fSpec) filecontent
    ; writePrototypeFile "Generics.php" filecontent
    ; when (genStaticFiles (flags fSpec))(
       case customCssFile (flags fSpec) of
        Just customCssFilePath ->
         do { customCssContents <- readCustomCssFile customCssFilePath
            ; writePrototypeFile customCssPath customCssContents
            }
        Nothing -> -- If no css file is specified, we use <filename>.css, if it exists.
         do { let dedicatedCSSPath = replaceExtension (fileName (flags fSpec)) "css"
            ; dedicatedCSSExists <- doesFileExist dedicatedCSSPath
            ; if dedicatedCSSExists then
               do { putStrLn $ "  Found " ++ dedicatedCSSPath ++ ", which will be used as Custom.css."
                  ; customCssContents <- readCustomCssFile dedicatedCSSPath
                  ; writePrototypeFile customCssPath customCssContents
                  }
              else -- If not, we check whether there is a css/Custom.css in the prototype directory and create a default one if there isn't.
               do { customExists <- doesFileExist (combine (dirPrototype (flags fSpec)) customCssPath)
                  ; if customExists
                    then verboseLn (flags fSpec) $ "  File " ++ customCssPath ++ " already exists."
                    else do { verboseLn (flags fSpec) $ "  File " ++ customCssPath ++ " does not exist, creating default for Oblomilan style."
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
        [ generateConstants (flags fSpec)
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
     do { verboseLn (flags fSpec) ("  Generating "++fname)
        ; writeFile (combine (dirPrototype (flags fSpec)) fname) content
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

generateSpecializations :: Fspc -> [String]
generateSpecializations fSpec =
  [ "$allSpecializations = // transitive, so including specializations of specializations"
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

generateRules :: Fspc -> [String]
generateRules fSpec =
  [ "$allRules ="
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
           ( if verboseP (flags fSpec)
             then   ["        // Normalization steps:"]
                  ++["        // "++ls | ls<-(showPrf showADL . cfProof (flags fSpec)) violExpr]
                  ++["        // "]
             else   []
           ) ++
           ( if development (flags fSpec)
             then [ "        // Rule ADL: "++escapePhpStr (showADL rExpr) ] ++
                  [ "        // Normalized complement (== violationsSQL): " ] ++
                  (lines ( "        // "++(showHS (flags fSpec) "\n        // ") violationsExpr))
             else [] ) ++
           [ "        , 'violationsSQL' => "++ showPhpStr (selectExpr fSpec 26 "src" "tgt" violationsExpr)
           ] ++
           [ "        , 'contentsSQL'   => " ++
             let contentsExpr = conjNF (flags fSpec) rExpr in
              showPhpStr (selectExpr fSpec 26 "src" "tgt" contentsExpr)
           | development (flags fSpec) -- with --dev, also generate sql for the rule itself (without negation) so it can be tested with
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
         , let violationsExpr = conjNF (flags fSpec) violExpr
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

generateConjuncts :: Fspc -> [String]
generateConjuncts fSpec =
  [ "$allConjuncts ="
  , "  array"
  ] ++
  addToLastLine ";"
     (indent 4
       (blockParenthesize  "(" ")" ","
         [ [ mkConjunctName conj ++ " =>"
           , "  array ( 'ruleName'   => "++(showPhpStr.rc_rulename)   conj -- the name of the rule that gave rise to this conjunct 
           ] ++
           ( if verboseP (flags fSpec)
             then   ["        // Normalization steps:"]
                  ++["        // "++ls | ls<-(showPrf showADL . cfProof (flags fSpec)) violExpr]
                  ++["        // "]
             else   [] ) ++
           ( if development (flags fSpec)
             then [ "        // Conjunct ADL: "++escapePhpStr (showADL rExpr) ] ++
                  [ "        // Normalized complement (== violationsSQL): " ] ++
                  (lines ( "        // "++(showHS (flags fSpec) "\n        // ") violationsExpr))
             else [] ) ++
           [ "        , 'violationsSQL' => "++ showPhpStr (selectExpr fSpec 36 "src" "tgt" violationsExpr)
           , "        )"
           ]
         | conj<-vconjs fSpec
         , let rExpr=rc_conjunct conj
         , rc_rulename conj `notElem` uniRuleNames fSpec
         , rc_rulename conj `elem` map name (invars fSpec)
         , let violExpr = notCpl rExpr
         , let violationsExpr = conjNF (flags fSpec) violExpr
         ]
     ) )
    
uniRuleNames :: Fspc -> [String]
uniRuleNames fSpec = [ name rule | Just rule <- map (rulefromProp Uni) $ declsInScope fSpec ]

-- note the similarity with showHSName :: Conjunct -> String
mkConjunctName :: Conjunct -> String
mkConjunctName conj = showPhpStr ("cjct_"++rc_rulename conj++"_"++show (rc_int conj))

generateRoles :: Fspc -> [String]
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

generateViews :: Fspc -> [String]
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

generateInterfaces :: Fspc -> [String]
generateInterfaces fSpec =
  [ "$allInterfaceObjects ="
  , "  array"
  ] ++
  addToLastLine ";"
     (indent 4
       (blockParenthesize  "(" ")" ","
         (map (generateInterface fSpec) (interfaceS fSpec ++ interfaceG fSpec))
     ) )

generateInterface :: Fspc -> Interface -> [String]
generateInterface fSpec interface =
  [ let roleStr = case ifcRoles interface of []    -> " for all roles"
                                             rolez -> " for role"++ (if length rolez == 1 then "" else "s") ++" " ++ intercalate ", " (ifcRoles interface)
    in  "// Top-level interface " ++ name interface ++ roleStr  ++ ":"
  , showPhpStr (name interface) ++ " => " ] ++
  indent 2 (genInterfaceObjects fSpec(ifcParams interface) (Just $ topLevelFields) 1 (ifcObj interface))
  where topLevelFields = -- for the top-level interface object we add the following fields (saves us from adding an extra interface node to the php data structure)
          [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map showPhpStr $ ifcRoles interface) ++")" 
          , "      , 'interfaceInvariantConjunctNames' => array ("++intercalate ", " (map mkConjunctName invConjs)++")"
          ]
          where invConjs = [ conj | conj <- ifcControls interface
                                  , rc_rulename conj `notElem` uniRuleNames fSpec
                                  , rc_rulename conj `elem` map name (invars fSpec) ] 
-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> [Expression] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec editableRels mTopLevelFields depth object =
  [ "array ( 'name' => "++showPhpStr (name object)]
  ++ (if verboseP (flags fSpec)  -- previously, this included the condition        objctx object /= normalizedInterfaceExp
      then   ["      // Normalization steps:"]
           ++["      // "++ls | ls<-(showPrf showADL.cfProof (flags fSpec).objctx) object] -- let's hope that none of the names in the relation contains a newline
           ++["      //"]
      else   []
     )
  ++ ["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (showADL normalizedInterfaceExp) ]
  ++ ["      // normalizedInterfaceExp = " ++ show normalizedInterfaceExp | development (flags fSpec) ]
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
 where normalizedInterfaceExp = conjNF (flags fSpec) $ objctx object

generateMSubInterface :: Fspc -> [Expression] -> Int -> Maybe SubInterface -> [String]
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
