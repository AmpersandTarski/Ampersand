{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand_Prototype.Generate (generateAll,otherQuerys) where

import Database.Design.Ampersand_Prototype.CoreImporter
-- import Database.Design.Ampersand.Fspec (showPrf,cfProof,lookupCpt,getSpecializations,getGeneralizations)
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Directory
import Database.Design.Ampersand_Prototype.Version
import Database.Design.Ampersand_Prototype.RelBinGenBasics(showPhpStr,escapePhpStr,showPhpBool)
import Database.Design.Ampersand_Prototype.RelBinGenSQL
import Control.Exception

--fatal :: Int -> String -> a
--fatal = fatalMsg "Generate"

customCssPath :: String
customCssPath = "css/Custom.css"

otherQuerys :: Fspc -> [String]
otherQuerys _ = []

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
    ; when (development (flags fSpec)) $
       do { verboseLn (flags fSpec) "Generated tables\n"
          ; verboseLn (flags fSpec) ( unlines ( concatMap showPlug [ plug | InternalPlug plug <- plugInfos fSpec]))
          }
    }
  where
    genericsPhpContent :: [String]
    genericsPhpContent =
      intercalate [""]
        [ generateConstants (flags fSpec)
        , generateSpecializations fSpec
        , generateTableInfos fSpec
        , generateRules fSpec
        , generateRoles fSpec
        , generateViews fSpec
        , generateInterfaces fSpec]
    readCustomCssFile f =
      catch (readFile f)
            (\e -> do let err = show (e :: IOException)
                      _ <- error ("ERROR: Cannot open custom css file ' " ++ f ++ "': " ++ err)
                      return "")
    writePrototypeFile fname content =
     do { verboseLn (flags fSpec) ("  Generating "++fname)
        ; writeFile (combine (dirPrototype (flags fSpec)) fname) content
        }

generateConstants :: Options -> [String]
generateConstants opts =
  [ "$versionInfo = "++showPhpStr prototypeVersionStr++";" -- so we can show the version in the php-generated html
  , ""
  , "$dbName = "++showPhpStr (dbName opts)++";"
  , ""
  , "$isDev = "++showPhpBool (development opts)++";"
  , ""
  , "$autoRefreshInterval = "++showPhpStr (show $ fromMaybe 0 $ autoRefresh opts)++";"
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
         [ [showPhpStr (name decl)++" => array ( 'srcConcept' => "++showPhpStr (name (source decl))
                                            ++", 'tgtConcept' => "++showPhpStr (name (target decl))
                                            ++", 'table'      => "++showPhpStr (name table)
                                            ++", 'srcCol'     => "++showPhpStr (fldname srcCol)
                                            ++", 'tgtCol'     => "++showPhpStr (fldname tgtCol)++")"]
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
           ( if verboseP (flags fSpec)
             then   ["        // Normalization steps:"]
                  ++["        // "++ls | ls<-(showPrf showADL . cfProof (flags fSpec)) violExpr]
                  ++["        // "]
             else   []
           ) ++
           [ "        // Normalized complement (== violationsSQL): " ] ++
           (lines ( "        // "++(showHS (flags fSpec) "\n        // ") violationsExpr))++
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
         , "          " ++ showPhpStr (selectExpr fSpec 33 "src" "tgt" exp)
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
  genInterfaceObjects fSpec(ifcParams interface) (Just $ ifcRoles interface) 1 (ifcObj interface)

-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> [Expression] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec editableRels mInterfaceRoles depth object =
  [ "array ( 'name' => "++showPhpStr (name object)]
  ++ (if verboseP (flags fSpec)  -- previously, this included the condition        objctx object /= normalizedInterfaceExp
      then   ["      // Normalization steps:"]
           ++["      // "++ls | ls<-(showPrf showADL.cfProof (flags fSpec).objctx) object] -- let's hope that none of the names in the relation contains a newline
           ++["      //"]
      else   []
     )
  ++ ["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (showADL normalizedInterfaceExp) ]
             -- escape for the pathological case that one of the names in the relation contains a newline
  ++ case mInterfaceRoles of -- interfaceRoles is present iff this is a top-level interface
       Just interfaceRoles -> [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map showPhpStr interfaceRoles) ++")"
                              , "      , 'editableConcepts' => array (" ++ intercalate ", " (map (showPhpStr . name) $ getEditableConcepts object) ++")" ]
                                       -- editableConcepts is not used in the interface itself, only globally (maybe we should put it in a separate array)
       Nothing             -> []
  ++ case getEditableRelation normalizedInterfaceExp of 
       Just (Just (srcConcept, d, tgtConcept, isFlipped)) ->
         [ "      , 'relation' => "++showPhpStr (name d) ++ " // this interface expression is editable"
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
  [ "      , 'expressionSQL' => " ++ showPhpStr (selectExpr fSpec (20+14*depth) "src" "tgt" normalizedInterfaceExp)
  ]
  ++ generateMSubInterface fSpec editableRels depth (objmsub object) ++
  [ "      )"
  ]
 where -- We allow editing on basic relations (Declarations) that may have been flipped, or narrowed/widened by composing with I.
       -- getEditableRelation returns Nothing if the expression contains unhandled nodes; Just Nothing if the expression is okay but
       -- does not contain a relation; and Just (Just dclInfo) if the expression contains an editable relation
       getEditableRelation :: Expression -> Maybe (Maybe (A_Concept, Declaration, A_Concept, Bool))
       getEditableRelation e@(EDcD d)      = if e `elem` editableRels 
                                             then Just $ Just (source d, d, target d, False) -- basic editable relation
                                             else Nothing                                    -- expression is not editable
       getEditableRelation (EDcI _)        = Just Nothing                                    -- narrowed/widened relation
       getEditableRelation (EEps _ _)      = Just Nothing                                    -- epsilon
       getEditableRelation (EFlp e)        = case getEditableRelation e of                   -- flipped relation
                                               Just (Just (s,d,t,isFlipped)) -> Just (Just (t,d,s,not isFlipped))
                                               x                             -> x 
       getEditableRelation (EBrk e)        = getEditableRelation e    -- brackets
       getEditableRelation (ECps (e1, e2)) =
         case getEditableRelation e1 of
           Nothing      -> Nothing                                    -- e1 is not editable
           Just Nothing -> getEditableRelation e2                     -- e1 does not contain a relation, so e2 should
           Just (Just relInfo) -> case getEditableRelation e2 of      -- e1 contains a relation
                                Nothing -> Nothing                    -- e2 contains unhandled nodes
                                Just (Nothing) -> Just (Just relInfo) -- e2 does not contain a relation, so we use the one from e1
                                Just (Just _)  -> Nothing             -- both contain a relation, so not editable
       getEditableRelation _               = Nothing
 
       normalizedInterfaceExp = conjNF (flags fSpec) $ objctx object
       getEditableConcepts obj = -- TODO: Nasty, instead of calling getEditableDeclaration recursively here (and only using it when the interface is
                                 --       top level), we should return the editable concepts together with genInterfaceObjects and collect at top level.
         case getEditableRelation $ conjNF (flags fSpec) $ objctx obj of
           Just (Just _) -> [target $ objctx obj]
           _             -> []
         ++ concatMap getEditableConcepts (attributes obj)

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

indent :: Int -> [String] -> [String]
indent n liness = [ replicate n ' ' ++ line | line <- liness ]

-- FSpec utils

showPlug :: PlugSQL -> [String]
showPlug plug =
 ("Table: "++showPhpStr (sqlname plug))
 :
 indent 4
   (blockParenthesize "[" "]" "," (map showField (plugFields plug)))

showField :: SqlField -> [String]
showField fld = ["{" ++ (if fldnull fld then "+" else "-") ++ "NUL," ++ (if flduniq fld then "+" else "-") ++ "UNQ} " ++
                 showPhpStr (fldname fld) ++ ":"++showADL (target $ fldexpr fld)]
