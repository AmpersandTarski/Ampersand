module Database.Design.Ampersand.Prototype.Generate 
  (generateGenerics
  , generateDBstructQueries, generateAllDefPopQueries
  )
where

import Database.Design.Ampersand
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Prelude hiding (writeFile,readFile,getContents,exp)
import Data.Function
import Data.List
import Data.Maybe
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.Prototype.ProtoUtil
import Database.Design.Ampersand.Basics (fatal)
import Database.Design.Ampersand.Prototype.PHP (getTableName, signalTableSpec)

-- Generate Generics.php
generateGenerics :: FSpec -> IO ()
generateGenerics fSpec =
 do { let filecontent = genPhp "Generate.hs" "Generics.php" genericsPhpContent
--  ; verboseLn (getOpts fSpec) filecontent
    ; writePrototypeFile fSpec "Generics.php" filecontent
    }
 where    
    genericsPhpContent :: [String]
    genericsPhpContent =
      intercalate [""]
        [ generateConstants fSpec
        , generateTableInfos fSpec
        --, generateRules fSpec
        , generateConjuncts fSpec
        , generateRoles fSpec
        , generateViews fSpec
        , generateInterfaces fSpec
        ]
        
generateConstants :: FSpec -> [String]
generateConstants fSpec =
  [ "$isDev = "++showPhpBool (development opts)++";"
  ]
  where opts = getOpts fSpec
  
generateDBstructQueries :: FSpec -> [String]
generateDBstructQueries fSpec = theSQLstatements
  where
    theSQLstatements :: [String]
    theSQLstatements =
       createTableStatements ++
       [ "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
       ]
    createTableStatements :: [String]
    createTableStatements = 
      map concat
      [ [ "CREATE TABLE "++ show "__SessionTimeout__"
        , "   ( "++show "SESSION"++" VARCHAR(255) UNIQUE NOT NULL"
        , "   , "++show "lastAccess"++" BIGINT NOT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      , [ "CREATE TABLE "++ show "__History__"
        , "   ( "++show "Seconds"++" VARCHAR(255) DEFAULT NULL"
        , "   , "++show "Date"++" VARCHAR(255) DEFAULT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      , [ "INSERT INTO "++show "__History__"++" ("++show "Seconds"++","++show "Date"++")"
        , "   VALUES (UNIX_TIMESTAMP(NOW(6)), NOW(6))"
        ]
      , [ "CREATE TABLE "++ show "__all_signals__"
        , "   ( "++show "conjId"++" VARCHAR(255) NOT NULL"
        , "   , "++show "src"++" VARCHAR(255) NOT NULL"
        , "   , "++show "tgt"++" VARCHAR(255) NOT NULL"
        , "   ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
        ]
      ] ++ 
      ( concatMap tableSpec2Queries [(plug2TableSpec p) | InternalPlug p <- plugInfos fSpec])
     
      where 
        tableSpec2Queries :: TableSpecNew -> [String]
        tableSpec2Queries ts = 
         -- [ "DROP TABLE "++show (tsName ts)] ++
          [ concat $  
                   ( tsCmnt ts ++ 
                     ["CREATE TABLE "++show (tsName ts)] 
                     ++ (map (uncurry (++)) 
                            (zip (" ( ": repeat " , " ) 
                                 (  map fld2sql (tsflds ts)
                                 ++ tsKey ts
                                 )
                            )
                        )
                     ++ [" , "++show "ts_insertupdate"++" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]
                     ++ [" ) ENGINE=InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"]
                   )
          ]
        fld2sql :: SqlAttribute -> String
        fld2sql = attributeSpec2Str . fld2AttributeSpec

data TableSpecNew 
  = TableSpec { tsCmnt :: [String]
              , tsName :: String
              , tsflds :: [SqlAttribute]
              , tsKey ::  [String]
              , tsEngn :: String
              }
data AttributeSpecNew
  = AttributeSpec { fsname :: String
                  , fstype :: String
                  , fsauto :: Bool
                  }
fld2AttributeSpec ::SqlAttribute -> AttributeSpecNew
fld2AttributeSpec att 
  = AttributeSpec { fsname = name att
                  , fstype = showSQL (attType att)
                  , fsauto = fldauto att 
                  }
attributeSpec2Str :: AttributeSpecNew -> String
attributeSpec2Str fs = intercalate " "
                        [ show (fsname fs)
                        , fstype fs
                        , if fsauto fs then " AUTO_INCREMENT" else " DEFAULT NULL"
                        ] 
plug2TableSpec :: PlugSQL -> TableSpecNew
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = commentBlockSQL (["Plug "++name plug,"","attributes:"]++map (\x->showADL (attExpr x)++"  "++(show.properties.attExpr) x) (plugAttributes plug))
     , tsName = name plug
     , tsflds = plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> []
                 (_,    primFld) ->
                      case attUse primFld of
                         TableKey isPrim _ -> [ (if isPrim then "PRIMARY " else "")
                                                ++ "KEY ("++(show . attName) primFld++")"
                                        ]
                         ForeignKey c  -> fatal 195 ("ForeignKey "++name c++"not expected here!")
                         PlainAttr     -> []
     , tsEngn = "InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN"
     }

commentBlockSQL :: [String] -> [String]
commentBlockSQL xs = 
   map (\cmmnt -> "/* "++cmmnt++" */") $ hbar ++ xs ++ hbar
  where hbar = [replicate (maximum . map length $ xs) '-']
  
generateAllDefPopQueries :: FSpec -> [String]
generateAllDefPopQueries fSpec = theSQLstatements
  where
    theSQLstatements
      = fillSignalTable (initialConjunctSignals fSpec) ++
        populateTablesWithPops
        

    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [String]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [concat $ 
            [ "INSERT INTO "++show (getTableName signalTableSpec)
            , "   ("++intercalate ", " (map show ["conjId","src","tgt"])++")"
            ] ++ lines 
              ( "VALUES " ++ intercalate "\n     , " 
                  [ "(" ++intercalate ", " (map showAsValue [rc_id conj, showValPHP (apLeft p), showValPHP (apRight p)])++ ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
              )
       ]
    populateTablesWithPops :: [String]
    populateTablesWithPops =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [String]
        populatePlug plug 
          = case tableContents fSpec plug of
             []  -> []
             tblRecords 
                 -> [concat $ 
                       [ "INSERT INTO "++show (name plug)
                       , "   ("++intercalate ", " (map (show . attName) (plugAttributes plug))++") "
                       ] ++ lines
                         ( "VALUES " ++ intercalate "\n     , " 
                          [ "(" ++valuechain md++ ")" | md<-tblRecords]
                         )
                    ]
         where
           valuechain record 
             = intercalate ", " 
                 [case att of 
                    Nothing -> "NULL"
                    Just val -> showValPHP val
                 | att <- record ]

generateTableInfos :: FSpec -> [String]
generateTableInfos fSpec =
  [ "$allRelations ="
  , "  array" ] ++
  addToLastLine ";"
    (indent 4 (blockParenthesize "(" ")" ","
         [ [showPhpStr (showHSName decl)++" => array ( 'name'       => "++showPhpStr (name decl)
                                                 ++ ", 'srcConcept' => "++showPhpStr (name (source decl))
                                                 ++ ", 'tgtConcept' => "++showPhpStr (name (target decl))
                                                 ++ ", 'table'      => "++showPhpStr (name table)
                                                 ++ ", 'srcCol'     => "++showPhpStr (attName srcCol)
                                                 ++ ", 'tgtCol'     => "++showPhpStr (attName tgtCol)
                                                 ++ ", 'affectedInvConjunctIds' => array ("++ intercalate ", " (map (showPhpStr . rc_id) affInvConjs) ++")"
                                                 ++ ", 'affectedSigConjunctIds' => array ("++ intercalate ", " (map (showPhpStr . rc_id) affSigConjs) ++")"
                                                 ++ ")"]
         | decl@Sgn{} <- allDecls fSpec  -- SJ 13 nov 2013: changed to generate all relations instead of just the ones used.
         , let (table,srcCol,tgtCol) = getDeclarationTableInfo fSpec decl
         , let affConjs = case lookup decl $ allConjsPerDecl fSpec of
                 Nothing    -> []
                 Just conjs -> conjs
               affInvConjs = filterFrontEndInvConjuncts affConjs
               affSigConjs = filterFrontEndSigConjuncts affConjs 
         ])) ++
  [ ""
  , "$allConcepts = array"
  ] ++
  addToLastLine ";"
    (indent 2 $
       blockParenthesize "(" ")" ","
         [ [ (showPhpStr.name) c++" => array"] ++
           (indent 2 $
              [ "( 'concept' => "++ (showPhpStr.name) c
              , ", 'affectedInvConjunctIds' => array ("++ intercalate ", " (map (showPhpStr . rc_id) affInvConjs) ++")"
              , ", 'affectedSigConjunctIds' => array ("++ intercalate ", " (map (showPhpStr . rc_id) affSigConjs) ++")"
              , ", 'conceptTables' => array" ] ++
              (indent 3
                (blockParenthesize "(" ")" ","
                  [ [ "array ( 'table' => "++(showPhpStr.name) table ++
                            ", 'cols' => array ("++ intercalate ", " (map (showPhpStr . attName) conceptAttributes) ++")" ++
                           " )"
                    ]
                  -- get the concept tables (pairs of table and column names) for the concept and its generalizations and group them per table name
                  | (table,conceptAttributes) <- groupOnTable . concatMap (lookupCpt fSpec) $ c : largerConcepts (vgens fSpec) c
                  ])) ++
              [ ", 'type' => '"++(show . cptTType fSpec) c++"'" ]++
              [ ", 'specializations' => array ("++intercalate ", " (map (showPhpStr . name)(smallerConcepts (vgens fSpec) c))++")"]++
              [ ", 'defaultViewId'   => "++(showPhpStr . vdlbl $ dfltV)
              | Just dfltV <- [getDefaultViewForConcept fSpec c]
              ]++
              [ ")" ]
           )
         | c <- concs fSpec
         , let 
               affConjs = nub [ conj  
                              | Just conjs<-[lookup c (allConjsPerConcept fSpec)]
                              , conj<-conjs
                              ]
               affInvConjs = filterFrontEndInvConjuncts affConjs
               affSigConjs = filterFrontEndSigConjuncts affConjs
         ]
    ) ++
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
                [ [ (showPhpStr.attName) attribute++ " => array ( 'concept' => "++(showPhpStr.name.target.attExpr) attribute++
                                                               ", 'unique' => " ++(showPhpBool.attUniq)            attribute++
                                                               ", 'null' => "  ++ (showPhpBool.attNull)            attribute++
                                                               ")"
                  ]
                | attribute <- plugAttributes plug]
              )
         | InternalPlug plug <- plugInfos fSpec
         ]
     )  )
 where groupOnTable :: [(PlugSQL,SqlAttribute)] -> [(PlugSQL,[SqlAttribute])]
       groupOnTable tablesAttributes = [(t,fs) | (t:_, fs) <- map unzip . groupBy ((==) `on` fst) $ sortBy (\(x,_) (y,_) -> name x `compare` name y) tablesAttributes ]

generateConjuncts :: FSpec -> [String]
generateConjuncts fSpec =
  [ "$allConjuncts ="
  , "  array"
  ] ++
  addToLastLine ";"
     (indent 4
       (blockParenthesize  "(" ")" ","
         [ [ showPhpStr (rc_id conj) ++ " =>  /* conj = " ++ showADL rExpr ++ " */"
           , "  array ( 'signalRuleNames' => array ("++ intercalate ", " signalRuleNames ++")"
           , "        , 'invariantRuleNames' => array ("++ intercalate ", " invRuleNames ++")"
                      -- the name of the rules that gave rise to this conjunct
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
           [ "        , 'violationsSQL' => "++ showPhpStr (prettySQLQuery fSpec 36 violationsExpr)
           , "        )"
           ]
         | conj<-vconjs fSpec
         , let rExpr=rc_conjunct conj
         , let signalRuleNames = [ showPhpStr $ name r | r <- rc_orgRules conj, isFrontEndSignal r ] 
         , let invRuleNames    = [ showPhpStr $ name r | r <- rc_orgRules conj, isFrontEndInvariant  r ]
         , let violExpr = notCpl rExpr
         , let violationsExpr = conjNF (getOpts fSpec) violExpr
         ]
     ) )

-- Because the signal/invariant condition appears both in generateConjuncts and generateInterface, we use
-- two abstractions to guarantee the same implementation.
isFrontEndInvariant :: Rule -> Bool
isFrontEndInvariant r = not (isSignal r) && not (ruleIsInvariantUniOrInj r)

isFrontEndSignal :: Rule -> Bool
isFrontEndSignal r = isSignal r

-- NOTE that results from filterFrontEndInvConjuncts and filterFrontEndSigConjuncts may overlap (conjunct appearing in both invariants and signals)
-- and that because of extra condition in isFrontEndInvariant (not (ruleIsInvariantUniOrInj r)), some parameter conjuncts may not be returned
-- as either inv or sig conjuncts (i.e. conjuncts that appear only in uni or inj rules) 
filterFrontEndInvConjuncts :: [Conjunct] -> [Conjunct]
filterFrontEndInvConjuncts conjs = filter (\c -> any isFrontEndInvariant $ rc_orgRules c) conjs

filterFrontEndSigConjuncts :: [Conjunct] -> [Conjunct]
filterFrontEndSigConjuncts conjs = filter (\c -> any isFrontEndSignal $ rc_orgRules c) conjs
  
generateRoles :: FSpec -> [String]
generateRoles fSpec =
  concatMap showRoles [False,True]
  where showRoles isService =
          [ if isService then "$allServices =" else "$allRoles ="
          , "  array"
          ] ++
          addToLastLine ";"
            (indent 4
              (blockParenthesize  "(" ")" ","
                 [ [ "array ( 'id' => "++show i 
                   , "      , 'name' => "++showPhpStr (name role)
                   , "      , 'ruleNames'  => array ("++ intercalate ", " ((map (showPhpStr . name . snd) . filter (maintainedByRole role) . fRoleRuls) fSpec) ++")"
                   , "      , 'interfaces' => array ("++ intercalate ", " (map (showPhpStr . name) ((roleInterfaces fSpec) role)) ++")"
                   , "      )" ]
                 | (i,role) <- zip [1::Int ..] (filter serviceOrRole $ fRoles fSpec) ]
            ) )
            where
             serviceOrRole Role{} = not isService
             serviceOrRole Service{} = isService 
        maintainedByRole role (role',_) = role == role'

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
           , "        , 'isDefault' => "++showPhpBool isDefault
           , "        , 'segments' =>" -- a labeled list of sql queries for the view expressions
           , "            array"
           ] ++
           indent 14 (blockParenthesize "(" ")" "," (map genViewSeg viewSegs)) ++
           [ "        )" ]
         | Vd _ label cpt isDefault  _ viewSegs <- [ v | c<-conceptsFromSpecificToGeneric, v <- vviews fSpec, vdcpt v==c ] --sort from spec to gen
         ]
    ) )
 where genViewSeg (ViewText i str) = [ "array ( 'segmentType' => 'Text'"
                                     , "      , 'label' => " ++ lab i
                                     , "      , 'Text' => " ++ showPhpStr str
                                     , "      )" ]
       genViewSeg (ViewHtml i str) = [ "array ( 'segmentType' => 'Html'"
                                     , "      , 'label' => " ++ lab i
                                     , "      , 'Html' => " ++ showPhpStr str
                                     , "      )" ]
       genViewSeg (ViewExp _ objDef) = [ "array ( 'segmentType' => 'Exp'"
                                     , "      , 'label' => " ++ showPhpStr (objnm objDef) ++ " // view exp: " ++ escapePhpStr (showADL $ objctx objDef) -- note: unlabeled exps are labeled by (index + 1)
                                     , "      , 'expSQL' =>"
                                     , "          " ++ showPhpStr (prettySQLQuery fSpec 33 (objctx objDef))
                                     , "      )"
                                     ]
       conceptsFromSpecificToGeneric = concatMap reverse (kernels fSpec)
       lab i = showPhpStr ("seg_"++show i)

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
  let roleStr = case ifcRoles interface of []    -> " for all roles"
                                           rolez -> " for role"++ (if length rolez == 1 then "" else "s") ++" " ++ intercalate ", " (map name (ifcRoles interface))
      arrayKey = escapeIdentifier $ name interface
  in  ["// Top-level interface " ++ name interface ++ roleStr  ++ ":"
      , showPhpStr arrayKey ++ " => " 
      ] ++
  indent 2 (genInterfaceObjects fSpec (ifcParams interface) (Just $ topLevelFields) 1 (ifcObj interface))
  where topLevelFields = -- for the top-level interface object we add the following fields (saves us from adding an extra interface node to the php data structure)
          [ "      , 'interfaceRoles' => array (" ++ intercalate ", " (map (showPhpStr.name) $ ifcRoles interface) ++")" 
          , "      , 'invConjunctIds' => array ("++intercalate ", " (map (showPhpStr . rc_id) $ invConjuncts) ++")"
          , "      , 'sigConjunctIds' => array ("++intercalate ", " (map (showPhpStr . rc_id) $ sigConjuncts) ++")"
          , "      , 'editableConcepts' => array ("++ intercalate ", " (map (showPhpStr . name) ((editableConcepts fSpec) interface)) ++")"
          ]
        invConjuncts = [ c | c <- ifcControls interface, any isFrontEndInvariant $ rc_orgRules c ] -- NOTE: these two
        sigConjuncts = [ c | c <- ifcControls interface, any isFrontEndSignal    $ rc_orgRules c ] --       may overlap

genInterfaceObjects :: FSpec -> [Declaration] -> Maybe [String] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec editableRels mTopLevelFields depth object =
     [ "array ( 'name'  => "++ showPhpStr (name object)
     , "      , 'id'    => " ++ show (escapeIdentifier $ name object) -- only for new front-end
     , "      , 'label' => " ++ showPhpStr (name object)              -- only for new front-end
     ]
  ++ maybe [] (\viewId -> ["      , 'viewId' => " ++ showPhpStr viewId]) mViewId 
  ++ (if verboseP (getOpts fSpec)  -- previously, this included the condition        objctx object /= normalizedInterfaceExp
      then    ["      // Normalization steps:"]
           ++ ["      // "++ls | ls<-(showPrf showADL.cfProof (getOpts fSpec).objctx) object] -- let's hope that none of the names in the relation contains a newline
           ++ ["      //"]
      else    []
     )
  ++ ["      // Normalized interface expression (== expressionSQL): "++escapePhpStr (showADL normalizedInterfaceExp) ]
  ++ ["      // normalizedInterfaceExp = " ++ show normalizedInterfaceExp | development (getOpts fSpec) ]
             -- escape for the pathological case that one of the names in the relation contains a newline
  ++ fromMaybe [] mTopLevelFields -- declare extra fields if this is a top level interface object
  ++ case mEditableDecl of
           Just (decl, isFlipped) ->
             [ "      , 'relation' => "++showPhpStr (showHSName decl) ++ " // this interface represents a declared relation"
             , "      , 'relationIsEditable' => "++ showPhpBool (decl `elem` editableRels) 
             , "      , 'relationIsFlipped' => "++showPhpBool isFlipped ] ++
             if isFlipped 
             then [ "      , 'min' => "++ if isSur decl then "'One'" else "'Zero'"
                  , "      , 'max' => "++ if isInj decl then "'One'" else "'Many'" ]
             else [ "      , 'min' => "++ if isTot decl then "'One'" else "'Zero'" 
                  , "      , 'max' => "++ if isUni decl then "'One'" else "'Many'" ] 
           Nothing ->
             [ "      , 'relation' => '' // this interface expression does not represent a declared relation"
             , "      , 'relationIsFlipped' => ''"
             ] 
  ++ [ "      , 'srcConcept'    => "++showPhpStr (name srcConcept) -- NOTE: these are src and tgt of the expression, not necessarily the relation (if there is one), 
     , "      , 'tgtConcept'    => "++showPhpStr (name tgtConcept) -- which may be flipped.
     , "      , 'crudC'         => "++ (showPhpMaybeBool . crudC . objcrud $ object)
     , "      , 'crudR'         => "++ (showPhpMaybeBool . crudR . objcrud $ object)
     , "      , 'crudU'         => "++ (showPhpMaybeBool . crudU . objcrud $ object)
     , "      , 'crudD'         => "++ (showPhpMaybeBool . crudD . objcrud $ object)
     , "      , 'exprIsUni'     => " ++ showPhpBool (isUni normalizedInterfaceExp) -- We could encode these by creating min/max also for non-editable,
     , "      , 'exprIsTot'     => " ++ showPhpBool (isTot normalizedInterfaceExp) -- but this is more in line with the new front-end templates.
     , "      , 'exprIsProp'    => " ++ showPhpBool (isProp normalizedInterfaceExp) 
     , "      , 'exprIsIdent'   => " ++ showPhpBool (isIdent normalizedInterfaceExp) 
     , "      , 'expressionSQL' => " ++ showPhpStr (prettySQLQuery fSpec (22+14*depth) normalizedInterfaceExp)
     ] 
  ++ generateMSubInterface fSpec editableRels depth (objmsub object)
  ++ [ "      )"
     ]
 where mViewId = case objmView object of
                   Just vId -> Just vId
                   Nothing  -> case getDefaultViewForConcept fSpec tgtConcept of
                                 Just Vd{vdlbl=vId} -> Just vId
                                 Nothing            -> Nothing
       normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
       (srcConcept, tgtConcept, mEditableDecl) =
         case getExpressionRelation normalizedInterfaceExp of
           Just (src, decl, tgt, isFlipped) ->
             (src, tgt, Just (decl, isFlipped))
           Nothing -> (source normalizedInterfaceExp, target normalizedInterfaceExp, Nothing) -- fall back to typechecker type

generateMSubInterface :: FSpec -> [Declaration] -> Int -> Maybe SubInterface -> [String]
generateMSubInterface fSpec editableRels depth subIntf =
  case subIntf of
    Nothing -> [ "      // No subinterfaces" ]
    Just (InterfaceRef isLink nm _)
            -> [ "      // InterfaceRef"
         --      , "      , 'refSubInterface' => " ++ showPhpStr nm
               , "      , 'refSubInterfaceId' => " ++ showPhpStr (escapeIdentifier nm) -- only for new front-end
               , "      , 'isLinkTo' => "++ show isLink
               ]
    Just (Box _ cl objects)
            -> [ "      // Box" ++ (maybe "" (\c -> "<"++c++">") cl)
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
showAsValue :: String -> String
showAsValue str = "'"++f str++"'"
  where f :: String -> String
        f str'= 
          case str' of
            []        -> []
            ('\'':cs) -> "\\\'"++ f cs  --This is required to ensure that the result of showValue will be a proper singlequoted string.
            (c:cs)    -> c : f cs
