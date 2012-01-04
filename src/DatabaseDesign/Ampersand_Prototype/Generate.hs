module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where


import DatabaseDesign.Ampersand_Prototype.CoreImporter 
import DatabaseDesign.Ampersand.Fspec.Fspec (lookupCpt)
import Prelude hiding (writeFile,readFile,getContents)
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Directory               
import DatabaseDesign.Ampersand_Prototype.Version 
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (getGeneralizations)

fatal :: Int -> String -> a
fatal = fatalMsg "Generate"

customCssPath :: String
customCssPath = "css/Custom.css"

generateAll :: Fspc -> Options -> IO ()
generateAll fSpec opts =
 do { writePrototypeFile "Generics.php" $ generateInterfaces fSpec opts
    
    ; case customCssFile opts of
        Just customCssFilePath ->
         do { customCssContents <- (readFile customCssFilePath `catch` error ("ERROR: Cannot open custom css file '" ++ customCssFilePath ++ "'"))
            ; writePrototypeFile customCssPath customCssContents
            }
        Nothing ->
         do { customExists <- doesFileExist (combine (dirPrototype opts) customCssPath)
            ; if customExists
              then verboseLn opts $ "  File " ++ customCssPath ++ " already exists."
              else do { verboseLn opts $ "  File " ++ customCssPath ++ " does not exist, creating default for Oblomilan style."
                      ; writePrototypeFile customCssPath "@import url(\"Oblomilan.css\");"
                      }
            }
            
    ; when (development opts) $ 
       do { verboseLn opts "Generated tables\n"
          ; verboseLn opts $ unlines $ concatMap showPlug $ [ plug | InternalPlug plug <- plugInfos fSpec]
          }
    }
  where
    writePrototypeFile fname content =
     do { verboseLn opts ("  Generating "++fname)
        --; verboseLn opts $ content
        ; writeFile (combine (dirPrototype opts) fname) content
        }
 
generateInterfaces fSpec opts = genPhp "Generate.hs" "Generics.php" $
  [ "$dbName = "++showPhpStr (dbName opts)++";"
  , ""
  , "$isDev = "++showPhpBool (development opts)++";"
  , ""
  , "$autoRefreshInterval = "++showPhpStr (show $ fromMaybe 0 $ autoRefresh opts)++";"
  , ""
  , "$relationTableInfo ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize "(" ")" ","
         [ [showPhpStr rnm++" => array ('srcConcept' => "++(showPhpStr $ name $ source rel)++", 'tgtConcept' => "++(showPhpStr $ name $ target rel)++", table => "++showPhpStr table++", srcCol => "++showPhpStr srcCol++", tgtCol => "++showPhpStr tgtCol++")"] 
         | rel@(Rel {relnm = rnm}) <- mors fSpec
         , let (table,srcCol,tgtCol) = case getRelationTableInfo fSpec rel of
                                         Just tableInfo -> tableInfo
                                         Nothing        -> fatal 61 $ "No table info for relation "++ show rel
         ]) ++
  [ ""
  , "$conceptTableInfo ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize "(" ")" ","
         [ [ (showPhpStr $ name c)++" => array "] ++
             (indent 4 $ blockParenthesize "(" ")" ","
               [ [ "array ( 'table' => "++showPhpStr (name table)
                 , "      , 'cols' => array ("++ intercalate ", " (map (showPhpStr . fldname) conceptFields) ++")" 
                 , "      )"]
               -- get the concept tables (pairs of table and column names) for the concept and its generalizations and group them per table name
               | (table,conceptFields) <- groupOnTable . concatMap (lookupCpt fSpec) $ c : getGeneralizations fSpec c ])
         | c <- concs fSpec]) ++        
  [ ""
  , "$tableColumnInfo ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize "(" ")" ","
         [ [(showPhpStr $ name plug)++" =>"
           , "  array" ] ++
                (indent 4 $ blockParenthesize "(" ")" ","
                [ [ (showPhpStr $ fldname field) ++ " => array ( 'unique' => "++showPhpBool (flduniq field)++
                                                              ", 'null' => " ++ showPhpBool (fldnull field)++")" ] 
                | field <- getPlugFields plug]) 
         | InternalPlug plug <- plugInfos fSpec]) ++
  [ ""
  , "$allInterfaceObjects ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize  "(" ")" "," $ 
         map (generateInterface fSpec opts) allInterfaces) ++
  [ ""
  , "$allRulesSql ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize  "(" ")" "," $
         [ [ showPhpStr (rrnm rule) ++ " =>"
           , "  array ( 'name' => "++showPhpStr (rrnm rule)
           , "        , 'ruleAdl' => "++showPhpStr (show $ rrexp rule)
           , "        , 'origin' => "++showPhpStr (show $ rrfps rule)
           , "        , 'meaning' => "++showPhpStr (showMeaning rule)
           , "        , 'message' => "++showPhpStr (showMessage rule)
           , "          // normalized complement (violations): "++ show violationsExpr
           , "        , 'violationsSQL' => '"++ (fromMaybe (fatal 100 $ "No sql generated for "++showHS opts "" violationsExpr) $
                                                  (selectExpr fSpec 26 "src" "tgt" $ violationsExpr))++"'" 
           ] ++
           (if development opts then -- with --dev, also generate sql for the rule itself (without negation) so it can be tested with
                                     -- php/Database.php?testRule=RULENAME
           [ "        , 'contentsSQL' => '"++ case conjNF . rrexp $ rule of
                                                EIsc [] -> "/* EIsc [], not handled by selectExpr */'"
                                                ECps [] -> "/* EIsc [], not handled by selectExpr */'"
                                                contentsExpr -> fromMaybe ("/*ERROR: no sql generated for "++escapePhpStr (showHS opts "" contentsExpr) ++"*/")
                                                                  -- no fatal here, we don't want --dev to break generation
                                                                  (selectExpr fSpec 26 "src" "tgt" $ contentsExpr)++"'"] 
              else []) ++
           [ "        )" ]
         | rule <- vrules fSpec ++ grules fSpec, let violationsExpr = conjNF . ECpl . rrexp $ rule ]) ++
  [ ""
  , "$invariantRuleNames = array ("++ intercalate ", " (map (showPhpStr . name) invRules) ++");"
  , ""
  , "$allRoles ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize  "(" ")" "," $
         [ [ "array ( 'name' => "++showPhpStr role
           , "      , 'ruleNames' => array ("++ intercalate ", " (map (showPhpStr . name) rules) ++")"
           , "      )" ]
         | (role,rules) <- rulesPerRole ])
       
 where allInterfaces = interfaceS fSpec ++ interfaceG fSpec
       showMeaning rule = maybe "" aMarkup2String (meaning (language opts) rule)
       showMessage rule = case [ markup | markup <- rrmsg rule, amLang markup == language opts ] of
                            []    -> ""
                            markup:_ -> aMarkup2String markup
       rulesPerRole = [ (role, [rule | (rl, rule) <- fRoleRuls fSpec, rl == role ]) | role <- nub $ map fst $ fRoleRuls fSpec ]
       processRuleNames = nub $ concatMap snd rulesPerRole
       invRules = grules fSpec ++ filter (`notElem` processRuleNames) (vrules fSpec)
       
       groupOnTable :: [(PlugSQL,SqlField)] -> [(PlugSQL,[SqlField])]       
       groupOnTable tablesFields = [(t,fs) | (t:_, fs) <- map unzip . groupBy ((==) `on` fst) $ sort tablesFields ]

generateInterface fSpec opts interface =
  [ "// Top-level interface "++name interface ++":"
  , showPhpStr (name interface) ++" => " ] ++
  genInterfaceObjects fSpec opts (ifcParams interface) 1 (ifcObj interface) 
  
-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> Options -> [Relation] -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec opts editableRels depth object = indent (depth*2) $
  [ "array ( 'name' => "++showPhpStr (name object)
  , "      // adl expression: "++showPhpStr (show normalizedInterfaceExp)  -- escape for the pathological case that one of the names in the relation contains a newline
  ] ++ case objctx object of
         ERel r        | isEditable r -> [ "      , 'relation' => "++showPhpStr (name r) -- only support editing on user-specified relations (no expressions, and no I or V)
                                         , "      , 'relationIsFlipped' => false" 
                                         , "      , 'min' => "++ if isTot r then "'One'" else "'Zero'"
                                         , "      , 'max' => "++ if isUni r then "'One'" else "'Many'"
                                         ]
         EFlp (ERel r) | isEditable r -> [ "      , 'relation' => "++showPhpStr (name r) -- and on flipped versions of those relations
                                         , "      , 'relationIsFlipped' => true" 
                                         , "      , 'min' => "++ if isSur r then "'One'" else "'Zero'"
                                         , "      , 'max' => "++ if isInj r then "'One'" else "'Many'"
                                         ]          
         _             -> [ "      , 'relation' => ''" 
                          , "      , 'relationIsFlipped' => ''" 
                          ]          
  ++     
  [ "      , 'srcConcept' => "++showPhpStr (show (source normalizedInterfaceExp))
  , "      , 'tgtConcept' => "++showPhpStr (show (target normalizedInterfaceExp))
  , "      , 'expressionSQL' => '" ++ (fromMaybe (fatal 151 $ "No sql generated for "++showHS opts "" normalizedInterfaceExp) $
                                        selectExpr fSpec 25 "src" "tgt" normalizedInterfaceExp) ++ "'"                                                  
  , "      , 'subInterfaces' =>"
  , "          array"
  ] ++ (indent 12 $ blockParenthesize "(" ")" "," $ map (genInterfaceObjects fSpec opts editableRels $ depth + 1) $ objats object) ++
  [ "      )"
  ]
  where isEditable rel = rel `elem` editableRels
        normalizedInterfaceExp = conjNF $ objctx object

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++prototypeVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
 
-- utils

blockParenthesize :: String -> String -> String -> [[String]] -> [String]
blockParenthesize open close sep [] = [open ++ close]
blockParenthesize open close sep  liness = concat [ zipWith (++) (pre:repeat "  ") (lines::[String]) 
                                                  | (pre, lines) <- zip ((open++" "): repeat (sep++" ")) liness ] ++ [close]
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
addToLastLine str lines = init lines ++ [last lines ++ str] 
  
toPhp str = map replace str
 where replace ' ' = '_'
       replace c   = c
 
showPhpStr str = "'"++escapePhpStr str++"'"

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr cs = concat [fromMaybe [c] $ lookup c [('\'', "\\'"),('\\', "\\\\"),('\n', "\\n")] | c<-cs ] -- escape '   \   \n
-- todo: escape everything else (unicode, etc)

showPhpBool b = if b then "true" else "false"
  
indent n lines = [ replicate n ' ' ++ line | line <- lines ]

-- FSpec utils


showPlug plug =  ["Table: '"++sqlname plug++"'"] ++ 
                    (indent 4 $ blockParenthesize "[" "]" "," $ map showField $ getPlugFields plug)

-- TODO: maybe this one exists already
getPlugFields (TblSQL  {fields = flds}) = flds
getPlugFields BinSQL  { columns = (fld1,fld2)} = [fld1, fld2]
getPlugFields ScalarSQL { sqlColumn = fld} = [fld]

showField fld = ["{" ++ (if fldnull fld then "+" else "-") ++ "NUL," ++ (if flduniq fld then "+" else "-") ++ "UNQ} " ++ 
                 "'"++fldname fld ++ "':"++show (target $ fldexpr fld)]

