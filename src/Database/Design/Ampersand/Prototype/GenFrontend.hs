{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Prelude hiding (putStrLn,readFile)
import Control.Monad
import Data.Data
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.StringTemplate
import Text.StringTemplate.GenericStandard () -- only import instances
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
import Database.Design.Ampersand.Misc
import qualified Database.Design.Ampersand.Misc.Options as Opts
import Database.Design.Ampersand.Prototype.ProtoUtil

fatal :: Int -> String -> a
fatal = fatalMsg "GenFrontend"

{- TODO
- Converse navInterfaces?
- Be more consistent with record selectors/pattern matching
- HStringTemplate hangs on uninitialized vars in anonymous template? (maybe only fields?)
- isRoot is a bit dodgy (maybe make dependency on ONE and SESSIONS a bit more apparent)
- Escape everything
- Keeping templates as statics requires that the static files are written before templates are used.
  Maybe we should keep them as cabal data-files instead. (file extensions and directory structure are predictable)


PROBLEM: Interface refs are not handled correctly at the moment. Consider:

INTERFACE MyInterface 
  BOX [ ref : rel[$a*$b]
        INTERFACE RefInterface
      ]
      
INTERFACE RefInterface relRef[$b*$c]
  BOX [ .. : ..[$c*$d]
      ]

This is basically mapped onto:

INTERFACE MyInterface 
  BOX [ ref : (rel;relRef)[$a*$c]
        BOX [ .. : ..[$c*$d]
            ]
      ]

When do we allow this to be editable? If relRef is I, we can use editability of rel, but relRef may be anything
(and does not even have to be an endorelation [$b*$b].)

Until this is sorted out, we disallow editing on interface refs
-}

-- Directories that will be copied to the prototype, if present in $adlSourceDir/includes/
-- Format: (directory name in include/, path in prototype directory)

data Include = Include { _fileOrDir :: FileOrDir, includeSrc :: String, _includeTgt :: String } deriving Show

data FileOrDir = File | Dir deriving Show

allowedIncludeSubDirs :: [Include]
allowedIncludeSubDirs = [ Include Dir  "templates"         "templates"
                        , Include Dir  "css"               "app/css"
                        , Include Dir  "js"                "app/js"
                        , Include Dir  "images"            "app/images"
                        , Include Dir  "extensions"        "extensions"
                        , Include File "localSettings.php" "localSettings.php"
                        ]

getTemplateDir :: FSpec -> String
getTemplateDir fSpec = Opts.dirPrototype (getOpts fSpec) </> 
                         "templates"

-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
-- NOTE: due to a bug in HStringTemplate's checkTemplateDeep, non-existent attribute names on
--       composite attributes in anonymous templates will hang the generator :-(
--       Eg.  "$subObjects:{subObj| .. $subObj.nonExistentField$ .. }$"

doGenFrontend :: FSpec -> IO ()
doGenFrontend fSpec =
 do { putStrLn "Generating new frontend.." 
    ; copyIncludes fSpec
    ; feInterfaces <- buildInterfaces fSpec
    ; genView_Interfaces fSpec feInterfaces
    ; genController_Interfaces fSpec feInterfaces
    ; genRouteProvider fSpec feInterfaces
    }

copyIncludes :: FSpec -> IO ()
copyIncludes fSpec =
 do { let adlSourceDir = takeDirectory $ fileName (getOpts fSpec)
          includeDir = adlSourceDir </> "include"
          protoDir = Opts.dirPrototype (getOpts fSpec)
    ; includeDirExists <- doesDirectoryExist $ includeDir
    ; if includeDirExists then
       do { putStrLn $ "Copying user includes from " ++ includeDir 
          ; includeDirContents <- fmap (map (includeDir </>)) $ getProperDirectoryContents includeDir
          
          ; let absIncludes = [ Include fOrD absSd (protoDir </> tgtPth)
                              | Include fOrD srcPth tgtPth <- allowedIncludeSubDirs
                              , let absSd = includeDir </> srcPth
                              , absSd `elem` includeDirContents
                              ]
                              
          ; sequence_ [ do { putStrLn $ "  Copying " ++ toFOrDStr fOrD ++ " " ++ srcPth ++ "\n    -> " ++ tgtPth
                           ; case fOrD of
                               File -> copyDeepFile srcPth tgtPth
                               Dir  -> copyDirRecursively srcPth tgtPth
                           }
                      | Include fOrD srcPth tgtPth <- absIncludes
                      ]
                      
          ; let ignoredPaths = includeDirContents \\ map includeSrc absIncludes
          ; when (not $ null ignoredPaths) $
             do { putStrLn $ "\nWARNING: only the following include/ paths are allowed:\n  " ++ show (map includeSrc allowedIncludeSubDirs) ++ "\n"
                ; mapM_ (\d -> putStrLn $ "  Ignored " ++ d) $ ignoredPaths
                }
          }
      else
        putStrLn $ "No user includes (there is no directory " ++ includeDir ++ ")"
    } 
  where toFOrDStr File = "file"
        toFOrDStr Dir  = "directory"
        
------ Build intermediate data structure

-- NOTE: _ disables 'not used' warning for fields
data FEInterface = FEInterface { ifcName :: String, ifcIdent :: String -- _ifcIdent is a version of ifcName that can be used as (part of) an identifier or file name
                               , _ifcMClass :: Maybe String 
                               , _ifcExp :: Expression, _ifcSource :: A_Concept, _ifcTarget :: A_Concept
                               , _ifcRoles :: [Role], _ifcEditableRels :: [Expression], _ifcObj :: FEObject }

data FEObject = FEObject { objName :: String
                         , objExp :: Expression, objSource :: A_Concept, objTarget :: A_Concept
                         , objIsEditable :: Bool, _exprIsUni :: Bool, _exprIsTot :: Bool
                         , _objNavInterfaces :: [NavInterface]
                         , atomicOrBox :: FEAtomicOrBox } deriving Show

-- Once we have mClass also for Atomic, we can get rid of FEAtomicOrBox and pattern match on _ifcSubIfcs to determine atomicity.
data FEAtomicOrBox = FEAtomic { objMPrimTemplate :: Maybe String }
                   | FEBox    {  _objMClass :: Maybe String, ifcSubObjs :: [FEObject] } deriving Show

data NavInterface = NavInterface { _navIfcName :: String, _navIfcRoles :: [Role] } deriving Show

flatten :: FEObject -> [FEObject]
flatten obj = obj : concatMap flatten subObjs
  where subObjs = case atomicOrBox obj of
                    FEAtomic{}                 -> []
                    FEBox{ ifcSubObjs = objs } -> objs 

buildInterfaces :: FSpec -> IO [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) allIfcs
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec

buildInterface :: FSpec -> [Interface] -> Interface -> IO FEInterface
buildInterface fSpec allIfcs ifc =
 do { let editableRels = ifcParams ifc
    ; obj <- buildObject editableRels (ifcObj ifc)
    ; return $
        FEInterface  (name ifc) (escapeIdentifier $ name ifc) (ifcClass ifc) (objExp obj) (objSource obj) (objTarget obj) (ifcRoles ifc) editableRels obj
    -- NOTE: due to Amperand's interface data structure, expression, source, and target are taken from the root object. 
    --       (name comes from interface, but is equal to object name)
    } 
  where    
    buildObject :: [Expression] -> ObjectDef -> IO FEObject
    buildObject editableRels object =
     do { let iExp = conjNF (getOpts fSpec) $ objctx object
              (isEditable, src, tgt) = 
                case getExpressionRelation iExp of
                  Nothing                          -> (False, source iExp, target iExp)
                  Just (declSrc, decl, declTgt, _) -> (EDcD decl `elem` editableRels, declSrc, declTgt) 
                                                   -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation
              navIfcs = [ NavInterface (name nIfc) nRoles -- only consider interfaces that share roles with the one we're building 
                        | nIfc <- allIfcs
                        , (source . objctx . ifcObj $ nIfc) == tgt
                        , let nRoles =  ifcRoles nIfc `intersect` ifcRoles ifc
                        ]
        ; (atomicOrBox', iExp', isEditable', tgt') <-
            case objmsub object of
              Nothing                  ->
               do { let specificTemplatePth = "views/Atomic-" ++ name tgt ++ ".html" -- TODO: escape
                  ; hasSpecificTemplate <- doesTemplateExist fSpec $ specificTemplatePth
                  ; let atomic = FEAtomic $ if hasSpecificTemplate then Just specificTemplatePth else Nothing
                  ; return (atomic, iExp, isEditable, tgt)
                  }
              Just (Box _ mCl objects) -> 
               do { subObjs <- mapM (buildObject editableRels) objects
                  ; return (FEBox mCl subObjs, iExp, isEditable, tgt)
                  }
              Just (InterfaceRef nm)   -> 
                case filter (\rIfc -> name rIfc == nm) $ allIfcs of -- Follow interface ref
                  []      -> fatal 44 $ "Referenced interface " ++ nm ++ " missing"
                  (_:_:_) -> fatal 45 $ "Multiple declarations of referenced interface " ++ nm
                  [i]     -> do { let editableRels' = editableRels `intersect` ifcParams i
                                ; refObj <- buildObject editableRels' (ifcObj i)
                                ; return (atomicOrBox refObj, ECps (iExp, objExp refObj), False, (objTarget refObj))
                                } -- TODO: interface ref basically skips a relation, so we make it not editable for now

        ; return $ FEObject (name object) iExp' src tgt' isEditable' (isUni iExp') (isTot iExp') navIfcs atomicOrBox'
        }


------ Generate RouteProvider.js

-- Helper data structure to pass attribute values to HStringTemplate
data RouteAttr = RouteAttr { interfaceName :: String, interfaceIdent:: String } deriving (Show, Data, Typeable)

genRouteProvider :: FSpec -> [FEInterface] -> IO ()
genRouteProvider fSpec ifcs =
 do { --verboseLn (getOpts fSpec) $ show $ map name (interfaceS fSpec)
    ; let routeAttrs = [ RouteAttr { interfaceName = ifcName ifc, interfaceIdent = ifcIdent ifc } | ifc <- ifcs ]
    ; template <- readTemplate fSpec "RouteProvider.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (fsName fSpec)
                     . setAttribute "routes"              routeAttrs
                     . setAttribute "ampersandVersionStr" ampersandVersionStr

    ; writePrototypeFile fSpec ("app/RouteProvider.js") $ contents 
    }
    
    
------ Generate view html code

genView_Interfaces :: FSpec -> [FEInterface] -> IO ()
genView_Interfaces fSpec ifcs =
 do { mapM_ (genView_Interface fSpec) $ ifcs
    }

genView_Interface :: FSpec -> FEInterface -> IO ()
genView_Interface fSpec (FEInterface iName iIdent _ iExp iSrc iTgt roles editableRels obj) =
 do { --verboseLn (getOpts fSpec) $ "\nTop-level interface: " ++ show iName ++ " [" ++ name iSrc ++ "*"++ name iTgt ++ "] "
    ; lns <- genView_Object fSpec 0 obj
    ; template <- readTemplate fSpec "views/Interface.html"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (fsName fSpec)
                     . setAttribute "isTopLevel"          (name (source iExp) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"               [ show r | r <- roles ] -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "editableRelations"   [ show $ name r | EDcD r <- editableRels] -- show name, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "interfaceName"       iName           -- TODO: escape
                     . setAttribute "expAdl"              (showADL iExp)
                     . setAttribute "source"              (name iSrc)     -- TODO: escape
                     . setAttribute "target"              (name iTgt)     -- TODO: escape
                     . setAttribute "contents"            (intercalate "\n" . indent 4 $ lns) -- intercalate, because unlines introduces a trailing \n

    ; let filename = iIdent ++ ".html" -- filenames with spaces aren't a huge problem, but it's probably safer to prevent them
    ; writePrototypeFile fSpec ("app/views" </> filename) $ contents 
    }

-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr = SubObjAttr { subObjName :: String, isBLOB ::Bool
                                , subObjContents :: String } deriving (Show, Data, Typeable)
 
genView_Object :: FSpec -> Int -> FEObject -> IO [String]
genView_Object fSpec depth obj@(FEObject nm oExp src tgt isEditable exprIsUni exprIsTot navInterfaces _) =
  let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
      atomicAndBoxAttrs = setAttribute "isEditable" isEditable
                        . setAttribute "exprIsUni"  exprIsUni
                        . setAttribute "exprIsTot"  exprIsTot
                        . setAttribute "name"       nm             -- TODO: escape
                        . setAttribute "expAdl"     (showADL oExp) 
                        . setAttribute "source"     (name src)     -- TODO: escape
                        . setAttribute "target"     (name tgt)     -- TODO: escape
                    
  in  case atomicOrBox obj of
        FEAtomic mPrimTemplate ->
         do { {-
              verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "ATOMIC "++show nm ++ 
                                            " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                            (if isEditable then "" else "not ") ++ "editable"
              -}
            -- For now, we choose specific template based on target concept. This will probably be too weak. 
            -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
            ; template <- readTemplate fSpec $ fromMaybe "views/Atomic.html" mPrimTemplate -- Atomic is the default template
                    
            --; verboseLn (getOpts fSpec) $ unlines [ replicate depth ' ' ++ "-NAV: "++ show n ++ " for "++ show rs 
            --                                      | NavInterface n rs <- navInterfaces ]
            ; let mNavInterface = case navInterfaces of -- TODO: do something with roles here. For now, simply use the first interface, if any.
                                    []                      -> Nothing
                                    NavInterface iName _ :_ -> Just iName
                                                                                  
            ; return $ lines $ renderTemplate template $ 
                                 atomicAndBoxAttrs
                               . setAttribute "navInterface" mNavInterface  -- TODO: escape
            }
        FEBox mClass subObjs ->
         do { {-
              verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "BOX" ++ maybe "" (\c -> "<"++c++">") mClass ++
                                            " " ++ show nm ++ " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                            (if isEditable then "" else "not ") ++ "editable"
              -}
            ; subObjAttrs <- mapM genView_SubObject subObjs
                    
            ; let clssStr = maybe "" (\cl -> "-" ++ cl) mClass
            ; parentTemplate <- readTemplate fSpec $ "views/Box" ++ clssStr ++ ".html"
            
            ; return $ lines $ renderTemplate parentTemplate $ 
                                 atomicAndBoxAttrs
                               . setAttribute "isRoot"     (depth == 0)
                               . setAttribute "subObjects" subObjAttrs
            }
  where genView_SubObject subObj = 
         do { lns <- genView_Object fSpec (depth + 1) subObj
            ; return SubObjAttr{ subObjName = objName subObj -- TODO: escape
                               , isBLOB = name (target $ objExp subObj) == "BLOB"
                               , subObjContents = intercalate "\n" $ indent 8 lns
                               -- Indentation is not context sensitive, so some templates will
                               -- be indented a bit too much (we take the maximum necessary value now)
                               } 
            }

            
------ Generate controller JavaScript code

genController_Interfaces :: FSpec -> [FEInterface] -> IO ()
genController_Interfaces fSpec ifcs =
 do { mapM_ (genController_Interface fSpec) $ ifcs
    }

-- Helper data structure to pass attribute values to HStringTemplate
data NonPrimEditableAttr = NPEAttr { labelName :: String, targetConcept :: String } deriving (Show, Data, Typeable)

genController_Interface :: FSpec -> FEInterface -> IO ()
genController_Interface fSpec (FEInterface iName iIdent _ iExp iSrc iTgt roles editableRels obj) =
 do { -- verboseLn (getOpts fSpec) $ "\nGenerate controller for " ++ show iName
    ; let allObjs = flatten obj
          allEditableNonPrims     = [ NPEAttr { labelName = objName o, targetConcept = name $ objTarget o } -- TODO: escape 
                                    | o@FEObject { atomicOrBox = a@FEAtomic {} } <- allObjs
                                    , objIsEditable o
                                    , not . isJust $ objMPrimTemplate a
                                    ]
          containsEditable        = any objIsEditable allObjs
          containsEditableNonPrim = not $ null allEditableNonPrims
          containsDATE            = any (\o -> name (objTarget o) == "DATE" && objIsEditable o) allObjs
          
    ; template <- readTemplate fSpec "controllers/controller.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   (name (source iExp) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"                    [ show r | r <- roles ] -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "editableRelations"        [ show $ name r | EDcD r <- editableRels] -- show name, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "allEditableNonPrims"      allEditableNonPrims
                     . setAttribute "containsDATE"             containsDATE
                     . setAttribute "containsEditable"         containsEditable
                     . setAttribute "containsEditableNonPrim"  containsEditableNonPrim
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            iName          -- TODO: escape
                     . setAttribute "interfaceIdent"           iIdent
                     . setAttribute "expAdl"                   (showADL iExp)
                     . setAttribute "source"                   (name iSrc)    -- TODO: escape
                     . setAttribute "target"                   (name iTgt)    -- TODO: escape

    ; let filename = iIdent ++ ".js" -- TODO: escape
    ; writePrototypeFile fSpec ("app/controllers" </> filename) $ contents 
    }
    
    
------ Utility functions

-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate String) String

-- TODO: better abstraction for specific template and fallback to default
doesTemplateExist :: FSpec -> String -> IO Bool
doesTemplateExist fSpec templatePath =
 do { let absPath = getTemplateDir fSpec </> templatePath
    ; doesFileExist absPath
    }
 
readTemplate :: FSpec -> String -> IO Template
readTemplate fSpec templatePath =
 do { let absPath = getTemplateDir fSpec </> templatePath
    ; res <- readUTF8File $ absPath
    ; case res of
        Left err          -> error $ "Cannot read template file " ++ templatePath ++ "\n" ++ err
        Right templateStr -> return $ Template (newSTMP templateStr) absPath
    }

-- having Bool attributes prevents us from using a [(String, String)] parameter for attribute settings
renderTemplate :: Template -> (StringTemplate String -> StringTemplate String) -> String
renderTemplate (Template template absPath) setAttrs =
  let appliedTemplate = setAttrs template
  in  case checkTemplateDeep appliedTemplate of
             ([],  [],    []) -> render appliedTemplate
             (parseErrs@(_:_), _, _)        -> templateError $ concat [ "Parse error in " ++ tmplt ++ " " ++ err ++ "\n" 
                                                                      | (tmplt,err) <- parseErrs]
             ([], attrs@(_:_), _)        -> templateError $ "Uninitialized template attributes: " ++ show attrs
             ([], [], ts@(_:_)) -> templateError $ "Missing invoked templates: " ++ show ts -- should not happen as we don't invoke templates
  where templateError msg = error $ "\n\n*** TEMPLATE ERROR in:\n" ++ absPath ++ "\n\n" ++ msg
