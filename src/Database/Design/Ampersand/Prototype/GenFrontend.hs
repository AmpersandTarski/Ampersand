{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend, clearTemplateDirs) where

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
- Keeping templates as statics requires that the static files are written before templates are used.
  Maybe we should keep them as cabal data-files instead. (file extensions and directory structure are predictable)


NOTE: interface refs are handled as follows:

INTERFACE MyInterface 
  BOX [ ref : rel[$a*$b]
        INTERFACE RefInterface
      ]
      
INTERFACE RefInterface relRef[$b*$c]
  BOX [ .. : ..[$c*$d]
      ]

is basically mapped onto:

INTERFACE MyInterface 
  BOX [ ref : (rel;relRef)[$a*$c]
        BOX [ .. : ..[$c*$d]
            ]
      ]

This is considered editable iff the composition rel;relRef yields an editable declaration (e.g. for editableR;I).

-}

data Include = Include { _fileOrDir :: FileOrDir, includeSrc :: String, _includeTgt :: String } deriving Show

data FileOrDir = File | Dir deriving Show

-- Files/directories that will be copied to the prototype, if present in $adlSourceDir/includes/
allowedIncludeSubDirs :: [Include]
allowedIncludeSubDirs = [ Include Dir  "templates"         "templates"
                        , Include Dir  "views"             "app/views"
                        , Include Dir  "css"               "app/css"
                        , Include Dir  "js"                "app/js"
                        , Include Dir  "images"            "app/images"
                        , Include Dir  "extensions"        "extensions"
                        , Include File "localSettings.php" "localSettings.php"
                        ]

getTemplateDir :: FSpec -> String
getTemplateDir fSpec = Opts.dirPrototype (getOpts fSpec) </> "templates"

-- Clear template dirs so the generator won't use lingering template files. 
-- (Needs to be called before statics are generated, otherwise the templates from statics/newFrontend/templates will get deleted)
-- TODO: refactor generate, so we can call generation of static files and generics.php from this module.
clearTemplateDirs :: FSpec -> IO ()
clearTemplateDirs fSpec = mapM_ emptyDir ["views", "controllers"]
  where emptyDir path = 
         do { let absPath = getTemplateDir fSpec </> path
            ; dirExists <- doesDirectoryExist absPath
            ; when dirExists $ -- dir may not exist if we haven't generated before
               removeAllDirectoryFiles absPath
            } -- Only remove files, withouth entering subdirectories, to prevent possible disasters with symbolic links.
        
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
data FEInterface = FEInterface { ifcName :: String
                               , _ifcMClass :: Maybe String 
                               , _ifcExp :: Expression, _ifcSource :: A_Concept, _ifcTarget :: A_Concept
                               , _ifcRoles :: [Role], _ifcEditableRels :: [Declaration], _ifcObj :: FEObject }

data FEObject = FEObject { objName :: String
                         , objExp :: Expression
                                                 , objSource :: A_Concept
                                                 , objTarget :: A_Concept
                         , objIsEditable :: Bool
                                                 , _exprIsUni :: Bool
                                                 , _exprIsTot :: Bool
                                                 , _exprIsProp :: Bool
                         , _objNavInterfaces :: [NavInterface]
                         , atomicOrBox :: FEAtomicOrBox
                                                 } deriving Show

-- Once we have mClass also for Atomic, we can get rid of FEAtomicOrBox and pattern match on _ifcSubIfcs to determine atomicity.
data FEAtomicOrBox = FEAtomic { objMPrimTemplate :: Maybe (String, [String]) }
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
        FEInterface (name ifc) (ifcClass ifc) (objExp obj) (objSource obj) (objTarget obj) (ifcRoles ifc) editableRels obj
    -- NOTE: due to Amperand's interface data structure, expression, source, and target are taken from the root object. 
    --       (name comes from interface, but is equal to object name)
    } 
  where    
    buildObject :: [Declaration] -> ObjectDef -> IO FEObject
    buildObject editableRels object =
     do { let iExp = conjNF (getOpts fSpec) $ objctx object
              
        ; (aOrB, iExp', isEditable, src, tgt, isLink) <-
            case objmsub object of
              Nothing                  ->
               do { let (isEditable, src, tgt) = getIsEditableSrcTgt iExp
                  ; let mView = case objmView object of
                                  Just nm -> Just $ lookupView fSpec nm
                                  Nothing -> getDefaultViewForConcept fSpec tgt
                  ; mSpecificTemplatePath <-
                          case mView of
                            Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just ("views" </> fName, [ viewAttr | ViewExp _ Obj{objnm=viewAttr} <- viewSegs])
                            _ -> -- no view, or no view with an html template, so we fall back to target-concept template
                                 -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                             do { let templatePath = "views/Atomic-" ++ (escapeIdentifier $ name tgt) ++ ".html"
                                ; hasSpecificTemplate <- doesTemplateExist fSpec $ templatePath
                                ; return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
                                }
                  ; return (FEAtomic mSpecificTemplatePath, iExp, isEditable, src, tgt, False)
                  }
              Just (Box _ mCl objects) -> 
               do { let (isEditable, src, tgt) = getIsEditableSrcTgt iExp
                  ; subObjs <- mapM (buildObject editableRels) objects
                  ; return (FEBox mCl subObjs, iExp, isEditable, src, tgt, False)
                  }
              Just (InterfaceRef isLink nm)   -> 
                case filter (\rIfc -> name rIfc == nm) $ allIfcs of -- Follow interface ref
                  []      -> fatal 44 $ "Referenced interface " ++ nm ++ " missing"
                  (_:_:_) -> fatal 45 $ "Multiple declarations of referenced interface " ++ nm
                  [i]     -> do { let editableRels' = editableRels `intersect` ifcParams i
                                ; refObj <- buildObject editableRels' (ifcObj i)
                                ; let comp = ECps (iExp, objExp refObj) 
                                      -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                                      (isEditable, src, tgt) = getIsEditableSrcTgt comp
                                ; return (atomicOrBox refObj, comp, isEditable, src, tgt, isLink)
                                } -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end

        ; let navIfcs = [ NavInterface (name nIfc) nRoles -- only consider interfaces that share roles with the one we're building 
                        | nIfc <- allIfcs
                        , (source . objctx . ifcObj $ nIfc) == tgt
                        , let nRoles = ifcRoles nIfc `intersect` ifcRoles ifc
                        ]

        ; return $ FEObject (name object) iExp' src tgt isEditable (isUni iExp') (isTot iExp') (isProp iExp') navIfcs aOrB
        }
      where getIsEditableSrcTgt expr = 
              case getExpressionRelation expr of
                Nothing                          -> (False,                    source expr, target expr)
                Just (declSrc, decl, declTgt, _) -> (decl `elem` editableRels, declSrc,     declTgt    ) 
                                                   -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation

------ Generate RouteProvider.js

genRouteProvider :: FSpec -> [FEInterface] -> IO ()
genRouteProvider fSpec ifcs =
 do { --verboseLn (getOpts fSpec) $ show $ map name (interfaceS fSpec)
    ; template <- readTemplate fSpec "RouteProvider.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (fsName fSpec)
                     . setAttribute "interfaceNames"      (map (escapeIdentifier . ifcName) ifcs)
                     . setAttribute "ampersandVersionStr" ampersandVersionStr

    ; writePrototypeFile fSpec ("app/RouteProvider.js") $ contents 
    }
    
    
------ Generate view html code

genView_Interfaces :: FSpec -> [FEInterface] -> IO ()
genView_Interfaces fSpec ifcs =
 do { mapM_ (genView_Interface fSpec) $ ifcs
    }

genView_Interface :: FSpec -> FEInterface -> IO ()
genView_Interface fSpec (FEInterface iName _ iExp iSrc iTgt roles editableRels obj) =
 do { --verboseLn (getOpts fSpec) $ "\nTop-level interface: " ++ show iName ++ " [" ++ name iSrc ++ "*"++ name iTgt ++ "] "
    ; lns <- genView_Object fSpec 0 obj
    ; template <- readTemplate fSpec "views/Interface.html"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (addSlashes $ fsName fSpec)
                     . setAttribute "isTopLevel"          (name (source iExp) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"               [ show r | r <- roles ] -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "editableRelations"   [ show $ escapeIdentifier (name r) | r <- editableRels ] -- show name, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "interfaceName"       (escapeIdentifier iName)
                     . setAttribute "interfaceLabel"      iName -- no escaping for labels in templates needed
                     . setAttribute "expAdl"              (showADL iExp)
                     . setAttribute "source"              (escapeIdentifier $ name iSrc)
                     . setAttribute "target"              (escapeIdentifier $ name iTgt)
                     . setAttribute "contents"            (intercalate "\n" . indent 4 $ lns) -- intercalate, because unlines introduces a trailing \n

    ; let filename = escapeIdentifier iName ++ ".html" -- filenames with spaces aren't a huge problem, but it's probably safer to prevent them
    ; writePrototypeFile fSpec ("app/views" </> filename) $ contents 
    }

-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr = SubObjAttr { subObjName :: String
                                                                , subObjLabel :: String
                                , subObjContents :: String 
                                                                , subObjExprIsUni :: Bool
                                                                } deriving (Show, Data, Typeable)
 
genView_Object :: FSpec -> Int -> FEObject -> IO [String]
genView_Object fSpec depth obj@(FEObject nm oExp src tgt isEditable exprIsUni exprIsTot exprIsProp navInterfaces _) =
  let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
      atomicAndBoxAttrs = setAttribute "isEditable" isEditable
                        . setAttribute "exprIsUni"  exprIsUni
                        . setAttribute "exprIsTot"  exprIsTot
                        . setAttribute "exprIsProp" exprIsProp
                        . setAttribute "name"       (escapeIdentifier nm)
                        . setAttribute "label"      nm -- no escaping for labels in templates needed
                        . setAttribute "expAdl"     (showADL oExp) 
                        . setAttribute "source"     (escapeIdentifier $ name src)
                        . setAttribute "target"     (escapeIdentifier $ name tgt)
                    
  in  case atomicOrBox obj of
        FEAtomic mPrimTemplate ->
         do { {-
              verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "ATOMIC "++show nm ++ 
                                            " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                            (if isEditable then "" else "not ") ++ "editable"
              -}
            -- For now, we choose specific template based on target concept. This will probably be too weak. 
            -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
            --; putStrLn $ nm ++ ":" ++ show mPrimTemplate
            ; let (templateFilename, viewAttrs) = fromMaybe ("views/Atomic.html", []) mPrimTemplate -- Atomic is the default template
            ; template <- readTemplate fSpec templateFilename
                    
            --; verboseLn (getOpts fSpec) $ unlines [ replicate depth ' ' ++ "-NAV: "++ show n ++ " for "++ show rs 
            --                                      | NavInterface n rs <- navInterfaces ]
            ; let mNavInterface = case navInterfaces of -- TODO: do something with roles here. For now, simply use the first interface, if any.
                                    []                      -> Nothing
                                    NavInterface iName _ :_ -> Just iName
                                                                                  
            ; return $ lines $ renderTemplate template $ 
                                 atomicAndBoxAttrs
                               . setManyAttrib [(viewAttr, "{{row['@view']['"++viewAttr++"']}}") | viewAttr <- viewAttrs ] -- TODO: escape/protect
                               . setAttribute "navInterface" (fmap escapeIdentifier mNavInterface)
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
            ; return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                               , subObjLabel = objName subObj -- no escaping for labels in templates needed
                               , subObjContents = intercalate "\n" $ indent 8 lns
                                                           , subObjExprIsUni = _exprIsUni subObj
                               -- Indentation is not context sensitive, so some templates will
                               -- be indented a bit too much (we take the maximum necessary value now)
                               } 
            }

            
------ Generate controller JavaScript code

genController_Interfaces :: FSpec -> [FEInterface] -> IO ()
genController_Interfaces fSpec ifcs =
 do { mapM_ (genController_Interface fSpec) $ ifcs
    }

genController_Interface :: FSpec -> FEInterface -> IO ()
genController_Interface fSpec (FEInterface iName _ iExp iSrc iTgt roles editableRels obj) =
 do { -- verboseLn (getOpts fSpec) $ "\nGenerate controller for " ++ show iName
    ; let allObjs = flatten obj
          allEditableNonPrimTargets = nub [ escapeIdentifier $ name (objTarget o) 
                                        | o@FEObject { atomicOrBox = a@FEAtomic {} } <- allObjs
                                        , objIsEditable o
                                        , not . isJust $ objMPrimTemplate a
                                        ]
          containsEditable          = any objIsEditable allObjs
          containsEditableNonPrim   = not $ null allEditableNonPrimTargets
          containsDATE              = any (\o -> name (objTarget o) == "DATE" && objIsEditable o) allObjs
          
    ; template <- readTemplate fSpec "controllers/controller.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   (name (source iExp) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"                    [ show r | r <- roles ] -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "editableRelations"        [ show $ escapeIdentifier (name r) | r <- editableRels ] -- show name, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "editableNonPrimTargets"   allEditableNonPrimTargets
                     . setAttribute "containsDATE"             containsDATE
                     . setAttribute "containsEditable"         containsEditable
                     . setAttribute "containsEditableNonPrim"  containsEditableNonPrim
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            (escapeIdentifier iName)
                     . setAttribute "expAdl"                   (showADL iExp)
                     . setAttribute "source"                   (escapeIdentifier $ name iSrc)
                     . setAttribute "target"                   (escapeIdentifier $ name iTgt)

    ; let filename = (escapeIdentifier iName) ++ ".js"
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
