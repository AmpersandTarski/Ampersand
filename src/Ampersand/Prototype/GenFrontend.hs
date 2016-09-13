{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module Ampersand.Prototype.GenFrontend (doGenFrontend, clearTemplateDirs) where

import Prelude hiding (putStr,putStrLn,readFile)
import Control.Monad
import Data.Data
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.StringTemplate
import Text.StringTemplate.GenericStandard () -- only import instances
import Ampersand.Basics
import Ampersand.Classes.Relational
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowADL
import Ampersand.FSpec.ToFSpec.NormalForms
import Ampersand.Misc
import qualified Ampersand.Misc.Options as Opts
import Ampersand.Prototype.ProtoUtil

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

data Include = Include { fileOrDir :: FileOrDir
                       , includeSrc :: String
                       , includeTgt :: String
                       } deriving Show

data FileOrDir = File | Dir deriving Show

-- Files/directories that will be copied to the prototype, if present in $adlSourceDir/includes/
allowedIncludeSubDirs :: [Include]
allowedIncludeSubDirs = [ Include Dir  "templates"         "templates"
                        , Include Dir  "views"             "app/views"
                        , Include Dir  "controllers"       "app/controllers"
                        , Include Dir  "css"               "app/css"
                        , Include Dir  "js"                "app/js"
                        , Include Dir  "lib"               "app/lib"
                        , Include Dir  "images"            "app/images"
                        , Include Dir  "extensions"        "extensions"
                        , Include File "localSettings.php" "localSettings.php"
                        ]

getTemplateDir :: FSpec -> String
getTemplateDir fSpec = Opts.dirPrototype (getOpts fSpec) </> "templates"

-- Clear template dirs so the generator won't use lingering template files. 
-- (Needs to be called before statics are generated, otherwise the templates from statics/ZwolleFrontend/templates will get deleted)
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
 do { putStr "Generating frontend.." 
    ; copyIncludes fSpec
    ; feInterfaces <- buildInterfaces fSpec
    ; genViewInterfaces fSpec feInterfaces
    ; genControllerInterfaces fSpec feInterfaces
    ; genRouteProvider fSpec feInterfaces
    ; putStrLn "frontend generated."
    }

copyIncludes :: FSpec -> IO ()
copyIncludes fSpec =
 do { let adlSourceDir = takeDirectory $ fileName (getOpts fSpec)
          includeDir = adlSourceDir </> dirInclude (getOpts fSpec)
          protoDir = Opts.dirPrototype (getOpts fSpec)
    ; includeDirExists <- doesDirectoryExist includeDir
    ; if includeDirExists then
       do { verboseLn (getOpts fSpec) $ "Copying user includes from " ++ includeDir 
          ; includeDirContents <- map (includeDir </>) <$> getProperDirectoryContents includeDir
          
          ; let absIncludes = [ Include { fileOrDir = fileOrDir incl
                                        , includeSrc = absSd
                                        , includeTgt = protoDir </> includeTgt incl
                                        }
                              | incl <- allowedIncludeSubDirs
                              , let absSd = includeDir </> includeSrc incl
                              , absSd `elem` includeDirContents
                              ]
                              
          ; sequence_ (fmap copyInclude absIncludes) -- recursively copy all includes
                      
          ; let ignoredPaths = includeDirContents \\ map includeSrc absIncludes
          ; when (any (\ str -> head str /= '.') ignoredPaths) $  --filter paths starting with a dot, because on mac this is very common and it is a nuisance to avoid (see issue #
             do { putStrLn $ "\nWARNING: only the following include/ paths are allowed:\n  " ++ show (map includeSrc allowedIncludeSubDirs) ++ "\n"
                ; mapM_ (\d -> putStrLn $ "  Ignored " ++ d) ignoredPaths
                }
          }
      else
        verboseLn (getOpts fSpec) $ "No user includes (there is no directory " ++ includeDir ++ ")"
    } 
  where copyInclude :: Include -> IO()
        copyInclude incl =
          do { verboseLn (getOpts fSpec) $ 
                          "  Copying " ++ (case fileOrDir incl of 
                                             File -> "file"
                                             Dir  -> "directory"
                                          )++ " " ++ includeSrc incl ++ "\n    -> " ++ includeTgt incl
             ; case fileOrDir incl of
                 File -> copyDeepFile (includeSrc incl) (includeTgt incl)
                 Dir  -> copyDirRecursively (includeSrc incl) (includeTgt incl)
             }
------ Build intermediate data structure

-- NOTE: _ disables 'not used' warning for fields
data FEInterface = FEInterface { ifcName :: String
                               , ifcLabel :: String
                               , _ifcExp :: Expression, _ifcSource :: A_Concept, _ifcTarget :: A_Concept
                               , _ifcRoles :: [Role],  _ifcObj :: FEObject
                               } deriving (Typeable, Data)

data FEObject = FEObject { objName :: String
                         , objExp :: Expression
                         , objSource :: A_Concept
                         , objTarget :: A_Concept
                         , objCrudC :: Bool
                         , objCrudR :: Bool
                         , objCrudU :: Bool
                         , objCrudD :: Bool
                         , exprIsUni :: Bool
                         , exprIsTot :: Bool
                         , relIsProp  :: Bool -- True iff the expression is a kind of simple relation and that relation is a property.
                         , exprIsIdent :: Bool
                         , objNavInterfaces :: [NavInterface]
                         , atomicOrBox :: FEAtomicOrBox
                         } deriving (Show, Data, Typeable )

-- Once we have mClass also for Atomic, we can get rid of FEAtomicOrBox and pattern match on _ifcSubIfcs to determine atomicity.
data FEAtomicOrBox = FEAtomic { objMPrimTemplate :: Maybe ( FilePath -- the absolute path to the template
                                                          , [String] -- the attributes of the template
                                                          ) }
                   | FEBox    { objMClass :: Maybe String
                              , ifcSubObjs :: [FEObject] 
                              } deriving (Show, Data,Typeable)

data NavInterface = NavInterface { navIfcName :: String
                                 , navIfcRoles :: [Role]
                                 } deriving (Show, Data, Typeable)

buildInterfaces :: FSpec -> IO [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) allIfcs
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec
            
buildInterface :: FSpec -> [Interface] -> Interface -> IO FEInterface
buildInterface fSpec allIfcs ifc =
 do { obj <- buildObject (ifcObj ifc)
    ; return 
        FEInterface { ifcName = escapeIdentifier $ name ifc
                    , ifcLabel = name ifc
                    , _ifcExp = objExp obj
                    , _ifcSource = objSource obj
                    , _ifcTarget = objTarget obj
                    , _ifcRoles = ifcRoles ifc
                    , _ifcObj = obj
                    }
    -- NOTE: due to Amperand's interface data structure, expression, source, and target are taken from the root object. 
    --       (name comes from interface, but is equal to object name)
    } 
  where    
    buildObject :: ObjectDef -> IO FEObject
    buildObject object =
     do { let iExp = conjNF (getOpts fSpec) $ objctx object
              
        ; (aOrB, iExp', src, tgt, mDecl) <-
            case objmsub object of
              Nothing                  ->
               do { let (src, mDecl, tgt) = getSrcDclTgt iExp
                  ; let mView = case objmView object of
                                  Just nm -> Just $ lookupView fSpec nm
                                  Nothing -> getDefaultViewForConcept fSpec tgt
                  ; mSpecificTemplatePath <-
                          case mView of
                            Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just ("views" </> fName, mapMaybe vsmlabel viewSegs)
                            _ -> -- no view, or no view with an html template, so we fall back to target-concept template
                                 -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                             do { let templatePath = "views" </> "Atomic-" ++ escapeIdentifier (name tgt) ++ ".html"
                                ; hasSpecificTemplate <- doesTemplateExist fSpec templatePath
                                ; return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
                                }
                  ; return (FEAtomic { objMPrimTemplate = mSpecificTemplatePath}
                           , iExp, src, tgt, mDecl)
                  }
              Just si ->
                case si of
                  Box{} -> 
                   do { let (src, mDecl, tgt) = getSrcDclTgt iExp
                      ; subObjs <- mapM buildObject (siObjs si)
                      ; return (FEBox { objMClass  = siMClass si
                                      , ifcSubObjs = subObjs
                                      }
                               , iExp, src, tgt, mDecl)
                      }
                  InterfaceRef{} -> 
                   case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                     []      -> fatal 44 $ "Referenced interface " ++ siIfcId si ++ " missing"
                     (_:_:_) -> fatal 45 $ "Multiple declarations of referenced interface " ++ siIfcId si
                     [i]     -> 
                           if siIsLink si
                           then do { let (src, mDecl, tgt) = getSrcDclTgt iExp
                                   ; let templatePath = "views" </> "View-LINKTO.html"
                                   ; return (FEAtomic { objMPrimTemplate = Just (templatePath, [])}
                                            , iExp, src, tgt, mDecl)
                                   }
                           else do { refObj <- buildObject  (ifcObj i)
                                   ; let comp = ECps (iExp, objExp refObj) 
                                         -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                                         (src, mDecl, tgt) = getSrcDclTgt comp
                                   ; return (atomicOrBox refObj, comp, src, tgt, mDecl)
                                   } -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end

        ; let navIfcs = [ NavInterface { navIfcName  = name nIfc
                                       , navIfcRoles = ifcRoles nIfc `intersect` ifcRoles ifc -- only consider interfaces that share roles with the one we're building
                                       } 
                        | nIfc <- allIfcs
                        , (source . objctx . ifcObj $ nIfc) == tgt
                        ]

        ; return FEObject{ objName = name object
                         , objExp = iExp'
                         , objSource = src
                         , objTarget = tgt
                         , objCrudC = crudC . objcrud $ object
                         , objCrudR = crudR . objcrud $ object
                         , objCrudU = crudU . objcrud $ object
                         , objCrudD = crudD . objcrud $ object
                         , exprIsUni = isUni iExp'
                         , exprIsTot = isTot iExp'
                         , relIsProp  = case mDecl of
                                          Nothing  -> False
                                          Just dcl -> isProp dcl
                         , exprIsIdent = isIdent iExp'
                         , objNavInterfaces = navIfcs
                         , atomicOrBox = aOrB
                         }
        }
      where getSrcDclTgt expr = 
              case getExpressionRelation expr of
                Nothing                          -> (source expr, Nothing  , target expr)
                Just (declSrc, decl, declTgt, _) -> (declSrc    , Just decl, declTgt    ) 
                                                   -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation

------ Generate RouteProvider.js

genRouteProvider :: FSpec -> [FEInterface] -> IO ()
genRouteProvider fSpec ifcs =
 do { --verboseLn opts $ show $ map name (interfaceS fSpec)
    ; template <- readTemplate fSpec "RouteProvider.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (fsName fSpec)
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "ifcs"                ifcs
                     . setAttribute "verbose"             (verboseP opts)

    ; writePrototypeAppFile opts "RouteProvider.js" contents 
    }
  where opts = getOpts fSpec
    
------ Generate view html code

genViewInterfaces :: FSpec -> [FEInterface] -> IO ()
genViewInterfaces fSpec = mapM_ (genViewInterface fSpec) 

genViewInterface :: FSpec -> FEInterface -> IO ()
genViewInterface fSpec interf =
 do { lns <- genViewObject fSpec 0 (_ifcObj interf)
    ; template <- readTemplate fSpec "views/Interface.html"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (addSlashes . fsName $ fSpec)
                     . setAttribute "isTopLevel"          ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"               (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "interfaceName"       (ifcName  interf)
                     . setAttribute "interfaceLabel"      (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"              (showADL . _ifcExp $ interf)
                     . setAttribute "source"              (escapeIdentifier . name . _ifcSource $ interf)
                     . setAttribute "target"              (escapeIdentifier . name . _ifcTarget $ interf)
                     . setAttribute "crudC"               (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"               (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"               (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"               (objCrudD (_ifcObj interf))
                     . setAttribute "contents"            (intercalate "\n" . indent 4 $ lns) -- intercalate, because unlines introduces a trailing \n
                     . setAttribute "verbose"             (verboseP opts)

    ; let filename = ifcName interf ++ ".html" 
    ; writePrototypeAppFile opts ("views" </> filename) contents 
    }
   where opts = getOpts fSpec
-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr = SubObjAttr { subObjName :: String
                                , subObjLabel :: String
                                , subObjContents :: String 
                                , subObjExprIsUni :: Bool
                                } deriving (Show, Data, Typeable)
 
genViewObject :: FSpec -> Int -> FEObject -> IO [String]
genViewObject fSpec depth obj =
  let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
      atomicAndBoxAttrs = setAttribute "exprIsUni"  (exprIsUni obj)
                        . setAttribute "exprIsTot"  (exprIsTot obj)
                        . setAttribute "name"       (escapeIdentifier . objName $ obj)
                        . setAttribute "label"      (objName obj) -- no escaping for labels in templates needed
                        . setAttribute "expAdl"     (showADL . objExp $ obj) 
                        . setAttribute "source"     (escapeIdentifier . name . objSource $ obj)
                        . setAttribute "target"     (escapeIdentifier . name . objTarget $ obj)
                        . setAttribute "crudC"      (objCrudC obj)
                        . setAttribute "crudR"      (objCrudR obj)
                        . setAttribute "crudU"      (objCrudU obj)
                        . setAttribute "crudD"      (objCrudD obj)
                        . setAttribute "verbose"    (verboseP (getOpts fSpec))
  in  case atomicOrBox obj of
        FEAtomic{} ->
         do { {-
              verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "ATOMIC "++show nm ++ 
                                            " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                            (if isEditable then "" else "not ") ++ "editable"
              -}
            -- For now, we choose specific template based on target concept. This will probably be too weak. 
            -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
            --; putStrLn $ nm ++ ":" ++ show mPrimTemplate
            ; conceptTemplate <- getTemplateForObject
            ; let (templateFilename, _) = fromMaybe (conceptTemplate, []) (objMPrimTemplate . atomicOrBox $ obj) -- Atomic is the default template
            ; template <- readTemplate fSpec templateFilename
                    
            --; verboseLn (getOpts fSpec) $ unlines [ replicate depth ' ' ++ "-NAV: "++ show n ++ " for "++ show rs 
            --                                      | NavInterface n rs <- navInterfaces ]
            ; let mNavInterface = listToMaybe (objNavInterfaces obj) -- TODO: can also be deleted, not used anymore?
                                                                                  
            ; return $ lines $ renderTemplate template $ 
                                 atomicAndBoxAttrs
                               -- . setManyAttrib [(viewAttr, "{{row['@view']['"++viewAttr++"']}}") | viewAttr <- viewAttrs ] -- TODO: delete this, not used anymore
                               . setAttribute "navInterface" (fmap (escapeIdentifier . navIfcName) mNavInterface) -- TODO: can also be deleted, not used anymore?
            }
        FEBox { objMClass  = mClass
              , ifcSubObjs = subObjs} ->
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
         do { lns <- genViewObject fSpec (depth + 1) subObj
            ; return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                               , subObjLabel = objName subObj -- no escaping for labels in templates needed
                               , subObjContents = intercalate "\n" $ indent 8 lns
                               , subObjExprIsUni = exprIsUni subObj
                               -- Indentation is not context sensitive, so some templates will
                               -- be indented a bit too much (we take the maximum necessary value now)
                               } 
            }
        getTemplateForObject :: IO FilePath
        getTemplateForObject 
           | relIsProp obj && (not . exprIsIdent) obj  -- special 'checkbox-like' template for propery relations
                       = return $  templatePath </> "View-PROPERTY"++".html"
           | otherwise = getTemplateForConcept (objTarget obj)
        getTemplateForConcept :: A_Concept -> IO FilePath
        getTemplateForConcept cpt = do exists <- doesTemplateExist fSpec cptfn
                                   --    verboseLn (getOpts fSpec) $ "Looking for: " ++cptfn ++ "("++(if exists then "" else " not")++ " found.)" 
                                       return $ if exists
                                                then cptfn
                                                else templatePath </> "Atomic-"++show ttp++".html" 
           where ttp = cptTType fSpec cpt
                 cptfn = templatePath </> "Concept-"++name cpt++".html" 
        templatePath = "views"     
------ Generate controller JavaScript code

genControllerInterfaces :: FSpec -> [FEInterface] -> IO ()
genControllerInterfaces fSpec = mapM_ (genControllerInterface fSpec)

genControllerInterface :: FSpec -> FEInterface -> IO ()
genControllerInterface fSpec interf =
 do { -- verboseLn (getOpts fSpec) $ "\nGenerate controller for " ++ show iName
    ; let controlerTemplateName = "controllers/controller.js"
    ; template <- readTemplate fSpec controlerTemplateName
    ; let contents = renderTemplate template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"                    (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            (ifcName interf)
                     . setAttribute "interfaceLabel"           (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"                   (showADL . _ifcExp $ interf)
                     . setAttribute "source"                   (escapeIdentifier . name . _ifcSource $ interf)
                     . setAttribute "target"                   (escapeIdentifier . name . _ifcTarget $ interf)
                     . setAttribute "crudC"                    (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"                    (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"                    (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"                    (objCrudD (_ifcObj interf))
                     . setAttribute "verbose"                  (verboseP opts)
                     . setAttribute "usedTemplate"             controlerTemplateName
    ; let filename = ifcName interf ++ ".js"
    ; writePrototypeAppFile opts ("controllers" </> filename) contents 
    }
    where 
      opts = getOpts fSpec
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
    ; res <- readUTF8File absPath
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
             ([], attrs@(_:_), _)        -> templateError $ "The following attributes are expected by the template, but not supplied: " ++ show attrs
             ([], [], ts@(_:_)) -> templateError $ "Missing invoked templates: " ++ show ts -- should not happen as we don't invoke templates
  where templateError msg = error $ "\n\n*** TEMPLATE ERROR in:\n" ++ absPath ++ "\n\n" ++ msg
