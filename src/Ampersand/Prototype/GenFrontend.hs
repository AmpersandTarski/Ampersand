{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module Ampersand.Prototype.GenFrontend (doGenFrontend) where

import           Ampersand.Basics
import           Ampersand.Classes.Relational
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.NormalForms
import           Ampersand.Misc
import           Ampersand.Prototype.ProtoUtil
import           Codec.Archive.Zip
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy  as BL
import           Data.Data
import           Data.List
import           Data.Maybe
import           Network.HTTP.Simple
import           System.Directory
import           System.FilePath
import           Text.StringTemplate
import           Text.StringTemplate.GenericStandard () -- only import instances



{- TODO
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

This is considered editable iff the composition rel;relRef yields an editable relation (e.g. for editableR;I).

-}

getTemplateDir :: FSpec -> String
getTemplateDir fSpec = dirPrototype (getOpts fSpec) </> "templates"
        
-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
-- NOTE: due to a bug in HStringTemplate's checkTemplateDeep, non-existent attribute names on
--       composite attributes in anonymous templates will hang the generator :-(
--       Eg.  "$subObjects:{subObj| .. $subObj.nonExistentField$ .. }$"

doGenFrontend :: FSpec -> IO ()
doGenFrontend fSpec =
 do { putStrLn "Generating frontend.."
    ; downloadPrototypeFramework (getOpts fSpec)
    ; copyTemplates fSpec
    ; feInterfaces <- buildInterfaces fSpec
    ; genViewInterfaces fSpec feInterfaces
    ; genControllerInterfaces fSpec feInterfaces
    ; genRouteProvider fSpec feInterfaces
    ; copyCustomizations fSpec
    -- ; deleteTemplateDir fSpec -- don't delete template dir anymore, because it is required the next time the frontend is generated
    ; putStrLn "Installing dependencies.."
    ; installComposerLibs (getOpts fSpec)
    ; putStrLn "Frontend generated."
    }

copyTemplates :: FSpec -> IO ()
copyTemplates fSpec =
 do { let adlSourceDir = takeDirectory $ fileName (getOpts fSpec)
          tempDir = adlSourceDir </> "templates"
          toDir = dirPrototype (getOpts fSpec) </> "templates"
    ; tempDirExists <- doesDirectoryExist tempDir
    ; if tempDirExists then
        do { verboseLn (getOpts fSpec) $ "Copying project specific templates from " ++ tempDir ++ " -> " ++ toDir
           ; copyDirRecursively tempDir toDir (getOpts fSpec) -- recursively copy all templates
           }
      else
        verboseLn (getOpts fSpec) $ "No project specific templates (there is no directory " ++ tempDir ++ ")"
    }

copyCustomizations :: FSpec -> IO ()
copyCustomizations fSpec = 
  mapM_ (copyDir protoDir) custDirs
    where
      adlSourceDir = takeDirectory $ fileName opts
      custDirs = map (adlSourceDir </>) (dirCustomizations opts)
      protoDir = dirPrototype opts
      opts = getOpts fSpec
      copyDir :: FilePath -> FilePath -> IO()
      copyDir targetDir sourceDir = do
        sourceDirExists <- doesDirectoryExist sourceDir
        if sourceDirExists then
          do verboseLn opts $ "Copying customizations from " ++ sourceDir ++ " -> " ++ targetDir
             copyDirRecursively sourceDir targetDir opts -- recursively copy all customizations
        else verboseLn opts $ "No customizations (there is no directory " ++ sourceDir ++ ")"

-- deleteTemplateDir :: FSpec -> IO ()
-- deleteTemplateDir fSpec = removeDirectoryRecursive $ dirPrototype (getOpts fSpec) </> "templates"

------ Build intermediate data structure
-- NOTE: _ disables 'not used' warning for fields
data FEInterface = FEInterface { ifcName :: String
                               , ifcLabel :: String
                               , _ifcExp :: Expression
                               , _ifcSource :: A_Concept
                               , _ifcTarget :: A_Concept
                               , _ifcRoles :: [Role]
                               , _ifcObj :: FEObject2
                               } deriving (Typeable, Data)

data FEObject2 =
    FEObjE { objName     :: String
           , objExp      :: Expression
           , objSource   :: A_Concept
           , objTarget   :: A_Concept
           , objCrudC    :: Bool
           , objCrudR    :: Bool
           , objCrudU    :: Bool
           , objCrudD    :: Bool
           , exprIsUni   :: Bool
           , exprIsTot   :: Bool
           , relIsProp   :: Bool -- True iff the expression is a kind of simple relation and that relation is a property.
           , exprIsIdent :: Bool
           , atomicOrBox :: FEAtomicOrBox
           }
  | FEObjT { objName     :: String
           , objTxt      :: String
           } deriving (Show, Data, Typeable )

-- Once we have mClass also for Atomic, we can get rid of FEAtomicOrBox and pattern match on _ifcSubIfcs to determine atomicity.
data FEAtomicOrBox = FEAtomic { objMPrimTemplate :: Maybe ( FilePath -- the absolute path to the template
                                                          , [String] -- the attributes of the template
                                                          ) }
                   | FEBox    { objMClass :: Maybe String
                              , ifcSubObjs :: [FEObject2] 
                              } deriving (Show, Data,Typeable)

buildInterfaces :: FSpec -> IO [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) allIfcs
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec
            
buildInterface :: FSpec -> [Interface] -> Interface -> IO FEInterface
buildInterface fSpec allIfcs ifc =
 do { obj <- buildObject (BxExpr $ ifcObj ifc)
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
    buildObject :: BoxItem -> IO FEObject2
    buildObject (BxExpr object') =
     do { let object = substituteReferenceObjectDef fSpec object'
        ; let iExp = conjNF (getOpts fSpec) $ objExpression object
        ; (aOrB, iExp') <-
            case objmsub object of
              Nothing                  ->
               do { let ( _ , _ , tgt) = getSrcDclTgt iExp
                  ; let mView = case objmView object of
                                  Just nm -> Just $ lookupView fSpec nm
                                  Nothing -> getDefaultViewForConcept fSpec tgt
                  ; mSpecificTemplatePath <-
                          case mView of
                            Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just (fName, mapMaybe vsmlabel viewSegs)
                            _ -> -- no view, or no view with an html template, so we fall back to target-concept template
                                 -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                             do { let templatePath = "Atomic-" ++ escapeIdentifier (name tgt) ++ ".html"
                                ; hasSpecificTemplate <- doesTemplateExist fSpec templatePath
                                ; return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
                                }
                  ; return (FEAtomic { objMPrimTemplate = mSpecificTemplatePath}
                           , iExp)
                  }
              Just si ->
                case si of
                  Box{} -> 
                   do { subObjs <- mapM buildObject (siObjs si)
                      ; return (FEBox { objMClass  = siMClass si
                                      , ifcSubObjs = subObjs
                                      }
                               , iExp)
                      }
                  InterfaceRef{} -> 
                   case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                     []      -> fatal ("Referenced interface " ++ siIfcId si ++ " missing")
                     (_:_:_) -> fatal ("Multiple relations of referenced interface " ++ siIfcId si)
                     [i]     -> 
                           if siIsLink si
                           then do { let templatePath = "View-LINKTO.html"
                                   ; return (FEAtomic { objMPrimTemplate = Just (templatePath, [])}
                                            , iExp)
                                   }
                           else do { refObj <- buildObject  (BxExpr $ ifcObj i)
                                   ; let comp = ECps (iExp, objExp refObj) 
                                         -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                                         
                                   ; return (atomicOrBox refObj, comp)
                                   } -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end
        

        ; let (src, mDecl, tgt) = getSrcDclTgt iExp'
        ; return FEObjE  { objName = name object
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
                                          Just dcl -> isProp (EDcD dcl)
                         , exprIsIdent = isIdent iExp'
                         , atomicOrBox = aOrB
                         }
        }
      where getSrcDclTgt expr = 
              case getExpressionRelation expr of
                Nothing                          -> (source expr, Nothing  , target expr)
                Just (declSrc, decl, declTgt, _) -> (declSrc    , Just decl, declTgt    ) 
                                                   -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation
    buildObject (BxTxt object') = do
      return FEObjT{ objName = name object'
                   , objTxt = objtxt object'
                   }

------ Generate RouteProvider.js

genRouteProvider :: FSpec -> [FEInterface] -> IO ()
genRouteProvider fSpec ifcs =
 do { --verboseLn opts $ show $ map name (interfaceS fSpec)
    ; template <- readTemplate fSpec "routeProvider.config.js"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (fsName fSpec)
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "ifcs"                ifcs
                     . setAttribute "verbose"             (verboseP opts)

    ; writePrototypeAppFile opts "routeProvider.config.js" contents 
    }
  where opts = getOpts fSpec
    
------ Generate view html code

genViewInterfaces :: FSpec -> [FEInterface] -> IO ()
genViewInterfaces fSpec = mapM_ (genViewInterface fSpec) 

genViewInterface :: FSpec -> FEInterface -> IO ()
genViewInterface fSpec interf =
 do { lns <- genViewObject fSpec 0 (_ifcObj interf)
    ; template <- readTemplate fSpec "interface.html"
    ; let contents = renderTemplate template $
                       setAttribute "contextName"         (addSlashes . fsName $ fSpec)
                     . setAttribute "isTopLevel"          ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"               (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr" ampersandVersionStr
                     . setAttribute "interfaceName"       (ifcName  interf)
                     . setAttribute "interfaceLabel"      (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"              (showA . _ifcExp $ interf)
                     . setAttribute "source"              (escapeIdentifier . name . _ifcSource $ interf)
                     . setAttribute "target"              (escapeIdentifier . name . _ifcTarget $ interf)
                     . setAttribute "crudC"               (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"               (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"               (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"               (objCrudD (_ifcObj interf))
                     . setAttribute "contents"            (intercalate "\n" lns) -- intercalate, because unlines introduces a trailing \n
                     . setAttribute "verbose"             (verboseP opts)

    ; let filename = "ifc" ++ ifcName interf ++ ".view.html" 
    ; writePrototypeAppFile opts filename contents 
    }
   where opts = getOpts fSpec
-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr2 = SubObjAttr{ subObjName :: String
                                , subObjLabel :: String
                                , subObjContents :: String 
                                , subObjExprIsUni :: Bool
                                } deriving (Show, Data, Typeable)
 
genViewObject :: FSpec -> Int -> FEObject2 -> IO [String]
genViewObject fSpec depth obj@FEObjE{} =
  let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
      atomicAndBoxAttrs = setAttribute "exprIsUni"  (exprIsUni obj)
                        . setAttribute "exprIsTot"  (exprIsTot obj)
                        . setAttribute "name"       (escapeIdentifier . objName $ obj)
                        . setAttribute "label"      (objName obj) -- no escaping for labels in templates needed
                        . setAttribute "expAdl"     (showA . objExp $ obj) 
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
                    
            ; return . indentation
                     . lines 
                     . renderTemplate template $ 
                                 atomicAndBoxAttrs
            }
        FEBox { objMClass  = mClass
              , ifcSubObjs = subObjs} ->
         do { subObjAttrs <- mapM genView_SubObject subObjs
                    
            ; let clssStr = maybe "Box-ROWS.html" (\cl -> "Box-" ++ cl ++ ".html") mClass
            ; parentTemplate <- readTemplate fSpec clssStr
            
            ; return . indentation
                     . lines 
                     . renderTemplate parentTemplate $ 
                                 atomicAndBoxAttrs
                               . setAttribute "isRoot"     (depth == 0)
                               . setAttribute "subObjects" subObjAttrs
            }
  where 
    indentation :: [String] -> [String]
    indentation = indent (if depth == 0 then 4 else 16) 
    genView_SubObject :: FEObject2 -> IO SubObjectAttr2
    genView_SubObject subObj =
      case subObj of
        FEObjE{} -> 
          do lns <- genViewObject fSpec (depth + 1) subObj
             return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj -- no escaping for labels in templates needed
                              , subObjContents = intercalate "\n" lns
                              , subObjExprIsUni = exprIsUni subObj
                              } 
        FEObjT{} -> 
          do return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj
                              , subObjContents = objTxt subObj
                              , subObjExprIsUni = True
                              }
    getTemplateForObject :: IO FilePath
    getTemplateForObject 
       | relIsProp obj && (not . exprIsIdent) obj  -- special 'checkbox-like' template for propery relations
                   = return $ "View-PROPERTY"++".html"
       | otherwise = getTemplateForConcept (objTarget obj)
    getTemplateForConcept :: A_Concept -> IO FilePath
    getTemplateForConcept cpt = do 
         exists <- doesTemplateExist fSpec cptfn
         return $ if exists
                  then cptfn
                  else "Atomic-"++show ttp++".html" 
       where ttp = cptTType fSpec cpt
             cptfn = "Concept-"++name cpt++".html" 
genViewObject _     _     FEObjT{} = pure []

------ Generate controller JavaScript code
genControllerInterfaces :: FSpec -> [FEInterface] -> IO ()
genControllerInterfaces fSpec = mapM_ (genControllerInterface fSpec)

genControllerInterface :: FSpec -> FEInterface -> IO ()
genControllerInterface fSpec interf =
 do { -- verboseLn (getOpts fSpec) $ "\nGenerate controller for " ++ show iName
    ; let controlerTemplateName = "interface.controller.js"
    ; template <- readTemplate fSpec controlerTemplateName
    ; let contents = renderTemplate template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"                    (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            (ifcName interf)
                     . setAttribute "interfaceLabel"           (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"                   (showA . _ifcExp $ interf)
                     . setAttribute "exprIsUni"                (exprIsUni (_ifcObj interf))
                     . setAttribute "source"                   (escapeIdentifier . name . _ifcSource $ interf)
                     . setAttribute "target"                   (escapeIdentifier . name . _ifcTarget $ interf)
                     . setAttribute "crudC"                    (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"                    (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"                    (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"                    (objCrudD (_ifcObj interf))
                     . setAttribute "verbose"                  (verboseP opts)
                     . setAttribute "usedTemplate"             controlerTemplateName
    ; let filename = "ifc" ++ ifcName interf ++ ".controller.js"
    ; writePrototypeAppFile opts filename contents 
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




downloadPrototypeFramework :: Options -> IO ()
downloadPrototypeFramework opts = 
  (do 
    x <- allowExtraction
    when x $ do 
      when (forceReinstallFramework opts) destroyDestinationDir
      verboseLn opts "Start downloading frontend framework."
      response <- 
        parseRequest ("https://github.com/AmpersandTarski/Prototype/archive/"++zwolleVersion opts++".zip") >>=
        httpBS  
      let archive = removeTopLevelFolder 
                  . toArchive 
                  . BL.fromStrict 
                  . getResponseBody $ response
      verboseLn opts "Start extraction of frontend framework."
      let zipoptions = 
              [OptVerbose | verboseP opts]
            ++ [OptDestination destination]
      extractFilesFromArchive zipoptions archive
      writeFile (destination </> ".prototypeSHA")
                (show . zComment $ archive)
  ) `catch` \err ->  -- git failed to execute
         exitWith . FailedToInstallPrototypeFramework $
            [ "Error encountered during installation of prototype framework:"
            , show (err :: SomeException)
            ]
            
  where
    destination = dirPrototype opts
    destroyDestinationDir :: IO ()
    destroyDestinationDir = removeDirectoryRecursive destination
    removeTopLevelFolder :: Archive -> Archive
    removeTopLevelFolder archive = 
       archive{zEntries = mapMaybe removeTopLevelPath . zEntries $ archive}
      where
        removeTopLevelPath :: Entry -> Maybe Entry
        removeTopLevelPath entry = 
            case tail . splitPath . eRelativePath $ entry of
              [] -> Nothing
              xs -> Just entry{eRelativePath = joinPath xs}

    allowExtraction :: IO Bool
    allowExtraction = do
      pathExist <- doesPathExist destination
      destIsDirectory <- doesDirectoryExist destination 
      if pathExist
      then 
          if destIsDirectory
          then do 
            dirContents <- listDirectory destination
            unless (null dirContents)
                   (verboseLn opts $
                         "Didn't install prototype framework, because\n"
                      ++ "  "++destination++" isn't empty.")
            return (null dirContents)
          else do 
             verboseLn opts $
                       "Didn't install prototype framework, because\n"
                    ++ "  "++destination++" isn't a directory."
             return False
      else return True
      