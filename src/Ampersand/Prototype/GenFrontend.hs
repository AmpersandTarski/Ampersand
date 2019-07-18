{-# LANGUAGE RecordWildCards #-}
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
import qualified RIO.ByteString.Lazy  as BL
import           RIO.Char
import           Data.Hashable (hash)
import qualified RIO.List as L
import qualified RIO.Text as T
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

        
-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
-- NOTE: due to a bug in HStringTemplate's checkTemplateDeep, non-existent attribute names on
--       composite attributes in anonymous templates will hang the generator :-(
--       Eg.  "$subObjects:{subObj| .. $subObj.nonExistentField$ .. }$"

doGenFrontend :: (HasProtoOpts env, HasZwolleVersion env, HasOptions env, HasDirCustomizations env,HasRootFile env, HasRunComposer env, HasGenTime env, HasDirPrototype env, HasVerbosity  env, HasHandle env) =>
                 FSpec -> RIO env ()
doGenFrontend fSpec = do
    genTime <- view genTimeL
    runComposer <- view runComposerL
    sayWhenLoudLn "Generating frontend..."
    isCleanInstall <- downloadPrototypeFramework
    copyTemplates
    feInterfaces <- buildInterfaces fSpec
    genViewInterfaces fSpec feInterfaces
    genControllerInterfaces fSpec feInterfaces
    genRouteProvider fSpec feInterfaces
    writePrototypeAppFile ".timestamp" (show . hash . show $ genTime) -- this hashed timestamp is used by the prototype framework to prevent browser from using the wrong files from cache
    copyCustomizations 
    when (isCleanInstall && runComposer) $ do
      sayLn "Installing dependencies..." -- don't use sayWhenLoudLn here, because installing dependencies takes some time and we want the user to see this
      installComposerLibs
    sayWhenLoudLn "Frontend generated"
  
copyTemplates :: (HasRootFile env, HasDirPrototype env, HasVerbosity  env, HasHandle env) =>
                 RIO env ()
copyTemplates = do
  env <- ask
  let tempDir = dirSource env </> "templates"
      toDir = getTemplateDir env
  tempDirExists <- liftIO $ doesDirectoryExist tempDir
  if tempDirExists then do
         sayWhenLoudLn $ "Copying project specific templates from " ++ tempDir ++ " -> " ++ toDir
         copyDirRecursively tempDir toDir -- recursively copy all templates
  else
         sayWhenLoudLn ("No project specific templates (there is no directory " ++ tempDir ++ ")") 

copyCustomizations :: (HasDirPrototype env, HasRootFile env,HasDirCustomizations env,HasVerbosity  env, HasHandle env) =>
                      RIO env ()
copyCustomizations = do
  env <- ask
  dirCustomizations <- view dirCustomizationsL
  dirPrototype <- view dirPrototypeL
  let custDirs = map (dirSource env</>) dirCustomizations
  mapM_ (copyDir dirPrototype) custDirs
    where
      copyDir :: (HasVerbosity  env, HasHandle env) =>
                 FilePath -> FilePath -> RIO env()
      copyDir targetDir sourceDir = do
        sourceDirExists <- liftIO $ doesDirectoryExist sourceDir
        if sourceDirExists then
          do sayWhenLoudLn $ "Copying customizations from " ++ sourceDir ++ " -> " ++ targetDir
             copyDirRecursively sourceDir targetDir -- recursively copy all customizations
        else sayWhenLoudLn $ "No customizations (there is no directory " ++ sourceDir ++ ")"

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

buildInterfaces :: (HasDirPrototype env, HasOptions env) => FSpec -> RIO env [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) topLevelUserInterfaces
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec

    topLevelUserInterfaces :: [Interface]
    topLevelUserInterfaces = filter (not . ifcIsAPI) allIfcs

buildInterface :: (HasDirPrototype env, HasOptions env) => FSpec -> [Interface] -> Interface -> RIO env FEInterface
buildInterface fSpec allIfcs ifc = do
  obj <- buildObject (BxExpr $ ifcObj ifc)
  return 
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
 
  where    
    buildObject :: (HasDirPrototype env, HasOptions env) => BoxItem -> RIO env FEObject2
    buildObject (BxExpr object') = do
      opts <- view optionsL
      let object = substituteReferenceObjectDef fSpec object'
      let iExp = conjNF opts $ objExpression object
      (aOrB, iExp') <-
        case objmsub object of
          Nothing -> do
            let ( _ , _ , tgt) = getSrcDclTgt iExp
            let mView = case objmView object of
                          Just nm -> Just $ lookupView fSpec nm
                          Nothing -> getDefaultViewForConcept fSpec tgt
            mSpecificTemplatePath <-
                  case mView of
                    Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just (fName, mapMaybe vsmlabel $ viewSegs)
                    _ -> do
                       -- no view, or no view with an html template, so we fall back to target-concept template
                       -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                      let templatePath = "Atomic-" ++ (idWithoutType tgt) ++ ".html"
                      hasSpecificTemplate <- doesTemplateExist templatePath
                      return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
            return (FEAtomic { objMPrimTemplate = mSpecificTemplatePath}
                   , iExp)
          Just si ->
            case si of
              Box{} -> do
                subObjs <- mapM buildObject (siObjs si)
                return (FEBox { objMClass  = siMClass si
                              , ifcSubObjs = subObjs
                              }
                        , iExp)
              InterfaceRef{} -> 
                case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                  []      -> fatal ("Referenced interface " ++ siIfcId si ++ " missing")
                  (_:_:_) -> fatal ("Multiple relations of referenced interface " ++ siIfcId si)
                  [i]     -> 
                        if siIsLink si
                        then do
                          let templatePath = "View-LINKTO.html"
                          return (FEAtomic { objMPrimTemplate = Just (templatePath, [])}
                                 , iExp)
                        else do 
                          refObj <- buildObject  (BxExpr $ ifcObj i)
                          let comp = ECps (iExp, objExp refObj) 
                               -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                          return (atomicOrBox refObj, comp)
                               -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end
      let (src, mDecl, tgt) = getSrcDclTgt iExp'
      return FEObjE  { objName = name object
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

genRouteProvider :: (HasDirPrototype env, HasVerbosity  env, HasHandle env) =>
                    FSpec -> [FEInterface] -> RIO env ()
genRouteProvider fSpec ifcs = do
  verbosity <- view verbosityL
  template <- readTemplate "routeProvider.config.js"
  let contents = renderTemplate template $
                   setAttribute "contextName"         (fsName fSpec)
                 . setAttribute "ampersandVersionStr" ampersandVersionStr
                 . setAttribute "ifcs"                ifcs
                 . setAttribute "verbose"             verbosity
  writePrototypeAppFile "routeProvider.config.js" contents 
      
------ Generate view html code

genViewInterfaces :: (HasDirPrototype env, HasVerbosity  env, HasHandle env) => 
                     FSpec -> [FEInterface] -> RIO env ()
genViewInterfaces fSpec = mapM_ (genViewInterface fSpec)

genViewInterface :: (HasDirPrototype env, HasVerbosity  env, HasHandle env) => 
                    FSpec -> FEInterface -> RIO env ()
genViewInterface fSpec interf = do
  verbosity <- view verbosityL
  lns <- genViewObject fSpec 0 (_ifcObj interf)
  template <- readTemplate "interface.html"
  let contents = renderTemplate template $
                    setAttribute "contextName"         (addSlashes . fsName $ fSpec)
                  . setAttribute "isTopLevel"          ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                  . setAttribute "roles"               (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                  . setAttribute "ampersandVersionStr" ampersandVersionStr
                  . setAttribute "interfaceName"       (ifcName  interf)
                  . setAttribute "interfaceLabel"      (ifcLabel interf) -- no escaping for labels in templates needed
                  . setAttribute "expAdl"              (showA . _ifcExp $ interf)
                  . setAttribute "source"              (idWithoutType . _ifcSource $ interf)
                  . setAttribute "target"              (idWithoutType . _ifcTarget $ interf)
                  . setAttribute "crudC"               (objCrudC (_ifcObj interf))
                  . setAttribute "crudR"               (objCrudR (_ifcObj interf))
                  . setAttribute "crudU"               (objCrudU (_ifcObj interf))
                  . setAttribute "crudD"               (objCrudD (_ifcObj interf))
                  . setAttribute "contents"            (L.intercalate "\n" lns) -- intercalate, because unlines introduces a trailing \n
                  . setAttribute "verbose"             (verbosity)
  let filename = "ifc" ++ ifcName interf ++ ".view.html" 
  writePrototypeAppFile filename contents 
  
-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr2 = SubObjAttr{ subObjName :: String
                                , subObjLabel :: String
                                , subObjContents :: String 
                                , subObjExprIsUni :: Bool
                                } deriving (Show, Data, Typeable)
 
genViewObject :: (HasDirPrototype env, HasVerbosity env) =>
                 FSpec -> Int -> FEObject2 -> RIO env [String]
genViewObject fSpec depth obj =
  case obj of 
    FEObjE{} -> do
      verbosity <- view verbosityL
      let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
          atomicAndBoxAttrs = setAttribute "exprIsUni"  (exprIsUni obj)
                            . setAttribute "exprIsTot"  (exprIsTot obj)
                            . setAttribute "name"       (escapeIdentifier . objName $ obj)
                            . setAttribute "label"      (objName obj) -- no escaping for labels in templates needed
                            . setAttribute "expAdl"     (showA . objExp $ obj) 
                            . setAttribute "source"     (idWithoutType . objSource $ obj)
                            . setAttribute "target"     (idWithoutType . objTarget $ obj)
                            . setAttribute "crudC"      (objCrudC obj)
                            . setAttribute "crudR"      (objCrudR obj)
                            . setAttribute "crudU"      (objCrudU obj)
                            . setAttribute "crudD"      (objCrudD obj)
                            . setAttribute "verbose"    (verbosity)
      case atomicOrBox obj of
            FEAtomic{} -> do
              {-
                  sayWhenLoudLn (getOpts fSpec) $ replicate depth ' ' ++ "ATOMIC "++show nm ++ 
                                                " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                                (if isEditable then "" else "not ") ++ "editable"
              -}
              -- For now, we choose specific template based on target concept. This will probably be too weak. 
              -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
              -- sayLn $ nm ++ ":" ++ show mPrimTemplate
              conceptTemplate <- getTemplateForObject
              let (templateFilename, _) = fromMaybe (conceptTemplate, []) (objMPrimTemplate . atomicOrBox $ obj) -- Atomic is the default template
              template <- readTemplate templateFilename
                        
              return . indentation
                     . lines 
                     . renderTemplate template $ 
                      atomicAndBoxAttrs

            FEBox { objMClass  = mClass
                  , ifcSubObjs = subObjs
                  } -> do
              subObjAttrs <- mapM genView_SubObject subObjs
                        
              let clssStr = maybe "Box-ROWS.html" (\cl -> "Box-" ++ cl ++ ".html") mClass
              parentTemplate <- readTemplate clssStr
                
              return . indentation
                     . lines 
                     . renderTemplate parentTemplate $ 
                           atomicAndBoxAttrs
                         . setAttribute "isRoot"     (depth == 0)
                         . setAttribute "subObjects" subObjAttrs
    FEObjT{} -> pure []
  where 
    indentation :: [String] -> [String]
    indentation = map ( (replicate (if depth == 0 then 4 else 16) ' ') ++)
    genView_SubObject :: (HasDirPrototype env, HasVerbosity env) =>
                         FEObject2 -> RIO env SubObjectAttr2
    genView_SubObject subObj =
      case subObj of
        FEObjE{} -> 
          do lns <- genViewObject fSpec (depth + 1) subObj
             return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj -- no escaping for labels in templates needed
                              , subObjContents = L.intercalate "\n" lns
                              , subObjExprIsUni = exprIsUni subObj
                              } 
        FEObjT{} -> 
          do return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj
                              , subObjContents = objTxt subObj
                              , subObjExprIsUni = True
                              }
    getTemplateForObject :: (HasDirPrototype env) =>
                            RIO env FilePath
    getTemplateForObject 
       | relIsProp obj && (not . exprIsIdent) obj  -- special 'checkbox-like' template for propery relations
                   = return $ "View-PROPERTY"++".html"
       | otherwise = getTemplateForConcept (objTarget obj)
    getTemplateForConcept :: (HasDirPrototype env) =>
                             A_Concept -> RIO env FilePath
    getTemplateForConcept cpt = do 
         exists <- doesTemplateExist cptfn
         return $ if exists
                  then cptfn
                  else "Atomic-"++show ttp++".html" 
       where ttp = cptTType fSpec cpt
             cptfn = "Concept-"++name cpt++".html" 


------ Generate controller JavaScript code
genControllerInterfaces :: (HasDirPrototype env, HasVerbosity env, HasHandle env) => FSpec -> [FEInterface] -> RIO env ()
genControllerInterfaces fSpec = mapM_ (genControllerInterface fSpec)

genControllerInterface :: (HasDirPrototype env, HasVerbosity env, HasHandle env) => FSpec -> FEInterface -> RIO env ()
genControllerInterface fSpec interf = do
    let controlerTemplateName = "interface.controller.js"
    template <- readTemplate controlerTemplateName
    verbosity <- view verbosityL
    let contents = renderTemplate template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   ((name . source . _ifcExp $ interf) `elem` ["ONE", "SESSION"])
                     . setAttribute "roles"                    (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            (ifcName interf)
                     . setAttribute "interfaceLabel"           (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"                   (showA . _ifcExp $ interf)
                     . setAttribute "exprIsUni"                (exprIsUni (_ifcObj interf))
                     . setAttribute "source"                   (idWithoutType . _ifcSource $ interf)
                     . setAttribute "target"                   (idWithoutType . _ifcTarget $ interf)
                     . setAttribute "crudC"                    (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"                    (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"                    (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"                    (objCrudD (_ifcObj interf))
                     . setAttribute "verbose"                  (verbosity)
                     . setAttribute "usedTemplate"             controlerTemplateName
    let filename = "ifc" ++ ifcName interf ++ ".controller.js"
    writePrototypeAppFile filename contents 

------ Utility functions
-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate String) String

-- TODO: better abstraction for specific template and fallback to default
doesTemplateExist :: (HasDirPrototype env) => String -> RIO env Bool
doesTemplateExist templatePath = do
  opts <- ask
  let absPath = getTemplateDir opts </> templatePath
  liftIO $ doesFileExist absPath

readTemplate :: (HasDirPrototype env) =>
                FilePath -> RIO env Template
readTemplate templatePath = do
  opts <- ask
  let absPath = getTemplateDir opts </> templatePath
  res <- readUTF8File absPath
  case res of
    Left err   -> exitWith $ ReadFileError $ "Error while reading template.\n" : err
    Right cont -> return $ Template (newSTMP . T.unpack $ cont) absPath

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
  where templateError msg = exitWith $ ReadFileError 
            ["*** TEMPLATE ERROR in:" ++ absPath
            , msg
            ]




downloadPrototypeFramework :: (HasProtoOpts env, HasZwolleVersion env, HasDirPrototype env, HasHandle env, HasVerbosity env) =>
                             RIO env Bool
downloadPrototypeFramework = ( do 
    destination <- view dirPrototypeL
    x <- extractionIsAllowed destination
    zwolleVersion <- view zwolleVersionL
    if x
    then do
      sayWhenLoudLn "Emptying folder to deploy prototype framework"
      liftIO $ removeDirectoryRecursive destination
      let url = "https://github.com/AmpersandTarski/Prototype/archive/"++zwolleVersion++".zip"
      sayWhenLoudLn "Start downloading prototype framework."
      response <- (parseRequest url >>= httpBS) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "  Failed to download "<>url
                              , show (err :: SomeException)
                              ]
      let archive = removeTopLevelFolder 
                  . toArchive 
                  . BL.fromStrict 
                  . getResponseBody $ response
      sayWhenLoudLn "Start extraction of prototype framework."
      verbosity <- view verbosityL
      let zipoptions = 
               [OptVerbose | verbosity == Loud]
            ++ [OptDestination destination]
      ((liftIO . extractFilesFromArchive zipoptions $ archive) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "  Failed to extract the archive found at "<>url
                              , show (err :: SomeException)
                              ])
      let dest = destination </> ".frameworkSHA"  
      ((liftIO . writeFile dest . show . zComment $ archive) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "Archive seems valid: "<>url
                              , "  Failed to write contents of archive to "<>dest
                              , show (err :: SomeException)
                              ])
      return x
    else return x
  ) `catch` \err ->  -- git failed to execute
         exitWith . FailedToInstallPrototypeFramework $
            [ "Error encountered during deployment of prototype framework:"
            , show (err :: SomeException)
            ]
            
  where
    removeTopLevelFolder :: Archive -> Archive
    removeTopLevelFolder archive = 
       archive{zEntries = mapMaybe removeTopLevelPath . zEntries $ archive}
      where
        removeTopLevelPath :: Entry -> Maybe Entry
        removeTopLevelPath entry = 
            case splitPath . eRelativePath $ entry of
              [] -> fatal "Impossible"
              _:[] -> Nothing
              _:tl -> Just entry{eRelativePath = joinPath tl}

    extractionIsAllowed :: (HasProtoOpts env, HasHandle env, HasVerbosity env) =>
                           FilePath ->  RIO env Bool
    extractionIsAllowed destination = do
      pathExist <- liftIO $ doesPathExist destination
      destIsDirectory <- liftIO $ doesDirectoryExist destination 
      if pathExist
      then 
          if destIsDirectory
          then do 
            dirContents <- liftIO $ listDirectory destination
            if null dirContents
            then return True
            else do
              forceReinstallFramework <- view forceReinstallFrameworkL
              if forceReinstallFramework
              then do
                sayLn "Deleting all files to deploy prototype framework in"
                sayLn ("  " ++ destination)
                sayLn "Are you sure? y/n"
                proceed <- promptUserYesNo
                return proceed
              else do
                (sayWhenLoudLn $
                         "(Re)deploying prototype framework not allowed, because\n"
                      ++ "  "++destination++" isn't empty. You could use the switch --force-reinstall-framework")
                return False
          else do 
             sayWhenLoudLn $
                       "(Re)deploying prototype framework not allowed, because\n"
                    ++ "  "++destination++" isn't a directory."
             return False
      else return True

promptUserYesNo :: (HasHandle env) => RIO env Bool
promptUserYesNo = do
    char <- liftIO $ getChar -- TODO: refactor that the first character is directly processed
    case toUpper char of
      'Y' -> return True
      'N' -> return False
      _ -> do when (char /= '\n') $ sayLn "Please specify y/n" -- Remove 'when' part if first char it directly processed
              x <- promptUserYesNo
              return x