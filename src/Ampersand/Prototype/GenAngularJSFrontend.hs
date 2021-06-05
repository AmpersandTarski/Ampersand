{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.GenAngularJSFrontend (
         genViewInterfaces
       , genControllerInterfaces
       , genRouteProvider
       , copyCustomizations
       ) where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes.Relational
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Runners (logLevel)
import           Ampersand.Types.Config
import qualified RIO.Text as T
import qualified RIO.List as L
import           System.Directory
import           System.FilePath
import           Text.StringTemplate(Stringable, StringTemplate, setAttribute, newSTMP, checkTemplateDeep, render)
import           Text.StringTemplate.GenericStandard () -- only import instances


        

copyCustomizations :: (HasDirPrototype env, HasFSpecGenOpts env , HasDirCustomizations env,HasLogFunc env) =>
                      RIO env ()
copyCustomizations = do
  env <- ask
  dirCustomizations <- view dirCustomizationsL
  let dirPrototype = getDirPrototype env
  let custDirs = maybe [] (map (dirSource env </>)) dirCustomizations
  mapM_ (copyDir dirPrototype) custDirs
    where
      copyDir :: (HasLogFunc env) =>
                 FilePath -> FilePath -> RIO env()
      copyDir targetDir sourceDir = do
        sourceDirExists <- liftIO $ doesDirectoryExist sourceDir
        if sourceDirExists then
          do logDebug $ "Copying customizations from " <> display (T.pack sourceDir) <> " -> " <> display (T.pack targetDir)
             copyDirRecursively sourceDir targetDir -- recursively copy all customizations
        else logDebug $ "No customizations (there is no directory " <> display (T.pack sourceDir) <> ")"



------ Generate RouteProvider.js

genRouteProvider :: (HasRunner env, HasDirPrototype env) =>
                    FSpec -> [FEInterface] -> RIO env ()
genRouteProvider fSpec ifcs = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  template <- readTemplate "routeProvider.config.js"
  let contents = renderTemplate Nothing template $
                   setAttribute "contextName"         (fsName fSpec)
                 . setAttribute "ampersandVersionStr" (longVersion appVersion)
                 . setAttribute "ifcs"                ifcs
                 . setAttribute "verbose"             (loglevel' == LevelDebug)
                 . setAttribute "loglevel"            (show loglevel')
  writePrototypeAppFile "routeProvider.config.js" contents
      
------ Generate view html code
isTopLevel :: A_Concept -> Bool
isTopLevel cpt = isONE cpt || isSESSION cpt
      
genViewInterfaces :: (HasRunner env, HasDirPrototype env) => 
                     FSpec -> [FEInterface] -> RIO env ()
genViewInterfaces fSpec = mapM_ (genViewInterface fSpec)

genViewInterface :: (HasRunner env, HasDirPrototype env) => 
                    FSpec -> FEInterface -> RIO env ()
genViewInterface fSpec interf = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  lns <- genViewObject fSpec 0 (_ifcObj interf)
  template <- readTemplate "interface.html"
  let contents = renderTemplate Nothing template $
                    setAttribute "contextName"         (addSlashes . fsName $ fSpec)
                  . setAttribute "isTopLevel"          (isTopLevel . source . _ifcExp $ interf)
                  . setAttribute "roles"               (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                  . setAttribute "ampersandVersionStr" (longVersion appVersion)
                  . setAttribute "interfaceName"       (ifcName  interf)
                  . setAttribute "interfaceLabel"      (ifcLabel interf) -- no escaping for labels in templates needed
                  . setAttribute "expAdl"              (showA . _ifcExp $ interf)
                  . setAttribute "source"              (idWithoutType . _ifcSource $ interf)
                  . setAttribute "target"              (idWithoutType . _ifcTarget $ interf)
                  . setAttribute "crudC"               (objCrudC (_ifcObj interf))
                  . setAttribute "crudR"               (objCrudR (_ifcObj interf))
                  . setAttribute "crudU"               (objCrudU (_ifcObj interf))
                  . setAttribute "crudD"               (objCrudD (_ifcObj interf))
                  . setAttribute "contents"            (T.intercalate "\n" lns) -- intercalate, because unlines introduces a trailing \n
                  . setAttribute "verbose"             (loglevel' == LevelDebug)
                  . setAttribute "loglevel"            (show loglevel')
  let filename :: FilePath
      filename = "ifc" <>(T.unpack . ifcName $ interf)<> ".view.html" 
  writePrototypeAppFile filename contents 
  
-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr2 = SubObjAttr{ subObjName :: Text
                                , subObjLabel :: Text
                                , subObjContents :: Text 
                                , subObjExprIsUni :: Bool
                                } deriving (Show, Data, Typeable)
 
genViewObject :: (HasRunner env, HasDirPrototype env) =>
                 FSpec -> Int -> FEObject2 -> RIO env [Text]
genViewObject fSpec depth obj =
  case obj of 
    FEObjE{} -> do
      runner <- view runnerL
      let loglevel' = logLevel runner
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
                            . setAttribute "verbose"    (loglevel' == LevelDebug)
                            . setAttribute "loglevel"   (show loglevel')
      case atomicOrBox obj of
            FEAtomic{} -> do
              {-
                  logDebug (getOpts fSpec) $ replicate depth ' ' <> "ATOMIC "<>show nm <> 
                                                " [" <> name src <> "*"<> name tgt <> "], " <>
                                                (if isEditable then "" else "not ") <> "editable"
              -}
              -- For now, we choose specific template based on target concept. This will probably be too weak. 
              -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
              -- logInfo $ nm <> ":" <> show mPrimTemplate
              conceptTemplate <- getTemplateForObject
              let (templateFilename, _) = fromMaybe (conceptTemplate, []) (objMPrimTemplate . atomicOrBox $ obj) -- Atomic is the default template
              template <- readTemplate templateFilename
                        
              return . indentation
                     . T.lines 
                     . renderTemplate Nothing template $ 
                       atomicAndBoxAttrs

            FEBox { objMClass  = header
                  , ifcSubObjs = subObjs
                  } -> do
              subObjAttrs <- mapM genView_SubObject subObjs
                        
              parentTemplate <- readTemplate $ "Box-" <> T.unpack (btType header) <.> "html"
                
              return . indentation
                     . T.lines 
                     . renderTemplate (Just . btKeys $ header) parentTemplate $ 
                           atomicAndBoxAttrs
                         . setAttribute "isRoot"     (depth == 0)
                         . setAttribute "subObjects" subObjAttrs
    FEObjT{} -> pure []
  where 
    indentation :: [Text] -> [Text]
    indentation = map (T.replicate (if depth == 0 then 4 else 16) " " <>)
    genView_SubObject :: (HasRunner env, HasDirPrototype env) =>
                         FEObject2 -> RIO env SubObjectAttr2
    genView_SubObject subObj =
      case subObj of
        FEObjE{} -> 
          do lns <- genViewObject fSpec (depth + 1) subObj
             return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj -- no escaping for labels in templates needed
                              , subObjContents = T.intercalate "\n" lns
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
                   = return $ "View-PROPERTY"<>".html"
       | otherwise = getTemplateForConcept (objTarget obj)
    getTemplateForConcept :: (HasDirPrototype env) =>
                             A_Concept -> RIO env FilePath
    getTemplateForConcept cpt = do 
         exists <- doesTemplateExist cptfn
         return $ if exists
                  then cptfn
                  else "Atomic-"<>show ttp<.>"html" 
       where ttp = cptTType fSpec cpt
             cptfn = "Concept-"<> T.unpack (name cpt) <.> "html"


------ Generate controller JavaScript code
genControllerInterfaces :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genControllerInterfaces fSpec = mapM_ (genControllerInterface fSpec)

genControllerInterface :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genControllerInterface fSpec interf = do
    let controlerTemplateName = "interface.controller.js"
    template <- readTemplate controlerTemplateName
    runner <- view runnerL
    let loglevel' = logLevel runner
    let contents = renderTemplate Nothing template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   (isTopLevel . source . _ifcExp $ interf)
                     . setAttribute "roles"                    (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr"      (longVersion appVersion)
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
                     . setAttribute "verbose"                  (loglevel' == LevelDebug)
                     . setAttribute "loglevel"                 (show loglevel')
                     . setAttribute "usedTemplate"             controlerTemplateName
    let filename = "ifc" <> T.unpack (ifcName interf) <> ".controller.js"
    writePrototypeAppFile filename contents 

------ Utility functions
-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate FilePath) Text


readTemplate :: (HasDirPrototype env) =>
                FilePath -> RIO env Template
readTemplate templatePath = do
  env <- ask
  let absPath = getTemplateDir env </> templatePath
  res <- readUTF8File absPath
  case res of
    Left err   -> exitWith $ ReadFileError $ "Error while reading template." : err
    Right cont -> return $ Template (newSTMP . T.unpack $ cont) (T.pack absPath)

-- having Bool attributes prevents us from using a [(Text, Text)] parameter for attribute settings
renderTemplate :: Maybe [TemplateKeyValue] -> Template -> (StringTemplate String -> StringTemplate String) -> Text
renderTemplate userAtts (Template template absPath) setRuntimeAtts =
    case checkTemplateDeep appliedTemplate of
             ([],  [],    []) -> T.pack $ render appliedTemplate
             (parseErrs@(_:_), _, _)
                -> templateError . T.concat $
                      [ T.pack $ "Parse error in " <> tmplt <> " " <> err <> "\n" 
                      | (tmplt,err) <- parseErrs
                      ]
             ([], attrs@(_:_), _)
                | isJust userAtts -> T.pack . render . fillInTheBlanks (L.nub attrs) $ appliedTemplate
                | otherwise -> templateError $  
                      "The following attributes are expected by the template, but not supplied: " <> tshow attrs
             ([], [], ts@(_:_))
                -> templateError $ 
                      "Missing invoked templates: " <> tshow ts -- should not happen as we don't invoke templates
  where templateError msg = exitWith $ ReadFileError 
            ["*** TEMPLATE ERROR in:" <> absPath
            , msg
            ]
        appliedTemplate = setRuntimeAtts . setUserAtts (fromMaybe [] userAtts) $ template
        -- Set all attributes not specified to False
        fillInTheBlanks :: [String] -> StringTemplate String -> StringTemplate String
        fillInTheBlanks [] = id
        fillInTheBlanks (h:tl) = setAttribute h False . fillInTheBlanks tl
        setUserAtts :: [TemplateKeyValue]  -> (StringTemplate String -> StringTemplate String)
        setUserAtts kvPairs = foldl' fun id kvPairs
          where
            fun :: (Stringable b) => (StringTemplate b -> StringTemplate b) -> TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
            fun soFar keyVal = soFar . doAttribute keyVal
            doAttribute :: (Stringable b) => TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
            doAttribute h = case tkval h of
                Nothing ->  setAttribute (T.unpack $ tkkey h) True
                Just val -> setAttribute (T.unpack $ tkkey h) val

