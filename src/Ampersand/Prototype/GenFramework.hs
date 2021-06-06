{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.GenFramework (doGenFrontend, doGenBackend, copyCustomizations) where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes.Relational
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.NormalForms
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Runners (logLevel)
import           Ampersand.Types.Config
import           Codec.Archive.Zip
import           Data.Hashable (hash)
import           Network.HTTP.Simple
import qualified RIO.ByteString.Lazy  as BL
import qualified RIO.Text as T
import           RIO.Time
import           System.Directory
import           System.FilePath
import           Text.StringTemplate.GenericStandard () -- only import instances
import           Ampersand.Prototype.GenAngularFrontend
import           Ampersand.Prototype.GenAngularJSFrontend
import           Ampersand.Prototype.GenBackend

doGenFrontend :: (HasFSpecGenOpts env, HasRunner env, HasZwolleVersion env, HasDirPrototype env) =>
                 FSpec -> RIO env ()
doGenFrontend fSpec = do
    now <- getCurrentTime
    logInfo "Generating frontend..."
    _ <- downloadPrototypeFramework
    copyTemplates
    feInterfaces <- buildInterfaces fSpec
    frontendVersion <- view frontendVersionL 
    case frontendVersion of 
      AngularJS -> do genViewInterfaces fSpec feInterfaces
                      genControllerInterfaces fSpec feInterfaces
                      genRouteProvider fSpec feInterfaces
      Angular   -> do genComponents fSpec feInterfaces
                      genAngularModule fSpec feInterfaces 
    writePrototypeAppFile ".timestamp" (tshow . hash . show $ now) -- this hashed timestamp is used by the prototype framework to prevent browser from using the wrong files from cache
    logInfo "Frontend generated"

-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
-- NOTE: due to a bug in HStringTemplate's checkTemplateDeep, non-existent attribute names on
--       composite attributes in anonymous templates will hang the generator :-(
--       Eg.  "$subObjects:{subObj| .. $subObj.nonExistentField$ .. }$"

  
copyTemplates :: (HasFSpecGenOpts env, HasDirPrototype env, HasLogFunc env) =>
                 RIO env ()
copyTemplates = do
  env <- ask
  let tempDir = dirSource env </> "templates"
      toDir = getTemplateDir env
  tempDirExists <- liftIO $ doesDirectoryExist tempDir
  if tempDirExists then do
         logDebug $ "Copying project specific templates from " <> display (T.pack tempDir) <> " -> " <> display (T.pack toDir)
         copyDirRecursively tempDir toDir -- recursively copy all templates
  else
         logDebug $ "No project specific templates (there is no directory " <> display (T.pack tempDir) <> ")"

downloadPrototypeFramework :: (HasRunner env, HasZwolleVersion env, HasDirPrototype env) =>
                             RIO env Bool
downloadPrototypeFramework = ( do 
    env <- ask
    let dirPrototype = getDirPrototype env
    x <- extractionIsAllowed dirPrototype
    zwolleVersion <- view zwolleVersionL
    if x
    then do
      logDebug "Emptying folder to deploy prototype framework"
      liftIO $ removeDirectoryRecursive dirPrototype
      let url :: FilePath
          url = "https://github.com/AmpersandTarski/Prototype/archive/"<>zwolleVersion<>".zip"
      logDebug "Start downloading prototype framework."
      response <- (parseRequest url >>= httpBS) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "  Failed to download "<>T.pack url
                              , tshow (err :: SomeException)
                              ]
      let archive = removeTopLevelFolder 
                  . toArchive 
                  . BL.fromStrict 
                  . getResponseBody $ response
      logDebug "Start extraction of prototype framework."
      runner <- view runnerL
      let zipoptions = 
               [OptVerbose | logLevel runner == LevelDebug]
            <> [OptDestination dirPrototype]
      (liftIO . extractFilesFromArchive zipoptions $ archive) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "  Failed to extract the archive found at "<>T.pack url
                              , tshow (err :: SomeException)
                              ]
      let dest = dirPrototype </> ".frameworkSHA"  
      (writeFileUtf8 dest . tshow . zComment $ archive) `catch` \err ->  
                          exitWith . FailedToInstallPrototypeFramework $
                              [ "Error encountered during deployment of prototype framework:"
                              , "Archive seems valid: "<>T.pack url
                              , "  Failed to write contents of archive to "<>T.pack dest
                              , tshow (err :: SomeException)
                              ]
      return x
    else return x
  ) `catch` \err ->  -- git failed to execute
         exitWith . FailedToInstallPrototypeFramework $
            [ "Error encountered during deployment of prototype framework:"
            , tshow (err :: SomeException)
            ]
            
  where
    removeTopLevelFolder :: Archive -> Archive
    removeTopLevelFolder archive = 
       archive{zEntries = mapMaybe removeTopLevelPath . zEntries $ archive}
      where
        removeTopLevelPath :: Entry -> Maybe Entry
        removeTopLevelPath entry = 
            case splitPath . eRelativePath $ entry of
              []   -> fatal "Impossible"
              [_]  -> Nothing
              _:tl -> Just entry{eRelativePath = joinPath tl}

    extractionIsAllowed :: (HasProtoOpts env, HasLogFunc env) =>
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
            --    logWarn $ "This will delete all files in" <> displayShow destination
            --    logWarn $ "Are you sure? y/n"
            --    proceed <- promptUserYesNo
                return True -- proceed
              else redeployNotAllowed
          else redeployNotAllowed
      else return True
      where redeployNotAllowed = do
                logError $ "(Re)deploying prototype framework not allowed, because "<>
                           "  "<>displayShow destination<>" isn't empty."
                logInfo    "You could use the switch --force-reinstall-framework"
                return False

buildInterfaces :: (HasDirPrototype env) => FSpec -> RIO env [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) topLevelUserInterfaces
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec

    topLevelUserInterfaces :: [Interface]
    topLevelUserInterfaces = filter (not . ifcIsAPI) allIfcs

buildInterface :: (HasDirPrototype env) => FSpec -> [Interface] -> Interface -> RIO env FEInterface
buildInterface fSpec allIfcs ifc = do
  obj <- buildObject (BxExpr $ ifcObj ifc)
  return 
    FEInterface { feiName = escapeIdentifier $ name ifc
                , feiLabel = name ifc
                , feiExp = objExp obj
                , feiSource = objSource obj
                , feiTarget = objTarget obj
                , feiRoles = ifcRoles ifc
                , feiObj = obj
                }
    -- NOTE: due to Amperand's interface data structure, expression, source, and target are taken from the root object. 
    --       (name comes from interface, but is equal to object name)
 
  where    
    buildObject :: (HasDirPrototype env) => BoxItem -> RIO env FEObject
    buildObject (BxExpr object') = do
      env <- ask
      let object = substituteReferenceObjectDef fSpec object'
      let feExp = fromExpr . conjNF env $ objExpression object
      (aOrB, iExp') <-
        case objmsub object of
          Nothing -> do
            let tgt = target feExp
            let mView = maybe (getDefaultViewForConcept fSpec tgt) (Just . lookupView fSpec) (objmView object)
            mSpecificTemplatePath <-
                  case mView of
                    Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just (fName, mapMaybe vsmlabel viewSegs)
                    _ -> do
                       -- no view, or no view with an html template, so we fall back to target-concept template
                       -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                      let templatePath = "Atomic-" <> T.unpack (idWithoutType tgt) <.> ".html"
                      hasSpecificTemplate <- doesTemplateExist templatePath
                      return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
            return (FEAtomic { objMPrimTemplate = mSpecificTemplatePath}
                   , feExp)
          Just si ->
            case si of
              Box{} -> do
                subObjs <- mapM buildObject (siObjs si)
                return (FEBox { boxHeader  = siHeader si
                              , boxSubObjs = subObjs
                              }
                        , feExp)
              InterfaceRef{} -> 
                case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                  []      -> fatal ("Referenced interface " <> siIfcId si <> " missing")
                  (_:_:_) -> fatal ("Multiple relations of referenced interface " <> siIfcId si)
                  [i]     -> 
                        if siIsLink si
                        then do
                          let templatePath = "View-LINKTO.html"
                          return (FEAtomic { objMPrimTemplate = Just (templatePath, [])}
                                 , feExp)
                        else do 
                          refObj <- buildObject  (BxExpr $ ifcObj i)
                          let comp = fromExpr $ ECps (toExpr feExp, toExpr $ objExp refObj) 
                               -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                          return (atomicOrBox refObj, comp)
                               -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end
      return FEObjE  { objName = name object
                      , objExp = iExp'
                      , objSource = source iExp'
                      , objTarget = target iExp'
                      , objCrudC = crudC . objcrud $ object
                      , objCrudR = crudR . objcrud $ object
                      , objCrudU = crudU . objcrud $ object
                      , objCrudD = crudD . objcrud $ object
                      , exprIsUni = isUni . toExpr $ iExp'
                      , exprIsTot = isTot . toExpr $ iExp'
                      , relIsProp  = case femRelation iExp' of
                                      Nothing  -> False
                                      Just dcl -> isProp (EDcD dcl)
                      , exprIsIdent = isIdent . toExpr $ iExp'
                      , atomicOrBox = aOrB
                      }

    buildObject (BxTxt object') = do
      return FEObjT{ objName = name object'
                   , objTxt = objtxt object'
                   }
