{-# LANGUAGE OverloadedStrings #-}

module Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes.Relational
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ToFSpec.NormalForms
import Ampersand.Misc.HasClasses
-- only import instances
import Ampersand.Prototype.GenAngularFrontend
import Ampersand.Prototype.GenAngularJSFrontend
import Ampersand.Prototype.ProtoUtil
import Ampersand.Types.Config
import Data.Hashable (hash)
import qualified Data.Set as Set
import RIO.Char
import qualified RIO.Text as T
import RIO.Time
import System.Directory
import System.FilePath
import Text.StringTemplate.GenericStandard ()

doGenFrontend ::
  (HasFSpecGenOpts env, HasRunner env, HasDirPrototype env) =>
  FSpec ->
  RIO env ()
doGenFrontend fSpec = do
  now <- getCurrentTime
  logInfo "Generating frontend..."
  copyTemplates
  feSpec <- buildFESpec fSpec
  let feInterfaces = interfaces feSpec
  frontendVersion <- view frontendVersionL
  logDebug . display $ tshow (length feInterfaces) <> " interfaces will be generated. (" <> tshow frontendVersion <> ")."
  case frontendVersion of
    AngularJS -> do
      genViewInterfaces fSpec feInterfaces
      genControllerInterfaces fSpec feInterfaces
      genRouteProvider fSpec feInterfaces
      logDebug "Finished generating files for AngularJS"
      logDebug "Write .timestamp"
      writePrototypeAppFile ".timestamp" (tshow . hash . show $ now) -- this hashed timestamp is used by the prototype framework to prevent browser from using the wrong files from cache
    Angular -> do
      mapM_ (genComponent fSpec) feInterfaces -- Angular Component files for each interface
      genSingleFileFromTemplate fSpec feSpec "project.concepts.ts.txt" "project.concepts.ts" -- File with all concept types
      genSingleFileFromTemplate fSpec feSpec "project.views.ts.txt" "project.views.ts" -- File with all view types
      genSingleFileFromTemplate fSpec feSpec "project.module.ts.txt" "project.module.ts" -- Angular Module file
      genSingleFileFromTemplate fSpec feSpec "backend.service.ts.txt" "backend.service.ts" -- BackendService file
  logInfo "Angular frontend module generated"

copyTemplates ::
  (HasFSpecGenOpts env, HasDirPrototype env, HasLogFunc env) =>
  RIO env ()
copyTemplates = do
  env <- ask
  logDebug "Start copy templates"
  let tempDir = dirSource env </> "templates"
      toDir = getTemplateDir env
  logDebug . display $ "  From: " <> T.pack tempDir
  logDebug . display $ "  To:   " <> T.pack toDir
  tempDirExists <- liftIO $ doesDirectoryExist tempDir
  if tempDirExists
    then do
      logDebug $ "Copying project specific templates from " <> display (T.pack tempDir) <> " -> " <> display (T.pack toDir)
      copyDirRecursively tempDir toDir -- recursively copy all templates
    else logDebug $ "No project specific templates are copied (there is no such directory " <> display (T.pack tempDir) <> ")"

buildFESpec :: (HasDirPrototype env) => FSpec -> RIO env FESpec
buildFESpec fSpec = do
  ifcs <- buildInterfaces fSpec
  return
    FESpec
      { interfaces = ifcs,
        concepts = buildConcepts fSpec,
        views = buildViews fSpec
      }

buildConcepts :: FSpec -> [FEConcept]
buildConcepts fSpec = map (
  \cpt -> FEConcept
    { cptId = idWithoutType cpt,
      typescriptType = typescriptTypeForConcept fSpec cpt
    }
  ) $ Set.elems . allConcepts $ fSpec

buildViews :: FSpec -> [FEView]
buildViews fSpec = map (
  \viewDef' -> FEView {
    viewId = toPascal . vdlbl $ viewDef',
    viewSegments = []
  }
  ) $ vviews fSpec

buildInterfaces :: (HasDirPrototype env) => FSpec -> RIO env [FEInterface]
buildInterfaces fSpec = mapM buildInterface . filter (not . ifcIsAPI) $ allIfcs
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec

    buildInterface :: (HasDirPrototype env) => Interface -> RIO env FEInterface
    buildInterface ifc = do
      obj <- buildObject (BxExpr $ ifcObj ifc)
      return
        FEInterface
          { ifcName = escapeIdentifier $ name ifc,
            ifcNameKebab = toKebab . safechars $name ifc,
            ifcNamePascal = toPascal . safechars $ name ifc,
            ifcLabel = name ifc,
            ifcExp = objExp obj,
            isSessionInterface = isSESSION . source . objExp $ obj,
            srcConcept = idWithoutType . source . objExp $ obj,
            feiRoles = ifcRoles ifc,
            feiObj = obj
          }
      where
        buildObject :: (HasDirPrototype env) => BoxItem -> RIO env FEObject
        buildObject boxItem = case boxItem of
          BxExpr object' -> do
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
                      Just Vd {vdhtml = Just (ViewHtmlTemplateFile fName), vdats = viewSegs} ->
                        return $ Just (fName, mapMaybe vsmlabel viewSegs)
                      _ -> do
                        -- no view, or no view with an html template, so we fall back to target-concept template
                        -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                        let templatePath = "Atomic-" <> T.unpack (idWithoutType tgt) <.> ".html"
                        hasSpecificTemplate <- doesTemplateExist templatePath
                        return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
                  return
                    ( FEAtomic
                        { objMPrimTemplate = mSpecificTemplatePath,
                          viewDef = mView
                        },
                      feExp
                    )
                Just si ->
                  case si of
                    Box {} -> do
                      subObjs <- mapM buildObject (siObjs si)
                      return
                        ( FEBox
                            { boxHeader = siHeader si,
                              boxSubObjs = subObjs
                            },
                          feExp
                        )
                    InterfaceRef {} ->
                      case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                        [] -> fatal ("Referenced interface " <> siIfcId si <> " missing")
                        (_ : _ : _) -> fatal ("Multiple relations of referenced interface " <> siIfcId si)
                        [i] ->
                          if siIsLink si
                            then do
                              let templatePath = "View-LINKTO.html"
                              return
                                ( FEAtomic
                                    { objMPrimTemplate = Just (templatePath, []),
                                      viewDef = Nothing
                                    },
                                  feExp
                                )
                            else do
                              refObj <- buildObject (BxExpr $ ifcObj i)
                              let comp = fromExpr $ ECps (toExpr feExp, toExpr $ objExp refObj)
                              -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                              return (atomicOrBox refObj, comp)
            -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end
            return
              FEObjE
                { objName = name object,
                  objExp = iExp',
                  objCrudC = crudC . objcrud $ object,
                  objCrudR = crudR . objcrud $ object,
                  objCrudU = crudU . objcrud $ object,
                  objCrudD = crudD . objcrud $ object,
                  objCrud = objcrud object,
                  exprIsUni = isUni . toExpr $ iExp',
                  exprIsTot = isTot . toExpr $ iExp',
                  relIsProp = case femRelation iExp' of
                    Nothing -> False
                    Just dcl -> isProp (EDcD dcl),
                  exprIsIdent = isIdent . toExpr $ iExp',
                  atomicOrBox = aOrB
                }
          BxTxt object' ->
            pure $
              FEObjT
                { objName = name object',
                  objTxt = objtxt object'
                }

toKebab :: Text -> Text
toKebab = T.intercalate "-" . fmap T.toLower . T.words

toPascal :: Text -> Text
toPascal = T.concat . map wordCase . T.words

wordCase :: Text -> Text
wordCase txt = case T.uncons txt of
  Nothing -> mempty
  Just (x, xs) -> T.cons (toUpper x) (T.toLower xs)

safechars :: Text -> Text
safechars = T.unwords . T.split (\c -> not (isDigit c || isAlpha c))

typescriptTypeForConcept :: FSpec -> A_Concept -> Text
typescriptTypeForConcept fSpec cpt = case cptTType fSpec cpt of
  Alphanumeric -> "string"
  BigAlphanumeric -> "string"
  HugeAlphanumeric -> "string"
  Password -> "string"
  Binary -> "string"
  BigBinary -> "string"
  HugeBinary -> "string"
  Date -> "string"
  DateTime -> "string"
  Boolean -> "boolean"
  Integer -> "number"
  Float -> "number"
  TypeOfOne -> "'ONE'" -- special concept ONE
  Object -> "ObjectBase"
