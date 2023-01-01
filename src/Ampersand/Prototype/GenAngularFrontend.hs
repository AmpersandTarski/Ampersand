{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Ampersand.Prototype.GenAngularFrontend (genComponents, genAngularModule) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.ProtoUtil
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config
import RIO.Char (toLower, toUpper)
import qualified RIO.Text as T
import System.FilePath
import Text.StringTemplate (StringTemplate, setAttribute)
import Text.StringTemplate.GenericStandard ()

crudsToString :: Cruds -> Text
crudsToString x = (T.pack $ zipWith (curry f) [crudC x, crudR x, crudU x, crudD x] "crud")
  where
    f :: (Bool, Char) -> Char
    f (b, c) = (if b then toUpper else toLower) c

genComponents :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genComponents fSpec = mapM_ (genComponent fSpec)

genComponent :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponent fspec ifc = do
  genComponentTs fspec ifc
  genComponentView fspec ifc
  -- genComponentInterface fspec ifc
  logInfo . display $ "Generated files for " <> ifcNamePascal ifc <> "Component"

genComponentView :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponentView fSpec interf = do
  let templateFileName = "component.html"
  template <- readTemplate templateFileName
  runner <- view runnerL
  let loglevel' = logLevel runner
  lns <- genViewObject fSpec 0 (feiObj interf)
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (addSlashes . fsName $ fSpec)
            . setAttribute "isSessionInterface" (isSessionInterface interf)
            . setAttribute "roles" (map show . feiRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcName" (ifcName interf)
            . setAttribute "ifcNamePascal" (ifcNamePascal interf)
            . setAttribute "ifcNameKebab" (ifcNameKebab interf)
            . setAttribute "ifcLabel" (ifcLabel interf) -- no escaping for labels in templates needed
            . setAttribute "expAdl" (showA . toExpr . ifcExp $ interf)
            . setAttribute "source" (idWithoutType . source . ifcExp $ interf)
            . setAttribute "target" (idWithoutType . target . ifcExp $ interf)
            . setAttribute "crudC" (objCrudC (feiObj interf))
            . setAttribute "crudR" (objCrudR (feiObj interf))
            . setAttribute "crudU" (objCrudU (feiObj interf))
            . setAttribute "crudD" (objCrudD (feiObj interf))
            . setAttribute "crud" (crudsToString . objCrud . feiObj $ interf)
            . setAttribute "contents" (T.intercalate "\n" lns) -- intercalate, because unlines introduces a trailing \n
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
            . setAttribute "usedTemplate" templateFileName
  let filename = T.unpack(ifcNameKebab interf) </> T.unpack (ifcNameKebab interf) <> ".component.html"
  writePrototypeAppFile filename contents

genComponentTs :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponentTs fSpec interf = do
  let templateFileName = "component.ts.txt"
  template <- readTemplate templateFileName
  mapM_ (logDebug . display) (showTemplate template)
  runner <- view runnerL
  let loglevel' = logLevel runner
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (fsName fSpec)
            . setAttribute "isSessionInterface" (isSessionInterface interf)
            . setAttribute "roles" (map show . feiRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcName" (ifcName interf)
            . setAttribute "ifcNamePascal" (ifcNamePascal interf)
            . setAttribute "ifcNameKebab" (ifcNameKebab interf)
            . setAttribute "ifcLabel" (ifcLabel interf) -- no escaping for labels in templates needed
            . setAttribute "expAdl" (showA . toExpr . ifcExp $ interf)
            . setAttribute "exprIsUni" (exprIsUni (feiObj interf))
            . setAttribute "source" (idWithoutType . source . ifcExp $ interf)
            . setAttribute "target" (idWithoutType . target . ifcExp $ interf)
            . setAttribute "crudC" (objCrudC (feiObj interf))
            . setAttribute "crudR" (objCrudR (feiObj interf))
            . setAttribute "crudU" (objCrudU (feiObj interf))
            . setAttribute "crudD" (objCrudD (feiObj interf))
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
            . setAttribute "usedTemplate" templateFileName
  let filename = T.unpack(ifcNameKebab interf) </> T.unpack (ifcNameKebab interf) <> ".component.ts"
  writePrototypeAppFile filename contents


genAngularModule :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genAngularModule fSpec ifcs = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  template <- readTemplate "project.module.ts.txt"
  mapM_ (logDebug . display) (showTemplate template)
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (fsName fSpec)
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcs" ifcs
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
  writePrototypeAppFile "project.module.ts" contents

-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr2 = SubObjAttr
  { subObjName :: Text,
    subObjLabel :: Text,
    subObjContents :: Text,
    subObjExprIsUni :: Bool
  }
  deriving (Show, Data, Typeable)

genViewObject :: (HasRunner env, HasDirPrototype env) => FSpec -> Int -> FEObject -> RIO env [Text]
genViewObject fSpec depth obj =
  case obj of
    FEObjE {} -> do
      runner <- view runnerL
      let loglevel' = logLevel runner
      let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
          atomicAndBoxAttrs =
            setAttribute "exprIsUni" (exprIsUni obj)
              . setAttribute "exprIsTot" (exprIsTot obj)
              . setAttribute "name" (escapeIdentifier . objName $ obj)
              . setAttribute "label" (objName obj) -- no escaping for labels in templates needed
              . setAttribute "expAdl" (showA . toExpr . objExp $ obj)
              . setAttribute "source" (idWithoutType . source . objExp $ obj)
              . setAttribute "target" (idWithoutType . target . objExp $ obj)
              . setAttribute "crudC" (objCrudC obj)
              . setAttribute "crudR" (objCrudR obj)
              . setAttribute "crudU" (objCrudU obj)
              . setAttribute "crudD" (objCrudD obj)
              . setAttribute "crud" (crudsToString . objCrud $ obj)
              . setAttribute "verbose" (loglevel' == LevelDebug)
              . setAttribute "loglevel" (show loglevel')
      case atomicOrBox obj of
        FEAtomic {} -> do
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
            . renderTemplate Nothing template
            $ atomicAndBoxAttrs
        FEBox
          { boxHeader = header,
            boxSubObjs = subObjs
          } -> do
            subObjAttrs <- mapM genView_SubObject subObjs

            parentTemplate <- readTemplate $ "Box-" <> T.unpack (btType header) <.> "html"

            return . indentation
              . T.lines
              . renderTemplate (Just . btKeys $ header) parentTemplate
              $ atomicAndBoxAttrs
                . setAttribute "isRoot" (depth == 0)
                . setAttribute "subObjects" subObjAttrs
    FEObjT {} -> pure []
  where
    indentation :: [Text] -> [Text]
    indentation = map (T.replicate (if depth == 0 then 4 else 16) " " <>)
    genView_SubObject ::
      (HasRunner env, HasDirPrototype env) =>
      FEObject ->
      RIO env SubObjectAttr2
    genView_SubObject subObj =
      case subObj of
        FEObjE {} ->
          do
            lns <- genViewObject fSpec (depth + 1) subObj
            return
              SubObjAttr
                { subObjName = escapeIdentifier $ objName subObj,
                  subObjLabel = objName subObj, -- no escaping for labels in templates needed
                  subObjContents = T.intercalate "\n" lns,
                  subObjExprIsUni = exprIsUni subObj
                }
        FEObjT {} ->
          do
            return
              SubObjAttr
                { subObjName = escapeIdentifier $ objName subObj,
                  subObjLabel = objName subObj,
                  subObjContents = objTxt subObj,
                  subObjExprIsUni = True
                }
    getTemplateForObject ::
      (HasDirPrototype env) =>
      RIO env FilePath
    getTemplateForObject
      | relIsProp obj && (not . exprIsIdent) obj -- special 'checkbox-like' template for propery relations
        =
        return $ "View-PROPERTY" <> ".html"
      | otherwise = getTemplateForConcept . target . objExp $ obj
    getTemplateForConcept ::
      (HasDirPrototype env) =>
      A_Concept ->
      RIO env FilePath
    getTemplateForConcept cpt = do
      exists <- doesTemplateExist cptfn
      return $
        if exists
          then cptfn
          else "Atomic-" <> show ttp <.> "html"
      where
        ttp = cptTType fSpec cpt
        cptfn = "Concept-" <> T.unpack (name cpt) <.> "html"
