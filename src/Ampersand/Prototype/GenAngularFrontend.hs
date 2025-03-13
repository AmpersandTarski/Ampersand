{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Ampersand.Prototype.GenAngularFrontend
  ( genComponent,
    genSingleFileFromTemplate,
    toPascal,
    toKebab,
    toTypescriptName,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.ProtoUtil
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config
import Data.Maybe (fromJust)
import RIO.Char (toLower, toUpper)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial (splitOn)
import System.FilePath
import Text.StringTemplate (StringTemplate, setAttribute)
import Text.StringTemplate.GenericStandard ()

crudsToString :: Cruds -> Text
crudsToString x = T.pack $ zipWith (curry f) [crudC x, crudR x, crudU x, crudD x] "crud"
  where
    f :: (Bool, Char) -> Char
    f (b, c) = (if b then toUpper else toLower) c

genComponent :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponent fspec ifc = do
  unless (isApi ifc) $ genComponentTs fspec ifc
  unless (isApi ifc) $ genComponentView fspec ifc
  genComponentInterface fspec ifc
  logInfo . display $ "Generated files for " <> ifcNamePascal ifc <> "Component"

genComponentView :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponentView fSpec interf = do
  let templateFilePath = "component.html"
  let targetFilePath = T.unpack (ifcNameKebab interf) </> T.unpack (ifcNameKebab interf) <> ".component.html"
  genComponentFileFromTemplate fSpec interf genHTMLView templateFilePath targetFilePath

genComponentTs :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponentTs fSpec interf = do
  let templateFilePath = "component.ts.txt"
  let targetFilePath = T.unpack (ifcNameKebab interf) </> T.unpack (ifcNameKebab interf) <> ".component.ts"
  genComponentFileFromTemplate fSpec interf templateFunction templateFilePath targetFilePath
  where
    templateFunction _ _ _ = pure ""

genComponentInterface :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genComponentInterface fSpec interf = do
  let templateFilePath = "component.interface.ts.txt"
  let targetFilePath = T.unpack (ifcNameKebab interf) </> T.unpack (ifcNameKebab interf) <> ".interface.ts"
  genComponentFileFromTemplate fSpec interf genTypescriptInterface templateFilePath targetFilePath

type FEObjectTemplateFunction = forall env. (HasRunner env, HasDirPrototype env) => FSpec -> Int -> FEObject -> RIO env Text

genComponentFileFromTemplate :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> FEObjectTemplateFunction -> FilePath -> FilePath -> RIO env ()
genComponentFileFromTemplate fSpec interf templateFunction templateFilePath targetFilePath = do
  template <- readTemplate templateFilePath
  runner <- view runnerL
  let loglevel' = logLevel runner
  lns <- templateFunction fSpec 0 (feiObj interf)
  let contents =
        T.intercalate "\n" -- intercalate, because unlines introduces a trailing \n
          . concatMap indentEOL
          . T.lines
          . renderTemplate Nothing template
          $ setAttribute "contextName" (addSlashes . fullName $ fSpec)
            . setAttribute "isSessionInterface" (isSessionInterface interf)
            . setAttribute "roles" (map show . feiRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcName" (ifcName interf)
            . setAttribute "ifcNamePascal" (ifcNamePascal interf)
            . setAttribute "ifcNameKebab" (ifcNameKebab interf)
            . setAttribute "ifcLabel" (ifcLabel interf) -- no escaping for labels in templates needed
            . setAttribute "expAdl" (showA . toExpr . ifcExp $ interf)
            . setAttribute "exprIsUni" (exprIsUni . feiObj $ interf)
            . setAttribute "exprIsTot" (exprIsTot . feiObj $ interf)
            . setAttribute "source" (text1ToText . idWithoutType' . source . ifcExp $ interf)
            . setAttribute "target" (text1ToText . idWithoutType' . target . ifcExp $ interf)
            . setAttribute "crudC" (objCrudC . feiObj $ interf)
            . setAttribute "crudR" (objCrudR . feiObj $ interf)
            . setAttribute "crudU" (objCrudU . feiObj $ interf)
            . setAttribute "crudD" (objCrudD . feiObj $ interf)
            . setAttribute "crud" (crudsToString . objCrud . feiObj $ interf)
            . setAttribute "contents" lns
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (tshow loglevel')
            . setAttribute "templateFilePath" templateFilePath
            . setAttribute "targetFilePath" targetFilePath
  writePrototypeAppFile targetFilePath contents

genSingleFileFromTemplate :: (HasRunner env, HasDirPrototype env) => FSpec -> FESpec -> FilePath -> FilePath -> RIO env ()
genSingleFileFromTemplate fSpec feSpec templateFilePath targetFilePath = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  template <- readTemplate templateFilePath
  mapM_ (logDebug . display) (showTemplate template)
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (fullName fSpec)
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcs" (interfaces feSpec) -- all interfaces
            . setAttribute "uis" (filter (not . isApi) $ interfaces feSpec) -- only the interfaces that need UI
            . setAttribute "apis" (filter isApi $ interfaces feSpec) -- only the interfaces that have API (no UI)
            . setAttribute "concepts" (concepts feSpec)
            . setAttribute "views" (views feSpec)
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
            . setAttribute "templateFilePath" templateFilePath
            . setAttribute "targetFilePath" targetFilePath
  writePrototypeAppFile targetFilePath contents

objectAttributes :: FEObject -> LogLevel -> StringTemplate String -> StringTemplate String
objectAttributes obj loglevel =
  setAttribute "exprIsUni" (exprIsUni obj)
    . setAttribute "exprIsTot" (exprIsTot obj)
    . setAttribute "name" (escapeIdentifier' . objName $ obj)
    . setAttribute "label" (objName obj) -- no escaping for labels in templates needed
    . setAttribute "expAdl" (showA . toExpr . objExp $ obj)
    . setAttribute "source" (text1ToText . idWithoutType' . source . objExp $ obj)
    . setAttribute "target" (text1ToText . idWithoutType' . target . objExp $ obj)
    . setAttribute "crudC" (objCrudC obj)
    . setAttribute "crudR" (objCrudR obj)
    . setAttribute "crudU" (objCrudU obj)
    . setAttribute "crudD" (objCrudD obj)
    . setAttribute "crud" (crudsToString . objCrud $ obj)
    . setAttribute "verbose" (loglevel == LevelDebug)
    . setAttribute "loglevel" (tshow loglevel)

escapeIdentifier' :: Text -> Text
escapeIdentifier' txt = case T.uncons txt of
  Nothing -> mempty
  Just _ -> text1ToText . escapeIdentifier . toText1Unsafe $ txt

-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr = SubObjAttr
  { subObjName :: Text,
    subObjLabel :: Text,
    subObjContents :: Text,
    subObjExprIsUni :: Bool
  }
  deriving (Show, Data, Typeable)

subObjectAttributes :: (HasRunner env, HasDirPrototype env) => FSpec -> Int -> FEObjectTemplateFunction -> FEObject -> RIO env SubObjectAttr
subObjectAttributes fSpec depth templateFunction subObj = do
  lns <- templateFunction fSpec (depth + 1) subObj
  case subObj of
    FEObjE {} ->
      return
        SubObjAttr
          { subObjName = escapeIdentifier' $ objName subObj,
            subObjLabel = objName subObj, -- no escaping for labels in templates needed
            subObjContents = lns,
            subObjExprIsUni = exprIsUni subObj
          }
    FEObjT {} ->
      return
        SubObjAttr
          { subObjName = escapeIdentifier' $ objName subObj,
            subObjLabel = objName subObj,
            subObjContents = lns,
            subObjExprIsUni = True
          }

genHTMLView :: FEObjectTemplateFunction
genHTMLView fSpec depth obj =
  case obj of
    FEObjE {} -> do
      runner <- view runnerL
      case atomicOrBox obj of
        FEAtomic {} -> do
          conceptTemplate <- getTemplateForObject
          let (templateFilename, _) = fromMaybe (conceptTemplate, []) (objMPrimTemplate . atomicOrBox $ obj) -- Atomic is the default template
          template <- readTemplate templateFilename

          return
            . T.intercalate eol
            . T.lines
            . renderTemplate Nothing template
            $ objectAttributes obj (logLevel runner)
        FEBox
          { boxHeader = header,
            boxSubObjs = subObjs
          } -> do
            subObjAttrs <- mapM (subObjectAttributes fSpec depth genHTMLView) subObjs

            parentTemplate <- readTemplate $ "Box-" <> (T.unpack . text1ToText . btType) header <.> "html"

            return
              . indentSubStructure
              . renderTemplate (Just . btKeys $ header) parentTemplate
              $ objectAttributes obj (logLevel runner)
                . setAttribute "isRoot" (depth == 0)
                . setAttribute "subObjects" subObjAttrs
    FEObjT {} -> pure $ "<span>" <> objTxt obj <> "</span>"
  where
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
        cptfn = "Concept-" <> show (name cpt) <.> "html"

-- This function is a helper function to add indentation using the EOL character sequence
-- Let us explain why! For the html view generator we are using HStringTemplates recursively;
-- other templates are used inside the 'blanks' of parent templates. The subtemplates need to be
-- indentated properly. However, the level of indentation (number of spaces) is determined by
-- the position in its parent templates, all the way up to the outmost template.
-- The solution we came up with:
--   * After rendering each (sub)template we replace the newlines (\n) by a EOL character sequence, resulting in a single line
--   * When this result is rendered in a parent template, we end up with a multi-line text with the sub results still on single lines
--   * For each line in the text, we post process the line, splitting based on EOL character and prefixing the lines (except the first)
--   * The resulting text is indented correctly
indentEOL :: Text -> [Text]
indentEOL x = case Partial.splitOn eol x of
  [] -> []
  (h : tl) ->
    h :
    map (prefix <>) tl
  where
    prefix = T.takeWhile (== ' ') x

indentSubStructure :: Text -> Text
indentSubStructure = T.intercalate eol . concatMap indentEOL . T.lines

eol :: Text
eol = "<<EOL>>"

prefixAllLines :: Text -> Text -> Text
prefixAllLines prefix txt = case T.lines txt of
  [] -> txt
  list -> T.intercalate "\n" $ map (prefix <>) list -- intercalate, because unlines introduces a trailing \n

genTypescriptInterface :: FEObjectTemplateFunction
genTypescriptInterface fSpec depth obj =
  case obj of
    FEObjE {} -> do
      runner <- view runnerL
      case atomicOrBox obj of
        FEAtomic {} -> return . indentSubStructure $ typescriptTypeForFEAtomic
        FEBox
          { boxHeader = header,
            boxSubObjs = subObjs
          } -> do
            subObjAttrs <- mapM (subObjectAttributes fSpec depth genTypescriptInterface) subObjs

            return
              . indentSubStructure
              . renderTemplate (Just . btKeys $ header) boxTemplate
              $ objectAttributes obj (logLevel runner)
                . setAttribute "isRoot" (depth == 0)
                . setAttribute "subObjects" subObjAttrs
    FEObjT {} -> pure $ "'" <> objTxt obj <> "'"
  where
    tgtCpt = target . objExp $ obj
    boxTemplate
      | exprIsUni obj = newTemplate (conceptIdWithImportAlias tgtCpt <> " & {\n  _view_: " <> addViewDefinition <> ";$subObjects:{subObj|\n  $subObj.subObjName$: $subObj.subObjContents$;}$\n}") "compiler"
      | otherwise = newTemplate ("Array<\n  " <> conceptIdWithImportAlias tgtCpt <> " & {\n    _view_: " <> addViewDefinition <> ";$subObjects:{subObj|\n    $subObj.subObjName$: $subObj.subObjContents$;}$\n  }\n>") "compiler"

    -- This is a mapping from FEAtomic to Typescript types
    -- When expression is not univalent 'Array<T>' wrapped around the type
    typescriptTypeForFEAtomic :: Text
    typescriptTypeForFEAtomic
      | relIsProp obj && (not . exprIsIdent) obj = "boolean" -- property expressions that are not ident map to Typescript boolean type
      | exprIsUni obj = typescriptTypeForConcept tgtCpt -- for univalent expressions use the Typescript type for target concept
      | cptTType fSpec tgtCpt == Object -- for non-uni Object expressions wrap Array<T> with newlines around Typescript type
        =
        "Array<\n"
          <> prefixAllLines "  " (typescriptTypeForConcept tgtCpt)
          <> "\n>"
      | otherwise = "Array<" <> typescriptTypeForConcept tgtCpt <> ">" -- otherwise simply wrap Array<T>
    typescriptTypeForConcept :: A_Concept -> Text
    typescriptTypeForConcept cpt = case cptTType fSpec cpt of
      Object ->
        conceptIdWithImportAlias cpt
          <> " & {\n"
          <> prefixAllLines "  " ("_view_: " <> addViewDefinition <> ";")
          <> "\n}"
      _ -> conceptIdWithImportAlias cpt

    addViewDefinition :: Text
    addViewDefinition
      | isJust maybeViewDef = viewIdWithImportAlias . fromJust $ maybeViewDef
      | otherwise = "undefined"
      where
        maybeViewDef = viewDef . atomicOrBox $ obj

    conceptIdWithImportAlias :: A_Concept -> Text
    conceptIdWithImportAlias cpt = "concepts." <> (toTypescriptName . text1ToText . idWithoutType' $ cpt)

    viewIdWithImportAlias :: ViewDef -> Text
    viewIdWithImportAlias viewDef' = "views." <> (toTypescriptName . toPascal . fullName $ viewDef') <> "View"

toKebab :: Text -> Text
toKebab = T.intercalate "-" . fmap T.toLower . T.words

toPascal :: Text -> Text
toPascal = T.concat . map wordCase . T.words

toTypescriptName :: Text -> Text
toTypescriptName = T.map dotToUnderscore
  where
    dotToUnderscore :: Char -> Char
    dotToUnderscore '.' = '_'
    dotToUnderscore x = x

wordCase :: Text -> Text
wordCase txt = case T.uncons txt of
  Nothing -> mempty
  Just (x, xs) -> T.cons (toUpper x) (T.toLower xs)
