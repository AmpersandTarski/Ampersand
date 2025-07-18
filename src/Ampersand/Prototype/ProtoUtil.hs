{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Prototype.ProtoUtil
  ( getGenericsDir,
    writePrototypeAppFile,
    copyDirRecursively,
    removeAllDirectoryFiles,
    getProperDirectoryContents,
    escapeIdentifier,
    commentBlock,
    strReplace,
    addSlashes,
    indentBlock,
    phpIndent,
    showPhpStr,
    escapePhpStr,
    writeFile,
    FESpec (..),
    FEConcept (..),
    FEInterface (..),
    FEView (..),
    FEViewSegment (..),
    FEAtomicOrBox (..),
    FEObject (..),
    FEExpression (..),
    toExpr,
    fromExpr,
    doesTemplateExist,
    Template (),
    readTemplate,
    renderTemplate,
    showTemplate,
    newTemplate,
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Misc.Defaults (defaultDirPrototype)
import Ampersand.Misc.HasClasses
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Text as T
import Text.StringTemplate
  ( StringTemplate,
    Stringable,
    checkTemplateDeep,
    newSTMP,
    render,
    setAttribute,
    toString,
  )
import Text.StringTemplate.GenericStandard ()

data FESpec = FESpec
  { interfaces :: [FEInterface],
    concepts :: [FEConcept],
    views :: [FEView]
  }
  deriving (Typeable, Data)

-- | data object that contains information about an interface, from the
--   perspective of the generated frontend
data FEInterface = FEInterface
  { ifcName :: Text,
    ifcNameKebab :: Text,
    ifcNamePascal :: Text,
    ifcLabel :: Text,
    ifcExp :: FEExpression,
    isApi :: Bool,
    isSessionInterface :: Bool, -- True if the source concept is the session. (then the interface will show a list of all)
    srcConcept :: Text,
    feiRoles :: [Role],
    feiObj :: FEObject
  }
  deriving (Typeable, Data)

data FEConcept = FEConcept
  { cptId :: Text,
    cptIdTmp :: Text, -- Temporarily, only for namespace phase 1. The `.` in names has no semantics yet, but will have that later. `.` in Typescript is a valid separtor, but for now it must not be used.
    typescriptType :: Text
  }
  deriving (Typeable, Data)

data FEView = FEView
  { viewId :: !Text,
    viewIdTmp :: Text, -- Temporarily, only for namespace phase 1. The `.` in names has no semantics yet, but will have that later. `.` in Typescript is a valid separtor, but for now it must not be used.
    viewSegments :: ![FEViewSegment],
    viewIsEmpty :: !Bool
  }
  deriving (Typeable, Data)

data FEViewSegment = FEViewSegment
  { segmentLabel :: !Text,
    segmentTypescriptType :: !Text
  }
  deriving (Typeable, Data)

data FEObject
  = FEObjE
      { objName :: Text,
        objLabel :: Maybe Text,
        objExp :: FEExpression,
        objCrud :: Cruds,
        objCrudC :: Bool,
        objCrudR :: Bool,
        objCrudU :: Bool,
        objCrudD :: Bool,
        exprIsUni :: Bool,
        exprIsTot :: Bool,
        relIsProp :: Bool, -- True iff the term is a kind of simple relation and that relation is a property.
        exprIsIdent :: Bool,
        atomicOrBox :: FEAtomicOrBox
      }
  | FEObjT
      { objName :: Text,
        objTxt :: Text
      }
  deriving (Show, Data, Typeable)

-- | distinguish FEExpression from Expression, to avoid accidents. The source/target
--   of a FEExpression is not allways the same as the source/target of the Expression.
data FEExpression = FEExpression
  { feExpr :: !Expression,
    feSign :: !Signature,
    femRelation :: !(Maybe Relation)
  }
  deriving (Data)

instance HasSignature FEExpression where
  sign = feSign

instance Show FEExpression where
  show = show . feExpr

toExpr :: FEExpression -> Expression
toExpr = feExpr

fromExpr :: Expression -> FEExpression
fromExpr expr =
  FEExpression
    { feExpr = expr,
      feSign = Sign src tgt,
      femRelation = mRel
    }
  where
    (src, mRel, tgt) = case getExpressionRelation expr of
      Nothing -> (source expr, Nothing, target expr)
      Just (declSrc, decl, declTgt, _) -> (declSrc, Just decl, declTgt)

-- if the term is a relation, use the (possibly narrowed type) from getExpressionRelation

-- | The part to render at the 'end' of the term
data FEAtomicOrBox
  = FEAtomic
      { objMPrimTemplate ::
          Maybe
            ( FilePath, -- the absolute path to the template
              [Text] -- the attributes of the template
            ),
        viewDef :: Maybe ViewDef
      }
  | FEBox
      { boxHeader :: HTMLtemplateCall,
        boxSubObjs :: [FEObject],
        viewDef :: Maybe ViewDef
      }
  deriving (Show, Data, Typeable)

writePrototypeAppFile ::
  (HasDirPrototype env, HasLogFunc env) =>
  FilePath ->
  Text ->
  RIO env ()
writePrototypeAppFile relFilePath content = do
  env <- ask
  logDebug $ "  Generating " <> display (T.pack relFilePath)
  let filePath = getAppDir env </> relFilePath
  liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
  writeFileUtf8 filePath content

writeFile :: (HasLogFunc env) => FilePath -> BL.ByteString -> RIO env ()
writeFile filePath content = do
  logDebug $ "  Generating " <> display (T.pack filePath)
  liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
  BL.writeFile filePath content

-- Copy entire directory tree from srcBase/ to tgtBase/, overwriting existing files, but not emptying existing directories.
-- NOTE: tgtBase specifies the copied directory target, not its parent
-- NOTE: directories with extension .proto are excluded. This would compromise regression tests,
--       where '.proto' is the default output directory (if not specified)
copyDirRecursively ::
  (HasLogFunc env) =>
  FilePath ->
  FilePath ->
  RIO env ()
copyDirRecursively srcBase tgtBase
  | srcBase == tgtBase =
      mapM_
        logError
        [ "Are you kidding me? I got the instruction to copy ",
          "     " <> display (T.pack srcBase),
          "  to itself!"
        ]
  | otherwise = do
      srcBaseA <- liftIO $ makeAbsolute srcBase
      tgtBaseA <- liftIO $ makeAbsolute tgtBase
      mapM_
        logDebug
        [ "Recursively copying ",
          "     " <> display (T.pack srcBaseA),
          "  to " <> display (T.pack tgtBaseA)
        ]
      copy ("." </> tgtBase) ""
  where
    copy shouldSkip fileOrDirPth = do
      let srcPath = srcBase </> fileOrDirPth
          tgtPath = tgtBase </> fileOrDirPth
      isDir <- liftIO $ doesDirectoryExist srcPath
      if isDir
        then
          if srcPath == shouldSkip
            then do
              logDebug $ "Skipping " <> display (T.pack srcPath) <> " because it is the target directory of the recursive copy action."
            else
              if takeExtension srcPath == defaultDirPrototype
                then do
                  logDebug $ "Skipping " <> display (T.pack srcPath) <> " because its extension is excluded by design" -- This is because of regression tests. (See what happend at https://travis-ci.org/AmpersandTarski/Ampersand/jobs/621565925 )
                else do
                  logDebug $ " Copying dir... " <> display (T.pack srcPath)
                  logDebug $ "      to dir... " <> display (T.pack tgtPath)
                  fOrDs <- getProperDirectoryContents srcPath
                  liftIO $ createDirectoryIfMissing True tgtPath
                  mapM_ (\fOrD -> copy shouldSkip $ fileOrDirPth </> fOrD) fOrDs
        else do
          logDebug $ "  file... " <> display (T.pack fileOrDirPth)
          liftIO $ copyFile srcPath tgtPath

-- Remove all files in directory dirPath, but don't enter subdirectories (for which a warning is emitted.)
removeAllDirectoryFiles ::
  (HasLogFunc env) =>
  FilePath ->
  RIO env ()
removeAllDirectoryFiles dirPath = do
  dirContents <- getProperDirectoryContents dirPath
  mapM_ removeDirectoryFile dirContents
  where
    removeDirectoryFile path =
      do
        let absPath = dirPath </> path
        isDir <- liftIO $ doesDirectoryExist absPath
        if isDir
          then logInfo $ "WARNING: directory '" <> display (T.pack dirPath) <> "' contains a subdirectory '" <> display (T.pack path) <> "' which is not cleared."
          else liftIO $ removeFile absPath

getProperDirectoryContents :: FilePath -> RIO env [String]
getProperDirectoryContents pth =
  filter (`notElem` [".", "..", ".svn"])
    <$> liftIO (getDirectoryContents pth)

commentBlock :: [String] -> [String]
commentBlock ls =
  ["/*" <> replicate lnth '*' <> "*\\"]
    <> ["* " <> strReplace "*/" "**" line <> replicate (lnth - length line) ' ' <> " *" | line <- ls]
    <> ["\\*" <> replicate lnth '*' <> "*/"]
  where
    lnth = foldl' max 0 (map length ls)

indentBlock :: Int -> [String] -> [String]
indentBlock i = map (replicate i ' ' <>)

strReplace :: String -> String -> String -> String
strReplace _ _ "" = ""
strReplace "" _ str = str
strReplace src dst inp =
  process inp
  where
    n = length src
    process "" = ""
    process st@(c : cs)
      | src `L.isPrefixOf` st = dst <> process (drop n st)
      | otherwise = c : process cs

phpIndent :: Int -> Text
phpIndent i
  | i < 0 = T.pack " " -- space instead of \n
  | otherwise = T.pack $ '\n' : replicate i ' '

addSlashes :: Text -> Text
addSlashes = T.pack . addSlashes' . T.unpack
  where
    addSlashes' ('\'' : cs) = "\\'" <> addSlashes' cs
    addSlashes' ('"' : cs) = "\\\"" <> addSlashes' cs
    addSlashes' ('\\' : cs) = "\\\\" <> addSlashes' cs
    addSlashes' (c : cs) = c : addSlashes' cs
    addSlashes' "" = ""

showPhpStr :: Text -> Text
showPhpStr txt = q <> escapePhpStr txt <> q
  where
    q = T.singleton '\''

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr :: Text -> Text
escapePhpStr txt =
  case T.uncons txt of
    Nothing -> mempty
    Just ('\'', s) -> "\\'" <> escapePhpStr s
    Just ('\\', s) -> "\\\\" <> escapePhpStr s
    Just (c, s) -> T.cons c $ escapePhpStr s

-- TODO: better abstraction for specific template and fallback to default
doesTemplateExist :: (HasDirPrototype env) => FilePath -> RIO env Bool
doesTemplateExist templatePath = do
  env <- ask
  let absPath = getTemplateDir env </> templatePath
  liftIO $ doesFileExist absPath

------ Utility functions
-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate FilePath) FilePath

newTemplate :: Text -> FilePath -> Template
newTemplate txt = Template (newSTMP . T.unpack $ txt)

readTemplate ::
  (HasDirPrototype env) =>
  FilePath ->
  RIO env Template
readTemplate templatePath = do
  env <- ask
  let absPath = getTemplateDir env </> templatePath
  res <- readFileUtf8 absPath
  case res of
    Left err ->
      exitWith
        . ReadFileError
        $ [ "Error while reading frontend template.",
            "   The generation of frontend files can only be done when the frontend",
            "   templates are available. This is normally done by the prototype image.",
            "   If you only want to generate the backend files, you can use the --no-frontend option."
          ]
        <> err
    Right cont -> return $ Template (newSTMP . T.unpack $ cont) absPath

renderTemplate :: Maybe [TemplateKeyValue] -> Template -> (StringTemplate String -> StringTemplate String) -> Text
renderTemplate userAtts (Template template absPath) setRuntimeAtts =
  case checkTemplateDeep appliedTemplate of
    ([], [], []) -> T.pack $ render appliedTemplate
    (parseErrs@(_ : _), _, _) ->
      templateError
        . T.concat
        $ [ T.pack $ "Parse error in " <> tmplt <> " " <> err <> "\n"
            | (tmplt, err) <- parseErrs
          ]
    ([], attrs@(_ : _), _)
      | isJust userAtts -> T.pack . render . fillInTheBlanks (L.nub attrs) $ appliedTemplate
      | otherwise ->
          templateError
            $ "The following attributes are expected by the template, but not supplied: "
            <> tshow attrs
    ([], [], ts@(_ : _)) ->
      templateError
        $ "Missing invoked templates: "
        <> tshow ts -- should not happen as we don't invoke templates
  where
    templateError msg =
      exitWith
        $ ReadFileError
          [ "*** TEMPLATE ERROR in:" <> T.pack absPath,
            msg
          ]
    appliedTemplate = setRuntimeAtts . setUserAtts (fromMaybe [] userAtts) $ template
    -- Set all attributes not specified to False
    fillInTheBlanks :: [String] -> StringTemplate String -> StringTemplate String
    fillInTheBlanks [] = id
    fillInTheBlanks (h : tl) = setAttribute h False . fillInTheBlanks tl
    setUserAtts :: [TemplateKeyValue] -> (StringTemplate String -> StringTemplate String)
    setUserAtts = foldl' fun id
      where
        fun :: (Stringable b) => (StringTemplate b -> StringTemplate b) -> TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
        fun soFar keyVal = soFar . doAttribute keyVal
        doAttribute :: (Stringable b) => TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
        doAttribute h = case tkval h of
          Nothing -> setAttribute (T.unpack . text1ToText $ tkkey h) True
          Just val -> setAttribute (T.unpack . text1ToText $ tkkey h) val

showTemplate :: Template -> [Text]
showTemplate (Template a b) =
  T.lines
    . T.intercalate "\n"
    $ ("Template (" <> T.pack b <> ")")
    : ["  " <> T.pack (toString a)]
