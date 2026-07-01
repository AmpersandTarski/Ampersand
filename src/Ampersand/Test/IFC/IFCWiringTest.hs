{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance test for the IFC compiler wiring (handoff WP4).
--
-- Unlike the WP3 binder test (which reads the EXPRESS schema from a fixed path on
-- the author's machine), this test proves the __end-to-end, self-contained__ path:
-- the EXPRESS schema is taken from the statically bundled @IFCSchemas@ resource
-- (no external @.exp@), selected via the @.ifc@ file's @FILE_SCHEMA@ header, and
-- the resulting 'P_Context' type-checks.
--
-- It therefore runs everywhere the binary is built (CI included), since the schema
-- is embedded in the executable.
module Ampersand.Test.IFC.IFCWiringTest
  ( ifcWiringTest,
  )
where

import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.IFC.IFCAnalyze
  ( defaultSchemaName,
    fileSchemaName,
    ifc2PContextFromTexts,
  )
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Prototype.StaticFiles_Generated
  ( FileKind (IFCSchemas),
    getStaticFileContent,
  )
import Ampersand.Types.Config (HasRunner, extendWith)
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.FilePath (takeExtension, takeFileName, (</>))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

examplesDir :: FilePath
examplesDir = "testing" </> "STEP" </> "examples"

-- | Run the WP4 wiring acceptance test. Returns 'True' on success.
ifcWiringTest :: (HasRunner env) => RIO env Bool
ifcWiringTest = do
  logInfo "Starting IFC wiring (bundled-schema) tests."
  -- The schema must be bundled for any of this to work.
  case getStaticFileContent IFCSchemas (T.unpack defaultSchemaName <> ".exp") of
    Nothing -> do
      logError
        $ "IFC wiring test: bundled schema "
        <> display defaultSchemaName
        <> ".exp not found in the IFCSchemas resource."
      pure False
    Just schemaBytes -> do
      let schemaText = decodeUtf8 schemaBytes
      dirThere <- doesDirectoryExist examplesDir
      if not dirThere
        then do
          logError $ "IFC fixtures not found at " <> display (T.pack examplesDir)
          pure False
        else runWiring schemaText

runWiring :: (HasRunner env) => Text -> RIO env Bool
runWiring defaultSchemaText = do
  entries <- listDirectory examplesDir
  let files = L.sort [examplesDir </> e | e <- entries, takeExtension e == ".ifc"]
  results <- mapM (checkOne defaultSchemaText) files
  let fails = [f | (f, False) <- results]
      passes = [f | (f, True) <- results]
  logInfo
    . display
    $ "IFC wiring: "
    <> tshow (length passes)
    <> "/"
    <> tshow (length results)
    <> " type-check from the bundled schema."
  if null fails
    then do
      logInfo "✅ Passed: IFC wiring (end-to-end from bundled schema, no external .exp)."
      pure True
    else do
      logError
        . display
        $ "❗ IFC wiring failures: "
        <> T.intercalate ", " (map (T.pack . takeFileName) fails)
      pure False

-- | Bind one example using the bundled schema (selected by its FILE_SCHEMA header)
-- and type-check the resulting context.
checkOne :: (HasRunner env) => Text -> FilePath -> RIO env (FilePath, Bool)
checkOne defaultSchemaText fp = do
  eContent <- readFileUtf8Lenient fp
  case eContent of
    Left _ -> pure (fp, False)
    Right ifcText -> do
      -- Resolve the schema exactly as the compiler dispatch does.
      let schemaText = case fileSchemaName ifcText of
            Just nm
              | nm /= defaultSchemaName ->
                  maybe defaultSchemaText decodeUtf8
                    $ getStaticFileContent IFCSchemas (T.unpack nm <> ".exp")
            _ -> defaultSchemaText
      case ifc2PContextFromTexts (T.pack fp) ifcText schemaText of
        Errors _ -> pure (fp, False)
        Checked pCtx _ -> do
          let fSpecGenOpts = defFSpecGenOpts (fp NE.:| [])
          checked <- extendWith fSpecGenOpts $ do
            env <- ask
            pure $ case pCtx2aCtx env pCtx of
              Checked _ _ -> True
              Errors _ -> False
          pure (fp, checked)
