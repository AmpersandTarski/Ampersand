{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance test for the IFC binder (handoff WP3).
--
-- Runs 'ifc2PContext' over the 45 buildingSMART example files in
-- @testing\/STEP\/examples@ and feeds every resulting 'P_Context' through the
-- compiler's type-checker ('pCtx2aCtx').
--
-- == Result: all 45 type-check (handoff predicted 44/45) ==
--
-- The handoff expected @tessellation-with-pixel-texture@ to fail, based on the
-- Python prototype. That prototype failed because of its @selects='isa'@ typology
-- blow-up (a universal synthetic root over every SELECT). This binder makes the
-- opposite, mandated choice — __SELECT becomes a relation, not ISA__, keeping
-- single inheritance and small typologies — which removes that failure at the
-- root, so all 45 examples (pixel-texture included) type-check cleanly. The test
-- therefore asserts 45/45; a regression to fewer is a real failure.
--
-- The EXPRESS schema is loaded from 'defaultSchemaPath' (the IFC4.3 schema on the
-- author's machine). That file is not in the repository, so when it is absent the
-- whole test is skipped with success (same convention as the EXPRESS reader test).
module Ampersand.Test.IFC.IFCBinderTest
  ( ifcBinderTest,
  )
where

import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.IFC.IFCAnalyze (defaultSchemaPath, ifc2PContext)
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import RIO.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import RIO.FilePath (takeExtension, takeFileName, (</>))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | Directory holding the committed STEP fixtures (relative to the repo root).
examplesDir :: FilePath
examplesDir = "testing" </> "STEP" </> "examples"

-- | Run the IFC-binder acceptance test. Returns 'True' on success.
ifcBinderTest :: (HasRunner env) => RIO env Bool
ifcBinderTest = do
  logInfo "Starting IFC binder tests."
  schemaThere <- doesFileExist defaultSchemaPath
  dirThere <- doesDirectoryExist examplesDir
  if not schemaThere
    then do
      logWarn $
        "Skipping IFC binder test: EXPRESS schema not found at "
          <> display (T.pack defaultSchemaPath)
      pure True
    else
      if not dirThere
        then do
          logError $ "IFC fixtures not found at " <> display (T.pack examplesDir)
          pure False
        else runOverExamples

runOverExamples :: (HasRunner env) => RIO env Bool
runOverExamples = do
  entries <- listDirectory examplesDir
  let files = L.sort [examplesDir </> e | e <- entries, takeExtension e == ".ifc"]
  results <- mapM checkOne files
  let passes = [f | (f, True) <- results]
      fails = [f | (f, False) <- results]
      failNames = map takeFileName fails
  logInfo . display $
    "IFC binder: " <> tshow (length passes) <> " type-checked, "
      <> tshow (length fails)
      <> " failed ("
      <> T.intercalate ", " (map T.pack failNames)
      <> ")."
  -- Acceptance: every example must type-check. The SELECT-as-relation design makes
  -- all 45 pass (the handoff's predicted pixel-texture failure does not occur; see
  -- the module header). Any failure is therefore a real regression.
  if null fails
    then do
      logInfo . display $
        "✅ Passed: IFC binder (" <> tshow (length passes) <> "/" <> tshow (length results) <> " type-check)."
      pure True
    else do
      logError . display $
        "❗ IFC type-check failures: " <> T.intercalate ", " (map T.pack failNames)
      pure False

-- | Type-check one example. 'True' when the resulting 'P_Context' type-checks.
checkOne :: (HasRunner env) => FilePath -> RIO env (FilePath, Bool)
checkOne fp = do
  gCtx <- ifc2PContext fp
  case gCtx of
    Errors _ -> do
      logError . display $ "Could not build P_Context for " <> T.pack fp
      pure (fp, False)
    Checked pCtx _ -> do
      -- Type-check the in-memory P_Context with an environment that carries
      -- FSpecGenOpts (required by 'pCtx2aCtx') on top of the runner.
      let fSpecGenOpts = defFSpecGenOpts (fp NE.:| [])
      checked <- extendWith fSpecGenOpts $ do
        env <- ask
        pure $ case pCtx2aCtx env pCtx of
          Checked _ _ -> True
          Errors _ -> False
      pure (fp, checked)
