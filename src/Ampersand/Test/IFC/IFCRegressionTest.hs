{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end IFC regression test (handoff WP5).
--
-- This is the committed regression for the 45 buildingSMART examples. Unlike the
-- WP3 binder test (which calls the binder with a fixed-path schema) and the WP4
-- wiring test (which checks the bundled-schema selection), this test exercises the
-- __exact same path as @ampersand check foo.ifc@__: it runs every example through
-- 'parseFilesTransitive' (the real file dispatch, including @FILE_SCHEMA@-based
-- schema selection from the bundled resource) and then through the type-checker
-- 'pCtx2aCtx', reporting a genuine per-example pass/fail with the real 'CtxError'
-- message on failure.
--
-- == Result and the pixel-texture diagnosis ==
--
-- All 45 examples pass (45/45). The handoff predicted 44/45, with
-- @tessellation-with-pixel-texture@ failing and — in the Python prototype — an
-- /empty/ error message. That failure does not occur here:
--
--   * The prototype failed because of its @selects='isa'@ typology blow-up (a
--     universal synthetic root over every SELECT), which this binder avoids by
--     modelling SELECT as a relation (single inheritance, small typologies).
--   * @IfcPixelTexture@ itself is unremarkable to the binder: its @Pixel : LIST
--     [1:?] OF IfcBinary@ becomes a @pixel[IfcPixelTexture*IfcRawBinary]@ relation;
--     because relations are sets, the many identical @"0FF000000"@ binaries
--     collapse to a single pair. The resulting context type-checks cleanly.
--   * The "empty message" symptom was a property of the prototype's reporting, not
--     of the data. Here, were any example to fail, this test prints the actual
--     'CtxError' text (see 'reportFailure'), so a regression can never be silent.
module Ampersand.Test.IFC.IFCRegressionTest
  ( ifcRegressionTest,
  )
where

import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Input.ADL1.CtxError (CtxError, Guarded (..))
import Ampersand.Input.Parsing (parseFilesTransitive)
import Ampersand.Misc.HasClasses (Roots (..))
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.FilePath (takeExtension, takeFileName, (</>))
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

examplesDir :: FilePath
examplesDir = "testing" </> "STEP" </> "examples"

-- | Per-example outcome.
data Outcome
  = Passed
  | -- | failed, with the real error messages
    Failed [Text]

-- | Run the end-to-end IFC regression over all 45 examples. Returns 'True' when
-- every example parses-and-type-checks via the real compiler path.
ifcRegressionTest :: (HasRunner env) => RIO env Bool
ifcRegressionTest = do
  logInfo "Starting IFC end-to-end regression (45 examples)."
  dirThere <- doesDirectoryExist examplesDir
  if not dirThere
    then do
      logError $ "IFC fixtures not found at " <> display (T.pack examplesDir)
      pure False
    else do
      entries <- listDirectory examplesDir
      let files = L.sort [examplesDir </> e | e <- entries, takeExtension e == ".ifc"]
      outcomes <- mapM checkExample files
      let fails = [(f, msgs) | (f, Failed msgs) <- outcomes]
          nPass = length [() | (_, Passed) <- outcomes]
      logInfo
        . display
        $ "IFC regression: "
        <> tshow nPass
        <> "/"
        <> tshow (length files)
        <> " examples pass end-to-end."
      mapM_ reportFailure fails
      if null fails
        then do
          logInfo "✅ Passed: IFC end-to-end regression (45/45)."
          pure True
        else do
          logError "❗❗❗ Failed: IFC end-to-end regression."
          pure False

-- | Run one example through the real dispatch + type-checker.
checkExample :: (HasRunner env) => FilePath -> RIO env (FilePath, Outcome)
checkExample fp = do
  let fSpecGenOpts = defFSpecGenOpts (fp NE.:| [])
  outcome <- extendWith fSpecGenOpts $ do
    -- parseFilesTransitive dispatches on the .ifc extension exactly like
    -- `ampersand check`, selecting the bundled schema from the FILE_SCHEMA header.
    (_, gCtx) <- parseFilesTransitive (Roots (fp NE.:| []))
    case gCtx of
      Errors es -> pure (Failed (errTexts (NE.toList es)))
      Checked pCtx _ -> do
        env <- ask
        pure $ case pCtx2aCtx env pCtx of
          Checked _ _ -> Passed
          Errors es -> Failed (errTexts (NE.toList es))
  pure (fp, outcome)

-- | Render parser/type-checker errors to text (the real messages, never empty).
errTexts :: [CtxError] -> [Text]
errTexts = map tshow

reportFailure :: (HasLogFunc env) => (FilePath, [Text]) -> RIO env ()
reportFailure (fp, msgs) = do
  logError . display $ "  " <> T.pack (takeFileName fp) <> " failed:"
  mapM_ (logError . display . ("    " <>)) (if null msgs then ["(no message)"] else msgs)
