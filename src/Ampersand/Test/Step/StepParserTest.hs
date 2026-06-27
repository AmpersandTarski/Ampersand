-- | Acceptance tests for the STEP / Part-21 reader (handoff §4-WP2).
--
--   * Parses all 45 example @.ifc@ fixtures under @testing\/STEP\/examples@ without error.
--   * Round-trip sanity on @wall-extrusion.ifc@: pins the instance count and a few known values.
--
--   These tests are schema-agnostic; they exercise only 'Ampersand.Input.Step.Parser'
--   and do not depend on EXPRESS or @P_Context@.
module Ampersand.Test.Step.StepParserTest
  ( stepParserTest,
  )
where

import Ampersand.Basics
import Ampersand.Input.Step.Parser
import Ampersand.Input.Step.Types
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.FilePath (takeExtension, (</>))
import qualified RIO.List as L
import qualified RIO.Text as T

-- | Directory (relative to the repo root) holding the committed STEP fixtures.
examplesDir :: FilePath
examplesDir = "testing" </> "STEP" </> "examples"

-- | Run all STEP-reader acceptance tests. Returns 'True' when everything passes.
stepParserTest :: (HasLogFunc env) => RIO env Bool
stepParserTest = do
  logInfo "Starting STEP/Part-21 reader tests."
  okDir <- doesDirectoryExist examplesDir
  if not okDir
    then do
      logError $ "STEP test fixtures not found at " <> display (T.pack examplesDir)
      pure False
    else do
      okAll <- parseAllExamples
      okWall <- wallExtrusionSanity
      let ok = okAll && okWall
      if ok
        then logInfo "✅ Passed: STEP/Part-21 reader."
        else logError "❗❗❗ Failed: STEP/Part-21 reader."
      pure ok

-- | Parse every @.ifc@ fixture; fail if any file errors out.
parseAllExamples :: (HasLogFunc env) => RIO env Bool
parseAllExamples = do
  entries <- listDirectory examplesDir
  let files = L.sort [examplesDir </> e | e <- entries, takeExtension e == ".ifc"]
  results <- mapM tryParse files
  let failures = [m | Left m <- results]
      n = length files
  if null failures
    then do
      logInfo . display $ "Parsed all " <> tshow n <> " STEP examples without error."
      pure True
    else do
      mapM_ (logError . display) failures
      pure False
  where
    tryParse fp = do
      e <- parseStepFile fp
      case e of
        Left errs -> pure (Left ("Cannot read " <> T.pack fp <> ": " <> T.unwords errs))
        Right [] -> pure (Left ("No instances parsed from " <> T.pack fp))
        Right insts ->
          -- Force the instances so any lazy parse error surfaces here.
          insts `seq` length insts `seq` pure (Right ())

-- | Known-answer checks on @wall-extrusion.ifc@ (cross-checked against @spf2json.py@).
wallExtrusionSanity :: (HasLogFunc env) => RIO env Bool
wallExtrusionSanity = do
  let fp = examplesDir </> "wall-extrusion.ifc"
  e <- parseStepFile fp
  case e of
    Left errs -> do
      logError . display $ "wall-extrusion.ifc could not be read: " <> T.unwords errs
      pure False
    Right insts -> do
      let checks :: [(Text, Bool)]
          checks =
            [ ("instance count is 46", length insts == 46),
              ( "first instance is #1 IFCGEOMETRICREPRESENTATIONCONTEXT",
                case insts of
                  (i : _) -> siId i == "#1" && siType i == "IFCGEOMETRICREPRESENTATIONCONTEXT"
                  _ -> False
              ),
              ("#303 is IFCWALL with the expected GlobalId", wall303 insts),
              ("#2 is a cartesian point with a nested coordinate list", point2 insts),
              ("enum .ADDED. parsed as SVEnum \"ADDED\"", enumAdded insts),
              ("null markers $ and * parsed as SVNull", hasNulls insts)
            ]
          fails = [lbl | (lbl, ok) <- checks, not ok]
      if null fails
        then do
          logInfo "wall-extrusion.ifc round-trip sanity OK."
          pure True
        else do
          mapM_ (\lbl -> logError . display $ "wall-extrusion check failed: " <> lbl) fails
          pure False
  where
    find iid = L.find ((== iid) . siId)

    -- #303 = IFCWALL('0DWgwt6o1FOx7466fPk$jl', #56, $, $, $, #306, #318, $, $)
    wall303 insts = case find "#303" insts of
      Just i ->
        siType i == "IFCWALL"
          && case siArgs i of
            (SVStr g : _) -> g == "0DWgwt6o1FOx7466fPk$jl"
            _ -> False
      Nothing -> False

    -- #2 = IFCCARTESIANPOINT((0.0, 0.0, 0.0))
    point2 insts = case find "#2" insts of
      Just i ->
        siType i == "IFCCARTESIANPOINT"
          && case siArgs i of
            [SVList xs] -> xs == [SVReal 0.0, SVReal 0.0, SVReal 0.0]
            _ -> False
      Nothing -> False

    -- #56 = IFCOWNERHISTORY(#51, #54, $, .ADDED., 1454575675, $, $, 1454575675)
    enumAdded insts = case find "#56" insts of
      Just i -> SVEnum "ADDED" `elem` siArgs i
      Nothing -> False

    -- #1 = IFCGEOMETRICREPRESENTATIONCONTEXT($, 'Model', 3, 0.0001, #3, $)
    hasNulls insts = case find "#1" insts of
      Just i -> case siArgs i of
        (SVNull : SVStr "Model" : SVInt 3 : _) -> True
        _ -> False
      Nothing -> False
