{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance tests for the EXPRESS schema reader (WP1).
--
-- These tests parse the three real IFC EXPRESS schemas (IFC2x3, IFC4,
-- IFC4.3) and check counts and a golden attribute list for @IfcWall@.
--
-- The schema files are not part of the Ampersand repository; they live under
-- the handoff's @BIM/bronnen/ifc-schemas@ tree. When a schema file is absent
-- (e.g. on CI), the corresponding check is skipped with a warning rather than
-- failing the suite.
module Ampersand.Test.Express.ExpressParserTest
  ( expressParserTest,
  )
where

import Ampersand.Basics
import Ampersand.Input.Express.Parser (parseExpressFile)
import Ampersand.Input.Express.Schema
import RIO.Directory (doesFileExist)
import qualified RIO.Map as Map
import qualified RIO.Text as T

-- | Root of the IFC EXPRESS schemas on the author's machine (handoff §6).
schemaRoot :: FilePath
schemaRoot =
  "/Users/stef/Library/CloudStorage/GoogleDrive-stefjoosten1@gmail.com/"
    <> "Mijn Drive/cloudDrive/Rijksvastgoedbedrijf/BIM/bronnen/ifc-schemas"

ifc43, ifc4, ifc2x3 :: FilePath
ifc43 = schemaRoot <> "/IFC4.3/IFC4X3_ADD2.exp"
ifc4 = schemaRoot <> "/IFC4/IFC4.exp"
ifc2x3 = schemaRoot <> "/IFC2x3/IFC2X3_TC1.exp"

-- | Run all EXPRESS-reader acceptance checks. Returns 'True' on success.
expressParserTest :: (HasLogFunc env) => RIO env Bool
expressParserTest = do
  logInfo "Starting EXPRESS schema reader tests."
  results <-
    sequence
      [ test43,
        sanityParse "IFC4" ifc4,
        sanityParse "IFC2x3" ifc2x3
      ]
  let ok = and results
  if ok
    then logInfo "✅ EXPRESS reader tests passed."
    else logError "❗ EXPRESS reader tests failed."
  pure ok

-- | The main IFC4.3 acceptance test: counts in the right ballpark and the
-- golden @IfcWall@ attribute list.
test43 :: (HasLogFunc env) => RIO env Bool
test43 = withSchema "IFC4.3" ifc43 $ \schema -> do
  let nEnt = Map.size (esEntities schema)
      nTyp = Map.size (esTypes schema)
  logInfo $ "IFC4.3: " <> display nEnt <> " entities, " <> display nTyp <> " types."
  let countOk =
        esName schema
          == "IFC4X3_ADD2"
          && nEnt
          == 876
          && nTyp
          == 436
  unless countOk
    $ logError
    $ "IFC4.3 counts off: name="
    <> display (esName schema)
    <> " entities="
    <> display nEnt
    <> " (expected 876) types="
    <> display nTyp
    <> " (expected 436)"
  wallOk <- checkWall schema
  pure (countOk && wallOk)

-- | Golden test on @IfcWall@: the full (inherited) attribute list must match
-- names, target types, optional and aggregate flags. Compared against the
-- reference @ifc_express.py@ output.
checkWall :: (HasLogFunc env) => ExpressSchema -> RIO env Bool
checkWall schema = do
  let actual =
        [ (atName a, atTarget a, atOptional a, isJust (atAggregate a))
          | a <- fullAttrs schema "IfcWall"
        ]
  if actual == expectedWall
    then do
      logInfo "IfcWall golden attribute list matches."
      pure True
    else do
      logError "IfcWall golden attribute list MISMATCH:"
      logError $ "  expected: " <> displayShow expectedWall
      logError $ "  actual  : " <> displayShow actual
      pure False

-- | Expected full attribute list of @IfcWall@:
-- (name, target concept, optional, isAggregate).
expectedWall :: [(Text, Text, Bool, Bool)]
expectedWall =
  [ ("GlobalId", "IfcGloballyUniqueId", False, False),
    ("OwnerHistory", "IfcOwnerHistory", True, False),
    ("Name", "IfcLabel", True, False),
    ("Description", "IfcText", True, False),
    ("ObjectType", "IfcLabel", True, False),
    ("ObjectPlacement", "IfcObjectPlacement", True, False),
    ("Representation", "IfcProductRepresentation", True, False),
    ("Tag", "IfcIdentifier", True, False),
    ("PredefinedType", "IfcWallTypeEnum", True, False)
  ]

-- | Parse a schema and report a rough count; succeed as long as parsing does
-- not error (handoff: "parses also IFC4 and IFC2x3 without crashing").
sanityParse :: (HasLogFunc env) => Text -> FilePath -> RIO env Bool
sanityParse lbl fp = withSchema lbl fp $ \schema -> do
  logInfo
    $ display lbl
    <> ": "
    <> display (Map.size (esEntities schema))
    <> " entities, "
    <> display (Map.size (esTypes schema))
    <> " types (name "
    <> display (esName schema)
    <> ")."
  pure True

-- | Parse a schema file, skipping (with success) if the file is absent.
withSchema ::
  (HasLogFunc env) =>
  Text ->
  FilePath ->
  (ExpressSchema -> RIO env Bool) ->
  RIO env Bool
withSchema lbl fp k = do
  exists <- doesFileExist fp
  if not exists
    then do
      logWarn $ "Skipping " <> display lbl <> ": file not found: " <> display (T.pack fp)
      pure True
    else do
      parsed <- parseExpressFile fp
      case parsed of
        Left err -> do
          logError $ "Failed to parse " <> display lbl <> ": " <> display (T.pack err)
          pure False
        Right schema -> k schema
