-- | A migration context reads the database of a system in production (claim PRF-27, phase 2 of
--   the migration plan in @memorybank/migration/plan.md@).
--
--   A system in production was compiled by itself. Since v5.9.4 such a compilation leaves out a
--   concept table that no query of the system reads. A context that includes the system reads its
--   database through views, so it has to expect exactly the tables and the columns that the
--   compilation by itself created. These tests compile the migration context in
--   @testing/Travis/testcases/Migration/layout@, compile each context it includes by itself
--   (the option @--context@), and compare the two layouts: the tables with their columns,
--   and for every concept the tables and columns in which a row names its atoms.
--
--   The regression runner compares exit codes, and both compilations succeed either way,
--   so these tests look at the layout itself.
module Ampersand.Test.MultiContext.IncludedLayoutTests
  ( doAllIncludedLayoutTests,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.FSpec (FSpec, PlugInfo (..), PlugSQL (..), SqlAttribute (..), plugAttributes, plugInfos)
import Ampersand.FSpec.ToFSpec.CreateFspec (createFspec)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Misc.HasClasses (FSpecGenOpts (..))
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Output.ToJSON.Concepts (TableCols (..), conceptTablesOf)
import Ampersand.Types.Config (HasRunner, extendWith)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import System.FilePath ((</>))

layoutDir :: FilePath
layoutDir = "testing" </> "Travis" </> "testcases" </> "Migration" </> "layout"

-- | The labels under which @migration.adl@ includes a context on a database of its own.
labels :: [Text]
labels = ["old", "orchard", "visits"]

doAllIncludedLayoutTests :: (HasRunner env) => RIO env Bool
doAllIncludedLayoutTests = do
  let root = layoutDir </> "migration.adl"
  gIncluding <- compileAs root ""
  case gIncluding of
    Errors es -> do
      logError . display $ "  " <> T.pack root <> " does not compile: " <> tshow (NE.head es)
      pure False
    Checked including _ -> and <$> traverse (compareWith root including) labels

compileAs :: (HasRunner env) => FilePath -> Text -> RIO env (Guarded FSpec)
compileAs root wanted =
  extendWith ((defFSpecGenOpts (root NE.:| [])) {xcompiledContext = wanted}) createFspec

compareWith :: (HasRunner env) => FilePath -> FSpec -> Text -> RIO env Bool
compareWith root including l = do
  gAlone <- compileAs root l
  case gAlone of
    Errors es -> do
      logError . display $ "  The context " <> l <> " does not compile by itself: " <> tshow (NE.head es)
      pure False
    Checked alone _ -> do
      let differences = tableDifferences alone <> conceptDifferences alone
      if null differences
        then do
          logInfo . display $ "  The context " <> l <> " is read in the layout of its compilation by itself."
          pure True
        else do
          logError . display $ "  The context " <> l <> " is read in another layout than its compilation by itself creates:"
          mapM_ (logError . display . ("    " <>)) differences
          pure False
  where
    prefix = l <> "."
    -- the tables of the context, each with its columns, under the name that the context itself gives the table
    tablesOf :: FSpec -> Set.Set (Text, [Text])
    tablesOf fSpec =
      Set.fromList
        [ (unquote (tshow (sqlname plug)), L.sort [unquote (tshow (attSQLColName att)) | att <- toList (plugAttributes plug)])
          | InternalPlug plug <- plugInfos fSpec
        ]
    tableDifferences :: FSpec -> [Text]
    tableDifferences alone =
      ["the context creates the table " <> tshow t <> ", which the including context does not read" | t <- Set.toList (expected Set.\\ found)]
        <> ["the including context reads the table " <> tshow t <> ", which the context does not create" | t <- Set.toList (found Set.\\ expected)]
      where
        expected = tablesOf alone
        found = Set.fromList [(n, cs) | (t, cs) <- Set.toList (tablesOf including), Just n <- [T.stripPrefix prefix t]]
    conceptDifferences :: FSpec -> [Text]
    conceptDifferences alone =
      [ "the atoms of " <> fullName c <> " are named in " <> tshow mine <> ", and the including context looks for them in " <> tshow theirs
        | c <- Set.toList (concs alone),
          c /= ONE,
          c' <- [c' | c' <- Set.toList (concs including), fullName c' == prefix <> fullName c],
          let mine = map shape (conceptTablesOf alone c),
          let theirs = map (first (\t -> fromMaybe t (T.stripPrefix prefix t)) . shape) (conceptTablesOf including c'),
          mine /= theirs
      ]
    shape tc = (unquote (tclJSONname tc), tclJSONcols tc)
    unquote = T.dropAround (== '"')
