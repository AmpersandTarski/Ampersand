-- | Tests of which concepts get a table (issues #1672, #1716 and #1727).
--
--   A typology is stored as a whole or not at all. A typology that stores no
--   relation, that no term enumerates and that holds values gets no table,
--   because such a table would administrate atoms that nothing reads. A
--   typology that holds objects is always stored. And when a typology falls
--   apart into several tables (@REPRESENT ... TYPE MULTITABLE@), every concept
--   of it has a place in one of them, whether or not a term names the concept:
--   the table of a specialisation is the only place that records which atoms
--   belong to it.
--
--   The regression runner compares exit codes, and these models compile with
--   exit code 0 either way. So the tests look at the layout itself, through
--   the real pipeline (parse → typecheck → 'makeFSpec').
module Ampersand.Test.Multitable.StorageLayoutTests
  ( doAllStorageLayoutTests,
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.AbstractSyntaxTree (largerConcepts)
import Ampersand.FSpec.FSpec (FSpec, PlugSQL (..), lookupCpt)
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | A model, and for some of its concepts the table that has to hold the
--   concept in a column of its own: @Just@ the name of that table, or @Nothing@
--   for a concept that has no table.
data Case = Case
  { caseLabel :: Text,
    caseModel :: [Text],
    caseExpect :: [(Text, Maybe Text)]
  }

cases :: [Case]
cases =
  [ Case
      "a specialisation that occurs in CLASSIFY only has a table of its own"
      (marked [])
      [("A", Just "A"), ("B", Just "B"), ("C", Just "C")],
    Case
      "a specialisation that only has a population has a table of its own"
      (marked ["POPULATION B CONTAINS [ \"b1\" ]"])
      [("A", Just "A"), ("B", Just "B"), ("C", Just "C")],
    Case
      "a specialisation that is only the target of a relation has a table of its own"
      ( marked
          [ "RELATION x[A*B]",
            "POPULATION x CONTAINS [ (\"a1\",\"b2\") ]",
            "RULE r : x~;x |- naam;naam~",
            "ROLE Tester MAINTAINS r"
          ]
      )
      [("A", Just "A"), ("B", Just "B"), ("C", Just "C")],
    Case
      "a specialisation below a marked concept that is itself below a marked concept has a table of its own"
      [ "CONTEXT Nested",
        "CLASSIFY M ISA A",
        "CLASSIFY B, C ISA M",
        "REPRESENT A, M TYPE MULTITABLE",
        "RELATION naam[A*Text] [UNI]",
        "RELATION rm[M*Text] [UNI]",
        "RELATION rc[C*Text] [UNI]",
        "POPULATION B CONTAINS [ \"b1\" ]",
        "ENDCONTEXT"
      ]
      [("A", Just "A"), ("M", Just "M"), ("B", Just "B"), ("C", Just "C")],
    Case
      "a marked typology is stored even when it stores no relation and no term names it"
      [ "CONTEXT BareMarked",
        "CLASSIFY B ISA A",
        "REPRESENT A TYPE MULTITABLE",
        "POPULATION B CONTAINS [ \"b1\" ]",
        "ENDCONTEXT"
      ]
      [("A", Just "A"), ("B", Just "B")],
    Case
      "without the mark, a specialisation is a column in the table of its typology"
      (filter (not . T.isPrefixOf "REPRESENT") (marked []))
      [("A", Just "A"), ("B", Just "A"), ("C", Just "A")],
    Case
      "a typology of values that stores no relation and that no term names has no table (issue #1672)"
      [ "CONTEXT BareValues",
        "CLASSIFY B ISA A",
        "POPULATION B CONTAINS [ \"b1\" ]",
        "ENDCONTEXT"
      ]
      [("A", Nothing), ("B", Nothing)],
    Case
      "a value that is only the target of a relation has no table (issue #1672)"
      (marked [])
      [("Text", Nothing)]
  ]
  where
    -- The smallest model of issue #1727: B and C are stored apart from A, and
    -- nothing but the classification names B.
    marked :: [Text] -> [Text]
    marked extra =
      [ "CONTEXT Marked",
        "CLASSIFY B ISA A",
        "CLASSIFY C ISA A",
        "REPRESENT A TYPE MULTITABLE",
        "RELATION naam[A*Text] [UNI]",
        "RELATION rc[C*Text] [UNI]"
      ]
        <> extra
        <> ["ENDCONTEXT"]

doAllStorageLayoutTests :: (HasRunner env) => RIO env Bool
doAllStorageLayoutTests = do
  results <- mapM runCase cases
  pure (and results)

runCase :: (HasRunner env) => Case -> RIO env Bool
runCase c = do
  let fileName = "StorageLayoutTest.adl"
      fSpecGenOpts = defFSpecGenOpts (fileName NE.:| [])
  extendWith fSpecGenOpts $ case parseCtx fileName (T.unlines (caseModel c)) of
    Errors es -> failWith ("could not be parsed: " <> tshow (NE.toList es))
    Checked (pCtx, _) _ -> do
      env <- ask
      case pCtx2aCtx env pCtx of
        Errors es -> failWith ("could not be typechecked: " <> tshow (NE.toList es))
        Checked aCtx _ ->
          let fSpec = makeFSpec env aCtx
           in case mapMaybe (wrong fSpec) (caseExpect c) <> borrowed fSpec of
                [] -> do
                  logInfo . display $ "✅ Passed: " <> caseLabel c
                  pure True
                problems -> failWith (T.intercalate "; " problems)
  where
    failWith msg = do
      logError . display $ "❗❗❗ Failed: " <> caseLabel c <> ": " <> msg
      pure False

-- | The name of the table that holds the concept in a column of its own.
ownTable :: FSpec -> A_Concept -> Maybe Text
ownTable fSpec cpt = case lookupCpt fSpec cpt of
  (plug, _) : _ -> Just (tshow (sqlname plug))
  [] -> Nothing

wrong :: FSpec -> (Text, Maybe Text) -> Maybe Text
wrong fSpec (nm, expected) =
  case [cpt | cpt <- toList (concs fSpec), fullName cpt == nm] of
    [] -> Just ("the model has no concept " <> nm)
    cpt : _
      | fmap bare found == fmap bare expected -> Nothing
      | otherwise -> Just (nm <> " is in " <> maybe "no table" id found <> ", expected " <> maybe "no table" id expected)
      where
        found = ownTable fSpec cpt
  where
    -- The name of a table is compared without regard to case and to the quotes around it.
    bare = T.toLower . T.filter (/= '"')

-- | No concept may be without a place of its own while a generalisation of it
--   has one: a runtime that is pointed to the table of the generalisation counts
--   every atom of the generalisation as an atom of the concept (issue #1727).
--   A union concept is the exception: it is read from the tables of its members.
borrowed :: FSpec -> [Text]
borrowed fSpec =
  [ fullName cpt <> " has no place of its own, while its generalisation " <> fullName gen <> " has a table"
    | cpt <- toList (concs fSpec),
      cpt /= ONE,
      isNothing (ownTable fSpec cpt),
      isNothing (unionMembersIn (conceptUnions fSpec) cpt),
      gen <- largerConcepts cpt,
      isJust (ownTable fSpec gen)
  ]
