-- | Property tests for the renaming behind @INCLUDE "file" AS alias@.
--
--   An included context contributes to the including context with all of its names
--   prefixed by the alias ('qualifyContext'). The properties below state, for arbitrary
--   contexts, what makes that contribution a /disjoint/ union:
--
--   * every name gets the prefix, and nothing else changes;
--   * the renaming is injective, so it identifies no two things;
--   * two different aliases yield names that have nothing in common,
--     apart from the names that belong to the Ampersand system itself;
--   * a nested include (@x@ inside @y@) yields the names @y.x.n@;
--   * the renamed context can be printed and parsed again.
--
--   They are the executable counterparts of the theorem "qualified names suffice"
--   in the Lean session @proofs/multicontext@ (proof-track claim PRF-11).
module Ampersand.Test.MultiContext.QualifyProperties
  ( doAllQualifyPropertyTests,
  )
where

import Ampersand.ADL1.PrettyPrinters (prettyPrint)
import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Input.Qualify
import Ampersand.Test.Parser.ArbitraryTree ()
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Test.QuickCheck

doAllQualifyPropertyTests :: (HasLogFunc env) => RIO env Bool
doAllQualifyPropertyTests = and <$> traverse run properties
  where
    run :: (HasLogFunc env) => (Text, Property) -> RIO env Bool
    run (nm, prop) = do
      result <- liftIO $ quickCheckWithResult checkArgs prop
      if isSuccess result
        then logInfo . display $ "\x2705 Passed: " <> nm
        else do
          logError . display $ "\x2757\x2757\x2757 Failed: " <> nm
          -- The end of the counterexample, where the explanation is: an arbitrary context can be very large.
          logError . display . T.takeEnd 3000 . T.pack . output $ result
      pure (isSuccess result)
    checkArgs =
      stdArgs
        { maxSuccess = 100,
          maxSize = 15, -- the same size as the parser round trip, to keep the suite quick
          chatty = False
        }

properties :: [(Text, Property)]
properties =
  [ ("An empty prefix changes no name.", property prop_emptyPrefix),
    ("Every name gets the prefix, except the names of the Ampersand system.", property prop_everyNameQualified),
    ("Qualifying identifies no two names.", property prop_injective),
    ("Two aliases yield disjoint names.", property prop_disjoint),
    ("A nested include yields the names of both aliases.", property prop_nested),
    ("The name of the context and the kinds of names are preserved.", property prop_preserves),
    ("A qualified context can be printed and parsed again.", property prop_roundtrip),
    ("The concept names are among the names.", property prop_conceptNames)
  ]

-- | An alias as a script can write it. The reserved name spaces are left out,
--   because the compiler refuses them as an alias.
newtype Alias = Alias NamePart
  deriving (Show)

instance Arbitrary Alias where
  arbitrary = elements (map mk ["old", "new", "Reg", "Lib", "x", "Copy2"])
    where
      mk txt = case try2Namepart txt of
        Right np -> Alias np
        _ -> fatal ("Not a valid alias in the test: " <> txt)

-- | All names that the traversal visits, in the order of the traversal.
namesOf :: P_Context -> [Name]
namesOf = getConst . traverseNames (\_ nm -> Const [nm])

qualified :: [NamePart] -> Name -> Name
qualified ns nm
  | isReservedName nm = nm
  | otherwise = withNameSpace ns nm

prop_emptyPrefix :: P_Context -> Property
prop_emptyPrefix ctx = namesOf (qualifyContext [] ctx) === namesOf ctx

prop_everyNameQualified :: Alias -> P_Context -> Property
prop_everyNameQualified (Alias a) ctx =
  namesOf (qualifyContext [a] ctx) === map (qualified [a]) (namesOf ctx)

prop_injective :: Alias -> P_Context -> Property
prop_injective (Alias a) ctx =
  Set.size (Set.fromList (namesOf (qualifyContext [a] ctx))) === Set.size (Set.fromList (namesOf ctx))

prop_disjoint :: Alias -> Alias -> P_Context -> Property
prop_disjoint (Alias a) (Alias b) ctx =
  a
    /= b
    ==> counterexample
      ("shared names: " <> show (Set.toList shared))
      (all isReservedName shared)
  where
    shared = namesIn a `Set.intersection` namesIn b
    namesIn x = Set.fromList (namesOf (qualifyContext [x] ctx))

prop_nested :: Alias -> Alias -> P_Context -> Property
prop_nested (Alias outer) (Alias inner) ctx =
  namesOf (qualifyContext [outer] (qualifyContext [inner] ctx)) === namesOf (qualifyContext [outer, inner] ctx)

prop_preserves :: Alias -> P_Context -> Property
prop_preserves (Alias a) ctx =
  conjoin
    [ counterexample "the name of the context changed" (ctx_nm ctx' === ctx_nm ctx),
      counterexample "the kind of a name changed" (map nameType (namesOf ctx') === map nameType (namesOf ctx)),
      counterexample "the local part of a name changed" (map localName (namesOf ctx') === map localName (namesOf ctx)),
      counterexample "the number of declarations changed" (sizes ctx' === sizes ctx)
    ]
  where
    ctx' = qualifyContext [a] ctx
    sizes c =
      [ length (ctx_pats c),
        length (ctx_rs c),
        length (ctx_ds c),
        length (ctx_cs c),
        length (ctx_ks c),
        length (ctx_rrules c),
        length (ctx_reprs c),
        length (ctx_vs c),
        length (ctx_gs c),
        length (ctx_ifcs c),
        length (ctx_ps c),
        length (ctx_pops c),
        length (ctx_metas c),
        length (ctx_enfs c)
      ]

-- | Printing a qualified context yields a script that the parser accepts,
--   and in which the same names occur.
prop_roundtrip :: Alias -> P_Context -> Property
prop_roundtrip (Alias a) ctx =
  case parseCtx "qualified context" (prettyPrint ctx') of
    Errors err -> counterexample (T.unpack . T.unlines $ tshow (NE.toList err) : T.lines (prettyPrint ctx')) False
    Checked (parsed, _) _ ->
      counterexample
        ("names that the printed script lost: " <> show (Set.toList (expected `Set.difference` found)))
        . counterexample ("names that the printed script gained: " <> show (Set.toList (found `Set.difference` expected)))
        $ found
        == expected
      where
        found = Set.fromList (map fullName (namesOf (withoutContainers parsed)))
        expected = Set.fromList (map fullName (namesOf (withoutContainers ctx')))
  where
    ctx' = qualifyContext [a] ctx
    -- A concept definition records the pattern in which it was written.
    -- A script does not print that name: the parser derives it from the place of the definition.
    -- An arbitrary context can record a pattern that does not exist, so the comparison leaves it out.
    -- The same holds for the source and target of a population that was read from a spreadsheet.
    withoutContainers c =
      c
        { ctx_cs = map forget (ctx_cs c),
          ctx_pops = map untyped (ctx_pops c),
          ctx_pats = [pat {pt_cds = map forget (pt_cds pat), pt_pop = map untyped (pt_pop pat)} | pat <- ctx_pats c]
        }
      where
        forget cd = cd {cdfrom = CONTEXT (ctx_nm c)}
        untyped pop = case pop of
          P_RelPopu {} -> pop {p_src = Nothing, p_tgt = Nothing}
          P_CptPopu {} -> pop

prop_conceptNames :: P_Context -> Property
prop_conceptNames ctx =
  counterexample
    (show (L.sort (Set.toList (conceptNamesOf ctx))))
    (conceptNamesOf ctx `Set.isSubsetOf` Set.fromList (namesOf ctx))
