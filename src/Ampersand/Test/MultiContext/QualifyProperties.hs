-- | Property tests for joining the contexts of a system (@CONTEXT A INCLUDES B@).
--
--   The compiler joins the context it compiles with every context that this one reaches.
--   A thing of another context gets the label of that context as a prefix ('flattenSystem').
--   The properties below state, for arbitrary contexts:
--
--   * giving every name a prefix ('qualifyContext') changes nothing else, identifies no two things,
--     and yields disjoint names for two prefixes;
--   * a context that includes nothing is joined to itself;
--   * an included context contributes its names with the prefix, and nothing else;
--   * a context that is reached along two paths contributes its names once;
--   * the renamed context can be printed and parsed again.
--
--   They are the executable counterparts of the theorems on references
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
import qualified RIO.Map as Map
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
    ("The concept names are among the names.", property prop_conceptNames),
    ("A context that includes nothing is joined to itself.", property prop_noInclusion),
    ("An included context contributes its names with the prefix.", property prop_included),
    ("A context that is reached along two paths contributes its names once.", property prop_diamond)
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

-- | A system of contexts for a test: the viewer comes first, and an edge is includer, included and alias.
systemOf :: [(Text, P_Context)] -> [(Text, Text, NamePart)] -> System
systemOf nodes edges =
  System
    { sysViewer = key (maybe "" fst (listToMaybe nodes)),
      sysNodes = Map.fromList [(key nm, SystemNode (key nm) ctx) | (nm, ctx) <- nodes],
      sysEdges =
        Map.fromListWith
          (flip (<>))
          [(key from, [SystemEdge OriginUnknown (key target) alias (Just alias)]) | (from, target, alias) <- edges]
    }
  where
    key nm = (T.unpack nm <> ".adl", nm)

-- | A context without declarations, with the name and the language of the given one.
emptied :: P_Context -> P_Context
emptied ctx =
  ctx
    { ctx_pats = [],
      ctx_rs = [],
      ctx_ds = [],
      ctx_cs = [],
      ctx_ks = [],
      ctx_rrules = [],
      ctx_reprs = [],
      ctx_vs = [],
      ctx_gs = [],
      ctx_ifcs = [],
      ctx_ps = [],
      ctx_pops = [],
      ctx_metas = [],
      ctx_enfs = []
    }

-- | The purposes of the interfaces of an included context are not joined.
--   The interfaces themselves are, until the types have been checked.
withoutInterfaces :: P_Context -> P_Context
withoutInterfaces ctx =
  ctx
    { ctx_ps = filter (not . isInterfacePurpose) (ctx_ps ctx),
      ctx_pats = [pat {pt_xps = filter (not . isInterfacePurpose) (pt_xps pat)} | pat <- ctx_pats ctx]
    }
  where
    isInterfacePurpose p = case pexObj p of
      PRef2Interface _ -> True
      _ -> False

joined :: System -> (P_Context -> Property) -> Property
joined sys check = case flattenSystem sys of
  Errors err -> counterexample (show (NE.toList err)) False
  Checked ctx _ -> check ctx

prop_noInclusion :: P_Context -> Property
prop_noInclusion ctx =
  joined (systemOf [("V", ctx)] []) $ \result ->
    conjoin
      [ namesOf result === namesOf ctx,
        length (ctx_metas result) === length (ctx_metas ctx)
      ]

prop_included :: Alias -> P_Context -> Property
prop_included (Alias a) ctx =
  joined (systemOf [("V", emptied ctx), ("J", ctx)] [("V", "J", a)]) $ \result ->
    Set.fromList (namesOf result) === Set.fromList (namesOf (qualifyContext [a] (withoutInterfaces ctx)))

prop_diamond :: Alias -> Alias -> Alias -> P_Context -> Property
prop_diamond (Alias b) (Alias c) (Alias d) ctx =
  b
    /= c
    ==> joined
      ( systemOf
          [("V", emptied ctx), ("B", emptied ctx), ("C", emptied ctx), ("D", ctx)]
          [("V", "B", b), ("V", "C", c), ("B", "D", d), ("C", "D", d)]
      )
      $ \result ->
        conjoin
          [ counterexample "the names of the shared context" $ case Set.toList (Set.map (take 1 . nameSpaceOf) (Set.filter (not . isReservedName) (Set.fromList (namesOf result)))) of
              prefixes -> property (length prefixes <= 1),
            counterexample "the relations of the shared context" (length (ctx_ds result) <= length (ctx_ds ctx))
          ]
