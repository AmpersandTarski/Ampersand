-- | Unit tests of the compile-time cost profile (issue #1692): one miniature
--   context in which every route class of contract DC-17 occurs, classified
--   through the real pipeline (parse → typecheck → 'makeFSpec' → 'conjNF'),
--   so the tests see exactly the violation terms the SQL generator receives.
--
--   The negative case is as load-bearing as the positive ones: a @UNI@
--   relation whose source is a specialization has no SQL uniqueness
--   constraint on its key column, so it must /not/ classify as structural
--   (claim PRF-8 covers only DDL-enforced keys).
module Ampersand.Test.Incremental.CostProfileTests
  ( doAllCostProfileTests,
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.FSpec.FSpec (allConjuncts)
import Ampersand.FSpec.Incremental.CostProfile
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | One conjunct family per route class, plus the structural counterexample.
--
--   The interfaces are load-bearing: they give @Script@ and @Account@ the
--   @Object@ representation, and only an Object-represented table key gets
--   the SQL primary key (issue #341) on which 'structurallyEnforced' rests.
--   Without them every concept is scalar, no table has a uniqueness
--   constraint, and nothing may classify structural.
model :: Text
model =
  T.unlines
    [ "CONTEXT CostProfileTest IN ENGLISH",
      "CLASSIFY Sub ISA Script",
      "RELATION content[Script*ScriptContent] [UNI]",
      "RELATION owner[Script*Account] [INJ]",
      "RELATION subattr[Sub*ScriptContent] [UNI]",
      "RELATION follows[Script*Script]",
      "INTERFACE Scripts : I[Script] cRud BOX [ content : content ]",
      "INTERFACE Accounts : I[Account] cRud BOX [ owns : owner~ ]",
      "RULE trans : follows+ |- follows",
      "RULE anch : \"x\"[Script];follows |- follows",
      "RULE scanr : follows;follows |- follows",
      "ENDCONTEXT"
    ]

doAllCostProfileTests :: (HasRunner env) => RIO env Bool
doAllCostProfileTests = do
  let fSpecGenOpts = defFSpecGenOpts ("CostProfileTest.adl" NE.:| [])
  extendWith fSpecGenOpts $ case parseCtx "CostProfileTest.adl" model of
    Errors es -> failWith ("could not parse its context: " <> tshow (NE.toList es))
    Checked (pCtx, _) _ -> do
      env <- ask
      case pCtx2aCtx env pCtx of
        Errors es -> failWith ("could not typecheck its context: " <> tshow (NE.toList es))
        Checked aCtx _ -> do
          let fSpec = makeFSpec env aCtx
              profilesFor whichRule =
                [ costProfileFor fSpec conj (conjNF env (notCpl (rcConjunct conj)))
                  | conj <- allConjuncts fSpec,
                    any whichRule (NE.toList (rc_orgRules conj))
                ]
              failures = lefts (map (check profilesFor) cases)
          forM_ failures $ \msg ->
            logError . display $ "❗❗❗ Failed: cost-profile case " <> msg
          if null failures
            then do
              logInfo "✅ Passed: every conjunct classifies to the route class DC-17 expects."
              pure True
            else pure False
  where
    failWith msg = do
      logError . display $ "❗❗❗ Failed: cost-profile test " <> msg
      pure False

-- | (case label, conjunct selector, expectation on each of its profiles).
cases :: [(Text, Rule -> Bool, CostProfile -> Bool)]
cases =
  [ ( "UNI on a primary-key column is structural, no scan tables",
      propRuleOn Uni "content",
      \p -> cpClass p == Structural && null (cpScanTables p)
    ),
    ( "INJ stored flipped on a primary-key column is structural",
      propRuleOn Inj "owner",
      \p -> cpClass p == Structural && null (cpScanTables p)
    ),
    ( "UNI on a specialization column is NOT structural but a scan",
      propRuleOn Uni "subattr",
      \p -> cpClass p == Scan && not (null (cpScanTables p))
    ),
    ( "a Kleene term is recursive, scan tables listed",
      namedRule "trans",
      \p -> cpClass p == Recursive && not (null (cpScanTables p))
    ),
    ( "a term pinned to an atom is anchored",
      namedRule "anch",
      \p -> cpClass p == Anchored
    ),
    ( "a plain composition is a scan over its tables",
      namedRule "scanr",
      \p -> cpClass p == Scan && not (null (cpScanTables p))
    )
  ]

check ::
  ((Rule -> Bool) -> [CostProfile]) ->
  (Text, Rule -> Bool, CostProfile -> Bool) ->
  Either Text ()
check profilesFor (lbl, whichRule, expectation) =
  case profilesFor whichRule of
    [] -> Left (lbl <> ": no conjunct found for its rule")
    ps
      | all expectation ps -> Right ()
      | otherwise -> Left (lbl <> ": got " <> tshow ps)

-- | Select the generated property rule of the given kind on the relation
--   with the given (local) name.
propRuleOn :: AProp -> Text -> Rule -> Bool
propRuleOn prp relName rule = case rrkind rule of
  Propty p rel -> p == prp && fullName rel == relName
  _ -> False

namedRule :: Text -> Rule -> Bool
namedRule nm rule = fullName rule == nm
