{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ampersand.Test.Parser.QuickChecks
  ( doAllQuickCheckPropertyTests,
    TestResult (..),
  )
where

import Ampersand.ADL1.PrettyPrinters (prettyPrint)
import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree (A_RoleIsa (..), denoteRoleTerm, roleHierarchy)
import Ampersand.Core.ParseTree (P_Context (..), P_Interface (..), P_RoleIsa (..), Role, Traced (..))
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Test.Parser.ArbitraryTree ()
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Test.QuickCheck

-- | An application specific data type that enables nice error
--   messages in the output.
data TestResult = TestResult
  { qcPropName :: !Text,
    qcIsSuccess :: !Bool,
    qcMessage :: ![Text],
    qcQuickCheckResult :: !Result
  }

doAllQuickCheckPropertyTests :: (HasLogFunc env) => RIO env Bool
doAllQuickCheckPropertyTests =
  whileSuccess
    [ doRoundtripTest,
      doObjectIdShorteningTest,
      doRoleTermTest
      -- More tests can be inserted here
    ]
  where
    whileSuccess :: (HasLogFunc env) => [RIO env TestResult] -> RIO env Bool
    whileSuccess [] = do
      logInfo "All tests succeeded."
      pure True
    whileSuccess (h : tl) = do
      res <- h
      if qcIsSuccess res
        then do
          logInfo . display $ "✅ Passed: " <> qcPropName res
          whileSuccess tl
        else do
          logInfo . display $ "❗❗❗Failed: " <> qcPropName res
          pure False

doRoundtripTest :: RIO env TestResult
doRoundtripTest = do
  qcResult <- liftIO . quickCheckWithResult checkArgs $ prop_parserRoundtrip
  pure
    TestResult
      { qcPropName = "Prettyprint/Parser roundtrip.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["---Some message---"],
        qcQuickCheckResult = qcResult
      }
  where
    checkArgs :: Args
    checkArgs =
      Args
        { replay = Nothing,
          maxSuccess = 100,
          maxDiscardRatio = 8,
          maxSize = 15, -- otherwise there's nothing quick about it.
          maxShrinks = 50,
          chatty = True
        }

-- | Known-answer test for OBJECT atom-identity shortening. This pins 'shortenObjectId' to the
--   exact same algorithm as the runtime framework (PrototypeFramework, Atom::setId, OBJECT case),
--   using the shared vector "a" x 300 -> "a" x 243 <> "_003ef1ba9e". If either side diverges, the
--   test suite fails. ('ampersand validate' only checks Haskell-vs-SQL agreement within the
--   compiler, so it cannot catch a hash-digit mismatch against the PHP side; this test does.)
doObjectIdShorteningTest :: RIO env TestResult
doObjectIdShorteningTest = do
  qcResult <- liftIO . quickCheckWithResult stdArgs $ once (conjoin checks)
  pure
    TestResult
      { qcPropName = "Object-id shortening matches the cross-repo contract.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["shortenObjectId must stay identical to PrototypeFramework Atom::setId (OBJECT case)."],
        qcQuickCheckResult = qcResult
      }
  where
    -- Shared known-answer vector with the runtime framework's PHP unit test.
    knownInput = T.replicate 300 "a"
    knownExpected = T.replicate 243 "a" <> "_003ef1ba9e"
    checks :: [Property]
    checks =
      [ counterexample "known-answer vector" (shortenObjectId knownInput === knownExpected),
        counterexample "short id kept verbatim" (shortenObjectId "plain-id" === "plain-id"),
        counterexample "254 chars kept verbatim" (shortenObjectId (T.replicate 254 "a") === T.replicate 254 "a"),
        counterexample "result within 254 chars" (T.length (shortenObjectId knownInput) === 254),
        counterexample
          "idempotent on shortened result"
          (shortenObjectId (shortenObjectId knownInput) === shortenObjectId knownInput),
        counterexample
          "distinct long ids stay distinct"
          (shortenObjectId (T.replicate 260 "x" <> "AAA") =/= shortenObjectId (T.replicate 260 "x" <> "BBB"))
      ]

prop_parserRoundtrip :: P_Context -> Bool
prop_parserRoundtrip pCtx =
  case roundtrip pCtx of
    Checked _ _ -> True
    Errors err ->
      exitWith
        . SomeTestsFailed
        $ T.lines (tshow err)
        <> T.lines (prettyCtx pCtx)

roundtrip :: P_Context -> Guarded P_Context
roundtrip pCtx =
  fst
    <$> parseCtx
      ( "❗❗❗ ERROR: There is something wrong with the parser and/or with the\n"
          <> "           way an arbitrary P_Context is defined. (See ArbitraryTree.hs)\n"
          <> "           Below file at position"
      )
      (prettyCtx pCtx)

prettyCtx :: P_Context -> Text
prettyCtx =
  T.unlines
    . zipWith (curry includeLineNr) [1 ..]
    . T.lines
    . prettyPrint
  where
    includeLineNr :: (Int, Text) -> Text
    includeLineNr (nr, str) = "{-" <> T.replicate (4 - T.length (tshow nr)) "0" <> tshow nr <> "-} " <> str

-- | Known-answer test for role hierarchies and role terms (issue #1376).
--   It parses a script with ROLE ... ISA ... statements and interfaces FOR role terms,
--   and checks the set of roles each interface is for.
--   A role stands for itself and every role that is transitively ISA it.
doRoleTermTest :: RIO env TestResult
doRoleTermTest = do
  qcResult <- liftIO . quickCheckWithResult stdArgs $ once property'
  pure
    TestResult
      { qcPropName = "Role terms after FOR denote the expected roles.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["The roles of an interface follow from its role terms and the ROLE ... ISA ... statements."],
        qcQuickCheckResult = qcResult
      }
  where
    script =
      T.unlines
        [ "CONTEXT RoleTerms",
          "ROLE FunctionalMaintainer, TechnicalMaintainer ISA Maintainer",
          "ROLE Maintainer ISA User",
          "ROLE Auditor ISA User",
          "INTERFACE All FOR User : I[SESSION] BOX [ \"s\" : I ]",
          "INTERFACE NotTechnical FOR User - TechnicalMaintainer : I[SESSION] BOX [ \"s\" : I ]",
          "INTERFACE Both FOR Maintainer /\\ (FunctionalMaintainer \\/ Auditor) : I[SESSION] BOX [ \"s\" : I ]",
          "INTERFACE Either FOR Maintainer \\/ Auditor : I[SESSION] BOX [ \"s\" : I ]",
          "INTERFACE OnlyUser FOR (User - Maintainer) - Auditor : I[SESSION] BOX [ \"s\" : I ]",
          "INTERFACE Listed FOR Auditor, TechnicalMaintainer - Maintainer, FunctionalMaintainer : I[SESSION] BOX [ \"s\" : I ]",
          "ENDCONTEXT"
        ]
    expected :: [(Text, [Text])]
    expected =
      [ ("All", ["Auditor", "FunctionalMaintainer", "Maintainer", "TechnicalMaintainer", "User"]),
        ("NotTechnical", ["Auditor", "FunctionalMaintainer", "Maintainer", "User"]),
        ("Both", ["FunctionalMaintainer"]),
        ("Either", ["Auditor", "FunctionalMaintainer", "Maintainer", "TechnicalMaintainer"]),
        ("OnlyUser", ["User"]),
        ("Listed", ["Auditor", "FunctionalMaintainer"])
      ]
    property' :: Property
    property' = case fst <$> parseCtx "role term test" script of
      Errors err -> counterexample (T.unpack (tshow err)) False
      Checked pCtx _ ->
        let isas =
              [ A_RoleIsa {arsPos = origin ri, arsSpec = spc, arsGen = risaGen ri}
                | ri <- ctx_risas pCtx,
                  spc <- NE.toList (risaSpecs ri)
              ]
            denoted :: P_Interface -> [Text]
            denoted ifc = map roleName . Set.toList . Set.unions . map (denoteRoleTerm (roleHierarchy isas)) $ ifc_Roles ifc
            actual = [(fullName ifc, denoted ifc) | ifc <- ctx_ifcs pCtx]
         in L.sortOn fst actual === L.sortOn fst expected
    roleName :: Role -> Text
    roleName = fullName
