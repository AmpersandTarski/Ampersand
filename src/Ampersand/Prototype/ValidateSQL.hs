module Ampersand.Prototype.ValidateSQL (validateRulesSQL) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.Prototype.PHP
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator.
-}

validateRulesSQL :: (HasLogFunc env) => FSpec -> RIO env [Text]
validateRulesSQL fSpec = do
  case filter (not . isSignal fSpec . fst) . allViolations $ fSpec of
    [] -> return ()
    viols -> exitWith . ViolationsInDatabase . map toTexts $ viols
      where
        toTexts :: (Rule, AAtomPairs) -> (Text, [Text])
        toTexts (rule, pairs) = (fullName rule, f <$> toList pairs)
          where
            f pair = "(" <> showValADL (apLeft pair) <> ", " <> showValADL (apRight pair) <> ")"

  hSetBuffering stdout NoBuffering

  logDebug "Initializing temporary database"
  succes <- createTempDatabase fSpec
  if succes
    then actualValidation
    else do
      logInfo "Error: Database creation failed. No validation could be done."
      return []
  where
    actualValidation = do
      let allExps =
            getAllInterfaceExps fSpec
              <> getAllRuleExps fSpec
              <> getAllPairViewExps fSpec
              <> getAllIdExps fSpec
              <> getAllViewExps fSpec
      logDebug $ "Number of terms to be validated: " <> displayShow (length allExps)
      results <- mapM (validateExp fSpec (length allExps)) $ zip allExps [1 ..]
      logStickyDone ""
      case [ve | (ve, False) <- results] of
        [] -> do
          logDebug "\nValidation successful.\nWith the provided populations, all generated SQL code has passed validation."
          return []
        ves ->
          return $
            "Validation error. The following terms failed validation:" :
            map showVExp ves

-- functions for extracting all terms from the context

getAllInterfaceExps :: FSpec -> [ValidationExp]
getAllInterfaceExps fSpec =
  concat
    [ getObjExps (name ifc) $ ifcObj ifc
      | ifc <- interfaceS fSpec <> interfaceG fSpec
    ]
  where
    getObjExps iName objDef =
      (objExpression objDef, "interface " <> tshow iName) :
      concatMap (getObjExps iName) (fields objDef)

-- we check the complement of the rule, since that is the term evaluated in the prototype
getAllRuleExps :: FSpec -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp . toList $ vrules fSpec `Set.union` grules fSpec
  where
    getRuleExp rule = (notCpl (formalExpression rule), "rule " <> fullName rule)

getAllPairViewExps :: FSpec -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps . toList $ vrules fSpec `Set.union` grules fSpec
  where
    getPairViewExps r = case rrviol r of
      Nothing -> []
      Just (PairView pvsegs) ->
        [ (expr, "violation view for rule " <> fullName r)
          | PairViewExp _ _ expr <- NE.toList pvsegs
        ]

getAllIdExps :: FSpec -> [ValidationExp]
getAllIdExps fSpec = concatMap getIdExps $ vIndices fSpec
  where
    getIdExps identity =
      [ (objExpression objDef, "identity " <> fullName identity)
        | IdentityExp objDef <- NE.toList $ identityAts identity
      ]

getAllViewExps :: FSpec -> [ValidationExp]
getAllViewExps fSpec = concatMap getViewExps $ vviews fSpec
  where
    getViewExps x =
      [ (expr, "view " <> fullName x)
        | ViewExp expr <- fmap vsmLoad (vdats x)
      ]

type ValidationExp = (Expression, Text)

-- a ValidationExp is an expression together with the place in the context where we
-- obtained it from (e.g. rule/interface/..)
showVExp :: (Expression, Text) -> Text
showVExp (expr, orig) = "Origin: " <> orig <> ", term: " <> showA expr

-- validate a single term and report the results
validateExp ::
  (HasLogFunc env) =>
  FSpec ->
  Int -> -- total amount of terms to be validated (for showing progress)
  ( ValidationExp, -- The expression to be validated
    Int -- The index of the expression (for showing progress)
  ) ->
  RIO env (ValidationExp, Bool)
validateExp fSpec total (vExp, i) = do
  let (e, _) = vExp
  logSticky . display $ "Validating terms: " <> tshow i <> " of " <> tshow total <> " " <> showA e <> " (" <> tshow e <> ")."
  case vExp of
    (EDcD {}, _) -> do
      -- skip all simple relations
      return (vExp, True)
    (expr, orig) -> do
      violationsSQL <- evaluateExpSQL fSpec (tempDbName fSpec) expr
      let violationsAmp = [(showValADL (apLeft p), showValADL (apRight p)) | p <- toList $ pairsInExpr fSpec expr]
      if L.sort violationsSQL == L.sort violationsAmp
        then do
          return (vExp, True)
        else do
          logInfo ""
          logInfo $ "Checking " <> display orig <> ": term = " <> display (showA expr)
          logInfo ""
          logInfo "Mismatch between SQL and Ampersand"
          logInfo $ display (showVExp vExp)
          logInfo "SQL violations:"
          logInfo $ displayShow violationsSQL
          logInfo "Ampersand violations:"
          logInfo $ displayShow violationsAmp
          return (vExp, False)
