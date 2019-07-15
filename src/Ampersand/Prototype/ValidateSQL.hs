{-# LANGUAGE RecordWildCards #-}
module Ampersand.Prototype.ValidateSQL (validateRulesSQL) where

import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Misc
import           Ampersand.Prototype.PHP
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified RIO.Set as Set
{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

validateRulesSQL :: (HasOptions env, HasHandle env, HasVerbosity env) => FSpec ->  RIO env [String]
validateRulesSQL fSpec = do
    case filter (not . isSignal . fst) (allViolations fSpec) of
       []    -> return()
       viols -> exitWith . ViolationsInDatabase . map stringify $ viols
    hSetBuffering stdout NoBuffering

    sayWhenLoudLn "Initializing temporary database (this could take a while)"
    succes <- createTempDatabase fSpec
    if succes 
    then actualValidation 
    else do 
        sayLn "Error: Database creation failed. No validation could be done."
        return []
  where
    actualValidation = do
        let allExps = getAllInterfaceExps fSpec ++
                      getAllRuleExps fSpec ++
                      getAllPairViewExps fSpec ++
                      getAllIdExps fSpec ++
                      getAllViewExps fSpec
        sayWhenLoudLn $ "Number of expressions to be validated: "++show (length allExps)
        results <- mapM (validateExp fSpec) allExps
        case [ ve | (ve, False) <- results] of
           [] -> do
               sayWhenLoudLn $ "\nValidation successful.\nWith the provided populations, all generated SQL code has passed validation."
               return []
           ves -> return $ "Validation error. The following expressions failed validation:"
                         : map showVExp ves
    

stringify :: (Rule,AAtomPairs) -> (String,[String])
stringify (rule,pairs) = (name rule, map f . Set.elems $ pairs )
  where f pair = "("++showValADL (apLeft pair)++", "++showValADL (apRight pair)++")"


-- functions for extracting all expressions from the context

getAllInterfaceExps :: FSpec -> [ValidationExp]
getAllInterfaceExps fSpec = concat [ getObjExps (name ifc) $ ifcObj ifc
                                   | ifc <- interfaceS fSpec ++ interfaceG fSpec ]
 where getObjExps iName objDef = (objExpression objDef, "interface " ++ show iName) :
                                 concatMap (getObjExps iName) (fields objDef)

-- we check the complement of the rule, since that is the expression evaluated in the prototype
getAllRuleExps :: FSpec -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp . Set.elems $ vrules fSpec `Set.union` grules fSpec
 where getRuleExp rule = (notCpl (formalExpression rule), "rule "++show (name rule))

getAllPairViewExps :: FSpec -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps . Set.elems $ vrules fSpec `Set.union` grules fSpec
 where getPairViewExps r@Ru{rrviol = Just (PairView pvsegs)} =
         [ (expr, "violation view for rule "++show (name r)) | PairViewExp _ _ expr <- NEL.toList pvsegs ]
       getPairViewExps _    = []

getAllIdExps :: FSpec -> [ValidationExp]
getAllIdExps fSpec = concatMap getIdExps $ vIndices fSpec
 where getIdExps identity = [ (objExpression objDef, "identity "++show (name identity))
                            | IdentityExp objDef <- NEL.toList $ identityAts identity ]

getAllViewExps :: FSpec -> [ValidationExp]
getAllViewExps fSpec = concatMap getViewExps $ vviews fSpec
 where getViewExps x = [ (expr, "view "++show (name x))
                       | ViewExp expr <- fmap vsmLoad (vdats x) ]

type ValidationExp = (Expression, String)
-- a ValidationExp is an expression together with the place in the context where we
-- obtained it from (e.g. rule/interface/..)
showVExp :: (Expression, String) -> String
showVExp (expr, orig) = "Origin: "++orig++", expression: "++showA expr

-- validate a single expression and report the results
validateExp :: (HasOptions env, HasHandle env) => FSpec -> ValidationExp ->  RIO env (ValidationExp, Bool)
validateExp fSpec vExp =
    case vExp of
        (EDcD{}, _) -> do -- skip all simple relations
            say "."
            return (vExp, True)
        (expr, orig) -> do
            opts <- view optionsL
            violationsSQL <- evaluateExpSQL fSpec (tempDbName opts) expr
            let violationsAmp = [(showValADL (apLeft p), showValADL (apRight p)) | p <- Set.elems $ pairsInExpr fSpec expr]
            if L.sort violationsSQL == L.sort violationsAmp
            then do
                say "."
                return (vExp, True)
            else do
                sayLn ""
                sayLn $ "Checking "++orig ++": expression = "++showA expr
                sayLn ""
                sayLn "Mismatch between SQL and Ampersand"
                sayLn $ showVExp vExp
                sayLn "SQL violations:"
                sayLn $ show violationsSQL
                sayLn "Ampersand violations:"
                sayLn $ show violationsAmp
                return (vExp, False)
