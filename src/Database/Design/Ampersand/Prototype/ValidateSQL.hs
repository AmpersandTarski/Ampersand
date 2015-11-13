module Database.Design.Ampersand.Prototype.ValidateSQL (validateRulesSQL) where

import Prelude hiding (exp)
import Data.List
import Control.Monad
import System.Exit
import System.IO hiding (hPutStr,hGetContents)
import Database.Design.Ampersand hiding (putStr, origin)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Prototype.Installer
import Database.Design.Ampersand.Prototype.PHP

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

tempDbName :: String
tempDbName = "ampersand_temporaryvalidationdb"

validateRulesSQL :: FSpec -> IO Bool
validateRulesSQL fSpec =
 do { when (any (not.isSignal.fst) (allViolations fSpec))
        (do { putStrLn "The population would violate invariants. Could not generate your database."
            ; exitWith $ ExitFailure 10
                 })
    ; hSetBuffering stdout NoBuffering

    ; putStrLn "Initializing temporary database"
    ; createTempDatabase fSpec

    ; let allExps = getAllInterfaceExps fSpec ++
                    getAllRuleExps fSpec ++
                    getAllPairViewExps fSpec ++
                    getAllIdExps fSpec ++
                    getAllViewExps fSpec

    ; putStrLn $ "Number of expressions to be validated: "++show (length allExps)
    ; results <- mapM (validateExp fSpec) allExps

--    ; putStrLn "\nRemoving temporary database"
--    ; removeTempDatabase (getOpts fSpec)

    ; case [ ve | (ve, False) <- results] of
        [] -> do { putStrLn "\nValidation successful.\nWith the provided populations, all generated SQL code has passed validation."
                 ; return True
                 }
        ves -> do { putStrLn ( "\n\nValidation error. The following expressions failed validation:\n" ++
                               unlines (map showVExp ves)
                             )
                  ; return False
                  }
    }

-- functions for extracting all expressions from the context

getAllInterfaceExps :: FSpec -> [ValidationExp]
getAllInterfaceExps fSpec = concat [ getObjExps (name ifc) $ ifcObj ifc
                                   | ifc <- interfaceS fSpec ++ interfaceG fSpec ]
 where getObjExps iName objDef = (objctx objDef, "interface " ++ show iName) :
                                 concatMap (getObjExps iName) (fields objDef)

-- we check the complement of the rule, since that is the expression evaluated in the prototype
getAllRuleExps :: FSpec -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp $ vrules fSpec ++ grules fSpec
 where getRuleExp rule = (notCpl (rrexp rule), "rule "++show (name rule))

getAllPairViewExps :: FSpec -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps $ vrules fSpec ++ grules fSpec
 where getPairViewExps r@Ru{rrviol = Just (PairView pvsegs)} =
         [ (exp, "violation view for rule "++show (name r)) | PairViewExp _ _ exp <- pvsegs ]
       getPairViewExps _    = []

getAllIdExps :: FSpec -> [ValidationExp]
getAllIdExps fSpec = concatMap getIdExps $ vIndices fSpec
 where getIdExps identity = [ (objctx objDef, "identity "++show (name identity))
                            | IdentityExp objDef <- identityAts identity ]

getAllViewExps :: FSpec -> [ValidationExp]
getAllViewExps fSpec = concatMap getViewExps $ vviews fSpec
 where getViewExps view = [ (objctx objDef, "view "++show (name view))
                          | ViewExp _ objDef <- vdats view ]

type ValidationExp = (Expression, String)
-- a ValidationExp is an expression together with the place in the context where we
-- obtained it from (e.g. rule/interface/..)
showVExp :: ShowADL a => (a, String) -> String
showVExp (exp, orig) = "Origin: "++orig++", expression: "++showADL exp

-- validate a single expression and report the results
validateExp :: FSpec -> ValidationExp -> IO (ValidationExp, Bool)
validateExp _  vExp@(EDcD{}, _)   = -- skip all simple relations
 do { putStr "."
    ; return (vExp, True)
    }
validateExp fSpec vExp@(exp, orig) =
 do { --putStr $ "Checking "++orig ++": expression = "++showADL exp
    ; violationsSQL <- fmap sort . evaluateExpSQL fSpec tempDbName $ exp
    ; let violationsAmp = sort [(showValSQL (apLeft p), showValSQL (apRight p)) | p <- pairsInExpr fSpec exp]

    ; if violationsSQL == violationsAmp
      then
       do { putStr "." -- ++show violationsSQL
          ; return (vExp, True)
          }
      else
       do { putStr $ "Checking "++orig ++": expression = "++showADL exp
          ; putStrLn "\nMismatch between SQL and Ampersand"
          ; putStrLn $ showVExp vExp
          ; putStrLn $ "SQL violations:\n"++show violationsSQL
          ; putStrLn $ "Ampersand violations:\n" ++ show violationsAmp
          ; return (vExp, False)
          }
    }

createTempDatabase :: FSpec -> IO ()
createTempDatabase fSpec =
 do { _ <- executePHPStr . showPHP $ sqlServerConnectPHP fSpec ++
                                     createTempDbPHP tempDbName ++
                                     createTablesPHP fSpec ++
                                     populateTablesPHP fSpec
    ; return ()
    }
 
