module DatabaseDesign.Ampersand_Prototype.ValidateSQL where

import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

validateRuleSQL :: Fspc -> Options -> IO ()
validateRuleSQL fSpec opts = putStrLn "Validation of generated SQL for rules has not been implemented yet."
