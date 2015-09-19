module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import Database.Design.Ampersand.Prototype.ObjBinGen   (generatePhp, writeStaticFiles)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand
import Database.Design.Ampersand.Prototype.GenBericht  (doGenBericht)
import Database.Design.Ampersand.Prototype.Generate    (generateGenerics, generateCustomCss)
import Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend, clearTemplateDirs)
import Database.Design.Ampersand.Prototype.ValidateSQL (validateRulesSQL)

main :: IO ()
main =
 do opts <- getOptions
    if showVersion opts || showHelp opts
    then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
    else do gFSpec <- createFSpec opts
            case gFSpec of
              Errors err    -> do putStrLn "Error(s) found:"
                                  mapM_ putStrLn (intersperse  (replicate 30 '=') (map showErr err))
                                  exitWith $ ExitFailure 10
              Checked fSpec -> do generateAmpersandOutput fSpec
                                  generateProtoStuff      fSpec

generateProtoStuff :: FSpec -> IO ()
generateProtoStuff fSpec
-- HJO: The following has been commented out, because:
-- 1) it does not seem to be used
-- 2) It's purpose is unclear
-- 3) underlying code has been modified. It is unclear what that would mean for this functionality
--     ==> Hence, we have bitrot.
--  | Just nm <- validateEdit (getOpts fSpec) =
--      do { verboseLn (getOpts fSpec) "Validating edit operations:"
--         ; gBeforePops <- getPopulationsFrom (getOpts fSpec) $ nm ++ ".before.pop"
--         ; gAfterPops <- getPopulationsFrom (getOpts fSpec) $ nm ++ ".after.pop"
--         ; case (,) <$> gBeforePops <*> gAfterPops of
--              Errors err -> do putStrLn "Error(s) found in before/after populations:"
--                               mapM_ putStrLn (intersperse  (replicate 30 '=') (map showErr err))
--                               exitWith $ ExitFailure 10
--              Checked (beforePops, afterPops) ->
--               do { isValid <- validateEditScript fSpec beforePops afterPops (nm++".edit.json")
--                  ; unless isValid (exitWith (ExitFailure 30))
--                  }
--         }
  | validateSQL (getOpts fSpec) =
      do { verboseLn (getOpts fSpec) "Validating SQL expressions..."
         ; isValid <- validateRulesSQL fSpec
         ; unless isValid (exitWith (ExitFailure 30))
         }
  | otherwise =
      do { when (genPrototype (getOpts fSpec)) $ doGenProto fSpec
         ; when (genBericht (getOpts fSpec))   $ doGenBericht fSpec
         ; case testRule (getOpts fSpec) of
             Just ruleName -> ruleTest fSpec ruleName
             Nothing       -> return ()
    ; when ((not . null $ allViolations fSpec) && (development (getOpts fSpec) || theme (getOpts fSpec)==StudentTheme)) $
        verboseLn (getOpts fSpec) "\nWARNING: There are rule violations (see above)."
    ; verboseLn (getOpts fSpec) "Done."  -- if there are violations, but we generated anyway (ie. with --dev or --theme=student), issue a warning
    }

doGenProto :: FSpec -> IO ()
doGenProto fSpec =
 do { verboseLn (getOpts fSpec) "Checking on rule violations..."
    ; reportViolations violationsOfInvariants
    ; reportSignals (initialConjunctSignals fSpec)
    ; if (not . null) violationsOfInvariants && not (development (getOpts fSpec)) && theme (getOpts fSpec)/=StudentTheme
      then do { putStrLn "\nERROR: No prototype generated because of rule violations.\n(Compile with --dev to generate a prototype regardless of violations)"
              ; exitWith $ ExitFailure 40
              }
      else do { verboseLn (getOpts fSpec) "Generating prototype..."

              ; when (newFrontend (getOpts fSpec)) $
                  clearTemplateDirs fSpec

              ; writeStaticFiles (getOpts fSpec)
              ; generatePhp fSpec
              ; generateGenerics fSpec

              ; if newFrontend (getOpts fSpec) then
                  doGenFrontend fSpec
                else
                  generateCustomCss fSpec

              ; verboseLn (getOpts fSpec) "\n"

              ; verboseLn (getOpts fSpec) $ "Prototype files have been written to " ++ dirPrototype (getOpts fSpec)
              }
    }
 where violationsOfInvariants :: [(Rule,[AAtomPair])]
       violationsOfInvariants
         = [(r,vs) |(r,vs) <- allViolations fSpec
                   , not (isSignal r)
           ]
       reportViolations :: [(Rule,[AAtomPair])] -> IO()
       reportViolations []    = verboseLn (getOpts fSpec) "No violations found."
       reportViolations viols =
         let ruleNamesAndViolStrings = [ (name r, showprs p) | (r,p) <- viols ]
         in  putStrLn $ intercalate "\n"
                          [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                          | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                          ]

       showprs :: [AAtomPair] -> String
       showprs aprs = "["++intercalate ", " (map showADL aprs)++"]"
--       showpr :: AAtomPair -> String
--       showpr apr = "( "++(showVal.apLeft) apr++", "++(showVal.apRight) apr++" )"
       reportSignals []        = verboseLn (getOpts fSpec) "No signals for the initial population."
       reportSignals conjViols = verboseLn (getOpts fSpec) $ "Signals for initial population:\n" ++ intercalate "\n"
         [   "Rule(s): "++(show . map name . rc_orgRules) conj
         ++"\n  Conjunct   : " ++ showADL (rc_conjunct conj)
         ++"\n  Violations : " ++ showprs viols
         | (conj, viols) <- conjViols
         ]
ruleTest :: FSpec -> String -> IO ()
ruleTest fSpec ruleName =
 case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
   [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found."
   (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showADL (rrexp rule)
                  ; putStrLn $ showContents rule
                  ; let rExpr = rrexp rule
                  ; let ruleComplement = rule { rrexp = notCpl (EBrk rExpr) }
                  ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                  ; putStrLn $ showContents ruleComplement
                  }
 where showContents rule = let pairs = [ "("++(show.showValADL.apLeft) v++"," ++(show.showValADL.apRight) v++")" | (r,vs) <- allViolations fSpec, r == rule, v <- vs]
                           in  "[" ++ intercalate ", " pairs ++ "]"
