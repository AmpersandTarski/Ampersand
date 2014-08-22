{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.FilePath        (combine)
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import Database.Design.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import Database.Design.Ampersand_Prototype.Apps.RAP   (atlas2context, atlas2populations)
import Database.Design.Ampersand_Prototype.CoreImporter
import Database.Design.Ampersand_Prototype.Version (prototypeVersionStr)
import Database.Design.Ampersand_Prototype.GenBericht (doGenBericht)
import Database.Design.Ampersand_Prototype.ValidateSQL (validateRulesSQL)
-- import Database.Design.Ampersand.Input.ADL1.CtxError (showErr)
-- import qualified Database.Design.Ampersand.Basics as Basics

main :: IO ()
main =
 do opts' <- getOptions
    let opts = opts'{genPrototype=True}
    if showVersion opts || showHelp opts
    then mapM_ putStr (helpNVersionTexts prototypeVersionStr opts)
    else do gFspec <- createFspec opts
            case gFspec of
              Errors err -> do putStrLn "Error(s) found:"
                               mapM_ putStrLn (intersperse  (replicate 30 '=') (map showErr err))
                               exitWith $ ExitFailure 10
              Checked fSpec -> generateAmpersandOutput fSpec
                             >> generateProtoStuff fSpec

generateProtoStuff :: Fspc -> IO ()
generateProtoStuff fSpec
  | validateSQL (flags fSpec) =
      do { verboseLn (flags fSpec) "Validating SQL expressions..."
         ; isValid <- validateRulesSQL fSpec
         ; unless isValid (exitWith (ExitFailure 30))
         }
  | export2adl (flags fSpec) && fileformat (flags fSpec)==Just Adl1Format =
      do { verboseLn (flags fSpec) "Exporting Atlas DB content to .adl-file..."
         ; cx<-atlas2context fSpec
         ; writeFile (combine (dirOutput (flags fSpec)) (outputfile (flags fSpec))) (showADL cx)
         ; verboseLn (flags fSpec) $ "Context written to " ++ combine (dirOutput (flags fSpec)) (outputfile (flags fSpec)) ++ "."
         }
  | export2adl (flags fSpec) && fileformat (flags fSpec)==Just Adl1PopFormat =
      do { verboseLn (flags fSpec) "Exporting Atlas DB content to .pop-file..."
         ; cxstr<-atlas2populations fSpec
         ; writeFile (combine (dirOutput (flags fSpec)) (outputfile (flags fSpec))) cxstr
         ; verboseLn (flags fSpec) $ "Population of context written to " ++ combine (dirOutput (flags fSpec)) (outputfile (flags fSpec)) ++ "."
         }
  | otherwise =
      do { verboseLn (flags fSpec) "Generating prototype artifacts..."
         ; when (genPrototype (flags fSpec)) $ doGenProto fSpec
         ; when (genBericht (flags fSpec))   $ doGenBericht fSpec
         ; case testRule (flags fSpec) of
             Just ruleName -> ruleTest fSpec ruleName
             Nothing       -> return ()
    ; when ((not . null $ allViolations fSpec) && (development (flags fSpec) || theme (flags fSpec)==StudentTheme)) $
        verboseLn (flags fSpec) "\nWARNING: There are rule violations (see above)."
    ; verboseLn (flags fSpec) "Done."  -- if there are violations, but we generated anyway (ie. with --dev or --theme=student), issue a warning
    }

doGenProto :: Fspc -> IO ()
doGenProto fSpec =
 do { verboseLn (flags fSpec) "Checking on rule violations..."
  --  ; let allViolations = violations fSpec
    ; reportViolations (allViolations fSpec)

    ; if (not . null) (allViolations fSpec) && not (development (flags fSpec)) && theme (flags fSpec)/=StudentTheme
      then do { putStrLn "\nERROR: No prototype generated because of rule violations.\n(Compile with --dev to generate a prototype regardless of violations)"
              ; exitWith $ ExitFailure 40
              }
      else do { verboseLn (flags fSpec) "Generating prototype..."
              ; phpObjInterfaces fSpec
              ; verboseLn (flags fSpec) $ "Prototype files have been written to " ++ dirPrototype (flags fSpec) ++ "."
              }
    }
 where reportViolations []    = verboseLn (flags fSpec) "No violations found."
       reportViolations viols =
         let ruleNamesAndViolStrings = [ (name r, show p) | (r,p) <- viols ]
         in  putStrLn $ intercalate "\n"
                          [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps
                          | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                          ]

ruleTest :: Fspc -> String -> IO ()
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
 where showContents rule = let pairs = [ "("++srcPaire v++"," ++trgPaire v++")" | (r,vs) <- allViolations fSpec, r == rule, v <- vs]
                           in  "[" ++ intercalate ", " pairs ++ "]"
              