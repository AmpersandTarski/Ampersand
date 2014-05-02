{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.FilePath        (combine)
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps.RAP   (atlas2context, atlas2populations)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version (prototypeVersionStr)
import DatabaseDesign.Ampersand_Prototype.GenBericht (doGenBericht)
import DatabaseDesign.Ampersand_Prototype.ValidateSQL (validateRulesSQL)
-- import DatabaseDesign.Ampersand.Input.ADL1.CtxError (showErr)
-- import qualified DatabaseDesign.Ampersand.Basics as Basics

main :: IO ()
main =
 do flags' <- getOptions
    let flags = flags'{genPrototype=True}
    if showVersion flags || showHelp flags
    then mapM_ putStr (helpNVersionTexts prototypeVersionStr flags)
    else do gFspec <- createFspec flags
            case gFspec of
              Errors err -> do putStrLn "Error(s) found:"
                               mapM_ putStrLn (intersperse  (replicate 30 '=') (map showErr err))
                               exitWith $ ExitFailure 10
              Checked fspc -> generateAmpersandOutput flags fspc
                             >> generateProtoStuff flags fspc

generateProtoStuff :: Options -> Fspc -> IO ()
generateProtoStuff flags fSpec 
  | validateSQL flags =
      do { verboseLn flags "Validating SQL expressions..."
         ; isValid <- validateRulesSQL fSpec flags
         ; unless isValid (exitWith (ExitFailure 30))
         }
  | export2adl flags && fileformat flags==Adl1Format =
      do { verboseLn flags "Exporting Atlas DB content to .adl-file..."
         ; cx<-atlas2context fSpec flags
         ; writeFile (combine (dirOutput flags) (outputfile flags)) (showADL cx)
         ; verboseLn flags $ "Context written to " ++ combine (dirOutput flags) (outputfile flags) ++ "."
         }
  | export2adl flags && fileformat flags==Adl1PopFormat =
      do { verboseLn flags "Exporting Atlas DB content to .pop-file..."
         ; cxstr<-atlas2populations fSpec flags
         ; writeFile (combine (dirOutput flags) (outputfile flags)) cxstr
         ; verboseLn flags $ "Population of context written to " ++ combine (dirOutput flags) (outputfile flags) ++ "."
         }
  | otherwise =
      do { verboseLn flags "Generating prototype artifacts..."
         ; when (genPrototype flags) $ doGenProto fSpec flags
         ; when (genBericht flags)   $ doGenBericht fSpec flags
         ; case testRule flags of 
             Just ruleName -> ruleTest fSpec flags ruleName
             Nothing       -> return ()
    ; when ((not . null $ allViolations fSpec) && (development flags || theme flags==StudentTheme)) $
        verboseLn flags "\nWARNING: There are rule violations (see above)."
    ; verboseLn flags "Done."  -- if there are violations, but we generated anyway (ie. with --dev or --theme=student), issue a warning
    }
               
doGenProto :: Fspc -> Options -> IO ()
doGenProto fSpec flags =
 do { verboseLn flags "Checking on rule violations..."
  --  ; let allViolations = violations fSpec
    ; reportViolations (allViolations fSpec)
    
    ; if (not . null) (allViolations fSpec) && not (development flags) && theme flags/=StudentTheme 
      then do { putStrLn "\nERROR: No prototype generated because of rule violations.\n(Compile with --dev to generate a prototype regardless of violations)"
              ; exitWith $ ExitFailure 40
              } 
      else do { verboseLn flags "Generating prototype..."
              ; phpObjInterfaces fSpec flags  
              ; verboseLn flags $ "Prototype files have been written to " ++ dirPrototype flags ++ "."
              }
    }
 where reportViolations []    = verboseLn flags "No violations found."
       reportViolations viols =
         let ruleNamesAndViolStrings = [ (name r, show p) | (r,p) <- viols ]
         in  putStrLn $ intercalate "\n"
                          [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps 
                          | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                          ]


ruleTest :: Fspc -> Options -> String -> IO ()
ruleTest fSpec _ ruleName =
 case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
   [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found." 
   (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showADL (rrexp rule)
                  ; putStrLn $ showContents rule
                  ; let rExpr = rrexp rule
                  ; let ruleComplement = rule { rrexp = notCpl (EBrk rExpr) }
                  ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                  ; putStrLn $ showContents ruleComplement
                  } 
 where showContents rule = let pairs = [ "("++f++"," ++s++")" | (r,vs) <- allViolations fSpec, r == rule, (f,s) <- vs]
                           in  "[" ++ intercalate ", " pairs ++ "]" 
              