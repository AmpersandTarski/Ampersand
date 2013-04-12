{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.FilePath        (replaceExtension,combine)
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import Data.GraphViz hiding (addExtension, C)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps.RAP   (atlas2context, atlas2populations)
import DatabaseDesign.Ampersand_Prototype.Apps.RAPImport
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version
import DatabaseDesign.Ampersand_Prototype.GenBericht
import DatabaseDesign.Ampersand_Prototype.ValidateSQL (validateRuleSQL)
import DatabaseDesign.Ampersand.InputProcessing
import DatabaseDesign.Ampersand.Input.ADL1.CtxError (showErr)
import qualified DatabaseDesign.Ampersand.Basics as Basics

fatal :: Int -> String -> a
fatal = fatalMsg "Main"

-- TODO: should be cleaned up
main :: IO ()
main
 = do flags <- getOptions
      if showVersion flags || showHelp flags
       then mapM_ putStr (helpNVersionTexts prototypeVersionStr flags)
       else do gFspec <- createFspec flags
               case gFspec of
                Errors err -> do Prelude.putStrLn $ "Error(s) found:"
                                 mapM_ Basics.putStrLn (intersperse  (replicate 30 '=') (map showErr err))
                                 exitWith $ ExitFailure 10
                Checked fspc -> generateProtoStuff flags fspc
  where
  parseAndTypeCheck :: Options -> IO A_Context 
  parseAndTypeCheck flags 
   = do { verboseLn flags "Start parsing...."
        ; pCtxOrErr <- parseContext flags (fileName flags)
        ; case pCtxOrErr of
           Left pErr ->
            do { Prelude.putStrLn $ "Parse error:"
               ; Prelude.putStrLn $ show pErr
               ; exitWith $ ExitFailure 10 
               }
           Right p_context ->  
            do { let importFilename = importfile flags
               ; pPops <- if null importFilename then return [] 
                          else
                           do popsText <- readFile importFilename
                              case fileformat flags of
                                Adl1PopFormat -> parsePopulations popsText flags importFilename
                                Adl1Format -> do verbose flags ("Importing "++importFilename++" in RAP... ")
                                                 imppCtxOrErr <- parseContext flags (importfile flags)
                                                 case imppCtxOrErr of
                                                   (Right imppcx) -> let (aCtxOrErr,_,_) = typeCheck imppcx [] in
                                                                     case aCtxOrErr of
                                                                      Checked a_context  -> importfspec  (makeFspec flags a_context) flags
                                                                      Errors _           -> importfailed imppCtxOrErr flags 
                                                   (Left _)       -> importfailed imppCtxOrErr flags 
               ; verboseLn flags "Type checking..."
               ; let (actxOrErrs,stTypeGraph,condensedGraph) = typeCheck p_context pPops
               ; if typeGraphs flags
                 then do { condensedGraphPath<-runGraphvizCommand Dot condensedGraph Png (replaceExtension ("Condensed_Graph_of_"++baseName flags) ".png")
                         ; putStr ("\n"++condensedGraphPath++" written.")
                         ; stDotGraphPath<-runGraphvizCommand Dot stTypeGraph Png (replaceExtension ("stGraph_of_"++baseName flags) ".png")
                         ; putStr ("\n"++stDotGraphPath++" written.")
                         }
                 else do { putStr "" }
               ; case actxOrErrs of
                  Errors type_errors-> do { Prelude.putStrLn $ "The following type errors were found:\n"
                                          ; Prelude.putStrLn $ intercalate "\n\n" (map show type_errors)
                                          ; exitWith $ ExitFailure 20
                                          }
                  Checked actx      -> return actx
               }
        }

generateProtoStuff :: Options -> Fspc -> IO ()
generateProtoStuff flags fSpec | validateSQL flags =
 do { verboseLn flags "Validating SQL expressions..."
    ; isValid <- validateRuleSQL fSpec flags
    ; when (not isValid) $
        exitWith $ ExitFailure 30
    }
generateProtoStuff flags fSpec | export2adl flags && fileformat flags==Adl1Format =
 do { verboseLn flags "Exporting Atlas DB content to .adl-file..."
    ; cx<-atlas2context fSpec flags
    ; writeFile (combine (dirOutput flags) (outputfile flags)) (showADL cx)
    ; verboseLn flags $ "Context written to " ++ combine (dirOutput flags) (outputfile flags) ++ "."
    }
generateProtoStuff flags fSpec | export2adl flags && fileformat flags==Adl1PopFormat =
 do { verboseLn flags "Exporting Atlas DB content to .pop-file..."
    ; cxstr<-atlas2populations fSpec flags
    ; writeFile (combine (dirOutput flags) (outputfile flags)) cxstr
    ; verboseLn flags $ "Population of context written to " ++ combine (dirOutput flags) (outputfile flags) ++ "."
    }
generateProtoStuff flags fSpec | otherwise        =
 do { verboseLn flags "Generating..."
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
              ; if test flags then verboseLn flags $ show (vplugInfos fSpec) else verboseLn flags ""
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
                  ; let ruleComplement = rule { rrexp = notCpl (sign rExpr) (EBrk rExpr) }
                  ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                  ; putStrLn $ showContents ruleComplement
                  } 
 where showContents rule = let pairs = [ "("++f++"," ++s++")" | (r,vs) <- allViolations fSpec, r == rule, (f,s) <- vs]
                           in  "[" ++ intercalate ", " pairs ++ "]" 
       
    
