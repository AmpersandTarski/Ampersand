{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import System.FilePath (replaceExtension)
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import Data.GraphViz --hiding (addExtension, C)
import Data.List (intercalate)
import DatabaseDesign.Ampersand.Misc 
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Parsing
import DatabaseDesign.Ampersand.Components
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
--import DatabaseDesign.Ampersand.Classes.ViewPoint (violations)

fatal :: Int -> String -> a
fatal = fatalMsg "Main"

-- TODO: this should be cleaned up more. Also, type checking should return Either.
main :: IO ()
main =
 do { flags <- getOptions
    ; if showVersion flags || showHelp flags
        then mapM_ putStr (helpNVersionTexts ampersandVersionStr flags)
        else do { actx <- parseAndTypeCheck flags
                ; let fspc = makeFspec flags actx
                ; generate flags fspc
                }
    }             
  where
      parseAndTypeCheck :: Options -> IO A_Context 
      parseAndTypeCheck flags =
       do { verboseLn flags "Start parsing...."
          ; pCtxOrErr <- parseContext flags (fileName flags)
          ; case pCtxOrErr of
              Left pErr ->
               do { Prelude.putStrLn $ "Parsing error:"
                  ; Prelude.putStrLn $ show pErr
                  ; exitWith $ ExitFailure 10 
                  }
              Right p_context -> 
               do { pPops <- case importfile flags of
                               []             -> return []
                               importFilename -> do { popsText <- readFile importFilename
                                                    ; parsePopulations popsText flags importFilename
                                                    }
                  ; verboseLn flags "Type checking..."
                  ; let (actxOrErrs,stTypeGraph,condensedGraph) = typeCheck p_context pPops
-- For the purpose of debugging the type checker, or for educational purposes, the switch "--typing" can be used.
-- It prints three graphs. For an explanation of those graphs, consult the corresponding papers (yet to be written).
-- Use only for very small scripts, or else the results will not be very informative.
-- For the large scripts that are used in projects, the program may abort due to insufficient resources.
                  ; if typeGraphs flags
                    then do { condensedGraphPath<-runGraphvizCommand Dot condensedGraph Png (replaceExtension ("Condensed_Graph_of_"++baseName flags) ".png")
                            ; verboseLn flags (condensedGraphPath++" written.")
                            ; stDotGraphPath<-runGraphvizCommand Dot stTypeGraph Png (replaceExtension ("stGraph_of_"++baseName flags) ".png")
                            ; verboseLn flags (stDotGraphPath++" written.")
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



--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spits out everything the user requested.
generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
 do { verboseLn flags "Generating..."
    ; when (genXML flags)      $ doGenXML      fSpec flags
    ; when (genUML flags)      $ doGenUML      fSpec flags 
    ; when (haskell flags)     $ doGenHaskell  fSpec flags 
    ; when (export2adl flags)  $ doGenADL      fSpec flags
    ; when (genFspec flags)    $ doGenDocument fSpec flags 
    ; when (genFPAExcel flags) $ doGenFPAExcel fSpec flags
    ; when (proofs flags)      $ doGenProofs   fSpec flags
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verboseLn flags "Done."
    }

