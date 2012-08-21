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
 do { opts <- getOptions
    ; if showVersion opts || showHelp opts
        then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
        else do { actx <- parseAndTypeCheck opts
                ; let fspc = makeFspec opts actx
                ; generate opts fspc
                }
    }             
  where
      parseAndTypeCheck :: Options -> IO A_Context 
      parseAndTypeCheck opts =
       do { verboseLn opts "Start parsing...."
          ; pCtx <- parseContext opts (fileName opts)
          ; case pCtx of
              Left msg ->
               do { Prelude.putStrLn $ "Parse error:"
                  ; Prelude.putStrLn $ show msg
                  ; exitWith $ ExitFailure 10 
                  }
              Right p_context -> 
               do { pPops <- case importfile opts of
                               [] -> return []
                               fn -> do { popsText <- readFile fn
                                        ; parsePopulations popsText opts fn
                                        }
                  ; verboseLn opts "Type checking..."
                  ; let (actx,type_errors,stTypeGraph,condensedGraph) = typeCheck p_context pPops
-- For the purpose of debugging the type checker, or for educational purposes, the switch "--typing" can be used.
-- It prints three graphs. For an explanation of those graphs, consult the corresponding papers (yet to be written).
-- Use only for very small scripts, or else the results will not be very informative.
-- For the large scripts that are used in projects, the program may abort due to insufficient resources.
                  ; if typeGraphs opts
                    then do { condensedGraphPath<-runGraphvizCommand Dot condensedGraph Png (replaceExtension ("Condensed_Graph_of_"++baseName opts) ".png")
                            ; putStr ("\n"++condensedGraphPath++" written.")
                            ; stDotGraphPath<-runGraphvizCommand Dot stTypeGraph Png (replaceExtension ("stGraph_of_"++baseName opts) ".png")
                            ; putStr ("\n"++stDotGraphPath++" written.")
                            }
                    else do { putStr "" }
                  ; case type_errors of
                     []    -> do { return actx }
                     [err] -> do { Prelude.putStrLn $ "There is one type error:"
                                 ; Prelude.putStrLn $ show err
                                 ; exitWith $ ExitFailure 20
                                 }
                     _     -> do { Prelude.putStrLn $ "The following type errors were found:\n"
                                 ; Prelude.putStrLn $ intercalate "\n\n" (map show type_errors)
                                 ; exitWith $ ExitFailure 20
                                 }
                  }
          }
             


--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spits out everything the user requested.
generate :: Options -> Fspc -> IO ()
generate opts fSpec = 
 do { verboseLn opts "Generating..."
    ; when (genXML opts)      $ doGenXML      fSpec opts
    ; when (genUML opts)      $ doGenUML      fSpec opts 
    ; when (haskell opts)     $ doGenHaskell  fSpec opts 
    ; when (export2adl opts)  $ doGenADL      fSpec opts
    ; when (genFspec opts)    $ doGenDocument fSpec opts 
    ; when (genExcel opts)    $ doGenExcel    fSpec opts
    ; when (proofs opts)      $ prove         fSpec opts
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verboseLn opts "Done."
    }

