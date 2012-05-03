{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import System.Exit
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand.Misc 
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
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
              Right pctx -> 
               do { pPops <- case importfile opts of
                               [] -> return []
                               fn -> do { popsText <- readFile fn
                                        ; parsePopulations popsText opts fn
                                        }
                  ; verboseLn opts "Type checking..."
                  ; let (actx,err) = typeCheck pctx pPops
                  ; if nocxe err 
                    then return actx
                    else do { Prelude.putStrLn $ "Type error:"
                            ; Prelude.putStrLn $ show err
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

