{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand.Misc 
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import DatabaseDesign.Ampersand.Components
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
--import DatabaseDesign.Ampersand.Classes.ViewPoint (violations)

fatal :: Int -> String -> a
fatal = fatalMsg "Main"

main :: IO ()
main
 = do opts <- getOptions
      if showVersion opts || showHelp opts
        then mapM_ putStr (helpNVersionTexts ampersandVersionStr opts)
        else do (ctx,err) <- parseAndTypeCheck opts
                if nocxe err 
                  then let fspc = makeFspec opts ctx in
                       generate opts fspc
                  else putStr (show err)
                  
  where
      parseAndTypeCheck :: Options -> IO(A_Context, CtxError) 
      parseAndTypeCheck opts 
                        = do verboseLn opts "Start parsing...."
                             pCtx <- parseCtxM_ opts fileName
                             pPops <- case importfile opts of
                                         [] -> return []
                                         fn -> do popsText <- readFile fn
                                                  parsePopsM_ popsText opts fn
                             verboseLn opts "Type checking..."
                             return (case pCtx of
                                        Right ctx -> typeCheck ctx pPops
                                        Left msg  -> (fatal 38 "There are errors that should have been presented!",PE [msg])
                                    )   


--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spits out everything the user requested.
generate :: Options -> Fspc -> IO ()
generate opts fSpec = 
 do { verboseLn opts "Generating..."
    ; when (genXML opts)      $ doGenXML      fSpec opts
    ; when (genUML opts)      $ doGenUML      fSpec opts 
    ; when (haskell opts)     $ doGenHaskell  fSpec opts 
    ; when (interfacesG opts) $ interfaceGen  fSpec opts
    ; when (genFspec opts)    $ doGenDocument fSpec opts 
    ; when (proofs opts)      $ prove         fSpec opts
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verbose opts "Done."
    }

