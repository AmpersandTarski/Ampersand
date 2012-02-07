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
 = do flags <- getOptions
      if showVersion flags || showHelp flags
        then mapM_ putStr (helpNVersionTexts ampersandVersionStr flags)
        else do (ctx,err) <- parseAndTypeCheck flags
                if nocxe err 
                  then let fspc = makeFspec flags ctx in
                       generate flags fspc
                  else putStr (show err)
                  
  where
      parseAndTypeCheck :: Options -> IO(A_Context, CtxError) 
      parseAndTypeCheck flags 
                        = let scriptName = fileName flags in
                          do scriptText <- readFile scriptName
                             verboseLn flags "Start parsing...."
                             pCtx <- parseCtxM_ scriptText flags scriptName
                             pPops <- case importfile flags of
                                         [] -> return []
                                         fn -> do popsText <- readFile fn
                                                  parsePopsM_ popsText flags fn
                             verboseLn flags "Type checking..."
                             return (case pCtx of
                                        Right ctx -> typeCheck ctx pPops
                                        Left msg  -> (fatal 38 "There are errors that should have been presented!",PE [msg])
                                    )   


--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spitts out everything the user requested.
generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
 do { verboseLn flags "Generating..."
    ; when (genXML flags)      $ doGenXML      fSpec flags
    ; when (genUML flags)      $ doGenUML      fSpec flags 
    ; when (haskell flags)     $ doGenHaskell  fSpec flags 
    ; when (interfacesG flags) $ interfaceGen  fSpec flags
    ; when (genFspec flags)    $ doGenDocument fSpec flags 
    ; when (proofs flags)      $ prove         fSpec flags
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verbose flags "Done."
    }

