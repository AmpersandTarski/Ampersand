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
fatal :: Int -> String -> a
fatal = fatalMsg "Main"

main :: IO ()
main
 = do flags <- getOptions
      if showVersion flags || showHelp flags
        then mapM_ putStr (helpNVersionTexts ("AmpersandVs" ++ versionNumber) flags)
        else do (ctx,err) <- parseAndTypeCheck flags
                if nocxe err 
                  then let fspc = calculate flags ctx in
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
                                        Left ctx   -> typeCheck ctx pPops
                                        Right msgs -> (fatal 38 "There are errors that should have been presented!",PE msgs)
                                    )   


--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spitts out everything the user requested.
generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
    sequence_ 
       ([ verboseLn     flags "Generating..."]++
        [ doGenXML      fSpec flags | genXML       flags] ++
        [ doGenHaskell  fSpec flags | haskell      flags] ++ 
        [ interfaceGen  fSpec flags | interfacesG  flags] ++
        [ doGenDocument fSpec flags | genFspec     flags] ++ 
        [ prove         fSpec flags | proofs       flags] ++
        [ verbose flags "Done."]
       ) 

