{-# OPTIONS_GHC -Wall #-}
module Main where

import Options               (getOptions,Options(..),verboseLn,verbose)
import Parser                (parseADL)
import Adl                   (Context)
import Data.Fspec            (Fspc)
import ADL2Fspec             (makeFspec)
import Generators            (generate)
import Control.Monad         ((>>=))

main :: IO ()
main
 = do flags <- getOptions   
      if showVersion flags || showHelp flags
       then mapM_ putStrLn (helpNVersionTexts flags)
       else (    parseFile flags 
             >>= calculate flags 
             >>= generate flags
            ) 

parseFile :: Options -> IO(Context)
parseFile flags  
      = let fnFull = adlFileName flags in
        do verbose flags "Parsing... "
           adlText <- readFile fnFull
           parseADL adlText flags fnFull 

calculate :: Options -> Context -> IO(Fspc)
calculate flags context = do verboseLn flags "Calculating..."
                             return (makeFspec flags context)
                          
                               

