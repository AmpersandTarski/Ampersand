{-# OPTIONS_GHC -Wall #-}
module Main where

import Options               (getOptions,Options(..),verboseLn,verbose)
import Parser                (parseADL)
import Adl                   (Context)
import Data.Fspec            (Fspc)
import ADL2Fspec             (makeFspec)
import Generators            (generate)
import Control.Monad         ((>>=))
import System.FilePath

main :: IO ()
main = mainHan
-- even tijdelijk een rommeltje van gemaakt. komt later weer goed.... (en als het goed is heeft niemand er last van...)
main1
 = do flags <- getOptions   
      if showVersion flags || showHelp flags
       then mapM_ putStrLn (helpNVersionTexts flags)
       else (    parseFile flags 
             >>= calculate flags 
             >>= generate flags
            ) 

parseFile :: Options -> IO(Context)
parseFile flags  
      = let fnFull = fileName flags in
        do verbose flags "Parsing... "
           adlText <- readFile fnFull
           parseADL adlText flags fnFull 

calculate :: Options -> Context -> IO(Fspc)
calculate flags context = do verboseLn flags "Calculating..."
                             return (makeFspec flags context)
                          
                               
mainHan = do flags <- getOptions
             if test flags 
               then
                 let fnFull = fileName flags in
                 do deTekst <- readFile fnFull
                    writeFile (addExtension fnFull ".oneliner") ("writeFile "++fnFull++" "++show deTekst)
               else main1
