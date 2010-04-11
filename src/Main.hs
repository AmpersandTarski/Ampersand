{-# OPTIONS_GHC -Wall #-}
module Main where

import Options               (getOptions,Options(..),verboseLn,verbose)
import Parser                (parseADL)
import Adl                   (Context)
import Data.Fspec            (Fspc)
import ADL2Fspec             (makeFspec)
import Generators            (generate)
import System.FilePath
import qualified Data.ByteString as Bin

main :: IO ()
main 
 = do flags <- getOptions   
      if showVersion flags || showHelp flags
       then mapM_ Prelude.putStr (helpNVersionTexts flags)
       else if test flags
            then testprog flags
            else
              (    parseFile flags 
               >>= calculate flags 
               >>= generate flags
              ) 
   where
     testprog flags = let fnFull = fileName flags in
                      do{ deBinTekst <- Bin.readFile fnFull
                        ; deTxtTekst <- readFile fnFull
                        ; (writeFile (addExtension fnFull ".onelinerbin") (", SF \""++fnFull++"\"     True   "++show (show deBinTekst))
                           >>
                           writeFile (addExtension fnFull ".onelinertxt") (", SF \""++fnFull++"\"     False  "++show deTxtTekst)
                           )}
                
parseFile :: Options -> IO(Context)
parseFile flags  
      = let fnFull = fileName flags in
        do verbose flags "Parsing... "
           adlText <- Prelude.readFile fnFull
           parseADL adlText flags fnFull 

calculate :: Options -> Context -> IO(Fspc)
calculate flags context = do verboseLn flags "Calculating..."
                             return (makeFspec flags context)
                          
                               
