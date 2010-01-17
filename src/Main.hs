{-# OPTIONS_GHC -Wall #-}
module Main where

import Options               (getOptions,Options(..),usageInfo',verboseLn,verbose)
import Version               (versionbanner)
import Parser                (parseADL)
import Adl                   (Context)
import Data.Fspec            (Fspc)
import ADL2Fspec             (makeFspec)
import Generators            


main :: IO ()
main
 = do flags <- getOptions   
      if showVersion flags
       then putStrLn versionbanner
       else if showHelp flags 
       then mapM_ putStrLn [usageInfo' flags]
       else do context <- phase1 flags 
               fSpec   <- phase2 flags context
               phase3 flags fSpec

phase1 :: Options -> IO(Context)
phase1 flags  
      = let fnFull = adlFileName flags in
        do verbose flags "Parsing... "
           adlText <- readFile fnFull
           parseADL adlText flags fnFull 

phase2 :: Options -> Context -> IO(Fspc)
phase2 flags context = do verboseLn flags "Calculating..."
                          return (makeFspec flags context)
                          
phase3 :: Options -> Fspc -> IO()
phase3 flags fSpec = 
    sequence_ 
       ([ verboseLn    flags "Generating..."]++
        [ doGenAtlas   fSpec flags | genAtlas     flags] ++
        [ doGenXML     fSpec flags | genXML       flags] ++
        [ doGenHaskell fSpec flags | haskell      flags] ++ 
        [ doGenProto   fSpec flags | genPrototype flags]++
        [ serviceGen   fSpec flags | services     flags] ++
        [ doGenFspec   fSpec flags | genFspec     flags] ++ 
        [ prove        fSpec flags | proofs       flags]++
        [ verbose flags "Done."]
       ) 
                               

