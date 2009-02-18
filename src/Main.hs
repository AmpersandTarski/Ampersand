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
       then mapM_ putStrLn [(usageInfo' (progrName flags))]
       else do verboseLn flags (show flags) 
               context <- phase1 flags 
               fSpec   <- phase2 flags context
               phase3 flags fSpec

phase1 :: Options -> IO(Context)
phase1 flags  
      = let fnFull = adlFileName flags in
        do verbose flags "Parsing... "
           adlText <- readFile fnFull
           context <- parseADL adlText flags fnFull 
           return context

phase2 :: Options -> Context -> IO(Fspc)
phase2 flags context = do verboseLn flags "Calculating..."
                          return (makeFspec context)
                          
phase3 :: Options -> Fspc -> IO()
phase3 flags fSpec = 
    sequence_ 
       ([ verboseLn flags "Generating..."]++
      --[ anal context ("-p" `elem` switches) (lineStyle switches) | null switches || "-h" `elem` switches]++
      --[ makeXML_depreciated context| "-XML" `elem` switches]++
        [ doGenAtlas fSpec flags | genAtlas flags] ++
        [ doGenXML   fSpec flags | genXML flags] ++
        [ doGenHaskell fSpec flags | haskell flags] ++ 
        [ doGenProto fSpec flags | genPrototype flags]++
        [ serviceGen  fSpec flags | services flags] ++
      --[ diagnose context| "-diag" `elem` switches]++
        [ doGenFspecLaTeX fSpec flags | fspecLaTeX flags] ++
        [ doGenFspecHtml fSpec flags | fspecHtml flags] ++
        [ doGenFspecWord fSpec flags | fspecWord flags] ++  
      --[ cdModel context | "-CD" `elem` switches]++
      --[ phpObjServices context fSpec filename dbName ("./"++filename++"/") | "-phpcode" `elem` switches]++
      --[ phpServices context filename dbName True True | "-beeper" `elem` switches]++
      --[ phpServices context filename dbName ("-notrans" `elem` switches) False| "-checker" `elem` switches]++
        [ prove fSpec flags| proofs flags]++
 --               ++[ projectSpecText context (lang switches) | "-project" `elem` switches]
 --               ++[ csvcontent context | "-csv" `elem` switches]
 --               ++[ putStr (show slRes) | "-dump" `elem` switches ]
 --    ) 
        [ verbose flags "Done."]
       ) 
                               

