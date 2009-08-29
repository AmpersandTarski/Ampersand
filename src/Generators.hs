{-# OPTIONS_GHC -Wall #-}
module Generators (doGenAtlas
                  ,doGenXML
                  ,doGenHaskell
                  ,doGenProto
                  ,doGenFspec
                  ,serviceGen
                  ,prove)
where

import Classes.Graphics
import System (system, ExitCode(ExitSuccess,ExitFailure))

import System.FilePath(combine,replaceExtension)
import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (printadl)
import ShowXML      (showXML)
import Char         (toUpper)
import Calc         (deriveProofs)
import Prototype.ObjBinGen (phpObjServices)
import Adl
import Fspec2Pandoc (render2Pandoc,fSpec2Pandoc)
import Version      (versionbanner)
import Rendering.ClassDiagram
--import System
serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ printadl (Just fSpec') 0 fSpec')
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where fSpec'= if (allServices flags)
                  then fSpec{serviceS=serviceG fSpec} 
                  else fSpec
          outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec _
    = putStr (deriveProofs fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile haskellCode 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")
         haskellCode =
                ("{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++versionbanner++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import UU_Scanner"
             ++"\n  --import Classification"
             ++"\n  import Typology"
             ++"\n  import Adl"
             ++"\n  import ShowHS (showHS)"
             ++"\n  import Data.Fspec"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = putStr (showHS \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS "\n     " fSpec
                ) 
doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generation of Atlas is currently not supported."
  >> verboseLn flags ("Atlas would be generated in " ++ show (dirAtlas flags) ++ ".")
  >> generatepngs fSpec flags
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML fSpec (genTime flags))   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Generating prototype..."
     >> phpObjServices fSpec flags  
     >> verboseLn flags ("Prototype files are written to " ++  (dirPrototype flags) ++ "." )
     >> if (test flags) then verboseLn flags (show $ vplugs fSpec) else verboseLn flags ""
     
 
doGenFspec :: Fspc -> Options -> IO()
doGenFspec fSpec flags
   =  do
      verboseLn flags "Generating functional specification document..."
      customheader <- readFile (texHdrFile flags)
      writeFile outputFile  ( render2Pandoc flags customheader (fSpec2Pandoc fSpec' flags))
      generatepngs fSpec' flags
      verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
   where  
   fSpec'= if (allServices flags)
           then fSpec{serviceS=serviceG fSpec} 
           else fSpec
   outputFile = combine (dirOutput flags) (replaceExtension (baseName flags) (outputExt $ fspecFormat flags))        
   outputExt FPandoc  = ".pandoc"
   outputExt FWord    = ".doc"
   outputExt FLatex   = ".tex"
   outputExt FHtml    = ".html"
   outputExt FUnknown = ".pandoc"


generatepngs :: Fspc -> Options -> IO() 
generatepngs fSpec flags = foldr (>>) (verboseLn flags "All pictures written..") (dots ++ cds)
   where 
   outputFile fnm = combine (dirOutput flags) fnm
   dots = [run (remSpaces (name p)) $ toDot fSpec flags p 
          | p<-vpatterns fSpec, (not.null) (concs p)]
   remSpaces [] = []
   remSpaces (' ':c:str) = toUpper c:remSpaces str 
   remSpaces xs = xs
   cds = [run_cd (remSpaces$"CD_"++fnm) cd|cd@(OOclassdiagram{nameandcpts=(fnm,_)})<-classdiagrams fSpec] 
   run_cd fnm cd =
       do
       writeFile (outputFile (fnm++".dot")) (classdiagram2dot cd)
       putStrLn ("Processing "++fnm++".dot ... :")
       result <- system $ "dot -Tpng "++(outputFile (fnm++".dot"))++" -o "++(outputFile (fnm++".png"))
       case result of
          ExitSuccess   -> putStrLn ("  "++fnm++".png created.")
          ExitFailure x -> putStrLn $ "Failure: " ++ show x
   run fnm dot =
       do 
       writeFile (outputFile (fnm++".dot")) (show dot)
       putStrLn ("Processing "++fnm++".dot ... :")
       result <- system $ "neato -Tpng "++(outputFile (fnm++".dot"))++" -o "++(outputFile (fnm++".png"))
       case result of
          ExitSuccess   -> putStrLn ("  "++fnm++".png created.")
          ExitFailure x -> putStrLn $ "Failure: " ++ show x
 --REMARK -> the Data.GraphViz.Command function does not work properly (tested on Windows only)
 --    success <- runGraphviz testdot Png (outputFile fnm)
 --    return ()
