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
import ShowHS       (fSpec2Haskell)
import ShowADL      (printadl)
import XML.ShowXMLtiny      (showXML)
import Calc         (deriveProofs)
import Prototype.ObjBinGen (phpObjServices)
import Adl
import Fspec2Pandoc (render2Pandoc,fSpec2Pandoc)
import Strings      (remSpaces)
import Atlas.Atlas

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
   >> writeFile outputFile (fSpec2Haskell fSpec flags) 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
           = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")


doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generation of Atlas is currently not supported."
  >> verboseLn flags ("Atlas would be generated in " ++ show (dirAtlas flags) ++ ".")
--  >> generatepngs fSpec flags
--  krijg ik plaatjes in de database?
  >> fillAtlas fSpec flags
--  data in de atlas database pompen
--  data in autorisatiecontext (=student) plaatsen
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML fSpec (genTime flags))   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Checking on rule violations..."
     >> if (not.null) (violations fSpec) 
        then verboseLn flags explainviols else verboseLn flags "No violations found." 
     >> verboseLn flags "Generating prototype..."
     >> phpObjServices fSpec flags  
     >> verboseLn flags ("Prototype files have been written to " ++  (dirPrototype flags) ++ "." )
     >> if (test flags) then verboseLn flags (show $ vplugs fSpec) else verboseLn flags ""
     where 
     explainviols = foldr (++) [] [show p++": "++printadl (Just fSpec) 0 r++"\n"|(r,p)<-violations fSpec]
     
 
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
generatepngs fSpec flags = foldr (>>) (verboseLn flags "All pictures written..") (dots)-- ++ cds)
   where 
   outputFile fnm = combine (dirOutput flags) fnm
   dots = [run (remSpaces (name p)) $ toDot fSpec flags p 
          | p<-vpatterns fSpec, (not.null) (concs p)]
--   cds = [run_cd (remSpaces$"CD_"++fnm) cd|cd@(OOclassdiagram{nameandcpts=(fnm,_)})<-classdiagrams fSpec] 
--   run_cd fnm cd =
--       do
--       writeFile (outputFile (fnm++".dot")) (classdiagram2dot cd)
--       putStrLn ("Processing "++fnm++".dot ... :")
--       result <- system $ "dot -Tpng "++(outputFile (fnm++".dot"))++" -o "++(outputFile (fnm++".png"))
--       result <- runGraphvizCommand Neato (classdiagram2dot cd) Png (outputFile (fnm++".png"))
--       case result of
--          ExitSuccess   -> putStrLn ("  "++fnm++".png created.")
--          ExitFailure x -> putStrLn $ "Failure: " ++ show x
   run fnm dot = makeGraphic (outputFile fnm) dot
--       do 
--       writeFile (outputFile (fnm++".dot")) (show dot)
--       putStrLn ("Processing "++fnm++".dot ... :")
--       result <- system $ "neato -Tpng "++(outputFile (fnm++".dot"))++" -o "++(outputFile (fnm++".png"))
--       succes <- runGraphvizCommand Neato dot Canon (outputFile (fnm++".dot"))
--       if succes 
--          then putStrLn ("  "++fnm++".png created.")
--          else putStrLn ("Failure: could not create " ++ fnm++".png")
 --REMARK -> the Data.GraphViz.Command function does not work properly (tested on Windows only)
 --    success <- runGraphviz testdot Png (outputFile fnm)
 --    return ()
   makeGraphic fullFile dot
     = do 
       succes <- runGraphvizCommand Neato dot Canon dotfile
       if succes
          then do
            result <- system ("neato -Tpng "++dotfile++ " -o "++pngfile)
            case result of 
               ExitSuccess   -> putStrLn (" "++pngfile++" created.")
               ExitFailure x -> putStrLn ("Failure: " ++ show x)
          else putStrLn ("Failure: could not create " ++ dotfile) 
     where
       dotfile = replaceExtension fullFile "dot"
       pngfile = replaceExtension fullFile "png"
       
            
