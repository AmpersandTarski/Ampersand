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

import System.FilePath        (combine,replaceExtension)
import Options
import FspecDef
import ShowHS                 (fSpec2Haskell)
import ShowADL                (printadl)
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (render2Pandoc,fSpec2Pandoc)
import Strings                (remSpaces)
import Atlas.Atlas
import Rendering.ClassDiagram (classdiagram2dot)
--import Switchboard            (toDotFspc)
import Data.GraphViz.Types
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Shared

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ printadl (Just fSpec) 0 fSpec)
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

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
     verboseLn flags "Generating Atlas ..."
  >> verboseLn flags ("The atlas application should have been installed in " ++ show (dirAtlas flags) ++ ".")
  >> fillAtlas fSpec flags
   
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
{-
 -- Switchboard
      verboseLn flags ("Processing "++switchboardname++".dot ... :")
      writeFile (switchboardname++".dot") (printDotGraph (toDotFspc fSpec flags fSpec))
      verboseLn flags ("dot -Tpng "++switchboardname++".dot -o "++switchboardname++".png")
      resultSwitchboard <- system $ "dot -Tpng "++switchboardname++".dot -o "++switchboardname++".png"
      case resultSwitchboard of 
         ExitSuccess   -> verboseLn flags (switchboardname++".png"++" written.")
         ExitFailure x -> putStrLn ("Failure: " ++ show x)
      verboseLn flags (switchboardname++".png"++" written.")
-}
 -- Class Diagram
      verboseLn flags ("Processing "++cdFilename++".dot ... :")
      writeFile (cdFilename++".dot") (classdiagram2dot cd)
      verboseLn flags ("dot -Tpng "++cdFilename++".dot -o "++cdFilename++".png")
      resultCD <- system $ "dot -Tpng "++cdFilename++".dot -o "++cdFilename++".png"
      case resultCD of 
         ExitSuccess   -> verboseLn flags (cdFilename++".png"++" written.")
         ExitFailure x -> putStrLn ("Failure: " ++ show x)
 -- Functional Specification Document
      verboseLn flags ("Processing "++name fSpec'++" towards "++outputFile)
      writeFile outputFile
        (let wropts = defaultWriterOptions{writerStandalone=True, writerHeader=customheader, writerTableOfContents=True,writerNumberSections=True}
         in writeLaTeX wropts (fSpec2Pandoc fSpec flags))   -- om Pandoc te snappen, vervang writeLaTeX wropts voor prettyPandoc
      verboseLn flags (outputFile++" written.")
      verboseLn flags ("Generating .png files in "++name fSpec')
      generatepngs fSpec flags
      verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
   where  
   switchboardname = "SB_"++baseName flags
   fSpec'= if (allServices flags)
           then fSpec{serviceS=serviceG fSpec} 
           else fSpec
   (cd,cdFilename)    = classdiagram fSpec
   outputFile         = combine (dirOutput flags) (replaceExtension (baseName flags) (outputExt $ fspecFormat flags))        
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
          | p<-vpatterns fSpec]
   run fnm dot = makeGraphic (outputFile fnm) dot
 --REMARK -> the Data.GraphViz.Command function does not work properly (tested on Windows only)
 --    success <- runGraphviz testdot Png (outputFile fnm)
 --    return ()
   makeGraphic fullFile dot
     = do 
       success <- runGraphvizCommand Neato dot Png dotfile
       verboseLn flags ("runGraphvizCommand("++dotfile++") " ++ (if success then "" else "un") ++ "successfully executed.")
     where
       dotfile = replaceExtension fullFile "dot"
       pngfile = replaceExtension fullFile "png"
       
            
