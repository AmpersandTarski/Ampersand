{-# OPTIONS_GHC -Wall #-}
module Generators where


import System.FilePath(combine,replaceExtension)
import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (showADLcode)
import ShowXML      (showXML)
import Strings      (chain)
import Calc         (deriveProofs)
import Version      (versionbanner)
import Data.Document
import Fspec2Doc 
import System
serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec _
    = putStr (chain "\n\n" (map (showADLcode fSpec) (serviceG fSpec)))

prove :: Fspc -> Options -> IO()
prove fSpec _
    = putStr (deriveProofs fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile haskellCode 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".lhs")
         haskellCode =
                ("> module Main where"
             ++"\n>  import UU_Scanner"
             ++"\n>  import Classification"
             ++"\n>  import Typology"
             ++"\n>  import ADLdef"
             ++"\n>  import ShowHS (showHS)"
             ++"\n>  import Adl.Fspec"
             ++"\n"
             ++"\n>  main = putStr (showHS \"\\n>  \" "++baseName flags++")"
             ++"\n\n"
             ++">  "++baseName flags++"\n>   = "++showHS "\n>     " fSpec
                ) 
doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generation of Atlas is currently not supported."
  >> verboseLn flags ("Atlas would be generated in " ++ show (dirAtlas flags) ++ ".")
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++
                             "<tns:ADL xmlns:tns=\"http://www.sig-cc.org/ADL\" "++
                             "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "++
                             "xsi:schemaLocation=\"http://www.sig-cc.org/AdlDocs "++
                             "ADL.xsd \">"++
                             "<!-- Generated with "++ versionbanner ++", at "++ show (genTime flags) ++" -->" ++
                             showXML fSpec ++
                             "</tns:ADL>"
                           )   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto _ flags
   =  verboseLn flags "Generation of Prototype is currently not supported."
   >> verboseLn flags ("Prototype files would be written into " ++  (dirPrototype flags) ++ "." ) 

doGenFspecLaTeX :: Fspc -> Options -> IO()
doGenFspecLaTeX fSpec flags
   =  verboseLn flags "Generating LaTeX functional specification document..." >>
      writeFile outputFile (render2LaTeX (fSpec2document fSpec flags)
                           )   
   >> verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
--   >> verboseLn flags ("Processing .tex file into .pdf...")
--   >> system ("pdftex outputFile ")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".tex")
               
doGenFspecHtml :: Fspc -> Options -> IO()
doGenFspecHtml fSpec flags
   =  verboseLn flags "Generating Html functional specification document..." >>
      writeFile outputFile ( "leeg"
                           )   
   >> verboseLn flags ("Functional specification written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".html")
               
doGenFspecRtf :: Fspc -> Options -> IO()
doGenFspecRtf fSpec flags
   =  verboseLn flags "Generating Rtf functional specification document..." >>
      writeFile outputFile ( "leeg"
                           )   
   >> verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".rtf")
               
          