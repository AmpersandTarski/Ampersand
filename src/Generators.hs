{-# OPTIONS_GHC -Wall #-}
module Generators where


import System.FilePath(combine,replaceExtension)
import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (showADLcode)
import ShowXML      (showXML')
import Strings      (chain)
import Calc         (deriveProofs)
--import Version      (versionbanner)
import Rendering.Document
import Rendering.Doc2LaTeX
import Rendering.Doc2Word
import Fspec2Doc 
import Version
--import System
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
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML' fSpec (genTime flags))   
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
      writeFile outputFile (render2LaTeX flags (fSpec2document fSpec flags)
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
               
doGenFspecWord :: Fspc -> Options -> IO()
doGenFspecWord fSpec flags
   =  verboseLn flags "Generating Word functional specification document..." >>
      writeFile outputFile ( render2Word flags (fSpec2document fSpec flags)
                           )   
   >> verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".doc")
               
          