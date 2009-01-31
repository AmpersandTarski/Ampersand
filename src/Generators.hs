
module Generators where


import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (showADL)
import ShowXML      (showXML)
import Strings      (chain)
import System.FilePath(combine,replaceExtension)
import Version      (versionbanner)

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
    = putStr (chain "\n\n" (map showADL (serviceG fSpec)))


doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags "Generation of Haskell code is currently not supported."
   >> verboseLn flags ("Haskell code would be written into " ++ show fileName ++ ".")
       where fileName
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".lhs")
           
showHaskell :: Fspc -> Options -> IO ()
showHaskell fSpec flags 
    = verboseLn flags ("\nGenerating Haskell source code for "++name fSpec) >>
      verboseLn flags outputFile >>
      putStr  haskellCode >>
      verboseLn flags (outputFile ++ " has been written...")
      where
       baseName = "f_Ctx_"++(name fSpec)
       outputFile = combine(dirOutput flags) (replaceExtension baseName ".lhs")
       haskellCode =
                ("> module Main where"
             ++"\n>  import UU_Scanner"
             ++"\n>  import Classification"
             ++"\n>  import Typology"
             ++"\n>  import ADLdef"
             ++"\n>  import ShowHS (showHS)"
             ++"\n>  import Data.Fspec"
             ++"\n"
             ++"\n>  main = putStr (showHS \"\\n>  \" "++baseName++")"
             ++"\n\n"
             ++">  "++baseName++"\n>   = "++showHS "\n>     " fSpec
                ) 
doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generation of Atlas is currently not supported."
  >> verboseLn flags ("Atlas would be generated in " ++ show (dirAtlas flags) ++ ".")
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags
   =  verboseLn flags "Generating XML..."
   >> writeFile fileName ( "<!-- "++ versionbanner ++" -->" ++ showXML fSpec)   
   >> verboseLn flags ("XML written into " ++ show fileName ++ ".")
   where fileName
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Generation of Prototype is currently not supported."
   >> verboseLn flags ("Prototype files would be written into " ++ show (dirPrototype flags) ++ "." ) 

          