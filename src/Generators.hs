
module Generators where


import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (showADL)
import Strings      (chain)
import System.FilePath(combine,replaceExtension)

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
    = putStr (chain "\n\n" (map showADL (serviceG fSpec)))



showHaskell :: Fspc -> Options -> IO ()
showHaskell fSpec flags 
    = verboseLn flags ("\nGenerating Haskell source code for "++name fSpec) >>
      verboseLn flags outputFile >>
      writeFile outputFile haskellCode >>
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
        --     ++">  "++baseName++"\n>   = "++showHS "\n>     " fSpec
             ++">  "++baseName++"\n>   = "++"<<showHS fSpec>>"
                ) 
       
       