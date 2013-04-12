{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder 
  (meatGrinder)
where

import Data.List
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc

meatGrinder :: Options -> Fspc -> (FilePath, String)
meatGrinder flags _ = ("TemporaryPopulationsFileOfRap" ,content)
 where 
  content =
    intercalate "\n"
     [ "{-Generated code by "++ampersandVersionStr++" at "++show (genTime flags)++"-}"
     , "CONTEXT RapPopulations"
     , "ENDCONTEXT"
     ]

