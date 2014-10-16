module Database.Design.Ampersand.Output.ToPandoc.ChapterFunctionPointAnalysis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Data.List
import Database.Design.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterFunctionPointAnalysis"

chpFunctionPointAnalysis :: Fspc -> (Blocks,[Picture])
chpFunctionPointAnalysis fSpec
 = ( (chptHeader (fsLang fSpec) FunctionPointAnalysis) <>
     fromList intro
   , []) -- no pictures at the moment
  where
  intro :: [Block]
  intro =
    case fsLang fSpec of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk ..."
                  ]]
      English -> [Para
                  [ Str "This chapter ..."
                  ]]
   