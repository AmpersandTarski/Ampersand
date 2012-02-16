module DatabaseDesign.Ampersand.Output.GenBericht where

import Data.List

import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Fspec

generateBericht :: Fspc -> Options -> IO [(String, String)]
generateBericht fSpec opts = return [("Bericht.csv", layout . genBerichtInterfaces $ interfaceS fSpec)]

genBerichtInterfaces :: [Interface] -> [[String]]
genBerichtInterfaces interfaces = [["Naam", "Type"]] ++
                                  concatMap genBerichtInterface interfaces

genBerichtInterface :: Interface -> [[String]]
genBerichtInterface interface = genBerichtObjDef 0 (ifcObj interface) ++ [["",""]]

genBerichtObjDef :: Int -> ObjectDef -> [[String]]
genBerichtObjDef nesting objDef = indentHead nesting $ 
   [[name objDef, name (target $ objctx objDef)]] ++
   concatMap (genBerichtObjDef (nesting+1)) (objats objDef)           


indentHead i lines = [[replicate i ' '++c1]++line | (c1:line) <- lines]

layout :: [[String]] -> String
layout lines = 
  let columns = transpose lines
      widths = map (sum . map length) $ columns
      formatColumn col = let width = maximum . map length $ col
                         in  map (fill width) col
  in  unlines . map (intercalate " ") . transpose . map formatColumn $ columns
 where fill i str = str ++ take (i - length str) (replicate i ' ') 