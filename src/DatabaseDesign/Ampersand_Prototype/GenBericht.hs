module DatabaseDesign.Ampersand_Prototype.GenBericht where

import Prelude hiding (writeFile)
import Data.List
import Text.CSV
import System.FilePath
import System.Directory
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree

-- TODO: only show Rel and Flp Rel? give error otherwise?
--       what about Typ, Brk etc.?

doGenBericht :: Fspc -> Options -> IO ()
doGenBericht fSpec opts =
 do { verboseLn opts "Generating 'Berichtendefinities'..."
    ; createDirectoryIfMissing True $ combine (dirPrototype opts) "Berichten"
    ; genFile "Berichten/Berichten.csv" $ generateBerichtCSV fSpec opts
    }
 where genFile filename contents = 
        do { writeFile (combine (dirPrototype opts) filename) contents
           ; verboseLn opts $ "\nGenerated file "++filename
           }
           
generateBerichtCSV :: Fspc -> Options -> String
generateBerichtCSV fSpec opts = printCSV {-layout-} . genBerichtInterfaces $ interfaceS fSpec

genBerichtInterfaces :: [Interface] -> CSV
genBerichtInterfaces interfaces = ["Naam", "Card.", "Definitie", "Type"] :
                                  concatMap genBerichtInterface interfaces

genBerichtInterface :: Interface -> CSV
genBerichtInterface interface = genBerichtObjDef (ifcObj interface) ++ [["","","",""]]

genBerichtObjDef :: ObjectDef -> CSV
genBerichtObjDef objDef = indentHead 1 $ 
    [ name objDef
    , card $ objctx objDef
    , def $ objctx objDef 
    , name (target $ objctx objDef)
    ] :
    case objmsub objDef of
      Nothing -> []
      Just (InterfaceRef name) -> [["INTERFACEREF "++name,"","",""]]
      Just (Box objs) -> concatMap genBerichtObjDef objs           
 where card e = (if isTot e then "1" else "0")++".."++(if isUni e then "1" else "*")
       
       def (ERel (Rel{reldcl=Sgn{decMean=meaning}}))        = showMeaning meaning
       def (EFlp (ERel (Rel{reldcl=Sgn{decMean=meaning}}))) = "~ "++showMeaning meaning
       def _                                         = "meaningless"
       
       showMeaning meaning = concat [ aMarkup2String m | m@A_Markup{amLang=Dutch} <- ameaMrk meaning ]

indentHead i lines = [(replicate i '+'++c1):line | (c1:line) <- lines]

layout :: [[String]] -> String
layout lines = 
  let columns = transpose lines
      widths = map (sum . map length) columns
      formatColumn col = let width = maximum . map length $ col
                         in  map (fill width) col
  in  unlines . map unwords . transpose . map formatColumn $ columns
 where fill i str = str ++ take (i - length str) (replicate i ' ') 
 