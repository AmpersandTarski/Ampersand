module DatabaseDesign.Ampersand.Output.GenBericht where

import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec

generateBericht :: Fspc -> Options -> IO [(String, String)]
generateBericht fSpec opts = return [("Bericht1.csv","bericht1"),("Bericht2.csv","bericht2")]