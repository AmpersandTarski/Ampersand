module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where

import DatabaseDesign.Ampersand_Prototype.CoreImporter  

generateAll :: Fspc -> Options -> IO ()
generateAll fSpec options =
 do { verboseLn options "Experimental Generation"
    }