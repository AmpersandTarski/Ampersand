-- This module is for the definition of Picture and PictureList.
module Picture where
import Options
import System             (system, ExitCode(ExitSuccess,ExitFailure))
import System.FilePath    (combine,replaceExtension)
import Char (isAlpha)
import Languages

type PictureList = [Picture]

data Picture = Pict { reference :: String    -- used to reference the picture in pandoc or tex
                    , dotSource :: String    -- the string representing the .dot 
                    , fullPng   :: FilePath  --the full file path where the .png file resides
                    , fullDot   :: FilePath  --the full file path where the .dot file resides
                    , dotProgName :: String  -- the name of the program to use  ("dot" or "neato" )
                    , figlabel  :: String    -- the label of a picture
                    , title     :: String    -- a human readable name of this picture
                    }
data PictType = PTClassDiagram | PTPattern | PTConcept | PTSwitchBoard

makePicture :: Options  
            -> String   -- Name of the picture
            -> PictType -- Type of the picture
            -> String   -- The dot source
            -> Picture  -- The ADT of a picture
makePicture flags name pTyp dotsource  
    = Pict { reference  = cdName
           , dotSource  = dotsource
           , fullDot    = replaceExtension fullName "dot"
           , fullPng    = replaceExtension fullName "png"
           , figlabel   = "fig:" ++ cdName
           , dotProgName = case pTyp of
                     PTClassDiagram -> "dot"
                     PTPattern      -> "neato"
                     PTConcept      -> "neato"
                     PTSwitchBoard  -> "neato"
           , title      = case (pTyp,language flags) of
                            (PTClassDiagram,English) -> "Class Diagram of " ++ name
                            (PTClassDiagram,Dutch  ) -> "Klassediagram van " ++ name
                            (PTPattern     ,English) -> "Conceptual analysis of " ++ name
                            (PTPattern     ,Dutch  ) -> "Conceptuele analyse van " ++ name
                            (PTConcept     ,English) -> "Neighbouhood of Concept " ++ name
                            (PTConcept     ,Dutch  ) -> "Omgeving van Concept " ++ name
                            (PTSwitchBoard ,English) -> "Switchboard diagram of " ++ name
                            (PTSwitchBoard ,Dutch  ) -> "Switchboard diagram van " ++ name
           }
       where
         cdName = (case pTyp of
                     PTClassDiagram -> "CD_"
                     PTPattern      -> "Pat_"
                     PTConcept      -> "Cpt_"
                     PTSwitchBoard  -> "SB_"
                  ) ++[c|c<-name, isAlpha c]
         fullName = combine (dirOutput flags) cdName

writePicture :: Options -> Picture -> IO()
writePicture flags pict
    = do verboseLn flags ("Generating picture of "++title pict++" ... :")
         writeFile (fullDot pict) (dotSource pict)
         verboseLn flags   (dotProgName pict++" -Tpng "++(fullDot pict)++" -o "++(fullPng pict))
         result <- system $ (dotProgName pict)++" -Tpng "++(fullDot pict)++" -o "++(fullPng pict)
         case result of 
             ExitSuccess   -> verboseLn flags (fullPng pict++" written.")
             ExitFailure x -> putStrLn ("Failure: " ++ show x)         