{-# OPTIONS_GHC -Wall #-}
-- This module is for the definition of Picture and PictureList.
module Picture ( Picture(origName,uniqueName,figlabel,caption,imgURL,pType) -- Other fields are hidden, for there is no need for them outside this module...
               , Pictures,PictType(..),uniquePicName
               , makePictureObj,writePicture) 
where
import Options
import System             (system, ExitCode(ExitSuccess,ExitFailure))
import System.FilePath   -- (replaceExtension,takeBaseName, (</>) )
import System.Directory
import Languages
import Strings (spacesToUnderscores)
import Control.Monad
import Data.GraphViz


type Pictures = [Picture]
data Picture = Pict { origName     :: String    -- The original name of the object this picture was made for. (could include spaces!)
                    , pType        :: PictType  -- the type of the picture
                    , uniqueName   :: String    -- used to reference the picture in pandoc or tex
                    , dotSource    :: String    -- the string representing the .dot 
                    , fullDot      :: FilePath  -- the full file path where the .dot file resides
                    , fspecPath    :: FilePath  -- the full file path where the .png file resides for functional specification
                    , atlasPath    :: FilePath  -- the full file path where the .png and .map file resides for Atlas
                    , imgURL       :: URL       -- the URL that points to the generated .png imagefile, for use in the atlas
                    , dotProgName  :: String    -- the name of the program to use  ("dot" or "neato" )
                    , figlabel     :: String    -- the label of a picture (usefull for reffering to it e.g. in LaTeX)
                    , caption      :: String    -- a human readable name of this picture
                    }
data PictType = PTClassDiagram | PTPattern | PTConcept | PTRule |PTSwitchBoard |PTFservice deriving Eq
picType2prefix :: PictType -> String
picType2prefix pt = case pt of
                      PTClassDiagram -> "CD_"
                      PTPattern      -> "Pat_"
                      PTConcept      -> "Cpt_"
                      PTRule         -> "Rul_"
                      PTSwitchBoard  -> "SB_"
                      PTFservice     -> "Serv_"
makePictureObj :: Options  
            -> String   -- Name of the picture
            -> PictType -- Type of the picture
            -> String   -- The dot source. Should be canonnical.
            
            -> Picture  -- The ADT of a picture
makePictureObj flags name pTyp dotsource
    = Pict { origName   = name 
           , uniqueName   = cdName
           , dotSource  = dotsource
           , fullDot    = dirOutput flags  </> relImgPath </> replaceExtension cdName "dot"
           , fspecPath  = dirOutput flags  </> relImgPath </> addExtension cdName "png" 
           , atlasPath  = dirAtlas  flags  </> relImgPath </> addExtension cdName "png"
           , imgURL     = UStr (dirAtlas  flags  </> relImgPath </> addExtension cdName "png")
           , pType      = pTyp
           , figlabel   = "fig:" ++ cdName
           , dotProgName = case pTyp of
                     PTClassDiagram -> "dot"
                     PTPattern      -> "neato"
                     PTConcept      -> "neato"
                     PTRule         -> "neato"
                     PTSwitchBoard  -> "dot"
                     PTFservice     -> "dot"
           , caption      = case (pTyp,language flags) of
                            (PTClassDiagram,English) -> "Class Diagram of " ++ name
                            (PTClassDiagram,Dutch  ) -> "Klassediagram van " ++ name
                            (PTPattern     ,English) -> "Conceptual analysis of " ++ name
                            (PTPattern     ,Dutch  ) -> "Conceptuele analyse van " ++ name
                            (PTConcept     ,English) -> "Neighbourhood of Concept " ++ name
                            (PTConcept     ,Dutch  ) -> "Omgeving van Concept " ++ name
                            (PTSwitchBoard ,English) -> "Switchboard diagram of " ++ name
                            (PTSwitchBoard ,Dutch  ) -> "Schakelpaneel van " ++ name
                            (PTRule        ,English) -> "Knowledge graph about " ++ name
                            (PTRule        ,Dutch  ) -> "Kennisgraaf rond " ++ name
                            (PTFservice    ,English) -> "Service graph "++ name  -- TODO betere tekts
                            (PTFservice    ,Dutch  ) -> "Service graaf "++ name  --TODO betere tekst
           }
       where
         relImgPath | genAtlas flags = "img" </> (takeWhile (/='.') (userAtlas flags)) </> (baseName flags)    
                    | otherwise = []
         cdName = uniquePicName pTyp name
--GMI voor Han -> (isAlpha c) verwijdert uit lijst comprehensie, dit gooit nummers (bv. rule nummers) uit de naam weg
--       zodat alle ongelabelde rules de naam RUL_Rule hebben, dat is niet uniek.
--       Deze functie garandeert sowieso geen uniekheid, is die garantie nodig?
--       unieke namen voor (Dotable) datatypes zouden moeten worden gegarandeerd op het datatype als dat nodig is
uniquePicName :: PictType -> String -> String
uniquePicName pt n = picType2prefix pt++[c|c<- spacesToUnderscores n]

--         relImgPath = "img" </> user </> (baseName flags)
--         user = takeWhile (/='.') (userAtlas flags)
writePicture :: Options -> Picture -> IO()
writePicture flags pict
    = sequence_ (
      [when (genAtlas flags ) (do createDirectoryIfMissing True  (takeDirectory (atlasPath pict)))]++
      [when (or [genFspec flags ,genAtlas flags])
                             (do verboseLn flags ("Generating .dot file...")
                                 writeFile (fullDot pict) (dotSource pict)
                             )
      ]++
      [when (genFspec flags) (do verboseLn flags ("Generating figure: "++caption pict++" ... :")
                                 verboseLn flags   (dotProgName pict++" -Tpng "++fullDot pict++" -o "++fspecPath pict)
                                 result <- system $ dotProgName pict++" -Tpng "++fullDot pict++" -o "++fspecPath pict
                                 case result of 
                                   ExitSuccess   -> verboseLn flags (fspecPath pict++" written.")
                                   ExitFailure x -> putStrLn ("Failure: " ++ show x)
                             )
      ]++
      [when (genAtlas flags ) (do verboseLn flags ("Generating image: "++caption pict++" ... :")
                                  verboseLn flags   (dotProgName pict++" -Tpng "++fullDot pict++" -o "++atlasPath pict)
                                  result1 <- system $ dotProgName pict++" -Tpng "++fullDot pict++" -o "++atlasPath pict
                                  case result1 of 
                                    ExitSuccess   -> verboseLn flags (atlasPath pict++" written.")
                                    ExitFailure x -> putStrLn ("Failure: " ++ show x)
                                  result2 <- system $ dotProgName pict++" -Tcmapx "++fullDot pict++" -o "++mapfile 
                                  case result2 of 
                                    ExitSuccess   -> verboseLn flags (mapfile ++" written.")
                                    ExitFailure x -> putStrLn ("Failure: " ++ show x)
                              )
      ])
   where 
     mapfile = replaceExtension (atlasPath pict) "map"
