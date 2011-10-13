{-# OPTIONS_GHC -Wall #-}
-- This module is for the definition of Picture and PictureList.
module DatabaseDesign.Ampersand.Fspec.Graphic.Picture
    ( Picture(origName,uniqueName,figlabel,caption,imgURL,pType) -- Other fields are hidden, for there is no need for them outside this module...
    , Pictures,PictType(..),uniquePicName
    , makePictureObj,writePicture)
where
import Data.Char (isAlphaNum)
import System             (system, ExitCode(ExitSuccess,ExitFailure))
import System.FilePath   -- (replaceExtension,takeBaseName, (</>) )
import System.Directory
import DatabaseDesign.Ampersand.Misc
import Control.Monad
import Data.GraphViz
import DatabaseDesign.Ampersand.Basics  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)

type Pictures = [Picture]
data Picture = Pict { origName     :: String    -- ^ The original name of the object this picture was made for. (could include spaces!)
                    , pType        :: PictType  -- ^ the type of the picture
                    , uniqueName   :: String    -- ^ used to reference the picture in pandoc or tex
                    , dotSource    :: String    -- ^ the string representing the .dot
                    , fullDot      :: FilePath  -- ^ the full file path where the .dot file resides
                    , fspecPath    :: FilePath  -- ^ the full file path where the .png file resides for functional specification
                    , atlasPath    :: FilePath  -- ^ the full file path where the .png and .map file resides for Atlas
                    , imgURL       :: EscString -- ^ the URL that points to the generated .png imagefile, for use in the atlas
                    , dotProgName  :: String    -- ^ the name of the program to use  ("dot" or "neato" )
                    , figlabel     :: String    -- ^ the label of a picture (usefull for reffering to it e.g. in LaTeX)
                    , caption      :: String    -- ^ a human readable name of this picture
                    }
data PictType = PTClassDiagram -- a UML class diagram, or something that comes close
              | PTPattern      -- a conceptual diagram with the relations USED in a pattern
              | PTFullPat      -- a conceptual diagram with the relations DECLARED in a pattern
              | PTProcess      -- a process diagram, that shows dependencies between activities
              | PTProcLang     -- a conceptual diagram that shows the language of a process
              | PTConcept      -- a conceptual diagram that shows a concept in relation with the rules it occurs in.
              | PTRule         -- a conceptual diagram that shows a rule
              | PTSwitchBoard
              | PTFinterface deriving Eq
picType2prefix :: PictType -> String
picType2prefix pt = case pt of
                      PTClassDiagram -> "CD_"
                      PTPattern      -> "Pat_"
                      PTFullPat      -> "Lat_"
                      PTProcess      -> "Proc_"
                      PTProcLang     -> "PL_"
                      PTConcept      -> "Cpt_"
                      PTRule         -> "Rul_"
                      PTSwitchBoard  -> "SB_"
                      PTFinterface     -> "Serv_"

filenameEsc :: String -> String
filenameEsc "" = ""
filenameEsc (' ':cs) = '_': filenameEsc cs
filenameEsc ('\\':cs) = '_': filenameEsc cs
filenameEsc ('/':cs) = '_': filenameEsc cs
filenameEsc (c:cs) | isAlphaNum c = c: filenameEsc cs
                   | otherwise    = filenameEsc cs

makePictureObj :: Options
            -> String   -- Name of the picture
            -> PictType -- Type of the picture
            -> String   -- The dot source. Should be canonnical.

            -> Picture  -- The ADT of a picture
makePictureObj flags nm pTyp dotsource
    = Pict { origName   = nm
           , uniqueName = cdName
           , dotSource  = dotsource
           , fullDot    = absImgPath </> replaceExtension cdName "dot"
           , fspecPath  = absImgPath </> System.FilePath.addExtension cdName "png"
           , atlasPath  = absImgPath </> System.FilePath.addExtension cdName "png"
           , imgURL     = relImgPath </> System.FilePath.addExtension cdName "png"
           , pType      = pTyp
           , figlabel   = "fig:" ++ cdName
           , dotProgName = case pTyp of
                     PTClassDiagram -> "dot"
                     PTSwitchBoard  -> "dot"
                     _              -> "neato"
           , caption      = case (pTyp,language flags) of
                            (PTClassDiagram,English) -> "Class Diagram of " ++ nm
                            (PTClassDiagram,Dutch  ) -> "Klassediagram van " ++ nm
                            (PTPattern     ,English) -> "Concept analysis of the rules in " ++ nm
                            (PTPattern     ,Dutch  ) -> "Conceptuele analysis van de regels in " ++ nm
                            (PTFullPat     ,English) -> "Concept analysis of relations in " ++ nm
                            (PTFullPat     ,Dutch  ) -> "Conceptuele analysis van relaties in " ++ nm
                            (PTProcess     ,English) -> "Process model of " ++ nm
                            (PTProcess     ,Dutch  ) -> "Procesmodel van " ++ nm
                            (PTSwitchBoard ,English) -> "Switchboard diagram of " ++ nm
                            (PTSwitchBoard ,Dutch  ) -> "Schakelpaneel van " ++ nm
                            (_             ,English) -> "Knowledge graph about " ++ nm
                            (_             ,Dutch  ) -> "Kennisgraaf rond " ++ nm
           }
       where
         absImgPath | genAtlas flags = dirPrototype flags </> relImgPath 
                    | otherwise = dirOutput flags  </> relImgPath
         relImgPath | genAtlas flags = "images" 
                    | otherwise = []
         cdName = uniquePicName pTyp nm
--GMI voor Han -> (isAlpha c) verwijdert uit lijst comprehensie, dit gooit nummers (bv. rule nummers) uit de naam weg
--       zodat alle ongelabelde rules de naam RUL_Rule hebben, dat is niet uniek.
--       Deze functie garandeert sowieso geen uniekheid, is die garantie nodig?
--       unieke namen voor (ConceptualGraph) datatypes zouden moeten worden gegarandeerd op het datatype als dat nodig is
uniquePicName :: PictType -> String -> String
uniquePicName pt nm = filenameEsc (picType2prefix pt++nm)

--         relImgPath = "img" </> user </> (baseName flags)
--         user = takeWhile (/='.') (userAtlas flags)
writePicture :: Options -> Picture -> IO()
writePicture flags pict
    = sequence_ (
      [when (genAtlas flags ) (createDirectoryIfMissing True  (takeDirectory (atlasPath pict)))]++
      [when (genFspec flags || genAtlas flags)
                             (do verboseLn flags ("Generating "++fullDot pict)
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
                                  verboseLn flags    (dotProgName pict++" -Tpng "++fullDot pict++" -o "++atlasPath pict)
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
