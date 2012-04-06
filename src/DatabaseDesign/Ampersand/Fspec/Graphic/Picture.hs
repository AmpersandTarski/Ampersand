{-# OPTIONS_GHC -Wall #-}
-- This module is for the definition of Picture and PictureList.
module DatabaseDesign.Ampersand.Fspec.Graphic.Picture
    ( Picture(origName,uniqueName,caption,relPng,pType) -- Other fields are hidden, for there is no need for them outside this module...
    , Pictures,PictType(..),uniquePicName
    , makePictureObj,writePicture
    )
where
import System.FilePath   -- (replaceExtension,takeBaseName, (</>) )
import System.Directory
import DatabaseDesign.Ampersand.Misc
import Control.Monad
import DatabaseDesign.Ampersand.Basics  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Commands

fatal :: Int -> String -> a
fatal = fatalMsg "Picture.hs"

type Pictures = [Picture]
data Picture = Pict { origName     :: String    -- ^ The original name of the object this picture was made for. (could include spaces!)
                    , pType        :: PictType  -- ^ the type of the picture
                    , uniqueName   :: String    -- ^ used to reference the picture in pandoc or tex
                    , dotSource    :: DotGraph String    -- ^ the string representing the .dot
                    , fullPath      :: FilePath  -- ^ the full file path where the .dot and .png file resides
                    , relPng       :: FilePath  -- ^ the relative file path where the .png file resides
                    , dotProgName  :: GraphvizCommand   -- ^ the name of the program to use  ("dot" or "neato" )
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

makePictureObj :: Options
            -> String   -- Name of the picture
            -> PictType -- Type of the picture
            -> DotGraph String  -- The dot source. Should be canonnical.

            -> Picture  -- The ADT of a picture
makePictureObj flags nm pTyp dotsource
    = Pict { origName   = nm
           , uniqueName = cdName
           , dotSource  = dotsource
           , fullPath    = absImgPath </> cdName 
           , relPng     = relImgPath </> cdName
           , pType      = pTyp
           , dotProgName = case pTyp of
                     PTClassDiagram -> Dot
                     PTSwitchBoard  -> Dot
                     _              -> Neato
           , caption      = case (pTyp,language flags) of
                            (PTClassDiagram,English) -> "Class Diagram of " ++ nm
                            (PTClassDiagram,Dutch  ) -> "Klassediagram van " ++ nm
                            (PTPattern     ,English) -> "Concept analysis of the rules in " ++ nm
                            (PTPattern     ,Dutch  ) -> "Conceptuele analyse van de regels in " ++ nm
                            (PTFullPat     ,English) -> "Concept analysis of relations in " ++ nm
                            (PTFullPat     ,Dutch  ) -> "Conceptuele analyse van relaties in " ++ nm
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
uniquePicName pt nm = escapeNonAlphaNum (picType2prefix pt++nm)

--         relImgPath = "img" </> user </> (baseName flags)
--         user = takeWhile (/='.') (userAtlas flags)
writePicture :: Options -> Picture -> IO()
writePicture flags pict
    = sequence_ (
      [createDirectoryIfMissing True  (takeDirectory (fullPath pict))     |                   genAtlas flags ]++
      [writeDot (dotProgName pict) Canon (dotSource pict) (fullPath pict) | genFspec flags || genAtlas flags ]++
--      [writeDot (dotProgName pict) XDot  (dotSource pict) (fullPath pict) | genFspec flags || genAtlas flags ]++
      [writeDot (dotProgName pict) Png   (dotSource pict) (fullPath pict) | genFspec flags || genAtlas flags ]++
      [writeDot (dotProgName pict) Cmapx (dotSource pict) (fullPath pict) |                   genAtlas flags ]
          )
   where 
     writeDot :: GraphvizCommand
              -> GraphvizOutput
              -> DotGraph String
              -> FilePath
              -> IO ()
     writeDot gvCommand gvOutput graph filePath = 
         do verboseLn flags ("Generating "++show gvOutput++" using "++show gvCommand++".")
            path <- runGraphvizCommand gvCommand graph gvOutput (replaceExtension filePath (extentionOf gvOutput))
            verboseLn flags (path++" written.")
       where extentionOf :: GraphvizOutput -> String
             extentionOf x = case x of
                Canon -> "dot"
                Png   -> "png"     
                Cmapx -> "map"
                XDot  -> "xdot"
                Svg   -> "svg"
                Gif   -> "gif"
                _     -> fatal 139 "GraphvizOutput has undefined extention"
             
       
