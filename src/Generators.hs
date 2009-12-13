{-# OPTIONS_GHC -Wall #-}
module Generators (doGenAtlas
                  ,doGenXML
                  ,doGenHaskell
                  ,doGenProto
                  ,doGenFspec
                  ,serviceGen
                  ,prove)
where

--import Char (isAlpha)
import Classes.Graphics
--import System (system, ExitCode(ExitSuccess,ExitFailure))

import System.FilePath        (combine,replaceExtension)
import Options
import FspecDef
import ShowHS                 (fSpec2Haskell)
import ShowADL                (printadl)
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (fSpec2Pandoc,laTeXheader)
--import Strings                (remSpaces)
import Atlas.Atlas
import Rendering.ClassDiagram (classdiagram2dot)
import Switchboard            (toDotFspc)
import Data.GraphViz.Types
import Text.Pandoc            ( defaultWriterOptions
                              , prettyPandoc
                              , writerStandalone
                              , writerHeader
                              , writerTableOfContents
                              , writerNumberSections
                              , writeLaTeX
                              , writeRTF
                              , writeOpenDocument
                              , writeHtmlString
                              )
import Picture

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ printadl (Just fSpec) 0 fSpec)
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec _
    = putStr (deriveProofs fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile (fSpec2Haskell fSpec flags) 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
           = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")


doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generating Atlas ..."
  >> verboseLn flags ("The atlas application should have been installed in " ++ show (dirAtlas flags) ++ ".")
  >> fillAtlas fSpec flags
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML fSpec (genTime flags))   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Checking on rule violations..."
     >> if (not.null) (violations fSpec) 
        then verboseLn flags explainviols else verboseLn flags "No violations found." 
     >> verboseLn flags "Generating prototype..."
     >> phpObjServices fSpec flags  
     >> verboseLn flags ("Prototype files have been written to " ++  (dirPrototype flags) ++ "." )
     >> if (test flags) then verboseLn flags (show $ vplugs fSpec) else verboseLn flags ""
     where 
     explainviols = concat [show p++": "++printadl (Just fSpec) 0 r++"\n"|(r,p)<-violations fSpec]


-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be refferenced while rendering the Fspc.
generateAllGraphics :: Options -> Fspc -> IO(Fspc)
generateAllGraphics flags fs
   = do f'  <- generateClassDiagram fs 
        f'' <- generatePatternPictures f'
        f''' <- generateSwitchBoard f''
        return f'''
   
   where 
     generateClassDiagram :: Fspc -> IO(Fspc)
     generateClassDiagram fSpec
        = do writePicture flags pict
             return fSpec{pictCD = Just pict}
        where pict = makePicture flags (name fSpec) PTClassDiagram (classdiagram2dot(classdiagram fSpec))

     generatePatternPictures :: Fspc -> IO(Fspc)     
     generatePatternPictures fSpec
        = do mapM (writePicture flags) pictures
             return fSpec{pictPatts = Just pictures}
          where pictures = map makeGraphic (vpatterns fSpec)
                makeGraphic :: Pattern -> Picture
                makeGraphic pat = makePicture flags (name pat) PTPattern (printDotGraph (toDot fSpec flags pat))
    
     generateSwitchBoard :: Fspc -> IO(Fspc)
     generateSwitchBoard fSpec
        = do writePicture flags pict
             return fSpec{pictSB = Just pict}
        where pict = makePicture flags (name fSpec) PTSwitchBoard (printDotGraph (toDotFspc fSpec flags fSpec))
        
        
doGenFspec :: Fspc -> Options -> IO()
doGenFspec fSpec flags
   =  do fSpec2 <- generateAllGraphics flags fSpec
         verboseLn flags "Generating functional specification document..."
         verboseLn flags ("Processing "++name fSpec2++" towards "++outputFile)
         makeOutput fSpec2
         verboseLn flags ("Functional specification has been written into " ++ outputFile ++ ".")
       where  
         outputFile = replaceExtension (combine (dirOutput flags) (baseName flags)) 
                                       (case fspecFormat flags of        
                                                 FPandoc       -> ".pandoc"
                                                 FRtf          -> ".rtf"
                                                 FLatex        -> ".tex"
                                                 FHtml         -> ".html"
                                                 FOpenDocument -> ".odt"
                                                 FUnknown      -> undefined
                                       )
         makeOutput f
              =  case fspecFormat flags of
             FPandoc -> do verboseLn flags "Generating Pandoc file."
                           writeFile outputFile (prettyPandoc thePandoc)
             FRtf   ->  do verboseLn flags "Generating Rich Text Format file."
                           writeFile outputFile (writeRTF ourDefaultWriterOptions thePandoc)
             FLatex  -> do verboseLn flags "Generating TeX file."
                           case texHdrFile flags of
                            Nothing -> writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerHeader=laTeXheader flags} thePandoc)
                            Just chFilename -> do customheader <- readFile chFilename
                                                  writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerHeader=customheader} thePandoc)
             FHtml   -> do verboseLn flags "Generating Html file."
                           writeFile outputFile (writeHtmlString  ourDefaultWriterOptions thePandoc)
             FOpenDocument 
                     -> do verboseLn flags "Generating OpenDocument file."
                           writeFile outputFile (writeOpenDocument ourDefaultWriterOptions thePandoc)
             FUnknown -> do putStrLn ("Unknown fspec format. Currently supported formats are "++allFspecFormats++".")
           where 
              thePandoc = fSpec2Pandoc f flags
         
         
              ourDefaultWriterOptions = defaultWriterOptions
                                          { writerStandalone=True
                                          , writerTableOfContents=True
                                          , writerNumberSections=True
                                          }
           

