{-# OPTIONS_GHC -Wall #-}
module Generators (doGenAtlas
                  ,doGenXML
                  ,doGenHaskell
                  ,doGenProto
                  ,doGenFspec
                  ,serviceGen
                  ,prove)
where

import System (system, ExitCode(ExitSuccess,ExitFailure))
import System.FilePath        (combine,replaceExtension)
import Options hiding (services)
import FspecDef
import ShowHS                 (fSpec2Haskell)
import ShowADL
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (fSpec2Pandoc,laTeXheader)
--import Strings                (remSpaces)
import Atlas.Atlas
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
  = (writeFile outputFile $ showADLcode fSpec fSpec)
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

instance ShowADL Fspc where
    showADL fSpec = showADLcode fSpec fSpec
    showADLcode fSpec' fSpec
     = "CONTEXT " ++name fSpec
       ++ (if null (objDefs fSpec)      then "" else "\n"++chain "\n\n" (map (showADLcode fSpec') (objDefs fSpec))      ++ "\n")
       ++ (if null (patterns fSpec)     then "" else "\n"++chain "\n\n" (map (showADLcode fSpec') (patterns fSpec))     ++ "\n")
       ++ (if null (vConceptDefs fSpec) then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vConceptDefs fSpec)) ++ "\n")
       ++ (if null (vgens fSpec)        then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vgens fSpec))        ++ "\n")
       ++ (if null (vkeys fSpec)        then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vkeys fSpec))        ++ "\n")
       ++ (if null (vrels fSpec)        then "" else "\n"++chain "\n"   (map (showADLcode fSpec') (vrels fSpec))        ++ "\n")
       ++ (if null showADLpops          then "" else "\n"++chain "\n\n" showADLpops                                     ++ "\n")
       ++ "\n\nENDCONTEXT"
       where showADLpops = [ showADLcode fSpec' (Popu{popm=makeMph d, popps=decpopu d})
                           | d<-declarations fSpec, not (null (decpopu d))]

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
     explainviols = concat [show p++": "++showADLcode fSpec r++"\n"|(r,p)<-violations fSpec]


-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.

doGenFspec :: Fspc -> Options -> IO()
doGenFspec fSpec flags
   = -- verboseLn flags "Generating functional specification document..."                        >>
     verboseLn flags ("Processing "++name fSpec++" towards "++outputFile)                     >>
     makeOutput                                                                               >>
     verboseLn flags ("Functional specification has been written into " ++ outputFile ++ ".") >>
     (if graphics flags then foldr1 (>>) [ writePicture flags p| p<-thePictures] else putStr "\nNo graphics generated.")>>
     case fspecFormat flags of
      FLatex  -> do result <- system ("pdflatex "++outputFile)
                    case result of 
                      ExitSuccess   -> putStrLn ("PDF file created.")
                      ExitFailure x -> putStrLn ("Failure: " ++ show x)
      _ -> putStr "\nDone."
     where  
         outputFile = replaceExtension (combine (dirOutput flags) (baseName flags)) 
                                       (case fspecFormat flags of        
                                                 FPandoc       -> ".pandoc"
                                                 FRtf          -> ".rtf"
                                                 FLatex        -> ".tex"
                                                 FHtml         -> ".html"
                                                 FOpenDocument -> ".odt"
                                       )
         (thePandoc,thePictures) = fSpec2Pandoc fSpec flags
         makeOutput
          =  case fspecFormat flags of
              FPandoc -> do verboseLn flags "Generating Pandoc file."
                            writeFile outputFile (prettyPandoc thePandoc)
              FRtf    -> do verboseLn flags "Generating Rich Text Format file."
                            writeFile outputFile (writeRTF ourDefaultWriterOptions thePandoc)
              FLatex  -> do verboseLn flags "Generating LaTeX file."
                            case texHdrFile flags of
                             Nothing -> writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerHeader=laTeXheader flags} thePandoc)
                             Just chFilename -> do customheader <- readFile chFilename
                                                   writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerHeader=customheader} thePandoc)
              FHtml   -> do verboseLn flags "Generating Html file."
                            writeFile outputFile (writeHtmlString  ourDefaultWriterOptions thePandoc)
              FOpenDocument 
                      -> do verboseLn flags "Generating OpenDocument file."
                            writeFile outputFile (writeOpenDocument ourDefaultWriterOptions thePandoc)
           where 
              ourDefaultWriterOptions = defaultWriterOptions
                                          { writerStandalone=True
                                          , writerTableOfContents=True
                                          , writerNumberSections=True
                                          }
