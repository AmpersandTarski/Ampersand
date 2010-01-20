{-# OPTIONS_GHC -Wall #-}
module Generators (doGenAtlas
                  ,doGenXML
                  ,doGenHaskell
                  ,doGenProto
                  ,doGenFspec
                  ,serviceGen
                  ,prove)
where

import System                 (system, ExitCode(ExitSuccess,ExitFailure))
--import System.Process
import System.FilePath        (combine,replaceExtension)
import System.Directory
import Control.Monad
import Options
import Data.Fspec
import ShowHS                 (fSpec2Haskell)
import ShowADL
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (fSpec2Pandoc,laTeXheader)
import Atlas.Atlas
import Data.List              (isInfixOf)
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
     (if graphics flags 
          then foldr1 (>>) [ writePicture flags p| p<-thePictures] 
          else verboseLn flags "No graphics generated." )                                      >>
     (case fspecFormat flags of
       FLatex  ->  makePdfFile
       _ -> verboseLn flags "Done."
     ) 

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
                            exists <- doesFileExist (texHdrFile flags)
                            header <- if exists 
                                      then readFile (texHdrFile flags)
                                      else return (laTeXheader flags)
                            writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerHeader=header} thePandoc)
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
         makePdfFile :: IO()
         makePdfFile = do curDir <- getCurrentDirectory
                          setCurrentDirectory (dirOutput flags)
                          verboseLn flags ("original directory: "++show curDir)
                          verboseLn flags ("now running in    : "++show (dirOutput flags))
                  --        removeOldFiles
                          (ready,nrOfRounds) <- doRestOfPdfLatex (False, 0)  -- initialize with: (<NotReady>, <0 rounds so far>)
                          verboseLn flags ("PdfLatex was called "++show nrOfRounds++" times"++
                               case ready of
                                  True  -> "."
                                  False -> ", but did not solve all refferences!")
                          when ready (setCurrentDirectory curDir)                                
            where 
--              removeOldFiles :: IO()
--              removeOldFiles
--                = mapM_ dump ["aux","pdf","toc","bbl","blg","brf","idx","ilg","ind","out",  -- possible output of pdfLatex
--                              "log0","log1","log2","log3","log4"]  --logfiles created on the fly. 
--              dump :: String -> IO()
--              dump extention =
--                do let file = replaceExtension outputFile extention
--                   exists <- doesFileExist file
--                   when exists (removeFile file)  
                
              doRestOfPdfLatex :: (Bool,Int) -> IO (Bool,Int)
              doRestOfPdfLatex (ready, roundsSoFar)
                = if or [ready, roundsSoFar > 4]    -- Make sure we will not hit a loop when something is wrong with call to pdfLatex ...
                  then return (ready, roundsSoFar)
                  else do callPdfLatexOnce
                          let needle = "LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right." -- This is the text of the warning that pdfLatex generates when a rerun is required.
                          {- The log file should be renamed before reading, because readFile opens the file
                             for lazy IO. In a next run, pdfLatex will try to write to the log file again. If it
                             was read using readFile, it will fail because the file is still open. 8-((  
                          -} 
                          renameFile (replaceExtension outputFile "log") (replaceExtension outputFile ("log"++show roundsSoFar))
                          haystack <- readFile (replaceExtension outputFile ("log"++show roundsSoFar))  
                          let notReady = isInfixOf needle haystack
                          when notReady (verboseLn flags "Another round of pdfLatex is required. Hang on...")
                        --  when notReady (dump "log")  -- Need to dump the last log file, otherwise pdfLatex cannot write its log.
                          doRestOfPdfLatex (not notReady, roundsSoFar +1)

                      
              callPdfLatexOnce :: IO ()
              callPdfLatexOnce = 
                 do result <- system ("pdflatex "++outputFile)  
                    case result of 
                       ExitSuccess   -> verboseLn flags ("PDF file created.")
                       ExitFailure x -> verboseLn flags ("Failure: " ++ show x)
              