{-# OPTIONS_GHC -Wall #-}
module Generators (generate)
where

import System                 (system, ExitCode(ExitSuccess,ExitFailure))
--import System.Process
import System.FilePath        (combine,replaceExtension)
import System.Directory
import System.Info (os)
import Control.Monad
import Maybe                  (fromJust)
import Options
import Data.Fspec   hiding (services)
import ShowHS                 (fSpec2Haskell)
import ShowADL
import XML.ShowXMLtiny        (showXML)
import Calc                   (deriveProofs)
import Prototype.ObjBinGen    (phpObjServices)
import Adl
import Fspec2Pandoc           (fSpec2Pandoc,laTeXtemplate)
import Atlas.Atlas
import Data.List              (isInfixOf)
import Text.Pandoc
import Picture

generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
    sequence_ 
       ([ verboseLn    flags "Generating..."]++
        [ doGenAtlas   fSpec flags | genAtlas     flags] ++
        [ doGenXML     fSpec flags | genXML       flags] ++
        [ doGenHaskell fSpec flags | haskell      flags] ++ 
        [ doGenProto   fSpec flags | genPrototype flags] ++
        [ serviceGen   fSpec flags | services     flags] ++
        [ doGenFspec   fSpec flags | genFspec     flags] ++ 
        [ prove        fSpec flags | proofs       flags] ++
        [ verbose flags "Done."]
       ) 

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
     >> if (test flags) then verboseLn flags (show (vplugs fSpec)) else verboseLn flags ""
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
     when (genGraphics flags) (foldr1 (>>) [ writePicture flags p| p<-thePictures] )          >>
     -- preprocessing of the generated output file depends on the format:
     case fspecFormat flags of   
       FLatex  -> makePdfFile
       _       -> return()
      

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
              FPandoc -> do verboseLn flags ("Generating to Pandoc: "++outputFile)
                            writeFile outputFile (prettyPandoc thePandoc)
              FRtf    -> do verboseLn flags ("Generating to Rich Text Format: "++outputFile)
                            writeFile outputFile (writeRTF ourDefaultWriterOptions thePandoc)
              FLatex  -> do --REMARK -> notice usage of fromJust
                            exists <- case texHdrFile flags of
                                         Just x -> doesFileExist x
                                         Nothing -> return False
                            header <- if exists 
                                      then readFile (fromJust$texHdrFile flags)
                                      else return (laTeXtemplate flags)
                            verboseLn flags ("Generating to LaTeX: "++outputFile)
                            writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerTemplate=header} thePandoc)
              FHtml   -> do verboseLn flags ("Generating to HTML: "++outputFile)
                            writeFile outputFile (writeHtmlString  ourDefaultWriterOptions thePandoc)
              FOpenDocument 
                      -> do verboseLn flags ("Generating to Open Document Format: "++outputFile)
                            writeFile outputFile (writeOpenDocument ourDefaultWriterOptions thePandoc)
           where 
              ourDefaultWriterOptions = defaultWriterOptions
                                          { writerStandalone=True
                                          , writerTableOfContents=True
                                          , writerNumberSections=True
                                          }
         makePdfFile :: IO()
         makePdfFile = do 
                  --        removeOldFiles
                          (ready,nrOfRounds) <- doRestOfPdfLatex (False, 0)  -- initialize with: (<NotReady>, <0 rounds so far>)
                          verboseLn flags ("PdfLatex was called "++
                                           (if nrOfRounds>1 then show nrOfRounds++" times" else "once")++
                                           case ready of
                                              True  -> "."
                                              False -> ", but did not solve all references!")                          
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
                          let needle = "Rerun to get cross-references right." -- This is the text of the LaTeX Warning telling that label(s) may have changed. 
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
                 do result <- if os=="mingw32" || os=="mingw64" || os=="cygwin" || os=="windows" --REMARK: not a clear enum to check for windows OS
                              then system ("pdflatex "++pdfflags++ outputFile++[x|x<-"> "++combine (dirOutput flags) "pdflog",not(verboseP flags)])  
                              --REMARK: MikTex is windows; Tex-live does not have the flag -include-directory.
                              else system ("cd "++(dirOutput flags)
                                         ++" && pdflatex "
                                         ++ replaceExtension 
                                              (baseName flags) 
                                              (case fspecFormat flags of        
                                                 FPandoc       -> ".pandoc"
                                                 FRtf          -> ".rtf"
                                                 FLatex        -> ".tex"
                                                 FHtml         -> ".html"
                                                 FOpenDocument -> ".odt"
                                              )++[x|x<-"> pdflog",not(verboseP flags)])
                    case result of 
                       ExitSuccess   -> verboseLn flags ("PDF file created.")
                       ExitFailure x -> verboseLn flags ("Failure: " ++ show x)
                    where
                    pdfflags = " -include-directory="++(dirOutput flags)++ " -output-directory="++(dirOutput flags)++" "
              
