{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import System.FilePath        (combine,replaceExtension)
import DatabaseDesign.Ampersand
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjServices)

--import Data.Ampersand.Main

main :: IO ()
main
--
-- = newmain
--oldmain
 = do flags <- getOptions
      if showVersion flags || showHelp flags
       then mapM_ putStr (helpNVersionTexts flags)
       else
          (    parseFile flags 
           >>= calculate flags 
           >>= generate flags
          ) 
                
parseFile :: Options -> IO(Context)
parseFile flags  
      = let fnFull = fileName flags in
        do verbose flags "Parsing... "
           adlText <- readFile fnFull
           importpops <- parseImportFile adlText fnFull flags 
           parseADL1 adlText importpops flags fnFull 

parseImportFile :: String -> String -> Options -> IO(Populations Concept)  
parseImportFile adlText adlfn flags  
      = let fn = importfile flags in
        if not(null(importfile flags))
        then do verbose flags "Parsing import file... "
                popText <- readFile fn
                case importformat flags of
                  Adl1PopFormat -> do verbose flags "Importing ADL1 populations file... "
                                      parseADL1Pop popText fn 
                  Adl1Format -> do verbose flags "Importing ADL1 file... "
                                   cx <- parseADL1 popText [] flags fn
                                   fspec <- calculate flags cx
                                   atlas <- parseADL1 adlText [] flags adlfn
                                   return (makeADL1Populations (declarations atlas) fspec)
        else return []


calculate :: Options -> Context -> IO(Fspc)
calculate flags context = do verboseLn flags "Calculating..."
                             return (makeFspec flags context)
                          
                               

generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
    sequence_ 
       ([ verboseLn     flags "Generating..."]++
        [ doGenXML      fSpec flags | genXML       flags] ++
        [ doGenHaskell  fSpec flags | haskell      flags] ++ 
        [ doGenProto    fSpec flags | genPrototype flags] ++
        [ serviceGen    fSpec flags | servicesG    flags] ++
        [ doGenDocument fSpec flags | genFspec     flags] ++ 
        [ prove         fSpec flags | proofs       flags] ++
        [ diagnose      fSpec flags | diag         flags] ++
        [ verbose flags "Done."]
       ) 

serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ showADLcode fSpec fSpec)
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec flags
    = putStr (deriveProofs flags fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile (fSpec2Haskell fSpec flags) 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
           = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")
   
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
     >> if (test flags) then verboseLn flags (show (vplugInfos fSpec)) else verboseLn flags ""
     where 
     explainviols = concat [show p++": "++showADLcode fSpec r++"\n"|(r,p)<-violations fSpec]

-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: Fspc -> Options -> IO()
doGenDocument fSpec flags
   = verboseLn flags ("Processing "++name fSpec++" towards "++outputFile)     >>
     makeOutput                                                               >>
     verboseLn flags ("Document has been written into " ++ outputFile ++ ".") >>
     when (genGraphics flags && not(null thePictures)) 
          (foldr1 (>>) [ writePicture flags p| p<-thePictures] )              >>
     -- postProcessing of the generated output file depends on the format:
     postProcessor
       where
       (thePandoc,thePictures) 
            = case theme flags of
                 ProofTheme   -> case fspecFormat flags of
                                   FLatex -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                                   _      -> error "!Fatal (module Generators 101): Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 DefaultTheme -> funcSpec
                 StudentTheme -> funcSpec
               where funcSpec = fSpec2Pandoc fSpec flags --generate a func spec
       (outputFile,makeOutput,postProcessor) = writepandoc flags thePandoc


-- The following function assumes a syntactically correct Ampersand script,
-- which may contain type errors
-- it prints a diagnosis of the script.
-- Status: this is a stub. Nothing has been done on this topic so far.
diagnose :: Fspc -> Options -> IO()
diagnose fSpec flags
   = verboseLn flags ("Processing "++name fSpec++" towards "++outputFile)     >>
--     putStr (diagnosis flags fSpec) >>
     verboseLn flags ("Nothing written into " ++ outputFile ++ ", because diagnosis is not yet implemented.")
       where
       (thePandoc,_)
            = case theme flags of
                 ProofTheme   -> case fspecFormat flags of
                                   FLatex -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                                   _      -> error "!Fatal (module Generators 122): Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 DefaultTheme -> funcSpec
                 StudentTheme -> funcSpec
               where funcSpec = fSpec2Pandoc fSpec flags --generate a func spec
       (outputFile,_,_) = writepandoc flags thePandoc

