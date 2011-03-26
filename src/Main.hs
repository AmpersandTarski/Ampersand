{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import System.FilePath        (combine,replaceExtension,dropFileName)
import System.Directory       (getDirectoryContents)
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjServices)
import DatabaseDesign.Ampersand_Prototype.Apps         (picturesForAtlas,atlas2context)
import DatabaseDesign.Ampersand
import DatabaseDesign.Ampersand_Prototype.Version 

fatal :: Int -> String -> a
fatal i msg = error (fatalMsg "Main" i msg)

--import Data.Ampersand.Main

main :: IO ()
main
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
           parsedfile <- parseADL1 adlText importpops flags fnFull 
           atlasfspec <- calculate flags parsedfile
           parsedatlas <- atlas2context atlasfspec flags
           if servicesG flags then return parsedatlas else return parsedfile

parseImportFile :: String -> String -> Options -> IO(Populations Concept)  
parseImportFile adlText adlfn flags  
 = let fn = importfile flags 
       fdir = let d=dropFileName fn in if null d then "." else d
       usr= namespace flags
       getr r = if length r==1 then head r else error "import error: no or multiple declarations for relvar"
       impctx atlas = [makeRelation d|d<-declarations atlas,name d=="loadcontext"]
       impfil atlas = [makeRelation d|d<-declarations atlas,name d=="loadedfile"]
       impupl atlas = [makeRelation d|d<-declarations atlas,name d=="upload"]
       usrfil atlas = [makeRelation d|d<-declarations atlas,name d=="fileof"]
       funrld atlas = [makeRelation d|d<-declarations atlas,name d=="reload"]
       funfsp atlas = [makeRelation d|d<-declarations atlas,name d=="funcspec"]
       funrep atlas = [makeRelation d|d<-declarations atlas,name d=="report"]
       funadl atlas = [makeRelation d|d<-declarations atlas,name d=="showADL"]
       loadcontext r fspec = [Popu{ popm=getr r, popps=[mkPair fn (name fspec)]}]
       loadedfile r        = [Popu{ popm=getr r, popps=[mkPair usr fn]         }| not (null usr)]
       uploadfile r        = [Popu{ popm=getr r, popps=[mkPair usr "browse"]   }| not (null usr)]
       --TODO -> the user has more files, how do I get them in this population
       fileof r myfiles    = [Popu{ popm=getr r, popps=[mkPair (combine fdir f) usr]  }| f<-myfiles, not (null usr)]
       contextfunction fspec r x
                           = [Popu{ popm=getr r, popps=[mkPair (name fspec) x] }]
   in
   if not(null fn)
   then do verbose flags "Parsing import file... "
           popText <- readFile fn
           case importformat flags of
             Adl1PopFormat -> do verbose flags "Importing ADL1 populations file... "
                                 parseADL1Pop popText fn 
             Adl1Format -> do verbose flags "Importing ADL1 file... "
                              cx <- parseADL1 popText [] flags fn
                              fspec <- calculate flags cx
                              verbose flags "writing pictures for atlas... "
                              sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                              verbose flags "pictures for atlas written... "
                              atlas <- parseADL1 adlText [] flags adlfn
                              myfiles <- getDirectoryContents fdir >>= return . filter (`notElem` [".", ".."])
                              verboseLn flags "Generating pictures for atlas..."
                              sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                              return (makeADL1Populations (declarations atlas) [fspec]
                                    ++makeADL1Populations (declarations atlas) (picturesForAtlas flags fspec)
                                    ++loadcontext (impctx atlas) fspec
                                    ++loadedfile (impfil atlas)
                                    ++uploadfile (impupl atlas)
                                    ++fileof (usrfil atlas) myfiles
                                    ++ contextfunction fspec (funrld atlas) (name fspec)
                                    ++ contextfunction fspec (funfsp atlas) "genereer"
                                    ++ contextfunction fspec (funrep atlas) "genereer"
                                    ++ contextfunction fspec (funadl atlas) usr
                                     )
   else return []

calculate :: Options -> Context -> IO(Fspc)
calculate flags context = do verboseLn flags "Calculating..."
                             return (makeFspec flags context)
                          
                               

generate :: Options -> Fspc -> IO ()
generate flags fSpec = 
    sequence_ 
       ([ verboseLn     flags "Generating..."]++
        [ doGenProto    fSpec flags | genPrototype flags] ++
        [ serviceGen    fSpec flags | servicesG    flags] ++
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
     >> if not (theme flags==StudentTheme) && (not.null) (violations fSpec) 
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
                                   _      -> fatal 118 $ "Ampersand only supports proof documents output in LaTeX format. try `-fLatex`"
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
                                   _      -> fatal 139 "Ampersand only supports proof documents output in LaTeX format. try `-fLatex`"
                 DefaultTheme -> funcSpec
                 StudentTheme -> funcSpec
               where funcSpec = fSpec2Pandoc fSpec flags --generate a func spec
       (outputFile,_,_) = writepandoc flags thePandoc

