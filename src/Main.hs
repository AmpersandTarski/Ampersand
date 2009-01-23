 {-# LANGUAGE ScopedTypeVariables#-}
module Main where
--   import System (getArgs,getProgName,system, ExitCode(ExitSuccess,ExitFailure))
--   import Char (toLower)
--   import UU_Scanner(scan,initPos)
--   import UU_Parsing(parseIO)
--   import CommonClasses ( Identified(name))
--   import Auxiliaries (chain, commaEng, adlVersion)
--   import Typology (Typology(Typ), typology, makeTrees)
--   import ADLdef
--   import ShowADL
--   import ShowHS (showHS)
--   import Languages( Lang(English,Dutch))
--   import AGtry (sem_Architecture)
--   import CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars)
--   import Calc (deriveProofs,triggers)
--   import Dataset
--   import Data.Fspec
--   import FspecDEPRECIATED( 
--                   projectClassic
--                  ,generateFspecLaTeX
--                  ,generateArchLaTeX
--                  ,generateGlossaryLaTeX
--                  ,funcSpec
--              --  ,nDesignPr
--             --     ,nServices
--             --     ,nFpoints
--           --     ,makeFspec
--                )
--   import HtmlFilenames(fnContext,fnPattern)
--   import Graphic(processCdDataModelFile,dotGraph,processDotgraphFile)
--   import Atlas (anal)
--   import Fspec2LaTeX
--   import Fspec2Xml (makeXML_depreciated)
--   import ClassDiagram (cdModel,cdDataModel)  
----   import RelBinGen(phpServices)  OBSOLETE as of Jan 1st, 2009 (SJ)
--   import ObjBinGen(phpObjServices)
--   import ADL2Fspec (makeFspec)
--   import Statistics 
import MainOUDEMEUK (mainold)

import System                        (getArgs, getProgName, exitFailure)
import System.Console.GetOpt         (usageInfo)
import List                          (isSuffixOf)
import Options
import Version       (versionbanner)
--import Parser        (parseAG, depsAG)
--import ErrorMessages (Error(ParserError), Errors)
--import CommonTypes
--import ATermWrite


main :: IO ()
main = mainold
--main = mainnew

mainnew :: IO ()
mainnew
 = do args     <- getArgs
      progName <- getProgName
      (flags,inputfile) <- getOptions args progName
   
      if showVersion flags
       then putStrLn versionbanner
       else if showHelp flags 
       then mapM_ putStrLn [(usageInfo' progName)]
       else goforit flags inputfile
       
--       if genFileDeps flags
--            then reportDeps flags files
--            else zipWithM_ (compile flags) files (outputFiles flags++repeat "")

goforit :: Options -> String -> IO()
goforit flags file = sequence_
                        ([putStrLn (show flags)] ++
                         [toonlijst [file]]
                        ) 
                               
    --              >>  putStrLn ("Klaar.")
--toonfiles :: [String -> IO()
--toonfiles s = case s of
--             Nothing -> putStrLn ("Niets te doen!")
--             Just s  -> putStrLn s

toonlijst :: [String] -> IO()
--toonlijst [] = putStrLn ""
--toonlijst [x] = putStrLn x
toonlijst x = mapM_ putStrLn x

inputFile :: String -> String
inputFile name 
 = if ".adl" `isSuffixOf` name
   then name
   else name ++ ".adl"

defaultContextName :: String -> String
defaultContextName name 
 = if ".adl" `isSuffixOf` name
   then take (length name - 4) name
   else name
