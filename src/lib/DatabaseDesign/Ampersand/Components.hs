{-# OPTIONS_GHC -Wall #-}
-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
-- 
module DatabaseDesign.Ampersand.Components 
  ( -- * Type checking and calculus
     makeFspec
    -- * Generators of output
   , generateAmpersandOutput
--   , doGenADL
--   , doGenProofs
--   , doGenHaskell
--   , doGenXML
--   , doGenUML
--   , doGenDocument
--   , doGenFPAExcel
   , Guarded(..)
    -- * etc...
  )
where
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.ADL1.P2A_Converters
import Text.Pandoc 
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Basics 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.GenerateUML
import DatabaseDesign.Ampersand.Fspec.ShowXMLtiny (showXML)
import DatabaseDesign.Ampersand.Output
import Control.Monad
import System.FilePath

fatal :: Int -> String -> a
fatal = fatalMsg "Components"

--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spits out everything the user requested.
generateAmpersandOutput :: Options -> Fspc -> IO ()
generateAmpersandOutput flags fSpec = 
 do { verboseLn flags "Generating..."
    ; when (genXML flags)      $ doGenXML      fSpec flags
    ; when (genUML flags)      $ doGenUML      fSpec flags 
    ; when (haskell flags)     $ doGenHaskell  fSpec flags 
    ; when (export2adl flags)  $ doGenADL      fSpec flags
    ; when (genFspec flags)    $ doGenDocument fSpec flags 
    ; when (genFPAExcel flags) $ doGenFPAExcel fSpec flags
    ; when (proofs flags)      $ doGenProofs   fSpec flags
    ; when (genMeat flags && not (includeRap flags))  -- When rap is included, the file is created there.
        $ doGenMeatGrinder fSpec flags
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verboseLn flags "Done."
    }

-- An expression e is type ambiguous means that   (showADL e) cannot be parsed (in the context of fSpec) without a type ambiguity error.
-- Q: Should we disambiguate the exprs in the fspec i.e. mapexprs disambiguate fSpec fSpec?
--    Or do we assume a correct implementation with unambiguous expressions only?
-- A: The fSpec may contain disambiguated expressions only. If one expression somewhere in fSpec is type-ambiguous, fSpec is wrong.
--    So the answer is: we assume a correct implementation with unambiguous expressions only.
doGenADL :: Fspc -> Options -> IO()
doGenADL    fSpec flags =
 do { writeFile outputFile (showADL fSpec) 
    ; verboseLn flags $ ".adl-file written to " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput flags) (outputfile flags)

doGenProofs :: Fspc -> Options -> IO()
doGenProofs fSpec flags =
 do { verboseLn flags $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
    ; writeFile outputFile $ writeHtmlString def thePandoc
    ; verboseLn flags "Proof written."
    }
 where outputFile = combine (dirOutput flags) $ replaceExtension ("proofs_of_"++baseName flags) ".html" 
       thePandoc = setTitle title (doc theDoc)
       title  = text $ "Proofs for "++name fSpec
       theDoc = para $ fromList (deriveProofs flags fSpec)
       --theDoc = plain (text "Aap")  -- use for testing...

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags =
 do { verboseLn flags $ "Generating Haskell source code for "++name fSpec
--    ; verboseLn flags $ fSpec2Haskell fSpec flags
    ; writeFile outputFile (fSpec2Haskell fSpec flags) 
    ; verboseLn flags $ "Haskell written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput flags) $ replaceExtension (baseName flags) ".hs"
   
doGenMeatGrinder :: Fspc -> Options -> IO()
doGenMeatGrinder fSpec flags =
 do verboseLn flags $ "Generating meta-population for "++name fSpec
    let (nm,content) = meatGrinder flags fSpec
        outputFile = combine (dirOutput flags) $ replaceExtension nm ".adl"
    writeFile outputFile content
    verboseLn flags $ "Meta population written into " ++ outputFile ++ "."
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags =
 do { verboseLn flags "Generating XML..."
    ; writeFile outputFile $ showXML fSpec (genTime flags)   
    ; verboseLn flags $ "XML written into " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput flags) $ replaceExtension (baseName flags) ".xml"
               
doGenUML :: Fspc -> Options -> IO()
doGenUML fSpec flags =
 do { verboseLn flags "Generating UML..."
    ; writeFile outputFile $ generateUML fSpec flags
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput flags) $ replaceExtension (baseName flags) ".xmi"

-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: Fspc -> Options -> IO()
doGenDocument fSpec flags =
 do { verboseLn flags ("Processing "++name fSpec)
    ; makeOutput
    ; verboseLn flags $ "Document has been written to " ++ outputFile ++ "."
    ; when (genGraphics flags && not(null thePictures) && fspecFormat flags/=FPandoc) $ 
        mapM_ (writePicture flags) thePictures
     -- postProcessing of the generated output file depends on the format:
    ; postProcessor
    }
  where (thePandoc,thePictures) = 
          case (theme flags, fspecFormat flags) of
 -- TODO Ticket #104: Could not find texOnly_proofdoc in any module? Where has in gone?
 --                (ProofTheme, FLatex ) -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                 (ProofTheme, _      ) -> fatal 116 "Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 (_         , _      ) -> fSpec2Pandoc fSpec flags
        (outputFile,makeOutput,postProcessor) = writepandoc flags fSpec thePandoc

-- | This function will generate an Excel workbook file, containing an extract from the Fspc
doGenFPAExcel :: Fspc -> Options -> IO()
doGenFPAExcel fSpec flags =
 do { verboseLn flags "Generating Excel..."
    ; writeFile outputFile (showSpreadsheet (fspec2Workbook fSpec flags))
    }
   where outputFile = combine (dirOutput flags) $ replaceExtension ("FPA_"++baseName flags) ".xml"  -- Do not use .xls here, because that generated document contains xml. 
