-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Database.Design.Ampersand.Components
  ( -- * Type checking and calculus
     makeFSpec
    -- * Generators of output
   , generateAmpersandOutput
--   , doGenADL
--   , doGenProofs
--   , doGenHaskell
--   , doGenXML
--   , doGenUML
--   , doGenDocument
--   , doGenFPAExcel
    -- * etc...
  )
where
import Prelude hiding (putStr,readFile,writeFile)
import Database.Design.Ampersand.Misc
import Text.Pandoc
import Text.Pandoc.Builder
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.FSpec.GenerateUML
import Database.Design.Ampersand.Output
import Control.Monad
import System.FilePath
import System.Time
import qualified Data.ByteString.Lazy as L

fatal :: Int -> String -> a
fatal = fatalMsg "Components"

--  | The FSpec is the datastructure that contains everything to generate the output. This monadic function
--    takes the FSpec as its input, and spits out everything the user requested.
generateAmpersandOutput :: FSpec -> IO ()
generateAmpersandOutput fSpec =
 do { when (genUML (getOpts fSpec))      $ doGenUML      fSpec
    ; when (haskell (getOpts fSpec))     $ doGenHaskell  fSpec
    ; when (export2adl (getOpts fSpec))  $ doGenADL      fSpec
    ; when (genFSpec (getOpts fSpec))    $ doGenDocument fSpec
    ; when (genFPAExcel (getOpts fSpec)) $ doGenFPAExcel fSpec
    ; when (genPOPExcel (getOpts fSpec)) $ doGenPopsXLSX fSpec
    ; when (proofs (getOpts fSpec))      $ doGenProofs   fSpec
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verboseLn (getOpts fSpec) "Done."
    }

-- An expression e is type ambiguous means that   (showADL e) cannot be parsed (in the context of fSpec) without a type ambiguity error.
-- Q: Should we disambiguate the exprs in the fSpec i.e. mapexprs disambiguate fSpec fSpec?
--    Or do we assume a correct implementation with unambiguous expressions only?
-- A: The fSpec may contain disambiguated expressions only. If one expression somewhere in fSpec is type-ambiguous, fSpec is wrong.
--    So the answer is: we assume a correct implementation with unambiguous expressions only.
doGenADL :: FSpec -> IO()
doGenADL fSpec =
 do { writeFile outputFile . showADL . originalContext $ fSpec
    ; verboseLn (getOpts fSpec) $ ".adl-file written to " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) (outputfile (getOpts fSpec))

doGenProofs :: FSpec -> IO()
doGenProofs fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
--  ; verboseLn (getOpts fSpec) $ writeTextile def thePandoc
    ; writeFile outputFile $ writeHtmlString def thePandoc
    ; verboseLn (getOpts fSpec) "Proof written."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension ("proofs_of_"++baseName (getOpts fSpec)) ".html"
       thePandoc = setTitle title (doc theDoc)
       title  = text $ "Proofs for "++name fSpec
       theDoc = fDeriveProofs fSpec
       --theDoc = plain (text "Aap")  -- use for testing...

doGenHaskell :: FSpec -> IO()
doGenHaskell fSpec =
 do { verboseLn (getOpts fSpec) $ "Generating Haskell source code for "++name fSpec
--  ; verboseLn (getOpts fSpec) $ fSpec2Haskell fSpec -- switch this on to display the contents of Installer.php on the command line. May be useful for debugging.
    ; writeFile outputFile (fSpec2Haskell fSpec)
    ; verboseLn (getOpts fSpec) $ "Haskell written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)) ".hs"

doGenUML :: FSpec -> IO()
doGenUML fSpec =
 do { verboseLn (getOpts fSpec) "Generating UML..."
    ; writeFile outputFile $ generateUML fSpec
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)) ".xmi"

-- This function will generate all Pictures for a given FSpec.
-- the returned FSpec contains the details about the Pictures, so they
-- can be referenced while rendering the FSpec.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: FSpec -> IO()
doGenDocument fSpec =
 do { verboseLn (getOpts fSpec) ("Processing "++name fSpec)
    ; makeOutput
    ; verboseLn (getOpts fSpec) $ "Document has been written to " ++ outputFile ++ "."
    ; when (not(null thePictures) && fspecFormat (getOpts fSpec)/=FPandoc) $
        mapM_ (writePicture (getOpts fSpec)) thePictures
     -- postProcessing of the generated output file depends on the format:
    ; postProcessor
    }
  where (thePandoc,thePictures) =
          case (theme (getOpts fSpec), fspecFormat (getOpts fSpec)) of
 -- TODO Ticket #104: Could not find texOnly_proofdoc in any module? Where has in gone?
 --                (ProofTheme, FLatex ) -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                 (ProofTheme, _      ) -> fatal 116 "Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 (_         , _      ) -> fSpec2Pandoc fSpec
        (outputFile,makeOutput,postProcessor) = writepandoc fSpec thePandoc

-- | This function will generate an Excel workbook file, containing an extract from the FSpec
doGenFPAExcel :: FSpec -> IO()
doGenFPAExcel fSpec =
 do { verboseLn (getOpts fSpec) "Generating Excel containing FPA..."
    ; writeFile outputFile $ fspec2FPA_Excel fSpec
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension ("FPA_"++baseName (getOpts fSpec)) ".xml"  -- Do not use .xls here, because that generated document contains xml.

doGenPopsXLSX :: FSpec -> IO()
doGenPopsXLSX fSpec =
 do { verboseLn (getOpts fSpec) "Generating .xlsx file containing the population "
    ; ct <- getClockTime
    ; L.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile
    }
   where outputFile = combine (dirOutput (getOpts fSpec)) $ replaceExtension (baseName (getOpts fSpec)++ "_generated_pop") ".xlsx"

    
   
   