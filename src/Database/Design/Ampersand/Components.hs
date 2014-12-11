-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
--
module Database.Design.Ampersand.Components
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
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1.P2A_Converters
import Text.Pandoc
import Text.Pandoc.Builder
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Fspec
import Database.Design.Ampersand.Fspec.GenerateUML
import Database.Design.Ampersand.Fspec.ShowXMLtiny (showXML)
import Database.Design.Ampersand.Output
import Control.Monad
import System.FilePath

fatal :: Int -> String -> a
fatal = fatalMsg "Components"

--  | The Fspc is the datastructure that contains everything to generate the output. This monadic function
--    takes the Fspc as its input, and spits out everything the user requested.
generateAmpersandOutput :: Fspc -> IO ()
generateAmpersandOutput fSpec =
 do { verboseLn (flags fSpec) "Generating common Ampersand artifacts..."
    ; when (genXML (flags fSpec))      $ doGenXML      fSpec
    ; when (genUML (flags fSpec))      $ doGenUML      fSpec
    ; when (haskell (flags fSpec))     $ doGenHaskell  fSpec
    ; when (export2adl (flags fSpec))  $ doGenADL      fSpec
    ; when (genFspec (flags fSpec))    $ doGenDocument fSpec
    ; when (genFPAExcel (flags fSpec)) $ doGenFPAExcel fSpec
    ; when (proofs (flags fSpec))      $ doGenProofs   fSpec
    ; when (genMeat (flags fSpec) && (not . includeRap) (flags fSpec))  -- When rap is included, the file is created there.
        $ doGenMeatGrinder fSpec
    --; Prelude.putStrLn $ "Declared rules:\n" ++ show (map showADL $ vrules fSpec)
    --; Prelude.putStrLn $ "Generated rules:\n" ++ show (map showADL $ grules fSpec)
    --; Prelude.putStrLn $ "Violations:\n" ++ show (violations fSpec)
    ; verboseLn (flags fSpec) "Done."
    }

-- An expression e is type ambiguous means that   (showADL e) cannot be parsed (in the context of fSpec) without a type ambiguity error.
-- Q: Should we disambiguate the exprs in the fSpec i.e. mapexprs disambiguate fSpec fSpec?
--    Or do we assume a correct implementation with unambiguous expressions only?
-- A: The fSpec may contain disambiguated expressions only. If one expression somewhere in fSpec is type-ambiguous, fSpec is wrong.
--    So the answer is: we assume a correct implementation with unambiguous expressions only.
doGenADL :: Fspc -> IO()
doGenADL fSpec =
 do { writeFile outputFile (showADL fSpec)
    ; verboseLn (flags fSpec) $ ".adl-file written to " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (flags fSpec)) (outputfile (flags fSpec))

doGenProofs :: Fspc -> IO()
doGenProofs fSpec =
 do { verboseLn (flags fSpec) $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
--  ; verboseLn (flags fSpec) $ writeTextile def thePandoc
    ; writeFile outputFile $ writeHtmlString def thePandoc
    ; verboseLn (flags fSpec) "Proof written."
    }
 where outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension ("proofs_of_"++baseName (flags fSpec)) ".html"
       thePandoc = setTitle title (doc theDoc)
       title  = text $ "Proofs for "++name fSpec
       theDoc = deriveProofs fSpec
       --theDoc = plain (text "Aap")  -- use for testing...

doGenHaskell :: Fspc -> IO()
doGenHaskell fSpec =
 do { verboseLn (flags fSpec) $ "Generating Haskell source code for "++name fSpec
--  ; verboseLn (flags fSpec) $ fSpec2Haskell fSpec -- switch this on to display the contents of Installer.php on the command line. May be useful for debugging.
    ; writeFile outputFile (fSpec2Haskell fSpec)
    ; verboseLn (flags fSpec) $ "Haskell written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension (baseName (flags fSpec)) ".hs"

doGenMeatGrinder :: Fspc -> IO()
doGenMeatGrinder fSpec =
 do verboseLn (flags fSpec) $ "Generating meta-population for "++name fSpec
    let (nm,content) = meatGrinder fSpec
        outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension nm ".adl"
    writeFile outputFile content
    verboseLn (flags fSpec) $ "Meta population written into " ++ outputFile ++ "."

doGenXML :: Fspc -> IO()
doGenXML fSpec =
 do { verboseLn (flags fSpec) "Generating XML..."
    ; writeFile outputFile $ showXML fSpec (genTime (flags fSpec))
    ; verboseLn (flags fSpec) $ "XML written into " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension (baseName (flags fSpec)) ".xml"

doGenUML :: Fspc -> IO()
doGenUML fSpec =
 do { verboseLn (flags fSpec) "Generating UML..."
    ; writeFile outputFile $ generateUML fSpec
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension (baseName (flags fSpec)) ".xmi"

-- This function will generate all Pictures for a given Fspc.
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: Fspc -> IO()
doGenDocument fSpec =
 do { verboseLn (flags fSpec) ("Processing "++name fSpec)
    ; makeOutput
    ; verboseLn (flags fSpec) $ "Document has been written to " ++ outputFile ++ "."
    ; when (genGraphics (flags fSpec) && not(null thePictures) && fspecFormat (flags fSpec)/=FPandoc) $
        mapM_ (writePicture (flags fSpec)) thePictures
     -- postProcessing of the generated output file depends on the format:
    ; postProcessor
    }
  where (thePandoc,thePictures) =
          case (theme (flags fSpec), fspecFormat (flags fSpec)) of
 -- TODO Ticket #104: Could not find texOnly_proofdoc in any module? Where has in gone?
 --                (ProofTheme, FLatex ) -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                 (ProofTheme, _      ) -> fatal 116 "Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 (_         , _      ) -> fSpec2Pandoc fSpec
        (outputFile,makeOutput,postProcessor) = writepandoc fSpec thePandoc

-- | This function will generate an Excel workbook file, containing an extract from the Fspc
doGenFPAExcel :: Fspc -> IO()
doGenFPAExcel fSpec =
 do { verboseLn (flags fSpec) "Generating Excel..."
    ; writeFile outputFile (showSpreadsheet (fspec2Workbook fSpec))
    }
   where outputFile = combine (dirOutput (flags fSpec)) $ replaceExtension ("FPA_"++baseName (flags fSpec)) ".xml"  -- Do not use .xls here, because that generated document contains xml.
