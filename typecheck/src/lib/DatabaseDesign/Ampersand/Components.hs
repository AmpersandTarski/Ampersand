{-# OPTIONS_GHC -Wall #-}
-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
-- 
module DatabaseDesign.Ampersand.Components 
  ( -- * Type checking and calculus
     typeCheck
   , makeFspec
    -- * Generators of output
   , doGenADL
   , prove
   , doGenHaskell
   , doGenXML
   , doGenUML
   , doGenDocument
   , doGenExcel
    -- * etc...
  )
where
import Prelude hiding (putStr,readFile,writeFile)
import Data.GraphViz (DotGraph(..))
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Input
import Text.Pandoc 
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Basics 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.GenerateUML
import DatabaseDesign.Ampersand.Output
import Control.Monad
import System.FilePath

fatal :: Int -> String -> a
fatal = fatalMsg "Components"

-- | Typechecking takes a P_Context, and a list of P_Population. The result is either a typed context, or an error object.
--   If the list of populations is not empty, then it overwrites the one included in the parsed context
typeCheck :: P_Context -> [P_Population] -> (A_Context, [CtxError],DotGraph String,DotGraph String,DotGraph String)
typeCheck p_context []   = pCtx2aCtx p_context                 
typeCheck p_context pops = pCtx2aCtx (p_context{ctx_pops=pops})
                           -- consisting of:  (aCtx,ctxcheck,stTypeGraph,condensedGraph,ambiguityGraph)
  

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

prove :: Fspc -> Options -> IO()
prove fSpec flags =
 do { verboseLn flags $ "Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ "."
    ; writeFile outputFile $ writeHtmlString defaultWriterOptions thePandoc
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
    ; writeFile outputFile (fSpec2Haskell fSpec flags) 
    ; verboseLn flags $ "Haskell written into " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput flags) $ replaceExtension (baseName flags) ".hs"
   
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
doGenExcel :: Fspc -> Options -> IO()
doGenExcel fSpec flags =
 do { verboseLn flags "Generating Excel..."
    ; writeFile outputFile (showSpreadsheet (fspec2Workbook fSpec flags))
    }
   where outputFile = combine (dirOutput flags) $ replaceExtension (baseName flags) ".xls"
