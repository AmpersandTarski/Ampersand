{-# OPTIONS_GHC -Wall #-}
-- | This module contains the building blocks that are available in the Ampersand Library. These building blocks will be described further at [ampersand.sourceforge.net |the wiki pages of our project].
-- 
module DatabaseDesign.Ampersand.Components 
  ( -- * Parsersing
    -- ** Pure parsers
    -- ** Monadic parsers
     parseCtxM_
   , parsePopsM_
   , parseADL1pExpr
    -- * Type checking and calculus
   , typeCheck
   , makeFspec
    -- * Generators of output
   , interfaceGen
   , prove
   , doGenHaskell
   , doGenXML
   , doGenUML
   , doGenDocument
    -- * etc...
  )
where
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Input
import Text.Pandoc 
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Basics 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.GenerateUML
import DatabaseDesign.Ampersand.Output
import Control.Monad
import System.FilePath
import DatabaseDesign.Ampersand.Parsing
fatal :: Int -> String -> a
fatal = fatalMsg "Components"


-- | The parser currently needs to be monadic, because there are multiple versions of the Ampersand language supported. Each parser
--   currently throws errors on systemerror level. They can only be 'catch'ed in a monad.
--   This parser is for parsing of a Context
parseCtxM_  :: String        -- ^ The string to be parsed
            -> Options       -- ^ flags to be taken into account
            -> String        -- ^ The name of the .adl file (used for error messages)
            -> IO (Either ParserError P_Context) -- ^ The IO monad with the parse tree. 
parseCtxM_ adlstring flags fn = tryAll versions2try
    where 
      versions2try :: [ParserVersion]
      versions2try = case forcedParserVersion flags of
         Just pv  -> [pv]
         Nothing  -> [PV664,PV211]
      
      try :: ParserVersion -> IO (Either ParserError P_Context)
      try pv = do { verbose flags $ "Parsing with "++show pv++"..."
                  ; eRes <- parseADLAndIncludes adlstring fn pv flags
                  ; case eRes of 
                      Right ctx  -> verboseLn flags " successful"
                                >> return (Right ctx)
                      Left err -> verboseLn flags "...failed"
                                 >> return (Left err)
                  }
                  
      tryAll :: [ParserVersion] -> IO (Either ParserError P_Context)
      tryAll [] = fatal 76 "tryAll must not be called with an empty list. Consult your dealer."
      tryAll [pv] = try pv 
      tryAll (pv:pvs) = do mCtx <- try pv 
                           case mCtx of
                            Right ctx  -> return (Right ctx)
                            Left _     -> tryAll pvs

                         
-- | Same as parseCtxM_ , however this one is for a list of populations
parsePopsM_   :: String            -- ^ The string to be parsed
              -> Options           -- ^ flags to be taken into account
              -> String            -- ^ The name of the .pop file (used for error messages)
              -> IO [P_Population] -- ^ The IO monad with the populations. 
parsePopsM_ popsstring flags fn 
               = verboseLn flags "Parsing populations."
              >> case parsePops popsstring fn PV211 of
                    Right res -> return res
                    Left err -> error err
                    
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String -> String -> IO P_Expression
parseADL1pExpr pexprstr fn 
               = case parseExpr pexprstr fn PV211 of
                    Right res -> return res
                    Left err -> error err

-- | Typechecking takes a P_Context, and a list of P_Population. The result is either a typed context, or an error object.
--   Apply nocxe on the error object to determine whether there are errors.
--   If the list of populations is not empty, then it overwrites the one included in the parsed context
typeCheck :: P_Context -> [P_Population] -> (A_Context, CtxError)
typeCheck pCtx []   = let (aCtx,ctxcheck)=pCtx2aCtx pCtx                  in (aCtx,cxes ctxcheck)
typeCheck pCtx pops = let (aCtx,ctxcheck)=pCtx2aCtx (pCtx{ctx_pops=pops}) in (aCtx,cxes ctxcheck)
  

-- An expression e is type ambiguous means that   (showADL e) cannot be parsed (in the context of fSpec) without a type ambiguity error.
-- Q: Should we disambiguate the exprs in the fspec i.e. mapexprs disambiguate fSpec fSpec?
--    Or do we assume a correct implementation with unambiguous expressions only?
-- A: The fSpec may contain disambiguated expressions only. If one expression somewhere in fSpec is type-ambiguous, fSpec is wrong.
--    So the answer is: we assume a correct implementation with unambiguous expressions only.
interfaceGen :: Fspc -> Options -> IO()
interfaceGen    fSpec flags
  =    writeFile outputFile (showADL fSpec) 
    >> verboseLn flags ("Ampersand-script written to " ++ outputFile ++ ".")
    where  outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec flags
    =  verboseLn flags ("Generating Proof for " ++ name fSpec ++ " into " ++ outputFile ++ ".")
    >> writeFile outputFile (writeHtmlString defaultWriterOptions thePandoc)
    >> verboseLn flags "Proof written."
    where outputFile
            = combine (dirOutput flags) (replaceExtension ("proofs_of_"++baseName flags) ".html") 
          thePandoc = setTitle title (doc theDoc)
          title  = text ("Proofs for "++name fSpec)
          theDoc = para (fromList(deriveProofs flags fSpec))
      --    theDoc = plain (text "Aap")  -- use for testing...

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
               
doGenUML :: Fspc -> Options -> IO()
doGenUML fSpec flags =
 do { verboseLn flags "Generating UML..."
    ; writeFile outputFile $ generateUML fSpec flags
    ; Prelude.putStrLn $ "Generated file: " ++ outputFile ++ "."
    }
   where outputFile = combine (dirOutput flags) (replaceExtension (baseName flags) ".xmi")

-- This function will generate all Pictures for a given Fspc. 
-- the returned Fspc contains the details about the Pictures, so they
-- can be referenced while rendering the Fspc.
-- This function generates a pandoc document, possibly with pictures from an fSpec.
doGenDocument :: Fspc -> Options -> IO()
doGenDocument fSpec flags
   = verboseLn flags ("Processing "++name fSpec)                              >>
     makeOutput                                                               >>
     verboseLn flags ("Document has been written into " ++ outputFile ++ ".") >>
     when (genGraphics flags && not(null thePictures) && fspecFormat flags/=FPandoc) 
          (foldr1 (>>) [ writePicture flags p | p<-thePictures] )              >>
     -- postProcessing of the generated output file depends on the format:
     postProcessor
     where
       (thePandoc,thePictures) 
            = case (theme flags, fspecFormat flags) of
 -- TODO Ticket #104: Could not find texOnly_proofdoc in any module? Where has in gone?
 --                (ProofTheme, FLatex ) -> (texOnly_proofdoc fSpec,[])     --generate a proof document
                 (ProofTheme, _      ) -> fatal 116 "Ampersand only supports proof documents output in LaTeX format. try `-fLatex` "
                 (_         , _      ) -> fSpec2Pandoc fSpec flags
       (outputFile,makeOutput,postProcessor) = writepandoc flags gis thePandoc
       gis = concs fSpec -- the glossary items

