{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Parsing ( parseADLAndIncludes
                                        , parsePops
                                        , parseExpr
                                        , ParserError)
where

import DatabaseDesign.Ampersand.Input.ADL1.CCv221 (pContext,pIncludeFile,pPopulations,pExpr,keywordstxt, keywordsops, specialchars, opchars)
import qualified DatabaseDesign.Ampersand.Input.ADL1.CC664 as CC664
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.ADL1
 
type ParserError = Message Token

-- parse the input file and read and parse the imported files
parseADLAndIncludes :: String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> ParserVersion -- ^ The specific version of the parser to be used
         -> Options       -- ^ Options to use
         -> IO (Either [ParserError] P_Context) -- ^ The result: Either some errors, or the parsetree.
     
parseADLAndIncludes str fn pv opts =
  do { case parseADL str fn pv of
           Left err -> return $ Left err
           Right (parsedContext, includeFilenames) -> 
             do { includeRes <- readAndParseIncludeFiles includeFilenames
                ; case includeRes of
                    Left err -> return $ Left err
                    Right  addIncluded -> return $ Right $ addIncluded parsedContext                 
                }
     }

parseADL :: String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> ParserVersion -- ^ The specific version of the parser to be used
         -> Either [ParserError] (P_Context, [String]) -- ^ The result: Either some errors, or the parsetree. 

parseADL str fn pv =
  case pv of
      PV664  -> runParser (addEmptyIncludes <$> CC664.pContext) fn (scan CC664.keywordstxt CC664.keywordsops CC664.specialchars CC664.opchars fn initPos str)
      PV211  -> runParser pContext                              fn (scan       keywordstxt       keywordsops       specialchars       opchars fn initPos str)
 where addEmptyIncludes parsedContext = (parsedContext, []) -- the old parsed does not return include filenames, so we add an empty list
    
runParser :: forall res . Parser Token res -> String -> [Token] -> Either [ParserError] res
runParser parser filename input = 
  let steps :: Steps (Pair res (Pair [Token] a)) Token 
      steps = parse parser input
  in  case  getMsgs steps of
        []  -> Right $ let Pair result _ = evalSteps steps in result
        msgs -> Left msgs

-- to keep things simple, we always parse all included files, even if one of them has a parse error
readAndParseIncludeFiles :: [String] -> IO (Either [ParserError] (P_Context -> P_Context))
readAndParseIncludeFiles filenames = 
 do { parsedIncludeFiles <- mapM readAndParseIncludeFile filenames
    ; let result = sequence parsedIncludeFiles -- we use the Either monad: result will be the first Left, or a list of all Rights
    ; return $ case result of 
                 Left errs -> Left errs
                 Right addIncludeds  -> Right $ applyAll addIncludeds
    }
 where applyAll :: [P_Context -> P_Context] -> P_Context -> P_Context
       applyAll fs ctxt = foldr ($) ctxt fs
 
readAndParseIncludeFile :: String -> IO (Either [ParserError] (P_Context -> P_Context))
readAndParseIncludeFile fn = 
 do { fileContents <- readFile fn `catch` (\exc -> do { error $ "\n\nError: cannot read include file "++show fn})
    ; return $ runParser pIncludeFile fn (scan keywordstxt keywordsops specialchars opchars fn initPos fileContents)
    }



-- | Same as passeCtx_ , however this one si for a list of populations
parsePops :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either [P_Population] String -- ^ The result: Either a list of populations, or some errors. 
parsePops str fn pv =
    case  getMsgs steps of
      []  -> Left presult
      msg -> Right $ "Parse errors for "++show pv++":\n"++show (head msg)
  where
    Pair presult _ = evalSteps steps 
    steps :: Steps (Pair [P_Population] (Pair [Token] a)) Token  
    steps = parse pPopulations (scan keywordstxt keywordsops specialchars opchars fn initPos str)

-- | Parse isolated ADL1 expression strings
parseExpr :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either P_Expression String -- ^ The result: Either a list of populations, or some errors. 
parseExpr str fn pv =
    case  getMsgs steps of
      []  -> Left presult
      msg -> Right $ "Parse errors for "++show pv++":\n"++show (head msg)
  where
    Pair presult _ = evalSteps steps 
    steps :: Steps (Pair P_Expression (Pair [Token] a)) Token  
    steps = parse pExpr (scan keywordstxt keywordsops specialchars opchars fn initPos str)



