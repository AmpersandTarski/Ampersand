{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Parsing ( parseADL
                                        , parsePops
                                        , parseExpr
                                        , ParserError)
where

import Prelude hiding (putStr,readFile,writeFile)
import Data.List
import DatabaseDesign.Ampersand.Input.ADL1.CCv221 (pContext,pPopulations,pExpr,keywordstxt, keywordsops, specialchars, opchars)
import qualified DatabaseDesign.Ampersand.Input.ADL1.CC664 as CC664
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.ADL1
 
type ParserError = Message Token

parseADL :: ParserVersion -- ^ The specific version of the parser to be used
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> IO (Either ParserError P_Context) -- ^ The result: Either some errors, or the parsetree.
parseADL parserVersion fileContents filename =
  parseADL' parserVersion [] fileContents filename

-- parse the input file and read and parse the imported files
-- The alreadyParsed parameter keeps track of filenames that have been parsed already, which are ignored when included again.
-- Hence, include cycles do not cause an error.
parseADL' :: ParserVersion -- ^ The specific version of the parser to be used
         -> [String]      -- ^ Already parsed contexts 
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> IO (Either ParserError P_Context) -- ^ The result: Either some errors, or the parsetree.
parseADL' parserVersion alreadyParsed fileContents filename =
  do { case parseSingleADL parserVersion fileContents filename of
           Left err -> return $ Left err
           Right (parsedContext, includeFilenames) -> 
             do { includeParseResults <- readAndParseIncludeFiles (filename:alreadyParsed) filename includeFilenames
                ; case includeParseResults of
                    Left err              -> return $ Left err
                    Right includeContexts -> return $ Right $ foldl mergeContexts parsedContext includeContexts                 
                }     
    }

readAndParseIncludeFiles :: [String] -> String -> [String] -> IO (Either ParserError [P_Context])
readAndParseIncludeFiles alreadyParsed includerFilename [] = return $ Right []
readAndParseIncludeFiles alreadyParsed includerFilename (filename:filenames) | filename `elem` alreadyParsed = return $ Right []
readAndParseIncludeFiles alreadyParsed includerFilename (filename:filenames) | otherwise = 
 do { result <- readAndParseIncludeFile alreadyParsed includerFilename filename
    ; case result of
        Left err -> return $ Left err
        Right context ->
         do { results <- readAndParseIncludeFiles (filename : alreadyParsed) includerFilename filenames
            ; case results of
                Left err -> return $ Left err
                Right contexts -> return $ Right $ context : contexts 
            }
    }

readAndParseIncludeFile :: [String] -> String -> String -> IO (Either ParserError P_Context)
readAndParseIncludeFile alreadyParsed includerFilename includedFilename = 
 do { fileContents <- readFile includedFilename `catch` (\exc -> do { error $ "\n\nError: cannot read include file " ++ show includedFilename ++ 
                                                                              ", included in " ++ show includerFilename})
    ; parseADL' PV211 alreadyParsed fileContents includedFilename     
    }
   
mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 gs1 ifcs1 ps1 pops1 sql1 php1)
              (PCtx nm2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 gs2 ifcs2 ps2 pops2 sql2 php2) =
  PCtx{ ctx_nm = nm1
      , ctx_lang = lang1
      , ctx_markup = markup1
      , ctx_thms = thms1 ++ thms2
      , ctx_pats = pats1 ++ pats2
      , ctx_PPrcs = pprcs1 ++ pprcs2
      , ctx_rs = rs1 ++ rs2
      , ctx_ds = ds1 ++ ds2
      , ctx_cs = cs1 ++ cs2
      , ctx_ks = ks1 ++ ks2
      , ctx_gs = gs1 ++ gs2
      , ctx_ifcs = ifcs1 ++ ifcs2
      , ctx_ps = ps1 ++ ps2
      , ctx_pops = pops1 ++ pops2
      , ctx_sql = sql1 ++ sql2
      , ctx_php = php1 ++ php2
      }


parseSingleADL :: ParserVersion -- ^ The specific version of the parser to be used
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> Either ParserError (P_Context, [String]) -- ^ The result: Either some errors, or the parsetree. 

parseSingleADL pv str fn =
  case pv of
      PV211  -> runParser pv pContext                              fn str
      PV664  -> runParser pv (addEmptyIncludes <$> CC664.pContext) fn str
 where addEmptyIncludes parsedContext = (parsedContext, []) -- the old parsed does not support include filenames, so we add an empty list


-- | Same as passeCtx_ , however this one is for a list of populations
parsePops :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either String [P_Population] -- ^ The result: Either a list of populations, or some errors. 
parsePops str fn pv = 
    case  runParser pv pPopulations fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors for "++show pv++":\n"++show msg

-- | Parse isolated ADL1 expression strings
parseExpr :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> ParserVersion     -- ^ The specific version of the parser to be used
          -> Either String P_Expression -- ^ The result: Either a list of populations, or some errors. 
parseExpr str fn pv =
    case runParser pv pExpr fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors for "++show pv++":\n"++show msg


      
runParser :: forall res . ParserVersion -> Parser Token res -> String -> String -> Either ParserError res
runParser parserVersion parser filename input = 
  let scanner = case parserVersion of 
                  PV664  -> scan CC664.keywordstxt CC664.keywordsops CC664.specialchars CC664.opchars filename initPos
                  PV211  -> scan       keywordstxt       keywordsops       specialchars       opchars filename initPos
      steps :: Steps (Pair res (Pair [Token] a)) Token
      steps = parse parser $ scanner input
  in  case  getMsgs steps of
        []       -> Right $ let Pair result _ = evalSteps steps in result
        msg:msgs -> Left msg

