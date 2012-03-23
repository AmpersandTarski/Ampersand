{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Parsing ( parseContext
                                        , parsePopulations
                                        , parseADL1pExpr
                                        , ParseError)
where

import Prelude hiding (putStr,readFile,writeFile)
import Control.Monad
import Data.List
import Data.Char
import System.Directory
import System.FilePath
import DatabaseDesign.Ampersand.Input.ADL1.Parser (pContext,pPopulations,pExpr,keywordstxt, keywordsops, specialchars, opchars)
import qualified DatabaseDesign.Ampersand.Input.ADL1.LegacyParser as LegacyParser
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.ADL1

type ParseError = Message Token

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- | The parser currently needs to be monadic, because there are multiple versions of the Ampersand language supported. Each parser
--   currently throws errors on systemerror level. They can only be 'catch'ed in a monad.
--   This parser is for parsing of a Context
parseContext  :: Options       -- ^ flags to be taken into account
            -> FilePath      -- ^ the full path to the file to parse 
            -> IO (Either ParseError P_Context) -- ^ The IO monad with the parse tree. 
parseContext opts file = tryAll versions2try
    where 
      versions2try :: [ParserVersion]
      versions2try = case forcedParserVersion opts of
         Just pv  -> [pv]
         Nothing  -> [Current,Legacy]
      
      try :: ParserVersion -> IO (Either ParseError P_Context)
      try pv = do { verboseLn opts $ "Parsing with "++show pv++"..."
                  ; eRes <- parseADL opts pv file
                  ; case eRes of 
                      Right ctx  -> verboseLn opts "Parsing successful"
                                >> return (Right $ ctx{ctx_experimental = experimental opts}) -- set the experimental flag
                      Left err -> verboseLn opts "Parsing failed"
                                 >> return (Left err)
                  }
                  
      tryAll :: [ParserVersion] -> IO (Either ParseError P_Context)
      tryAll [] = fatal 76 "tryAll must not be called with an empty list. Consult your dealer."
      tryAll [pv] = try pv 
      tryAll (pv:pvs) = do mCtx <- try pv 
                           case mCtx of
                            Right ctx  -> return (Right ctx)
                            Left _     -> tryAll pvs

                         
-- | Same as parseContext , however this one is for a list of populations
parsePopulations   :: String            -- ^ The string to be parsed
                   -> Options           -- ^ flags to be taken into account
                   -> String            -- ^ The name of the .pop file (used for error messages)
                   -> IO [P_Population] -- ^ The IO monad with the populations. 
parsePopulations popsstring flags fn =
 do { verboseLn flags "Parsing populations."
    ; case parsePops popsstring fn Current of
        Right res -> return res
        Left err -> error err
    }
                    
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String -> String -> IO P_Expression
parseADL1pExpr pexprstr fn = 
  case parseExpr pexprstr fn Current of
    Right res -> return res
    Left err -> error err


parseADL :: Options
         -> ParserVersion -- ^ The specific version of the parser to be used
         -> FilePath      -- ^ The name of the .adl file
         -> IO (Either ParseError P_Context) -- ^ The result: Either some errors, or the parsetree.
parseADL opts parserVersion file =
 do { when (parserVersion==Current) $ verboseLn opts $ "Files read:"
    ; (result, parsedFiles) <- readAndParseFile opts parserVersion 0 [] Nothing "" file
    ; when (parserVersion==Current) $ verboseLn opts $ "\n"
    ; return result
    }

-- parse the input file and read and parse the imported files
-- The alreadyParsed parameter keeps track of filenames that have been parsed already, which are ignored when included again.
-- Hence, include cycles do not cause an error.
-- We don't distinguish between "INCLUDE SomeADL" and "INCLUDE SoMeAdL" to prevent errors on case-insensitive file systems.
-- (on a case-sensitive file system you do need to keep your includes with correct capitalization though)

readAndParseFile :: Options -> ParserVersion -> Int -> [String] -> Maybe String -> String -> String ->
                    IO (Either ParseError P_Context, [String])
readAndParseFile opts parserVersion depth alreadyParsed mIncluderFilepath fileDir relativeFilepath =
 do { canonicFilepath <- fmap (map toUpper) $ canonicalizePath filepath
      -- Legacy parser has no includes, so no need to print here

    ; if canonicFilepath `elem` alreadyParsed 
      then do { when (parserVersion==Current) $ verboseLn opts $ replicate (3*depth) ' ' ++ "(" ++ filepath ++ ")"
              ; return (Right emptyContext, alreadyParsed) -- returning an empty context is easier than a maybe (leads to some plumbing in readAndParseIncludeFiles)
              } 
      else do { fileContents <- readFile filepath
              ; when (parserVersion==Current) $ verboseLn opts $ replicate (3*depth) ' ' ++ filepath
              ; parseFileContents opts parserVersion  (depth+1) (canonicFilepath:alreadyParsed)
                                  fileContents newFileDir newFilename     
              }
    }
 `catch` (\exc -> do { error $ case mIncluderFilepath of
                                 Nothing -> 
                                   "\n\nError: cannot read ADL file " ++ show filepath
                                 Just includerFilepath ->
                                   "\n\nError: cannot read include file " ++ show filepath ++ 
                                   ", included by " ++ show includerFilepath})
 where filepath = combine fileDir relativeFilepath
       newFileDir = let dir = takeDirectory filepath in if dir == "." then "" else dir
       newFilename = takeFileName filepath

parseFileContents :: Options -- ^ command-line options 
         -> ParserVersion -- ^ The specific version of the parser to be used
         -> Int           -- ^ The include depth
         -> [String]      -- ^ Already parsed files (canonicalized) 
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The path to the .adl file 
         -> String        -- ^ The name of the .adl file
         -> IO (Either ParseError P_Context, [String]) -- ^ The result: The updated already-parsed contexts and Either some errors, or the parsetree.
parseFileContents opts parserVersion depth alreadyParsed fileContents fileDir filename =
  do { let filepath = combine fileDir filename
     ; case parseSingleADL parserVersion fileContents filepath of
           Left err -> return (Left err, alreadyParsed)
           Right (parsedContext, includeFilenames) ->
             do { (includeParseResults, alreadyParsed') <-
                     readAndParseIncludeFiles opts alreadyParsed depth (Just $ combine fileDir filename) fileDir includeFilenames
                ; return ( case includeParseResults of
                             Left err              -> Left err
                             Right includeContexts -> Right $ foldl mergeContexts parsedContext includeContexts
                         , alreadyParsed' )             
                }     
    }

readAndParseIncludeFiles :: Options -> [String] -> Int -> Maybe String -> String -> [String] ->
                            IO (Either ParseError [P_Context], [String])
readAndParseIncludeFiles opts alreadyParsed depth mIncluderFilepath fileDir [] = return (Right [], alreadyParsed)
readAndParseIncludeFiles opts alreadyParsed depth mIncluderFilepath fileDir (relativeFilepath:relativeFilepaths) = 
 do { (result, alreadyParsed') <- readAndParseFile opts Current depth alreadyParsed mIncluderFilepath fileDir relativeFilepath
    ; case result of                               -- Include is only implemented in Current parser
        Left err -> return (Left err, alreadyParsed')
        Right context ->
         do { (results, alreadyParsed'') <- readAndParseIncludeFiles opts alreadyParsed' depth mIncluderFilepath fileDir relativeFilepaths
            ; case results of
                Left err -> return (Left err, alreadyParsed'')
                Right contexts -> return (Right $ context : contexts, alreadyParsed'') 
            }
    }
 
emptyContext :: P_Context
emptyContext = PCtx "" [] Nothing Nothing [] [] [] [] [] [] [] [] [] [] [] [] [] [] False

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1 _)
              (PCtx nm2 pos2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2 _) =
  PCtx{ ctx_nm = nm1
      , ctx_pos = pos1 ++ pos2
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
      , ctx_metas = metas1 ++ metas2
      , ctx_experimental = False -- is set in Components.hs
      }


parseSingleADL :: ParserVersion -- ^ The specific version of the parser to be used
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> Either ParseError (P_Context, [String]) -- ^ The result: Either some errors, or the parsetree. 

parseSingleADL pv str fn =
  case pv of
      Current  -> runParser pv pContext                              fn str
      Legacy  -> runParser pv (addEmptyIncludes <$> LegacyParser.pContext) fn str
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


      
runParser :: forall res . ParserVersion -> Parser Token res -> String -> String -> Either ParseError res
runParser parserVersion parser filename input = 
  let scanner = case parserVersion of 
                  Legacy  -> scan LegacyParser.keywordstxt LegacyParser.keywordsops LegacyParser.specialchars LegacyParser.opchars filename initPos
                  Current  -> scan       keywordstxt       keywordsops       specialchars       opchars filename initPos
      steps :: Steps (Pair res (Pair [Token] a)) Token
      steps = parse parser $ scanner input
  in  case  getMsgs steps of
        []       -> Right $ let Pair result _ = evalSteps steps in result
        msg:msgs -> Left msg

