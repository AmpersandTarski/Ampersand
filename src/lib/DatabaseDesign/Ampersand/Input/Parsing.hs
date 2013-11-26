{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Input.Parsing ( parseContext
                                        , parsePopulations
                                        , parseADL1pExpr
                                        , ParseError)
where

import Control.Monad
import Data.List
import Data.Char
import System.Directory
import System.FilePath
import DatabaseDesign.Ampersand.Input.ADL1.Parser (pContext,pPopulations,pTerm,keywordstxt, keywordsops, specialchars, opchars)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing -- (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.ADL1
import Control.Exception
import Paths_ampersand

type ParseError = Message Token

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- | The parser currently needs to be monadic, because there are multiple versions of the Ampersand language supported. Each parser
--   currently throws errors on systemerror level. They can only be 'catch'ed in a monad.
--   This parser is for parsing of a Context
parseContext :: Options                          -- ^ flags to be taken into account
             -> FilePath                         -- ^ the full path to the file to parse 
             -> IO (Either ParseError P_Context) -- ^ The IO monad with the parse tree. 
parseContext flags file 
             = do { verboseLn flags $ "Parsing with "++show (parserVersion flags)++"..."
                  ; rapRes <- if includeRap flags
                              then do dataDir <- getDataDir
                                      let rapFile = dataDir </> "AmpersandData" </> "RepoRap" </> "RAP.adl"
                                      exists <- doesFileExist rapFile
                                      when (not exists) (fatal 39 $ "RAP file isn't installed properly. RAP.adl expected at:"
                                                                  ++"\n  "++show rapFile
                                                                  ++"\n  (You might want to reinstall ampersand...)") 
                                      parseADL flags rapFile
                              else return (Right emptyContext)
                  ; (case rapRes of
                      Left err -> do verboseLn flags "Parsing of RAP failed"
                                     return rapRes
                      Right rapCtx 
                               -> do eRes   <- parseADL flags file
                                     case eRes of  
                                       Right ctx  -> verboseLn flags "Parsing successful"
                                                  >> return (Right (mergeContexts ctx rapCtx))
                                       Left err -> verboseLn flags "Parsing failed"
                                                >> return eRes
                    )
                  }
                  
                         
-- | Same as parseContext , however this one is for a list of populations
parsePopulations :: String            -- ^ The string to be parsed
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
parseADL1pExpr :: String -> String -> IO (Term TermPrim)
parseADL1pExpr pexprstr fn = 
  case parseExpr pexprstr fn Current of
    Right res -> return res
    Left err -> error err


parseADL :: Options
         -> FilePath      -- ^ The name of the .adl file
         -> IO (Either ParseError P_Context) -- ^ The result: Either some errors, or the parsetree.
parseADL flags file =
 do { verboseLn flags $ "Files read:"
    ; (result, parsedFiles) <- readAndParseFile flags 0 [] Nothing "" file
    ; verboseLn flags $ "\n"
    ; return result
    }

-- parse the input file and read and parse the imported files
-- The alreadyParsed parameter keeps track of filenames that have been parsed already, which are ignored when included again.
-- Hence, include cycles do not cause an error.
-- We don't distinguish between "INCLUDE SomeADL" and "INCLUDE SoMeAdL" to prevent errors on case-insensitive file systems.
-- (on a case-sensitive file system you do need to keep your includes with correct capitalization though)

readAndParseFile :: Options -> Int -> [String] -> Maybe String -> String -> String ->
                    IO (Either ParseError P_Context, [String])
readAndParseFile flags depth alreadyParsed mIncluderFilepath fileDir relativeFilepath =
 catch myMonad myHandler
   where 
     myMonad =
       do { canonicFilepath <- fmap (map toUpper) $ canonicalizePath filepath
            -- Legacy parser has no includes, so no need to print here
      
          ; if canonicFilepath `elem` alreadyParsed 
            then do { verboseLn flags $ replicate (3*depth) ' ' ++ "(" ++ filepath ++ ")"
                    ; return (Right emptyContext, alreadyParsed) -- returning an empty context is easier than a maybe (leads to some plumbing in readAndParseIncludeFiles)
                    } 
            else do { fileContents <- DatabaseDesign.Ampersand.Basics.readFile filepath
                    ; verboseLn flags $ replicate (3*depth) ' ' ++ filepath
                    ; parseFileContents flags (depth+1) (canonicFilepath:alreadyParsed)
                                        fileContents newFileDir newFilename     
                    }
          }
     myHandler :: IOException ->
                  IO (Either ParseError P_Context, [String])
     myHandler =   
             (\exc -> do { error $ case mIncluderFilepath of
                                 Nothing -> 
                                   "\n\nError: cannot read ADL file " ++ show filepath
                                 Just includerFilepath ->
                                   "\n\nError: cannot read include file " ++ show filepath ++ 
                                   ", included by " ++ show includerFilepath})
     filepath = combine fileDir relativeFilepath
     newFileDir = let dir = takeDirectory filepath in if dir == "." then "" else dir
     newFilename = takeFileName filepath

parseFileContents :: Options  -- ^ command-line options 
                  -> Int      -- ^ The include depth
                  -> [String] -- ^ Already parsed files (canonicalized) 
                  -> String   -- ^ The string to be parsed
                  -> String   -- ^ The path to the .adl file 
                  -> String   -- ^ The name of the .adl file
                  -> IO (Either ParseError P_Context, [String]) -- ^ The result: The updated already-parsed contexts and Either some errors, or the parsetree.
parseFileContents flags depth alreadyParsed fileContents fileDir filename =
  do { let filepath = combine fileDir filename
     ; case parseSingleADL (parserVersion flags) fileContents filepath of
           Left err -> return (Left err, alreadyParsed)
           Right (parsedContext, includeFilenames) ->
             do { (includeParseResults, alreadyParsed') <-
                     readAndParseIncludeFiles flags alreadyParsed depth (Just $ combine fileDir filename) fileDir includeFilenames
                ; return ( case includeParseResults of
                             Left err              -> Left err
                             Right includeContexts -> Right $ foldl mergeContexts parsedContext includeContexts
                         , alreadyParsed' )             
                }     
    }

readAndParseIncludeFiles :: Options -> [String] -> Int -> Maybe String -> String -> [String] ->
                            IO (Either ParseError [P_Context], [String])
readAndParseIncludeFiles flags alreadyParsed depth mIncluderFilepath fileDir [] = return (Right [], alreadyParsed)
readAndParseIncludeFiles flags alreadyParsed depth mIncluderFilepath fileDir (relativeFilepath:relativeFilepaths) = 
 do { (result, alreadyParsed') <- readAndParseFile flags depth alreadyParsed mIncluderFilepath fileDir relativeFilepath
    ; case result of                               -- Include is only implemented in Current parser
        Left err -> return (Left err, alreadyParsed')
        Right context ->
         do { (results, alreadyParsed'') <- readAndParseIncludeFiles flags alreadyParsed' depth mIncluderFilepath fileDir relativeFilepaths
            ; case results of
                Left err -> return (Left err, alreadyParsed'')
                Right contexts -> return (Right $ context : contexts, alreadyParsed'') 
            }
    }
 
emptyContext :: P_Context
emptyContext = PCtx "" [] Nothing Nothing [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 vs1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1)
              (PCtx nm2 pos2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 vs2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2) =
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
      , ctx_vs = vs1 ++ vs2
      , ctx_gs = gs1 ++ gs2
      , ctx_ifcs = ifcs1 ++ ifcs2
      , ctx_ps = ps1 ++ ps2
      , ctx_pops = pops1 ++ pops2
      , ctx_sql = sql1 ++ sql2
      , ctx_php = php1 ++ php2
      , ctx_metas = metas1 ++ metas2
      }


parseSingleADL :: ParserVersion -- ^ The specific version of the parser to be used
         -> String        -- ^ The string to be parsed
         -> String        -- ^ The name of the .adl file (used for error messages)
         -> Either ParseError (P_Context, [String]) -- ^ The result: Either some errors, or the parsetree. 

parseSingleADL pv str fn =
  case pv of
      Current  -> runParser pv pContext                              fn str
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
          -> Either String (Term TermPrim)  -- ^ The result: Either a list of populations, or some errors. 
parseExpr str fn pv =
    case runParser pv pTerm fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors for "++show pv++":\n"++show msg


      
runParser :: forall res . ParserVersion -> Parser Token res -> String -> String -> Either ParseError res
runParser parserVersion parser filename input = 
  let scanner = case parserVersion of 
                  Current -> scan              keywordstxt              keywordsops              specialchars              opchars filename initPos
      steps :: Steps (Pair res (Pair [Token] a)) Token
      steps = parse parser $ scanner input
  in  case  getMsgs steps of
        []       -> Right $ let Pair result _ = evalSteps steps in result
        msg:msgs -> Left msg

