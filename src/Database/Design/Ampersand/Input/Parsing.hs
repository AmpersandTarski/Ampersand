{-# OPTIONS_GHC  -XScopedTypeVariables #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Database.Design.Ampersand.Input.Parsing 
  ( parseADL, parseADL1pExpr, parseRule, parseCtx
  )
where

import Database.Design.Ampersand.ADL1
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Input.ADL1.UU_Scanner (scan,initPos, Token)
import UU.Parsing (parse, evalSteps, getMsgs, Pair(..), Message, Parser)
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Input.ADL1.CtxError
import Data.List
import System.Directory
import System.FilePath
import Data.Traversable (sequenceA)

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- Parse an Ampersand file and all transitive includes
parseADL  :: Options -> FilePath -> IO (Guarded P_Context)
parseADL opts filePath =
  whenCheckedIO (parseSingleADL opts filePath) $ \(ctxt, filePaths) ->
    whenCheckedIO (parseADLs opts [filePath] filePaths) $ \ctxts ->
      return $ Checked $ foldl mergeContexts ctxt ctxts

parseADLs :: Options -> [FilePath] -> [FilePath] -> IO (Guarded [P_Context])
parseADLs _    _               []        = return $ Checked []
parseADLs opts parsedFilePaths filePaths =
 do { let filePathsToParse = nub filePaths \\ parsedFilePaths
    ; whenCheckedIO (fmap sequenceA $ mapM (parseSingleADL opts) filePathsToParse) $ \ctxtNewFilePathss ->
       do { let (ctxts, newFilessToParse) = unzip ctxtNewFilePathss
          ; whenCheckedIO (parseADLs opts (parsedFilePaths ++ filePaths) $ concat newFilessToParse) $ \ctxts' ->
              return $ Checked $ ctxts ++ ctxts'
          }
    }

-- Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL :: Options -> FilePath -> IO (Guarded (P_Context, [FilePath]))
parseSingleADL opts filePath =
 do { verboseLn opts $ "Reading file " ++ filePath
    ; mFileContents <- readUTF8File filePath
    ; case mFileContents of
        Left err -> error $ "ERROR reading file " ++ filePath ++ ":\n" ++ err 
                    -- TODO: would like to return an Errors value here, but this datatype currently only accommodates UUParsing Messages 
        Right fileContents ->
         do { whenCheckedIO (return $ runParser pContext filePath fileContents) $ \(ctxts,relativePaths) -> 
               do { filePaths <- mapM normalizePath relativePaths
                  ; return $ Checked (ctxts, filePaths)
                  }
             }
    }
 where normalizePath relativePath = canonicalizePath $ takeDirectory filePath </> relativePath 

runParser :: AmpParser res -> String -> String -> Guarded res
runParser parser filename input =
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps = parse parser (scanner input)
  in  case  getMsgs steps of
         []    -> let Pair res _ = evalSteps steps
                  in  Checked res
         msg:_ -> Errors [PE msg]

runParser' :: forall res . Parser Token res -> String -> String -> Either ParseError res
runParser' parser filename input =
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps = parse parser (scanner input)
  in  case  getMsgs steps of
         []    -> let Pair res _ = evalSteps steps
                  in  Right res
         msg:_ -> Left msg




type ParseError = Message Token (Maybe Token)

-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule :: String -> Term TermPrim
parseRule str
   = case  runParser' pRule "inside Haskell code" str of
       Right result -> result
       Left  msg    -> fatal 274 ("Parse errors in "++str++":\n   "++show msg)
 
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String -> String -> Either String (Term TermPrim)
parseADL1pExpr pexprstr fn = parseExpr pexprstr fn

-- | Parse isolated ADL1 expression strings
parseExpr :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the file (used for error messages)
          -> Either String (Term TermPrim)  -- ^ The result: Either an error message,  or a good result
parseExpr str fn =
  case runParser' pTerm fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors:\n"++show msg
     
parseCtx :: String -> String -> Guarded (P_Context, [String])
parseCtx = runParser pContext