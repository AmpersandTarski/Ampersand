{-# LANGUAGE TupleSections #-}
-- | Reads a project and parses it
module Ampersand.Daemon.Daemon.Parser (
    parseProject
) where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Daemon.Daemon.Types
import           Ampersand.FSpec.ToFSpec.CreateFspec
import           Ampersand.Input.Parsing
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import qualified Data.List.NonEmpty as NEL (toList)

parseProject :: Options -> FilePath -> IO ([Load],[FilePath])
parseProject opts rootAdl = do
    (pc,gPctx) <- parseADL opts rootAdl 
    let loadedFiles = map pcCanonical pc
    let gActx = pCtx2Fspec opts gPctx
    return ( case gActx of
              Checked _ ws -> map warning2Load $ ws
              Errors  es   -> NEL.toList . fmap error2Load $ es
           , loadedFiles
           )

warning2Load :: Warning -> Load
warning2Load warn = Message
    {loadSeverity = Warning
    ,loadFile = file 
    ,loadFilePos = (line,col) 
    ,loadFilePosEnd = (line,col)
    ,loadMessage = lines $ show warn
    }
  where (file, line, col) = (filenm warn, linenr warn, colnr warn)
        
error2Load :: CtxError -> Load
error2Load err = Message
    {loadSeverity = Error
    ,loadFile = file 
    ,loadFilePos = (line,col) 
    ,loadFilePosEnd = (line,col)
    ,loadMessage = lines $ show err
    }
  where (file, line, col) = (filenm err, linenr err, colnr err)
