{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Reads a project and parses it
module Ampersand.Daemon.Parser (
    parseProject
) where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Daemon.Types
import           Ampersand.Input.Parsing
import           Ampersand.Input.ADL1.CtxError
import qualified RIO.NonEmpty as NE
import           Ampersand.FSpec.MetaModels
import           Ampersand.Types.Config
import           Ampersand.Misc.HasClasses (HasRootFile(..),rootFileL, HasDaemonOpts(..), showWarningsL)

-- | parseProject will try to parse a file. If it succeeds, it will 
--   also parse all INCLUDED files transitive. Any of these parses could
--   fail. It will return a tuple containing the Loads and a list of 
--   the filepaths that are read. 
parseProject :: (HasDaemonOpts env, HasRunner env) => 
                FilePath ->  RIO env ([Load],[FilePath])
parseProject rootAdl =  local (set rootFileL (Just rootAdl)) $ do
    showWarnings <- view showWarningsL
    (pc,gPctx) <- parseFileTransitive rootAdl 
    env <- ask
    let loadedFiles = map pcCanonical pc
        gActx = pCtx2Fspec env =<< gPctx
    return ( case gActx of
                Checked _ ws 
                   | showWarnings -> map warning2Load ws
                   | otherwise -> [] 
                Errors  es   -> NE.toList . fmap error2Load $ es
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
