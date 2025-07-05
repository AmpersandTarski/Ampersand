{-# LANGUAGE FlexibleInstances #-}

-- | Reads a project and parses it
module Ampersand.Daemon.Parser
  ( parseProject,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Daemon.Types
import Ampersand.FSpec.ToFSpec.CreateFspec (pCtx2Fspec)
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.Parsing
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import qualified RIO.NonEmpty as NE

-- | parseProject will try to parse a file. If it succeeds, it will
--   also parse all INCLUDED files transitively. Any of these parses could
--   fail. It will return a tuple containing the Loads and a list of
--   the filepaths that are read.
parseProject ::
  (HasDirOutput env, HasTrimXLSXOpts env, HasDaemonOpts env, HasRunner env) =>
  FilePath ->
  RIO env ([Load], [FilePath])
parseProject rootAdl = local (set rootFileL (Roots (rootAdl NE.:| []))) $ do
  showWarnings <- view showWarningsL
  (pc, gPctx) <- parseFilesTransitive (Roots (rootAdl NE.:| []))
  env <- ask
  let loadedFiles = fmap pcCanonical pc
      gActx = pCtx2Fspec env =<< gPctx
  return
    ( case gActx of
        Checked _ ws
          | showWarnings -> map warning2Load ws
          | otherwise -> []
        Errors es -> NE.toList . fmap error2Load $ es,
      NE.toList loadedFiles
    )

warning2Load :: Warning -> Load
warning2Load warn =
  Message
    { loadSeverity = Warning,
      loadFile = file,
      loadFilePos = (line, col),
      loadFilePosEnd = (line, col),
      loadMessage = lines $ show warn
    }
  where
    (file, line, col) = (filenm warn, linenr warn, colnr warn)

error2Load :: CtxError -> Load
error2Load err =
  Message
    { loadSeverity = Error,
      loadFile = file,
      loadFilePos = (line, col),
      loadFilePosEnd = (line, col),
      loadMessage = lines $ show err
    }
  where
    (file, line, col) = (filenm err, linenr err, colnr err)
