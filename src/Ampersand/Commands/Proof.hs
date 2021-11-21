{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate a proofs output document from a project.
module Ampersand.Commands.Proof
  ( proof,
    ProtoOpts (..),
    HasProtoOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import qualified RIO.Text as T
import System.Directory
import System.FilePath
import Text.Pandoc (def, handleError, runIO, writeHtml5String)
import Text.Pandoc.Builder

-- | Generate a proofs output document from a project.
proof ::
  (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
proof fSpec = do
  env <- ask
  logInfo $ "Generating Proof for " <> display (name fSpec) <> " into " <> display (T.pack $ outputFile env) <> "..."
  content <- liftIO $ runIO (writeHtml5String def thePandoc) >>= handleError
  liftIO $ createDirectoryIfMissing True (takeDirectory (outputFile env))
  writeFileUtf8 (outputFile env) content
  logDebug "Proof written."
  where
    outputFile env = view dirOutputL env </> "proofs_of_" <> baseName env -<.> ".html"
    thePandoc = setTitle title (doc theDoc)
    title = text $ "Proofs for " <> name fSpec
    theDoc = fDeriveProofs fSpec

--theDoc = plain (text "Aap")  -- use for testing...
