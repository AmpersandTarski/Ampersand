{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
  ( proto,
    ProtoOpts (..),
    HasProtoOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.GenBackend (doGenBackend)
import Ampersand.Prototype.GenFrontend
import Ampersand.Types.Config
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T

-- | Builds a prototype of the current project.
proto ::
  (Show env, HasRunner env, HasFSpecGenOpts env, HasDirPrototype env, HasGenerateFrontend env, HasGenerateBackend env, HasGenerateMetamodel env) =>
  FSpec ->
  RIO env ()
proto fSpec = do
  env <- ask
  let dirPrototype = getDirPrototype env
  logDebug "Generating prototype..."
  liftIO $ createDirectoryIfMissing True dirPrototype
  generateFrontend <- view generateFrontendL
  if generateFrontend
    then do doGenFrontend fSpec
    else do logDebug "  Skipping generating frontend files"
  generateBackend <- view generateBackendL
  if generateBackend
    then do doGenBackend fSpec
    else do logDebug "  Skipping generating backend files"
  generateMetamodel <- view generateMetamodelL
  if generateMetamodel -- TODO @stefjoosten, Why should this be generated at the proto command??
    then do doGenMetaModel fSpec
    else do logDebug "  Skipping generating metamodel.adl"

-- TODO @StefJoosten, please replace this functionality with exportAsAdl
doGenMetaModel :: (HasLogFunc env, HasDirPrototype env) => FSpec -> RIO env ()
doGenMetaModel fSpec = case originalContext fSpec of
  Nothing -> logInfo "To generate a metamodel, your model should contain a context, but it contains a module."
  Just ctx -> do
    env <- ask
    logInfo "Generating metamodel ..."
    let dir = getMetamodelDir env
        filepath = dir </> "metamodel.adl"
    logDebug $ "  Generating " <> display (T.pack filepath)
    liftIO $ createDirectoryIfMissing True dir
    writeFileUtf8 filepath (showA ctx)
