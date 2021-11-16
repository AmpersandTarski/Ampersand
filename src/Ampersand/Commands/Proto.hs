{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.GenFrontend
import           Ampersand.Types.Config
import           System.Directory
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasFSpecGenOpts env, HasProtoOpts env, HasDirPrototype env, HasGenerateFrontend env, HasGenerateBackend env, HasCheckCompilerVersion env, HasGenerateMetamodel env)
       => FSpec -> RIO env ()
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
    if generateMetamodel --TODO @stefjoosten, Why should this be generated at the proto command??
      then do doGenMetaModel fSpec
      else do logDebug "  Skipping generating metamodel.adl"
