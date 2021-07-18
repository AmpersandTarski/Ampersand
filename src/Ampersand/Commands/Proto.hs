{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.GenFramework
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath

-- | Builds a prototype of the current project.
proto :: ( Show env
         , HasRunner env
         , HasFSpecGenOpts env
         , HasDirCustomizations env
         , HasZwolleVersion env
         , HasDirPrototype env
         , HasGenerateFrontend env
         , HasGenerateBackend env
         , HasGenerateMetamodel env
         ) 
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
    if generateMetamodel
      then do doGenMetaModel fSpec
      else do logDebug "  Skipping generating metamodel.adl"
    copyCustomizations
    dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
    logInfo $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)

doGenMetaModel :: (HasLogFunc env, HasDirPrototype env) => FSpec -> RIO env()
doGenMetaModel fSpec = do
  env <- ask
  logInfo "Generating metamodel ..."
  let dir = getMetamodelDir env
      filepath = dir </> "metamodel.adl"
  logDebug $ "  Generating "<>display (T.pack filepath) 
  liftIO $ createDirectoryIfMissing True dir
  writeFileUtf8 filepath (showA (originalContext fSpec))

