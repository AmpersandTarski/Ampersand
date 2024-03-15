{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate a configuration file for a new project.
module Ampersand.Commands.AtlasImport
  ( atlasImport,
    InitOpts (..),
    HasInitOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
import Ampersand.FSpec.FSpec
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import RIO.Text as T

-- | Read a file containing the population of an Atlas.
atlasImport ::
  (HasOutputFile env, HasImportFile env, HasRunner env) => --  , Show env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env, HasRootFile env)
  FSpec -> -- This will be the FSpec of Formal Ampersand, if we need it at all.
  RIO env ()
atlasImport _fSpec = do
  env <- ask
  json <- readFileUtf8 (view importFileL env)
  let outputFn = view outputfileL env
  case parseJson json of
    Checked ctx _ -> do
      writeFileUtf8 outputFn (showP ctx)
      logInfo . display . T.pack $ outputFn <> " written"
    Errors _ -> pure ()

parseJson :: Text -> Guarded P_Context
parseJson contents = fatal "Still has to be implemented..."
