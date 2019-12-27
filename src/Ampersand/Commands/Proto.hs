{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2SQL
import           Ampersand.Output.ToJSON.ToJson
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env) 
       => FSpec -> RIO env ()
proto fSpec = do
    env <- ask
    let dirPrototype = getDirPrototype env
    allowInvariantViolations <- view allowInvariantViolationsL
    if null (violationsOfInvariants fSpec) || allowInvariantViolations
    then do
       sayLn "Generating prototype..."
       liftIO $ createDirectoryIfMissing True dirPrototype
       doGenFrontend fSpec
       generateDatabaseFile fSpec
       generateJSONfiles False fSpec
       dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
       logDebug $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)
    else exitWith NoPrototypeBecauseOfRuleViolations


