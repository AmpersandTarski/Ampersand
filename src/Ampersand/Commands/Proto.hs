{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Clean a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
--    ,AmpersandProtoExceptionException(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Misc
import           Ampersand.Types.Config
import           Ampersand.FSpec
import           System.Directory
import           Ampersand.Output.FSpec2SQL
import           Ampersand.Output.ToJSON.ToJson
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env, HasRootFile env) 
       => FSpec -> RIO env ()
proto fSpec = do
    dirPrototype <- view dirPrototypeL
    allowInvariantViolations <- view allowInvariantViolationsL
    if null (violationsOfInvariants fSpec) || allowInvariantViolations
    then do
       sayLn "Generating prototype..."
       liftIO $ createDirectoryIfMissing True dirPrototype
       doGenFrontend fSpec
       generateDatabaseFile fSpec
       generateJSONfiles False fSpec
       sayWhenLoudLn $ "Prototype files have been written to " ++ dirPrototype
    else exitWith NoPrototypeBecauseOfRuleViolations


