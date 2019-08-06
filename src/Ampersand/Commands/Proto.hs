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






{- Stuff from Options, TODO:
          , (Option []      ["force-reinstall-framework"]
               (NoArg (\opts -> opts{protoOpts = set forceReinstallFrameworkL True (protoOpts opts)}))
               "re-install the prototype framework. This discards any previously installed version."
            , Hidden)
          , (Option ['d']  ["dbName"]
               (ReqArg (\nm opts -> opts{protoOpts = (if nm == ""
                                                         then id
                                                         else set dbNameL (map toLower nm)) (protoOpts opts)}
                       ) "NAME")
               ("database name (This overrules environment variable "++ dbNameVarName ++ ", defaults to filename) to which the prototype will connect for persistent storage.")
            , Hidden)
          , (Option []  ["sqlHost"]
               (ReqArg (\nm opts -> opts{protoOpts = (if nm == ""
                                                          then id 
                                                          else set sqlHostL nm ) (protoOpts opts)}
                       ) "HOSTNAME")
               "set SQL host name (Defaults to `localhost`), to identify the host on which the persistent store resides"
            , Hidden)
          , (Option []  ["sqlLogin"]
               (ReqArg (\nm opts -> opts{protoOpts = (if nm == ""
                                                          then id 
                                                          else set sqlLoginL nm ) (protoOpts opts)}
                       ) "USER")
               "set SQL user name (Defaults to `ampersand`), to let your application login to the database."
            , Hidden)
          , (Option []  ["sqlPwd"]
               (ReqArg (\nm opts -> opts{protoOpts = (if nm == ""
                                                          then id 
                                                          else set sqlPwdL nm ) (protoOpts opts)}
                       ) "PASSWORD")
               "set SQL password (Defaults to `ampersand`), to let your application login to the database."
            , Hidden)
-}
