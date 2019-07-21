{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Clean a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,ProtoCommand(..)
    ,AmpersandProtoExceptionException(..)
    ) where

import           Ampersand.Basics
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
--import           Path.IO (ignoringAbsence, removeDirRecur)
--import           Stack.Constants.Config (rootDistDirFromDir, workDirFromDir)
--import           Stack.Types.Config
--import           Stack.Types.SourceMap
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Misc
import           Ampersand.FSpec
import           System.Directory
import           Ampersand.Output.FSpec2SQL

-- | Builds a prototype of the current project.
--
proto :: (HasGenTime env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env,HasOutputFile env, HasOptions env, HasRootFile env, HasVerbosity env, HasHandle env) 
       => ProtoOpts -> FSpec -> RIO env ()
proto opts fSpec = do
    dirPrototype <- view dirPrototypeL
    allowInvariantViolations <- view allowInvariantViolationsL
    if null (violationsOfInvariants fSpec) || allowInvariantViolations
    then do
       sayLn "Generating prototype..."
       liftIO $ createDirectoryIfMissing True dirPrototype
       doGenFrontend fSpec
       generateDatabaseFile fSpec
       generateJSONfiles fSpec
       sayWhenLoudLn $ "Prototype files have been written to " ++ dirPrototype
    else exitWith NoPrototypeBecauseOfRuleViolations


-- | Options for @ampersand proto@.
data ProtoOpts = ProtoOpts
   { protOdbName :: String
   -- ^ Name of the database that is generated as part of the prototype
   , protOsqlHost ::  String  
   -- ^ do database queries to the specified host
   , protOsqlLogin :: String
   -- ^ pass login name to the database server
   , protOsqlPwd :: String
   -- ^ pass password on to the database server
   , protOforceReinstallFramework :: Bool
   -- ^ when true, an existing prototype directory will be destroyed and re-installed
   }
instance HasProtoOpts ProtoOpts where
   dbNameL   = lens protOsqlHost  (\x y -> x { protOdbName   = y })
   sqlHostL  = lens protOsqlHost  (\x y -> x { protOsqlHost  = y })
   sqlLoginL = lens protOsqlLogin (\x y -> x { protOsqlLogin = y })
   sqlPwdL   = lens protOsqlPwd   (\x y -> x { protOsqlPwd   = y })
   forceReinstallFrameworkL
             = lens protOforceReinstallFramework (\x y -> x { protOforceReinstallFramework   = y })

defProtoOpts :: Maybe FilePath -> ProtoOpts 
defProtoOpts fName = ProtoOpts
  { protOdbName = fmap toLower . fromMaybe ("ampersand_" ++ takeBaseName (fromMaybe "prototype" fName)) $ envDbName envOpts
  , protOsqlHost = "localhost"
  , protOsqlLogin = "ampersand"
  , protOsqlPwd = "ampersand"
  , protOforceReinstallFramework = False
  }


-- | Proto commands
data ProtoCommand
    = Proto
    | Purge




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
