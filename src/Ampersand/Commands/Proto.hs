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
--    ,AmpersandProtoExceptionException(..)
    ) where

import           Ampersand.Basics
--import qualified RIO.List as L ((\\),intercalate)
--import qualified Data.Map.Strict as Map
--import qualified RIO.Char as C
--import           RIO.FilePath
--import           Path.IO (ignoringAbsence, removeDirRecur)
--import           Stack.Constants.Config (rootDistDirFromDir, workDirFromDir)
--import           Stack.Types.Config
--import           Stack.Types.SourceMap
import           Ampersand.Prototype.GenFrontend (doGenFrontend)
import           Ampersand.Misc
import           Ampersand.Misc.Config
import           Ampersand.FSpec
import           System.Directory
import           Ampersand.Output.FSpec2SQL
import           Ampersand.Output.ToJSON.ToJson 
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasGenTime env, HasRunComposer env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env, HasRootFile env) 
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


-- | Options for @ampersand proto@.
data ProtoOpts = ProtoOpts
   { protOdbName :: !String
   -- ^ Name of the database that is generated as part of the prototype
   , protOsqlHost ::  !String
   -- ^ do database queries to the specified host
   , protOsqlLogin :: !String
   -- ^ pass login name to the database server
   , protOsqlPwd :: !String
   -- ^ pass password on to the database server
   , protOforceReinstallFramework :: !Bool
   -- ^ when true, an existing prototype directory will be destroyed and re-installed
   , protOOutputLangugage :: !(Maybe Lang)
   , protORootFile :: !FilePath
   , protOsqlBinTables :: !Bool
   } deriving Show
instance HasProtoOpts ProtoOpts where
   dbNameL   = lens protOsqlHost  (\x y -> x { protOdbName   = y })
   sqlHostL  = lens protOsqlHost  (\x y -> x { protOsqlHost  = y })
   sqlLoginL = lens protOsqlLogin (\x y -> x { protOsqlLogin = y })
   sqlPwdL   = lens protOsqlPwd   (\x y -> x { protOsqlPwd   = y })
   forceReinstallFrameworkL
             = lens protOforceReinstallFramework (\x y -> x { protOforceReinstallFramework   = y })
instance HasOutputLanguage ProtoOpts where
  languageL = lens protOOutputLangugage (\x y -> x { protOOutputLangugage = y })
instance HasSqlBinTables ProtoOpts where
instance HasGenInterfaces ProtoOpts where
instance HasDefaultCrud ProtoOpts where
instance HasExcellOutputOptions ProtoOpts where
instance HasRootFile ProtoOpts where
instance HasNamespace ProtoOpts where
instance HasRunComposer ProtoOpts where
instance HasDirCustomizations ProtoOpts where
instance HasZwolleVersion ProtoOpts where
instance HasAllowInvariantViolations ProtoOpts where
instance HasDirPrototype ProtoOpts where


-- | Proto commands
data ProtoCommand
    = Proto




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
