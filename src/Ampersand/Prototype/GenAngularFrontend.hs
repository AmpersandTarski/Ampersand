{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.GenAngularFrontend (genComponents,genAngularModule) 

where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes.Relational
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.NormalForms
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2SQL (databaseStructureSql)
import           Ampersand.Output.ToJSON.ToJson
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Runners (logLevel)
import           Ampersand.Types.Config
import           Codec.Archive.Zip
import           Data.Hashable (hash)
import           Network.HTTP.Simple
import qualified RIO.ByteString.Lazy  as BL
import qualified RIO.Text as T
import qualified RIO.List as L
import           RIO.Time
import           System.Directory
import           System.FilePath
import           Text.StringTemplate(Stringable, StringTemplate, setAttribute, newSTMP, checkTemplateDeep, render)
import           Text.StringTemplate.GenericStandard () -- only import instances


genComponents :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genComponents = undefined

genAngularModule :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genAngularModule = undefined