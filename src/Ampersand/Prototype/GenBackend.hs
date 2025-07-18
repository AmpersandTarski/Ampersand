{-# LANGUAGE OverloadedStrings #-}

module Ampersand.Prototype.GenBackend (doGenBackend) where

import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Output.FSpec2SQL (databaseStructureSql)
import Ampersand.Output.ToJSON.ToJson
import Ampersand.Prototype.ProtoUtil
import Ampersand.Types.Config
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath
import Text.StringTemplate.GenericStandard ()

-- only import instances

doGenBackend ::
  (Show env, HasRunner env, HasDirPrototype env) =>
  FSpec ->
  RIO env ()
doGenBackend fSpec = do
  env <- ask
  logInfo "Generating backend..."
  let dir = getGenericsDir env
  logDebug $ "  generating backend in: " <> displayShow dir
  liftIO $ createDirectoryIfMissing True dir
  writeFileUtf8 (dir </> "database" <.> "sql") $ databaseStructureSql fSpec
  writeFile (dir </> "settings" <.> "json") $ settingsToJSON env fSpec
  writeFile (dir </> "relations" <.> "json") $ relationsToJSON env fSpec
  writeFile (dir </> "rules" <.> "json") $ rulesToJSON env fSpec
  writeFile (dir </> "concepts" <.> "json") $ conceptsToJSON env fSpec
  writeFile (dir </> "conjuncts" <.> "json") $ conjunctsToJSON env fSpec
  writeFile (dir </> "interfaces" <.> "json") $ interfacesToJSON env fSpec
  writeFile (dir </> "views" <.> "json") $ viewsToJSON env fSpec
  writeFile (dir </> "roles" <.> "json") $ rolesToJSON env fSpec
  writeFile (dir </> "populations" <.> "json") $ populationToJSON env fSpec
  logInfo "Backend generated"
