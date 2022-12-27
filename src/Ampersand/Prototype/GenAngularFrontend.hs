{-# LANGUAGE OverloadedStrings #-}

module Ampersand.Prototype.GenAngularFrontend (genComponents, genAngularModule) where

import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.ProtoUtil
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config
import qualified RIO.Text as T
import Text.StringTemplate (setAttribute)
import Text.StringTemplate.GenericStandard ()

-- only import instances

genComponents :: (HasLogFunc env) => FSpec -> [FEInterface] -> RIO env ()
genComponents fSpec = mapM_ (genComponent fSpec)

genComponent :: (HasLogFunc env) => FSpec -> FEInterface -> RIO env ()
genComponent _ ifc = do
  logError . display $ "Still TODO: Generate component for " <> ifcName ifc

genAngularModule :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genAngularModule fSpec ifcs = do
  logDebug $ "Generate module for " <> displayShow (map ifcName ifcs)
  runner <- view runnerL
  let loglevel' = logLevel runner
  template <- readTemplate "project.module.ts.txt"
  mapM_ (logDebug . display) (showTemplate template)
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (fsName fSpec)
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcs" ifcs
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
  mapM_ (logDebug . display) $ "Generated template: " : (map ("   " <>) . T.lines $ contents)
  writePrototypeAppFile "project.module.ts" contents
  logDebug "Finish genAngularModule."
