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
genComponent fspec ifc = do
  getComponentTs fspec ifc
  logError . display $ "Still TODO: Generate html file for " <> ifcNamePascalComponent ifc

genComponentTs :: (HasLogFunc env) => FSpec -> FEInterface -> RIO env ()
genComponentTs fspec interf = do
  let controlerTemplateName = "interface.component.ts.txt"
  template <- readTemplate controlerTemplateName
  mapM_ (logDebug . display) (showTemplate template)
  runner <- view runnerL
  let loglevel' = logLevel runner
  let contents =
        renderTemplate Nothing template $
          setAttribute "contextName" (fsName fSpec)
            . setAttribute "isRoot" (isTopLevel . source . ifcExp $ interf)
            . setAttribute "roles" (map show . feiRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
            . setAttribute "ampersandVersionStr" (longVersion appVersion)
            . setAttribute "ifcName" (ifcName interf)
            . setAttribute "ifcNamePascal" (ifcNamePascal interf)
            . setAttribute "ifcNameKebab" (ifcNameKebab interf)
            . setAttribute "ifcLabel" (ifcLabel interf) -- no escaping for labels in templates needed
            . setAttribute "expAdl" (showA . toExpr . ifcExp $ interf)
            . setAttribute "exprIsUni" (exprIsUni (feiObj interf))
            . setAttribute "source" (idWithoutType . source . ifcExp $ interf)
            . setAttribute "target" (idWithoutType . target . ifcExp $ interf)
            . setAttribute "crudC" (objCrudC (feiObj interf))
            . setAttribute "crudR" (objCrudR (feiObj interf))
            . setAttribute "crudU" (objCrudU (feiObj interf))
            . setAttribute "crudD" (objCrudD (feiObj interf))
            . setAttribute "verbose" (loglevel' == LevelDebug)
            . setAttribute "loglevel" (show loglevel')
            . setAttribute "usedTemplate" controlerTemplateName
  let filename = T.unpack (ifcNameKebab interf) <> ".component.ts"
  writePrototypeAppFile filename contents
  logDebug . display $ "Generated file " <> filename

genAngularModule :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genAngularModule fSpec ifcs = do
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
  writePrototypeAppFile "project.module.ts" contents
  logDebug . display $ "Generated file project.module.ts"
