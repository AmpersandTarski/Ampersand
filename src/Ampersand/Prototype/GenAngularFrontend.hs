{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.GenAngularFrontend (genComponents,genAngularModule) 

where

-- import           Ampersand.ADL1
import           Ampersand.Basics
-- import           Ampersand.Classes.Relational
-- import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
-- import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.ProtoUtil
-- import           Ampersand.Runners (logLevel)
-- import           Ampersand.Types.Config
-- import qualified RIO.Text as T
-- import qualified RIO.List as L
-- import           System.Directory
-- import           System.FilePath
-- import           Text.StringTemplate(Stringable, StringTemplate, setAttribute, newSTMP, checkTemplateDeep, render)
import           Text.StringTemplate.GenericStandard () -- only import instances


genComponents :: (HasLogFunc env) => FSpec -> [FEInterface] -> RIO env ()
genComponents fSpec = mapM_ (genComponent fSpec)

genComponent :: (HasLogFunc env) => FSpec -> FEInterface -> RIO env ()
genComponent _ ifc = do
    logError . display $ "Still TODO: Generate component for "<> ifcName ifc


genAngularModule :: (HasLogFunc env) => FSpec -> [FEInterface] -> RIO env ()
genAngularModule _ ifcs = do
    logError . display $ "Still TODO: Generate module for "<> tshow (map ifcName ifcs)
    return ()