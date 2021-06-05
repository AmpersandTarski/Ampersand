{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoOptsParser 
   (protoOptsParser)
where

import           Options.Applicative.Builder.Extra
import           Ampersand.Basics
import           Ampersand.Misc.Defaults (defaultDirPrototype)
import           Ampersand.Misc.HasClasses ( ProtoOpts(..), FrontendVersion (..))
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)
import           Options.Applicative
import           RIO.Char

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser = 
   ( \forceReinstall 
        outputLanguage fSpecGenOpts 
        dirPrototype dirCustomizations 
        zwolleVersion generateFrontend generateBackend 
        frontendVersion -> ProtoOpts
            { xforceReinstallFramework = forceReinstall
            , x1OutputLanguage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xdirPrototype = dirPrototype 
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xgenerateFrontend = generateFrontend
            , xgenerateBackend = generateBackend
            , xfrontendVersion = frontendVersion
            }) 
  <$> forceReinstallP
  <*> outputLanguageP <*> fSpecGenOptsParser False
  <*> optional dirPrototypeP <*> optional dirCustomizationsP
  <*> zwolleVersionP 
  <*> generateFrontendP <*> generateBackendP
  <*> frontendVersionP

forceReinstallP :: Parser Bool
forceReinstallP = switch
        ( long "force-reinstall-framework"
        <> help ("Re-install the prototype framework. This discards any previously "<>
                "installed version.")
        )
dirPrototypeP :: Parser String
dirPrototypeP = strOption
        ( long "proto-dir"
        <> metavar "DIRECTORY"
        <> value defaultDirPrototype
        <> showDefault
        <> help "Specify the directory where the prototype will be generated"
        )
dirCustomizationsP :: Parser [String]
dirCustomizationsP = splitWhen (== ';') <$> strOption
        ( long "customizations"
        <> metavar "DIR;DIR;.."
        <> help "Copy one or more directories into the generated prototype. "
        )
zwolleVersionP :: Parser String
zwolleVersionP = strOption
        ( long "prototype-framework-version"
        <> metavar "VERSION"
        <> value "v1.6.0"
        <> showDefault
        <> help ( "Tag, branch or SHA of the prototype framework on Github. "
                <>"Normally you shouldn't need to use anohter version "
                <>"than the default. Only a developer of the framework "
                <>"can make good use of it. ")
        )
frontendVersionP :: Parser FrontendVersion
frontendVersionP = toFrontendVersion <$> strOption
         ( long "frontend-version"
        <> metavar "VERSION"
        <> value "angularjs"
        <> showDefault
        <> help  "Temporary switch to enable the development of the new angular frontend."
        )
    where
      toFrontendVersion :: String -> FrontendVersion
      toFrontendVersion x = case map toLower x of
                                "angular"   -> Angular 
                                "angularjs" -> AngularJS
                                _           -> AngularJS    

generateFrontendP :: Parser Bool
generateFrontendP = boolFlags True "frontend"
        "Generate prototype frontend files (Angular application)"
        mempty

generateBackendP :: Parser Bool
generateBackendP = boolFlags True "backend"
        "Generate backend files (PHP application)"
        mempty

