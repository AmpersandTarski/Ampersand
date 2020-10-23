{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoOptsParser 
   (protoOptsParser)
where

import           Options.Applicative.Builder.Extra
import           Ampersand.Commands.Proto (ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Misc.Defaults (defaultDirPrototype)
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)
import           Options.Applicative

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser = 
   ( \  outputLanguage fSpecGenOpts 
        dirPrototype dirCustomizations 
        
        zwolleVersion generateFrontend generateBackend -> ProtoOpts
            { x1OutputLanguage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xdirPrototype = dirPrototype 
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xgenerateFrontend = generateFrontend
            , xgenerateBackend = generateBackend
            }) 
  <$> outputLanguageP <*> fSpecGenOptsParser False
  <*> optional dirPrototypeP <*> optional dirCustomizationsP
  <*> zwolleVersionP 
  <*> generateFrontendP <*> generateBackendP

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
generateFrontendP :: Parser Bool
generateFrontendP = boolFlags True "frontend"
        "Generate prototype frontend files (Angular application)"
        mempty

generateBackendP :: Parser Bool
generateBackendP = boolFlags True "backend"
        "Generate backend files (PHP application)"
        mempty

