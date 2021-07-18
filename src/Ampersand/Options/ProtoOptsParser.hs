module Ampersand.Options.ProtoOptsParser (protoOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.Defaults (defaultDirPrototype)
import Ampersand.Misc.HasClasses
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Options.Utils
import Data.List.Split (splitWhen)
import Options.Applicative
import Options.Applicative.Builder.Extra

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser =
  standardToProtoType
    <$> ( ProtoOpts
            <$> forceReinstallP
            <*> outputLanguageP
            <*> fSpecGenOptsParser False
            <*> optional dirPrototypeP
            <*> optional dirCustomizationsP
            <*> zwolleVersionP
            <*> generateFrontendP
            <*> generateBackendP
            <*> generateMetamodelP
        )
  where
    standardToProtoType :: ProtoOpts -> ProtoOpts
    standardToProtoType opts =
      case view recipeL opts of
        Standard -> set recipeL Prototype opts
        _ -> opts

    forceReinstallP :: Parser Bool
    forceReinstallP =
      switch
        ( long "force-reinstall-framework"
            <> help
              ( "Re-install the prototype framework. This discards any previously "
                  <> "installed version."
              )
        )

    dirPrototypeP :: Parser String
    dirPrototypeP =
      strOption
        ( long "proto-dir"
            <> metavar "DIRECTORY"
            <> value defaultDirPrototype
            <> showDefault
            <> help "Specify the directory where the prototype will be generated"
        )

    dirCustomizationsP :: Parser [String]
    dirCustomizationsP =
      splitWhen (== ';')
        <$> strOption
          ( long "customizations"
              <> metavar "DIR;DIR;.."
              <> help "Copy one or more directories into the generated prototype. "
          )

    zwolleVersionP :: Parser String
    zwolleVersionP =
      strOption
        ( long "prototype-framework-version"
            <> metavar "VERSION"
            <> value "v1.6.0"
            <> showDefault
            <> help
              ( "Tag, branch or SHA of the prototype framework on Github. "
                  <> "Normally you shouldn't need to use anohter version "
                  <> "than the default. Only a developer of the framework "
                  <> "can make good use of it. "
              )
        )

    generateFrontendP :: Parser Bool
    generateFrontendP =
      boolFlags
        True
        "frontend"
        "Generate prototype frontend files (Angular application)"
        mempty

    generateBackendP :: Parser Bool
    generateBackendP =
      boolFlags
        True
        "backend"
        "Generate backend files (PHP application)"
        mempty

    -- This metamodel shows what the meatgrinder has made. This is useful for building prototypes that build on the meatgrinder.
    generateMetamodelP :: Parser Bool
    generateMetamodelP =
      boolFlags
        False
        "metamodel" -- the default is "do NOT generate a metamodel"
        "Generate metamodel.adl"
        mempty
