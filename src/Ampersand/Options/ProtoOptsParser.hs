module Ampersand.Options.ProtoOptsParser (protoOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.Defaults (defaultDirPrototype)
import Ampersand.Misc.HasClasses
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Options.Utils
import Options.Applicative
import Options.Applicative.Builder.Extra

protoOptsParser :: Parser ProtoOpts
protoOptsParser =
  standardToProtoType
    <$> ( ProtoOpts
            <$> outputLanguageP
            <*> fSpecGenOptsParser False
            <*> optional dirPrototypeP
            <*> generateFrontendP
            <*> generateBackendP
            <* frontendVersionP
            <*> generateMetamodelP
        )
  where
    standardToProtoType :: ProtoOpts -> ProtoOpts
    standardToProtoType opts =
      case view recipeL opts of
        Standard -> set recipeL Prototype opts
        _ -> opts

    dirPrototypeP :: Parser String
    dirPrototypeP =
      strOption
        ( long "proto-dir"
            <> metavar "DIRECTORY"
            <> value defaultDirPrototype
            <> showDefault
            <> help "Specify the directory where the prototype will be generated"
        )

    frontendVersionP :: Parser String
    frontendVersionP =
      strOption
        ( long "frontend-version"
            <> metavar "VERSION"
            <> value "angular"
            <> showDefault
            <> help "Remains of a temporary switch to enable the development of the new angular frontend."
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
