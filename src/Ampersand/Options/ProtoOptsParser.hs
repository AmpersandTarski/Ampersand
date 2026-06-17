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
    <$> ( mkProtoOpts
            <$> outputLanguageP
            <*> fSpecGenOptsParser False
            <*> optional dirPrototypeP
            <*> generateFrontendP
            <*> generateBackendP
            <* frontendVersionP
            <*> generateMetamodelP
            <*> productionP
            <*> generateOpenAPIP
        )
  where
    -- The openapi default depends on the build target: a development build
    -- generates openapi.json, a production build does not. An explicit
    -- --openapi / --no-openapi (parsed as `Just`) always overrides this.
    mkProtoOpts ::
      Maybe Lang ->
      FSpecGenOpts ->
      Maybe FilePath ->
      Bool ->
      Bool ->
      Bool ->
      Bool ->
      Maybe Bool ->
      ProtoOpts
    mkProtoOpts lang fsgOpts dir frontend backend metamodel production openAPI =
      ProtoOpts lang fsgOpts dir frontend backend metamodel production (fromMaybe (not production) openAPI)

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

    -- A production build hides developer interfaces in the prototype and, unless
    -- overridden, suppresses openapi.json. The value is passed on to the prototype
    -- framework as `global.productionEnv` so compiler and framework behave consistently.
    productionP :: Parser Bool
    productionP =
      boolFlags
        False
        "production" -- the default is "development build"
        "Build for a production deployment: hide developer interfaces and (unless overridden) do not generate openapi.json"
        mempty

    -- The OpenAPI document (generics/openapi.json) describes the REST API that the
    -- backend serves. It is part of the backend output, so it is only written when the
    -- backend is generated. The default follows the build target (on for development,
    -- off for production); --openapi / --no-openapi force the value either way.
    -- `Nothing` means "not specified", so the production-derived default applies.
    generateOpenAPIP :: Parser (Maybe Bool)
    generateOpenAPIP =
      flag'
        (Just True)
        (long "openapi" <> help "Generate generics/openapi.json (OpenAPI 3.0 description of the backend REST API)")
        <|> flag'
          (Just False)
          (long "no-openapi" <> help "Do not generate generics/openapi.json")
        <|> pure Nothing
