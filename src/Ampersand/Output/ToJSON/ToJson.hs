{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.ToJson
  ( settingsToJSON, relationsToJSON, rulesToJSON, conceptsToJSON, conjunctsToJSON
  , interfacesToJSON, viewsToJSON, rolesToJSON, populationToJSON
  )
where
import           Ampersand.Output.ToJSON.Concepts 
import           Ampersand.Output.ToJSON.Conjuncts 
import           Ampersand.Output.ToJSON.Interfaces 
import           Ampersand.Output.ToJSON.JSONutils
import           Ampersand.Output.ToJSON.Populations
import           Ampersand.Output.ToJSON.Relations
import           Ampersand.Output.ToJSON.Roles
import           Ampersand.Output.ToJSON.Rules 
import           Ampersand.Output.ToJSON.Settings
import           Ampersand.Output.ToJSON.Views
import           Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL

settingsToJSON :: (Show env, HasProtoOpts env) => env -> FSpec -> BL.ByteString
settingsToJSON env fSpec = encodePretty'' (fromAmpersand' env fSpec fSpec :: Settings)

relationsToJSON :: env -> FSpec -> BL.ByteString
relationsToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Relationz)

rulesToJSON :: env -> FSpec -> BL.ByteString
rulesToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Rulez)

conceptsToJSON :: env -> FSpec -> BL.ByteString
conceptsToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Concepts)

conjunctsToJSON :: env -> FSpec -> BL.ByteString
conjunctsToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Conjuncts)

interfacesToJSON :: env -> FSpec -> BL.ByteString
interfacesToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Interfaces)

viewsToJSON :: env -> FSpec -> BL.ByteString
viewsToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Views)

rolesToJSON :: env -> FSpec -> BL.ByteString
rolesToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Roles)

populationToJSON :: env -> FSpec -> BL.ByteString
populationToJSON env fSpec = encodePretty'' (fromAmpersand env fSpec fSpec :: Populations)


encodePretty'' :: ToJSON a => a -> BL.ByteString
encodePretty'' =  
  encodePretty' Config { confIndent = Spaces 4
                       , confCompare = compare
                       , confNumFormat = Generic
                       , confTrailingNewline = False
                       }