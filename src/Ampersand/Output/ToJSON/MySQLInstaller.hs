{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller,Populations)
where
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Prototype.Generate
import Ampersand.Prototype.TableSpec
import qualified Data.Text as Text
import Data.Maybe

data MySQLInstaller = MySQLInstaller
   { msiJSONallDBstructQueries :: [Text.Text]
   , msiJSONallDefPopQueries   :: [Text.Text]
   } deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON MultiFSpecs MySQLInstaller where
 fromAmpersand _ multi = MySQLInstaller
        { msiJSONallDBstructQueries = map queryAsSQL $ generateDBstructQueries fSpec False
        , msiJSONallDefPopQueries = map queryAsSQL $ generateInitialPopQueries fSpec False
        }
  where
    fSpec = userFSpec multi

data Populations = Populations
   { epJSONatoms :: [AtomValuesOfConcept]
   , epJSONlinks :: [PairsOfRelation]
   } deriving (Generic, Show)
data AtomValuesOfConcept = AtomValuesOfConcept
   { avcJSONconcept :: Text.Text
   , avcJSONatoms :: [Text.Text]
   } deriving (Generic, Show)
data PairsOfRelation = PairsOfRelation
   { porJSONrelation :: Text.Text
   , porJSONlinks :: [JPair]
   } deriving (Generic, Show)
data JPair = JPair
   { prJSONsrc :: Text.Text
   , prJSONtgt :: Text.Text
   } deriving (Generic, Show)
instance ToJSON Populations where
  toJSON = amp2Jason
instance ToJSON AtomValuesOfConcept where
  toJSON = amp2Jason
instance ToJSON PairsOfRelation where
  toJSON = amp2Jason
instance ToJSON JPair where
  toJSON = amp2Jason
instance JSON (MultiFSpecs,Bool) Populations where
 fromAmpersand _ (multi,doMeta) = Populations
   { epJSONatoms = map (fromAmpersand multi) (zip (elems $ allConcepts theFSpec) (repeat doMeta))
   , epJSONlinks = map (fromAmpersand multi) (zip (elems $ vrels       theFSpec) (repeat doMeta))
   }
  where 
   theFSpec 
    | doMeta    = fromMaybe ftl (metaFSpec multi)
    | otherwise = userFSpec multi
     where ftl = fatal "There is no grinded fSpec."
instance JSON (A_Concept,Bool) AtomValuesOfConcept where
 fromAmpersand multi (cpt,doMeta) = AtomValuesOfConcept
   { avcJSONconcept = Text.pack (name cpt)
   , avcJSONatoms   = map (Text.pack . showValADL) (elems $ atomsBySmallestConcept theFSpec cpt)
   }
  where 
   theFSpec 
    | doMeta    = fromMaybe ftl (metaFSpec multi)
    | otherwise = userFSpec multi
     where ftl = fatal "There is no grinded fSpec."

instance JSON (Relation,Bool) PairsOfRelation where
 fromAmpersand multi (dcl,doMeta) = PairsOfRelation
   { porJSONrelation = Text.pack . showRel $ dcl
   , porJSONlinks = map (fromAmpersand multi) . pairsInExpr theFSpec $ EDcD dcl
   }
  where 
   theFSpec 
    | doMeta    = fromMaybe ftl (metaFSpec multi)
    | otherwise = userFSpec multi
     where ftl = fatal "There is no grinded fSpec."
instance JSON AAtomPair JPair where
  fromAmpersand _ p = JPair
    { prJSONsrc = Text.pack . showValADL . apLeft $ p 
    , prJSONtgt = Text.pack . showValADL . apRight $ p
    }

