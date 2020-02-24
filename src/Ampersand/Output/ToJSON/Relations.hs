{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.Relations 
  (Relationz)
where
import           Ampersand.ADL1
import           Ampersand.FSpec.FSpecAux
import           Ampersand.Output.ToJSON.JSONutils 
import qualified RIO.Set as Set

data Relationz = Relationz [RelationJson]deriving (Generic, Show)
data RelationJson = RelationJson
  { relJSONname         :: Text
  , relJSONsignature    :: Text
  , relJSONsrcConceptId :: Text
  , relJSONtgtConceptId :: Text
  , relJSONuni          :: Bool
  , relJSONtot          :: Bool
  , relJSONinj          :: Bool
  , relJSONsur          :: Bool
  , relJSONprop         :: Bool
  , relJSONaffectedConjuncts :: [Text]
  , relJSONmysqlTable   :: RelTableInfo
  } deriving (Generic, Show)
data RelTableInfo = RelTableInfo -- Contains info about where the relation is implemented in SQL
  { rtiJSONname    :: Text
  , rtiJSONtableOf :: Maybe Text -- specifies if relation is administrated in table of srcConcept (i.e. "src"), tgtConcept (i.e. "tgt") or its own n-n table (i.e. null).
  , rtiJSONsrcCol  :: TableCol
  , rtiJSONtgtCol  :: TableCol
  } deriving (Generic, Show)
data TableCol = TableCol
  { tcJSONname     :: Text
  , tcJSONnull     :: Bool
  , tcJSONunique   :: Bool
  } deriving (Generic, Show)
instance ToJSON Relationz where
  toJSON = amp2Jason
instance ToJSON RelationJson where
  toJSON = amp2Jason
instance ToJSON RelTableInfo where
  toJSON = amp2Jason
instance ToJSON TableCol where
  toJSON = amp2Jason
instance JSON FSpec Relationz where
 fromAmpersand env fSpec _ = Relationz (map (fromAmpersand env fSpec) (Set.elems $ vrels fSpec))
instance JSON Relation RelationJson where
 fromAmpersand env fSpec dcl = RelationJson 
         { relJSONname       = name dcl
         , relJSONsignature  = name dcl <> (tshow . sign) dcl
         , relJSONsrcConceptId  = idWithoutType . source $ dcl 
         , relJSONtgtConceptId  = idWithoutType . target $ dcl
         , relJSONuni      = isUni bindedExp
         , relJSONtot      = isTot bindedExp
         , relJSONinj      = isInj bindedExp
         , relJSONsur      = isSur bindedExp
         , relJSONprop     = isProp bindedExp
         , relJSONaffectedConjuncts = map rc_id  $ fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)
         , relJSONmysqlTable = fromAmpersand env fSpec dcl
         }
      where bindedExp = EDcD dcl
         
instance JSON Relation RelTableInfo where
 fromAmpersand env fSpec dcl = RelTableInfo
  { rtiJSONname    = name plug
  , rtiJSONtableOf = srcOrtgt
  , rtiJSONsrcCol  = fromAmpersand env fSpec . rsSrcAtt $ relstore
  , rtiJSONtgtCol  = fromAmpersand env fSpec . rsTrgAtt $ relstore
  }
   where (plug,relstore) = getRelationTableInfo fSpec dcl
         (plugSrc,_)     = getConceptTableInfo fSpec (source dcl)
         (plugTrg,_)     = getConceptTableInfo fSpec (target dcl)
         srcOrtgt :: Maybe Text
         srcOrtgt
           | (plug == plugSrc) && (plugSrc == plugTrg) = Just $ if rsStoredFlipped relstore then "tgt" else "src" -- relations where src and tgt concepts are in the same classification tree as well as relations that are UNI or INJ
           | plug == plugSrc = Just "src" -- relation in same table as src concept (UNI relations)
           | plug == plugTrg = Just "tgt" -- relation in same table as tgt concept (INJ relations that are not UNI)
           | otherwise       = Nothing -- relations in n-n table (not UNI and not INJ)
instance JSON SqlAttribute TableCol where
 fromAmpersand _ _ att = TableCol
  { tcJSONname   = attName att
  , tcJSONnull   = attDBNull att
  , tcJSONunique = attUniq att
  }


