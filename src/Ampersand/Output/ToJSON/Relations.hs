{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Relations (Relationz) where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters(pReldefault2aReldefaults)
import Ampersand.FSpec.FSpecAux
import Ampersand.Output.ToJSON.JSONutils
import qualified RIO.Set as Set

newtype Relationz = Relationz [RelationJson] deriving (Generic, Show)

data RelationJson = RelationJson
  { relJSONname :: !Text,
    relJSONsignature :: !Text,
    relJSONlabel :: !Text,
    relJSONsrcConceptName :: !Text,
    relJSONtgtConceptName :: !Text,
    relJSONuni :: !Bool,
    relJSONtot :: !Bool,
    relJSONinj :: !Bool,
    relJSONsur :: !Bool,
    relJSONprop :: !Bool,
    relJSONaffectedConjuncts :: ![Text],
    relJSONmysqlTable :: !RelTableInfo,
    relJSONdefaultSrc :: ![Text],
    relJSONdefaultTgt :: ![Text]
  }
  deriving (Generic, Show)

data RelTableInfo = RelTableInfo -- Contains info about where the relation is implemented in SQL
  { rtiJSONname :: !Text,
    rtiJSONtableOf :: !(Maybe Text), -- specifies if relation is administrated in table of srcConcept (i.e. "src"), tgtConcept (i.e. "tgt") or its own n-n table (i.e. null).
    rtiJSONsrcCol :: !TableCol,
    rtiJSONtgtCol :: !TableCol
  }
  deriving (Generic, Show)

data TableCol = TableCol
  { tcJSONname :: !Text,
    tcJSONnull :: !Bool,
    tcJSONunique :: !Bool
  }
  deriving (Generic, Show)

instance ToJSON Relationz where
  toJSON = amp2Jason

instance ToJSON RelationJson where
  toJSON = amp2Jason

instance ToJSON RelTableInfo where
  toJSON = amp2Jason

instance ToJSON TableCol where
  toJSON = amp2Jason

instance JSON FSpec Relationz where
  fromAmpersand env fSpec _ = Relationz (map (fromAmpersand env fSpec) (toList $ vrels fSpec))

instance JSON Relation RelationJson where
  fromAmpersand env fSpec dcl =
    RelationJson
      { relJSONname = fullName dcl,
        relJSONsignature = fullName dcl <> (tshow . sign) dcl,
        relJSONlabel = label dcl,
        relJSONsrcConceptName = text1ToText . idWithoutType' . source $ dcl,
        relJSONtgtConceptName = text1ToText . idWithoutType' . target $ dcl,
        relJSONuni = isUni bindedExp,
        relJSONtot = isTot bindedExp,
        relJSONinj = isInj bindedExp,
        relJSONsur = isSur bindedExp,
        relJSONprop = isProp bindedExp,
        relJSONaffectedConjuncts = maybe [] (map $ text1ToText . rc_id) . lookup dcl . allConjsPerDecl $ fSpec,
        relJSONmysqlTable = fromAmpersand env fSpec dcl,
        relJSONdefaultSrc = concatMap toText . Set.toList . Set.filter (is Src) $ defaults,
        relJSONdefaultTgt = concatMap toText . Set.toList . Set.filter (is Tgt) $ defaults
      }
    where
      defaults = mapM (pReldefault2aReldefaults cptMap pd) dcl
      bindedExp = EDcD dcl
      is :: SrcOrTgt -> ARelDefault -> Bool
      is st x = case x of
        ARelDefaultAtom st' _ -> st == st'
        ARelDefaultEvalPHP st' _ -> st == st'
      toText :: ARelDefault -> [Text]
      toText x = case x of
        ARelDefaultAtom _ vals -> toList $ fmap showValADL vals
        ARelDefaultEvalPHP _ txt -> ["{php}" <> txt]

instance JSON Relation RelTableInfo where
  fromAmpersand env fSpec dcl =
    RelTableInfo
      { rtiJSONname = text1ToText . showUnique $ plug,
        rtiJSONtableOf = srcOrtgt,
        rtiJSONsrcCol = fromAmpersand env fSpec . rsSrcAtt $ relstore,
        rtiJSONtgtCol = fromAmpersand env fSpec . rsTrgAtt $ relstore
      }
    where
      (plug, relstore) = getRelationTableInfo fSpec dcl
      (plugSrc, _) = getConceptTableInfo fSpec (source dcl)
      (plugTrg, _) = getConceptTableInfo fSpec (target dcl)
      srcOrtgt :: Maybe Text
      srcOrtgt
        | (plug == plugSrc) && (plugSrc == plugTrg) = Just $ if rsStoredFlipped relstore then "tgt" else "src" -- relations where src and tgt concepts are in the same classification tree as well as relations that are UNI or INJ
        | plug == plugSrc = Just "src" -- relation in same table as src concept (UNI relations)
        | plug == plugTrg = Just "tgt" -- relation in same table as tgt concept (INJ relations that are not UNI)
        | otherwise = Nothing -- relations in n-n table (not UNI and not INJ)

instance JSON SqlAttribute TableCol where
  fromAmpersand _ _ att =
    TableCol
      { tcJSONname = text1ToText . sqlColumNameToText1 . attSQLColName $ att,
        tcJSONnull = attDBNull att,
        tcJSONunique = attUniq att
      }
