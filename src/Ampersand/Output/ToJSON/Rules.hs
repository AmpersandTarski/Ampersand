{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Rules 
  (Rulez)
where
import           Ampersand.ADL1
import           Ampersand.FSpec
import           Ampersand.Output.ToJSON.JSONutils 
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

data Rulez = Rulez
  { rulJSONinvariants :: [JsonRule]
  , rulJSONsignals    :: [JsonRule]
  } deriving (Generic, Show)
data JsonRule = JsonRule
  { rulJSONname         :: String
  , rulJSONruleAdl      :: String
  , rulJSONorigin       :: String
  , rulJSONmeaning      :: String
  , rulJSONmessage      :: String
  , rulJSONsrcConceptId :: String
  , rulJSONtgtConceptId :: String
  , rulJSONconjunctIds  :: [String]
  , rulJSONpairView     :: Maybe JsonPairView
  } deriving (Generic, Show)
data JsonPairView = JsonPairView [JsonPairViewSegment]
    deriving (Generic, Show)
data JsonPairViewSegment = JsonPairViewSegment
  { pvsJSONseqNr   :: Int
  , pvsJSONsegType :: String
  , pvsJSONtext        :: Maybe String
  , pvsJSONsrcOrTgt    :: Maybe String
  , pvsJSONexpTgt      :: Maybe String
  , pvsJSONexpSQL      :: Maybe String
  , pvsJSONexpIsIdent  :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON Rulez where
  toJSON = amp2Jason
instance ToJSON JsonRule where
  toJSON = amp2Jason
instance ToJSON JsonPairView where
  toJSON = amp2Jason
instance ToJSON JsonPairViewSegment where
  toJSON = amp2Jason
instance JSON FSpec Rulez where
 fromAmpersand env fSpec _ = Rulez
   { rulJSONinvariants = map (fromAmpersand env fSpec) . Set.elems $ invariants fSpec
   , rulJSONsignals    = map (fromAmpersand env fSpec) . Set.elems $ signals fSpec
   }
    
instance JSON Rule JsonRule where
 fromAmpersand env fSpec rule = JsonRule
  { rulJSONname        = rrnm         rule
  , rulJSONruleAdl     = showA.formalExpression $ rule
  , rulJSONorigin      = show.origin $ rule
  , rulJSONmeaning     = showMeaning
  , rulJSONmessage     = showMessage
  , rulJSONsrcConceptId = idWithoutType . source . formalExpression $ rule
  , rulJSONtgtConceptId = idWithoutType . target . formalExpression $ rule
  , rulJSONconjunctIds = map rc_id  $ fromMaybe [] (fmap NE.toList . lookup rule $ allConjsPerRule fSpec)
  , rulJSONpairView    = fmap (fromAmpersand env fSpec) (rrviol rule)
  } 
   where 
    showMeaning = maybe "" aMarkup2String (fmap ameaMrk . meaning (defOutputLang fSpec) $ rule)
    showMessage = case filter (\x -> amLang x == defOutputLang fSpec) (rrmsg rule) of
                              [] -> ""
                              h:_ -> aMarkup2String h
instance JSON (PairView Expression) JsonPairView where
 fromAmpersand env fSpec pv = JsonPairView $ map (fromAmpersand env fSpec) (zip [0..] (NE.toList . ppv_segs $ pv))
instance JSON (Int,PairViewSegment Expression)  JsonPairViewSegment where
 fromAmpersand _ fSpec (nr,pvs) = JsonPairViewSegment
  { pvsJSONseqNr   = nr
  , pvsJSONsegType = case pvs of
                           PairViewText{} -> "Text"
                           PairViewExp{}  -> "Exp"
  , pvsJSONtext        = case pvs of
                           PairViewText _ str -> Just str
                           PairViewExp{}  -> Nothing
  , pvsJSONsrcOrTgt    = case pvs of
                           PairViewText{} -> Nothing
                           PairViewExp _ srcOrTgt _  -> Just . show $ srcOrTgt
  , pvsJSONexpTgt      = case pvs of
                           PairViewText{} -> Nothing
                           PairViewExp _ _ e         -> Just . show . target $ e
  , pvsJSONexpSQL      = case pvs of
                           PairViewText{} -> Nothing
                           PairViewExp _ _ e         -> Just . sqlQuery fSpec $ e
  , pvsJSONexpIsIdent  = case pvs of
                           PairViewText{} -> Nothing
                           PairViewExp _ _ e         -> Just . isIdent $ e --show $ e
  } 

    
 
  

