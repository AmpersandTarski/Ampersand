{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Rules 
  (Rulez)
where
import           Ampersand.ADL1
import           Ampersand.FSpec
import           Ampersand.Output.ToJSON.JSONutils 
import qualified Data.List.NonEmpty as NEL (toList)
import           Data.Maybe
import qualified Data.Set as Set

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
instance JSON MultiFSpecs Rulez where
 fromAmpersand opts multi _ = Rulez
   { rulJSONinvariants = map (fromAmpersand opts multi) . Set.elems $ invariants fSpec
   , rulJSONsignals    = map (fromAmpersand opts multi) . Set.elems $ signals fSpec
   }
  where
   fSpec = userFSpec multi
    
instance JSON Rule JsonRule where
 fromAmpersand opts multi rule = JsonRule
  { rulJSONname        = rrnm         rule
  , rulJSONruleAdl     = showA.formalExpression $ rule
  , rulJSONorigin      = show.rrfps     $ rule
  , rulJSONmeaning     = showMeaning
  , rulJSONmessage     = showMessage
  , rulJSONsrcConceptId = escapeIdentifier . name . source . formalExpression $ rule
  , rulJSONtgtConceptId = escapeIdentifier . name . target . formalExpression $ rule
  , rulJSONconjunctIds = map rc_id  $ fromMaybe [] (fmap NEL.toList . lookup rule $ allConjsPerRule fSpec)
  , rulJSONpairView    = fmap (fromAmpersand opts multi) (rrviol rule)
  } 
   where 
    fSpec = userFSpec multi
    showMeaning = maybe "" aMarkup2String (fmap ameaMrk . meaning (fsLang fSpec) $ rule)
    showMessage = case filter (\x -> amLang x == fsLang fSpec) (rrmsg rule) of
                              [] -> ""
                              h:_ -> aMarkup2String h
instance JSON (PairView Expression) JsonPairView where
 fromAmpersand opts multi pv = JsonPairView $ map (fromAmpersand opts multi) (zip [0..] (NEL.toList . ppv_segs $ pv))
instance JSON (Int,PairViewSegment Expression)  JsonPairViewSegment where
 fromAmpersand _ multi (nr,pvs) = JsonPairViewSegment
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
  where
    fSpec = userFSpec multi
    
 
  

