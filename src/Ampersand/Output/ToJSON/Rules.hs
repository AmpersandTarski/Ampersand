{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Rules 
  (Rules)
where
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Core.AbstractSyntaxTree 
import Ampersand.FSpec
import Data.Maybe

data Rules = Rules
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
  , pvsJSONsegmentType :: String
  , pvsJSONText        :: Maybe String
  , pvsJSONsrcOrTgt    :: Maybe String
  , pvsJSONexpTgt      :: Maybe String
  , pvsJSONexpSQL      :: Maybe String
  , pvsJSONexpIsIdent  :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON Rules where
  toJSON = amp2Jason
instance ToJSON JsonRule where
  toJSON = amp2Jason
instance ToJSON JsonPairView where
  toJSON = amp2Jason
instance ToJSON JsonPairViewSegment where
  toJSON = amp2Jason
instance JSON MultiFSpecs Rules where
 fromAmpersand multi _ = Rules
   { rulJSONinvariants = map (fromAmpersand multi) (invariants fSpec)
   , rulJSONsignals    = map (fromAmpersand multi) (signals fSpec)
   }
  where
   fSpec = userFSpec multi
    
instance JSON Rule JsonRule where
 fromAmpersand multi rule = JsonRule
  { rulJSONname        = rrnm         rule
  , rulJSONruleAdl     = showA.rrexp $ rule
  , rulJSONorigin      = show.rrfps     $ rule
  , rulJSONmeaning     = showMeaning
  , rulJSONmessage     = showMessage
  , rulJSONsrcConceptId = escapeIdentifier . name . source . rrexp $ rule
  , rulJSONtgtConceptId = escapeIdentifier . name . target . rrexp $ rule
  , rulJSONconjunctIds = map rc_id  $ fromMaybe [] (lookup rule $ allConjsPerRule fSpec)
  , rulJSONpairView    = fmap (fromAmpersand multi) (rrviol rule)
  } 
   where 
    fSpec = userFSpec multi
    showMeaning = maybe "" (aMarkup2String Markdown) (meaning (fsLang fSpec) rule)
    showMessage = case [ markup | markup <- rrmsg rule, amLang markup == fsLang fSpec ] of
                              []    -> ""
                              markup:_ -> aMarkup2String Markdown markup
instance JSON (PairView Expression) JsonPairView where
 fromAmpersand multi pv = JsonPairView $ map (fromAmpersand multi) (zip [0..] (ppv_segs pv))
instance JSON (Int,PairViewSegment Expression)  JsonPairViewSegment where
 fromAmpersand multi (nr,pvs) = JsonPairViewSegment
  { pvsJSONseqNr   = nr
  , pvsJSONsegmentType = case pvs of
                           PairViewText{} -> "Text"
                           PairViewExp{}  -> "Exp"
  , pvsJSONText        = case pvs of
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
    
 
  

