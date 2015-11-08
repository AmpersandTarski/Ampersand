{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Rules 
  (Rules)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand
import Data.Maybe

data Rules = Rules
  { rulJSONinvariants :: [JsonRule]
  , rulJSONsignals    :: [JsonRule]
  } deriving (Generic, Show)
data JsonRule = JsonRule
  { rulJSONname        :: String
  , rulJSONruleAdl     :: String
  , rulJSONorigin      :: String
  , rulJSONmeaning     :: String
  , rulJSONmessage     :: String
  , rulJSONsrcConcept  :: String
  , rulJSONtgtConcept  :: String
  , rulJSONconjunctIds :: [String]
  , rulJSONpairView    :: Maybe JsonPairView
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
  } deriving (Generic, Show)

instance ToJSON Rules where
  toJSON = amp2Jason
instance ToJSON JsonRule where
  toJSON = amp2Jason
instance ToJSON JsonPairView where
  toJSON = amp2Jason
instance ToJSON JsonPairViewSegment where
  toJSON = amp2Jason
instance JSON FSpec Rules where
 fromAmpersand fSpec _ = Rules
   { rulJSONinvariants = map (fromAmpersand fSpec) (invariants fSpec)
   , rulJSONsignals    = map (fromAmpersand fSpec) (signals fSpec)
   }
instance JSON Rule JsonRule where
 fromAmpersand fSpec rule = JsonRule
  { rulJSONname        = rrnm         rule
  , rulJSONruleAdl     = showADL.rrexp $ rule
  , rulJSONorigin      = show.rrfps     $ rule
  , rulJSONmeaning     = showMeaning
  , rulJSONmessage     = showMessage
  , rulJSONsrcConcept  = name . source . rrexp $ rule
  , rulJSONtgtConcept  = name . target . rrexp $ rule
  , rulJSONconjunctIds = map rc_id  $ fromMaybe [] (lookup rule $ allConjsPerRule fSpec)
  , rulJSONpairView    = fmap (fromAmpersand fSpec) (rrviol rule)
  } 
   where showMeaning = maybe "" (aMarkup2String ReST) (meaning (fsLang fSpec) rule)
         showMessage = case [ markup | markup <- rrmsg rule, amLang markup == fsLang fSpec ] of
                              []    -> ""
                              markup:_ -> aMarkup2String ReST markup
instance JSON (PairView Expression) JsonPairView where
 fromAmpersand fSpec pv = JsonPairView $ map (fromAmpersand fSpec) (zip [0..] (ppv_segs pv))
instance JSON (Int,PairViewSegment Expression)  JsonPairViewSegment where
 fromAmpersand fSpec (nr,pvs) = JsonPairViewSegment
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
                           PairViewExp _ _ e         -> Just . prettySQLQuery fSpec 0 $ e
  } 
 
  

