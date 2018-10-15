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
 fromAmpersand multi _ = Rulez
   { rulJSONinvariants = map (fromAmpersand multi) . Set.elems $ invariants fSpec
   , rulJSONsignals    = map (fromAmpersand multi) . Set.elems $ signals fSpec
   }
  where
   fSpec = userFSpec multi
    
instance JSON Rule JsonRule where
 fromAmpersand multi rule = JsonRule
  { rulJSONname        = rrnm         rule
  , rulJSONruleAdl     = showA.formalExpression $ rule
  , rulJSONorigin      = show.rrfps     $ rule
  , rulJSONmeaning     = showMeaning
  , rulJSONmessage     = showMessage
  , rulJSONsrcConceptId = escapeIdentifier . name . source . formalExpression $ rule
  , rulJSONtgtConceptId = escapeIdentifier . name . target . formalExpression $ rule
  , rulJSONconjunctIds = map rc_id  $ fromMaybe [] (lookup rule $ allConjsPerRule fSpec)
  , rulJSONpairView    = fmap (fromAmpersand multi) (rrviol rule)
  } 
   where 
    fSpec = userFSpec multi
    showMeaning = 
        case meaning (fsLang fSpec) rule of
          []  -> ""
          xs  -> aMarkup2String . ameaMrk . head $ xs
    showMessage = case filter (\x -> amLang x == fsLang fSpec) (rrmsg rule) of
                              [] -> ""
                              xs -> aMarkup2String (head xs)
instance JSON (PairView Expression) JsonPairView where
 fromAmpersand multi pv = JsonPairView $ map (fromAmpersand multi) (zip [0..] (NEL.toList . ppv_segs $ pv))
instance JSON (Int,PairViewSegment Expression)  JsonPairViewSegment where
 fromAmpersand multi (nr,pvs) = JsonPairViewSegment
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
    
 
  

