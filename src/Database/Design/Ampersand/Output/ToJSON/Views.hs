{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Views 
    (Views)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Output.ToJSON.Concepts 


data Views = Views [View] deriving (Generic, Show)
data View = View
  { vwJSONlabel      :: String
  , vwJSONconcept    :: String
  , vwJSONisDefault  :: Bool
  , vwJSONsegments   :: [Segment]
  } deriving (Generic, Show)
instance ToJSON View where
  toJSON = amp2Jason
instance ToJSON Views where
  toJSON = amp2Jason
instance JSON FSpec Views where
 fromAmpersand fSpec _ = Views $ (map (fromAmpersand fSpec) 
      [ v | c<-conceptsFromSpecificToGeneric, v <- vviews fSpec, vdcpt v==c ]) --sort from spec to gen
  where
   conceptsFromSpecificToGeneric = concatMap (reverse . tyCpts) . ftypologies $ fSpec
instance JSON ViewDef View where
 fromAmpersand fSpec vd = View
  { vwJSONlabel      = vdlbl vd
  , vwJSONconcept    = name . vdcpt $ vd
  , vwJSONisDefault  = vdIsDefault vd
  , vwJSONsegments   = map (fromAmpersand fSpec) . vdats $ vd
  } 








