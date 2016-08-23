{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Views 
    (Views)
where
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Core.AbstractSyntaxTree 
import Ampersand.Output.ToJSON.Concepts 

data Views = Views [View] deriving (Generic, Show)
data View = View
  { vwJSONlabel      :: String
  , vwJSONconceptId  :: String
  , vwJSONisDefault  :: Bool
  , vwJSONsegments   :: [Segment]
  } deriving (Generic, Show)
instance ToJSON View where
  toJSON = amp2Jason
instance ToJSON Views where
  toJSON = amp2Jason
instance JSON FSpec Views where
 fromAmpersand fSpec _ = Views . map (fromAmpersand fSpec) 
                               . vviews $ fSpec
instance JSON ViewDef View where
 fromAmpersand fSpec vd = View
  { vwJSONlabel      = name vd
  , vwJSONconceptId  = escapeIdentifier . name . vdcpt $ vd
  , vwJSONisDefault  = vdIsDefault vd
  , vwJSONsegments   = map (fromAmpersand fSpec) . vdats $ vd
  } 








