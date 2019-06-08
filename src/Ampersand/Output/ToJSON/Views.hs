{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Views 
    (Views)
where
import           Ampersand.ADL1
import           Ampersand.Output.ToJSON.Concepts 
import           Ampersand.Output.ToJSON.JSONutils 

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
instance JSON MultiFSpecs Views where
 fromAmpersand opts@Options{..} multi _ = Views . map (fromAmpersand opts multi) 
                               . vviews $ fSpec
   where 
    fSpec = userFSpec multi
    
instance JSON ViewDef View where
 fromAmpersand opts@Options{..} multi vd = View
  { vwJSONlabel      = name vd
  , vwJSONconceptId  = idWithoutType . vdcpt $ vd
  , vwJSONisDefault  = vdIsDefault vd
  , vwJSONsegments   = fmap (fromAmpersand opts multi) . vdats $ vd
  } 








