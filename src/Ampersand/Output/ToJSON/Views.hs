{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Views (Views) where

import Ampersand.ADL1
import Ampersand.Output.ToJSON.Concepts
import Ampersand.Output.ToJSON.JSONutils

newtype Views = Views [View] deriving (Generic, Show)

data View = View
  { vwJSONname :: !Text,
    vwJSONlabel :: !Text,
    vwJSONconceptName :: !Text,
    vwJSONisDefault :: !Bool,
    vwJSONsegments :: ![Segment]
  }
  deriving (Generic, Show)

instance ToJSON View where
  toJSON = amp2Jason

instance ToJSON Views where
  toJSON = amp2Jason

instance JSON FSpec Views where
  fromAmpersand env fSpec _ =
    Views
      . map (fromAmpersand env fSpec)
      . vviews
      $ fSpec

instance JSON ViewDef View where
  fromAmpersand env fSpec vd =
    View
      { vwJSONname = fullName vd,
        vwJSONlabel = label vd,
        vwJSONconceptName = text1ToText . idWithoutType' . vdcpt $ vd,
        vwJSONisDefault = vdIsDefault vd,
        vwJSONsegments = fmap (fromAmpersand env fSpec) . vdats $ vd
      }
