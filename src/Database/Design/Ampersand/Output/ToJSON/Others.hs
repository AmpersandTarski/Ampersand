{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Others 
    (TableColumnInfos,Views)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Output.ToJSON.Concepts 
fatal :: Int -> String -> a
fatal = fatalMsg "ToJSON.Others"


data TableColumnInfos = TableColumnInfos [TableColumnInfo] deriving (Generic, Show)
data TableColumnInfo = TableColumnInfo
  { plgJSONname      :: String
  , plgJSONatts      :: [Attribute]
  } deriving (Generic, Show)
data Attribute = Attribute
  { attJSONattName   :: String
  , attJSONconcept   :: String
  , attJSONunique    :: Bool
  , attJSONnull      :: Bool
  } deriving (Generic, Show)
instance ToJSON TableColumnInfo where
  toJSON = amp2Jason
instance ToJSON TableColumnInfos where
  toJSON = amp2Jason
instance ToJSON Attribute where
  toJSON = amp2Jason
instance JSON FSpec TableColumnInfos where
 fromAmpersand fSpec _ = TableColumnInfos (map (fromAmpersand fSpec) (plugInfos fSpec))
instance JSON PlugInfo TableColumnInfo where
 fromAmpersand fSpec (InternalPlug plugSql) = TableColumnInfo
  { plgJSONname  = name $ plugSql
  , plgJSONatts  = map (fromAmpersand fSpec) (plugAttributes plugSql)
  } 
 fromAmpersand _ (ExternalPlug _) = fatal 36 "Non-SQL plugs are currently not supported" 
 
instance JSON SqlAttribute Attribute where
 fromAmpersand _ att = Attribute
  { attJSONattName   = attName $ att
  , attJSONconcept   = name . target . attExpr $ att
  , attJSONunique    = attUniq $ att
  , attJSONnull      = attNull $ att
  }


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
   conceptsFromSpecificToGeneric = concatMap reverse (kernels fSpec)
instance JSON ViewDef View where
 fromAmpersand fSpec vd = View
  { vwJSONlabel      = vdlbl vd
  , vwJSONconcept    = name . vdcpt $ vd
  , vwJSONisDefault  = vdIsDefault vd
  , vwJSONsegments   = map (fromAmpersand fSpec) . vdats $ vd
  } 








