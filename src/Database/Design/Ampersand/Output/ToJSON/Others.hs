{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Others 
    (TableColumnInfos,Views)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.FSpec.SQL (prettySQLQuery)
import Database.Design.Ampersand.Basics

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
  , vwJSONsegments   :: [JSONViewSegment]
  } deriving (Generic, Show)
data JSONViewSegment = JSONViewSegment
  { vwsJSONsegmentType :: String
  , vwsJSONlabel       :: String
  , vwsJSONText        :: Maybe String
  , vwsJSONHtml        :: Maybe String
  , vwsJSONexpSQL      :: Maybe String
  } deriving (Generic, Show)
instance ToJSON View where
  toJSON = amp2Jason
instance ToJSON Views where
  toJSON = amp2Jason
instance ToJSON JSONViewSegment where
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
instance JSON ViewSegment JSONViewSegment where
 fromAmpersand fSpec seg = 
  case seg of 
   (ViewText i str) -> JSONViewSegment
    { vwsJSONsegmentType = "Text"
    , vwsJSONlabel     = lab i
    , vwsJSONText      = Just str
    , vwsJSONHtml      = Nothing
    , vwsJSONexpSQL    = Nothing
    }
   (ViewHtml i str) -> JSONViewSegment
    { vwsJSONsegmentType = "Html"
    , vwsJSONlabel     = lab i
    , vwsJSONText      = Nothing
    , vwsJSONHtml      = Just str
    , vwsJSONexpSQL    = Nothing
    }
   (ViewExp _ objDef) -> JSONViewSegment
    { vwsJSONsegmentType = "Exp"
    , vwsJSONlabel     = objnm objDef
    , vwsJSONText      = Nothing
    , vwsJSONHtml      = Nothing
    , vwsJSONexpSQL    = Just . prettySQLQuery fSpec 0 . objctx $ objDef
    }
  where
   lab i = "segmnt_"++show i








