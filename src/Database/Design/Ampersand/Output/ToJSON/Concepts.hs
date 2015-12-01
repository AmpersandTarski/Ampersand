{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Concepts 
  (Concepts,Segment)
where
import Database.Design.Ampersand.FSpec(showADL)
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.FSpec.SQL (prettySQLQuery)
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Data.Maybe

data Concepts = Concepts [Concept] deriving (Generic, Show)
data Concept = Concept
  { cptJSONname              :: String
  , cptJSONtype              :: String
  , cptJSONgeneralizations   :: [String]
  , cptJSONspecializations   :: [String]
  , cptJSONaffectedConjuncts :: [String]
  , cptJSONinterfaces        :: [String]
  , cptJSONviews             :: [View]
  } deriving (Generic, Show)
data View = View
  { vwJSONlabel        :: String
  , vwJSONisDefault    :: Bool
  , vwJSONhtmlTemplate :: Maybe String
  , vwJSONsegments :: [Segment]
  } deriving (Generic, Show)
data Segment = Segment
  { segJSONlabel :: Maybe String
  , segJSONsegType :: String
  , segJSONadl  :: Maybe String
  , segJSONexpSQL :: Maybe String
  , segJSONtext  :: Maybe String
  , segJSONhtml  :: Maybe String
  } deriving (Generic, Show)
instance ToJSON Concept where
  toJSON = amp2Jason
instance ToJSON Concepts where
  toJSON = amp2Jason
instance ToJSON View where
  toJSON = amp2Jason
instance ToJSON Segment where
  toJSON = amp2Jason
instance JSON FSpec Concepts where
 fromAmpersand fSpec _ = Concepts (map (fromAmpersand fSpec) (concs fSpec))
instance JSON A_Concept Concept where
 fromAmpersand fSpec cpt = Concept
  { cptJSONname              = name cpt
  , cptJSONtype              = show . cptTType fSpec $ cpt
  , cptJSONgeneralizations   = map name . largerConcepts  (vgens fSpec) $ cpt
  , cptJSONspecializations   = map name . smallerConcepts (vgens fSpec) $ cpt
  , cptJSONaffectedConjuncts = map rc_id . fromMaybe [] . lookup cpt . allConjsPerConcept $ fSpec
  , cptJSONinterfaces        = map name . filter hasAsSourceCpt . interfaceS $ fSpec
  , cptJSONviews = map (fromAmpersand fSpec) . filter isForCpt . vviews $ fSpec
  } 
  where
    hasAsSourceCpt :: Interface -> Bool
    hasAsSourceCpt ifc = (source . objctx . ifcObj) ifc `elem` cpts
    isForCpt :: ViewDef -> Bool
    isForCpt vd = vdcpt vd `elem` cpts
    cpts = cpt : largerConcepts  (vgens fSpec) cpt
instance JSON ViewDef View where
 fromAmpersand fSpec vd = View
  { vwJSONlabel        = vdlbl vd
  , vwJSONisDefault    = vdIsDefault vd
  , vwJSONhtmlTemplate = fmap templateName . vdhtml $ vd
  , vwJSONsegments     = map (fromAmpersand fSpec) . vdats $ vd
  }
  where templateName (ViewHtmlTemplateFile fn) = fn
instance JSON ViewSegment Segment where
 fromAmpersand fSpec seg = Segment
  { segJSONlabel = vsmlabel seg
  , segJSONsegType = case vsmLoad seg of
                       ViewExp{}  -> "Exp"
                       ViewText{} -> "Text"
                       ViewHtml{} -> "Html"
  , segJSONadl  = case vsmLoad seg of
                       ViewExp expr -> Just . showADL $ expr
                       _            -> Nothing
  , segJSONexpSQL = case vsmLoad seg of
                       ViewExp expr -> Just $ prettySQLQuery fSpec 0 expr
                       _            -> Nothing
  , segJSONtext    = case vsmLoad seg of
                       ViewText str -> Just str
                       _            -> Nothing
  , segJSONhtml    = case vsmLoad seg of
                       ViewHtml str -> Just str
                       _            -> Nothing
  }