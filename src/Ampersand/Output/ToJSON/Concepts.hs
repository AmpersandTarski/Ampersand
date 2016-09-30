{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Concepts 
  (Concepts,Segment)
where
import Ampersand.FSpec(showADL)
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Core.AbstractSyntaxTree 
import Data.Maybe
import Data.List(nub)

data Concepts = Concepts [Concept] deriving (Generic, Show)
data Concept = Concept
  { cptJSONid                :: String
  , cptJSONlabel             :: String
  , cptJSONtype              :: String
  , cptJSONgeneralizations   :: [String]
  , cptJSONspecializations   :: [String]
  , cptJSONaffectedConjuncts :: [String]
  , cptJSONinterfaces        :: [String]
  , cptJSONdefaultViewId     :: Maybe String 
  , cptJSONconceptTable      :: TableCols
  , cptJSONlargestConcept    :: String
  } deriving (Generic, Show)
data TableCols = TableCols
  { tclJSONname              :: String
  , tclJSONcols              :: [String]
  } deriving (Generic, Show)
data View = View
  { vwJSONlabel        :: String
  , vwJSONisDefault    :: Bool
  , vwJSONhtmlTemplate :: Maybe String
  , vwJSONsegments :: [Segment]
  } deriving (Generic, Show)
data Segment = Segment
  { segJSONseqNr   :: Integer
  , segJSONlabel :: Maybe String
  , segJSONsegType :: String
  , segJSONexpADL  :: Maybe String
  , segJSONexpSQL :: Maybe String
  , segJSONtext  :: Maybe String
  } deriving (Generic, Show)
instance ToJSON Concept where
  toJSON = amp2Jason
instance ToJSON Concepts where
  toJSON = amp2Jason
instance ToJSON View where
  toJSON = amp2Jason
instance ToJSON Segment where
  toJSON = amp2Jason
instance ToJSON TableCols where
  toJSON = amp2Jason
instance JSON MultiFSpecs Concepts where
 fromAmpersand multi _ = Concepts (map (fromAmpersand multi) (concs fSpec))
   where fSpec = userFSpec multi
instance JSON A_Concept Concept where
 fromAmpersand multi cpt = Concept
  { cptJSONid                = escapeIdentifier . name $ cpt
  , cptJSONlabel             = name cpt
  , cptJSONtype              = show . cptTType fSpec $ cpt
  , cptJSONgeneralizations   = map (escapeIdentifier . name) . largerConcepts  (vgens fSpec) $ cpt
  , cptJSONspecializations   = map (escapeIdentifier . name) . smallerConcepts (vgens fSpec) $ cpt
  , cptJSONaffectedConjuncts = map rc_id . fromMaybe [] . lookup cpt . allConjsPerConcept $ fSpec
  , cptJSONinterfaces        = map name . filter hasAsSourceCpt . interfaceS $ fSpec
  , cptJSONdefaultViewId     = fmap name . getDefaultViewForConcept fSpec $ cpt
  , cptJSONconceptTable = fromAmpersand multi cpt
  , cptJSONlargestConcept = escapeIdentifier . name . largestConcept fSpec $ cpt
  } 
  where
    fSpec = userFSpec multi
    hasAsSourceCpt :: Interface -> Bool
    hasAsSourceCpt ifc = (source . objctx . ifcObj) ifc `elem` cpts
    cpts = cpt : largerConcepts  (vgens fSpec) cpt
instance JSON A_Concept TableCols where
 fromAmpersand multi cpt = TableCols
  { tclJSONname    = name cptTable
  , tclJSONcols    = case nub . map fst $ cols of
                       [t] -> if name t == name cptTable
                              then map (attName . snd) cols
                              else fatal 78 $ "Table names should match: "++name t++" "++name cptTable++"." 
                       _   -> fatal 79 "All concepts in a typology should be in exactly one table."
  }
  where
    fSpec = userFSpec multi
    cols = concatMap (lookupCpt fSpec) $ cpt : largerConcepts (vgens fSpec) cpt
    cptTable = case lookupCpt fSpec cpt of
      [(table,_)] -> table
      []      -> fatal 80 $ "Concept `"++name cpt++"` not found in a table."
      _       -> fatal 81 $ "Concept `"++name cpt++"` found in multiple tables."
instance JSON ViewDef View where
 fromAmpersand multi vd = View
  { vwJSONlabel        = name vd
  , vwJSONisDefault    = vdIsDefault vd
  , vwJSONhtmlTemplate = fmap templateName . vdhtml $ vd
  , vwJSONsegments     = map (fromAmpersand multi) . vdats $ vd
  }
  where templateName (ViewHtmlTemplateFile fn) = fn
instance JSON ViewSegment Segment where
 fromAmpersand multi seg = Segment
  { segJSONseqNr = vsmSeqNr seg
  , segJSONlabel = vsmlabel seg
  , segJSONsegType = case vsmLoad seg of
                       ViewExp{}  -> "Exp"
                       ViewText{} -> "Text"
  , segJSONexpADL  = case vsmLoad seg of
                       ViewExp expr -> Just . showADL $ expr
                       _            -> Nothing
  , segJSONexpSQL  = case vsmLoad seg of
                       ViewExp expr -> Just $ sqlQuery fSpec expr
                       _            -> Nothing
  , segJSONtext    = case vsmLoad seg of
                       ViewText str -> Just str
                       _            -> Nothing
  }
  where
    fSpec = userFSpec multi
    