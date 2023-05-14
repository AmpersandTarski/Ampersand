{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Concepts (Concepts, Segment) where

import Ampersand.ADL1
import Ampersand.Output.ToJSON.JSONutils
import qualified RIO.List as L

newtype Concepts = Concepts [Concept] deriving (Generic, Show)

data Concept = Concept
  { cptJSONid :: Text,
    cptJSONlabel :: Text,
    cptJSONtype :: Text,
    cptJSONgeneralizations :: [Text],
    cptJSONspecializations :: [Text],
    cptJSONdirectGens :: [Text],
    cptJSONdirectSpecs :: [Text],
    cptJSONaffectedConjuncts :: [Text],
    cptJSONinterfaces :: [Text],
    cptJSONdefaultViewId :: Maybe Text,
    cptJSONconceptTable :: TableCols,
    cptJSONlargestConcept :: Text
  }
  deriving (Generic, Show)

data TableCols = TableCols
  { tclJSONname :: Text,
    tclJSONcols :: [Text]
  }
  deriving (Generic, Show)

data View = View
  { vwJSONlabel :: Text,
    vwJSONisDefault :: Bool,
    vwJSONhtmlTemplate :: Maybe FilePath,
    vwJSONsegments :: [Segment]
  }
  deriving (Generic, Show)

data Segment = Segment
  { segJSONseqNr :: Integer,
    segJSONlabel :: Maybe Text,
    segJSONsegType :: Text,
    segJSONexpADL :: Maybe Text,
    segJSONexpSQL :: Maybe Text,
    segJSONtext :: Maybe Text
  }
  deriving (Generic, Show)

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

instance JSON FSpec Concepts where
  fromAmpersand env fSpec _ = Concepts (map (fromAmpersand env fSpec) (toList $ concs fSpec))

instance JSON A_Concept Concept where
  fromAmpersand env fSpec cpt =
    Concept
      { cptJSONid = text1ToText . idWithoutType $ cpt,
        cptJSONlabel = text1ToText . tName $ cpt,
        cptJSONtype = tshow . cptTType fSpec $ cpt,
        cptJSONgeneralizations = map (text1ToText . idWithoutType) . largerConcepts (vgens fSpec) $ cpt,
        cptJSONspecializations = map (text1ToText . idWithoutType) . smallerConcepts (vgens fSpec) $ cpt,
        cptJSONdirectGens = map (text1ToText . idWithoutType) $ L.nub [g | (s, g) <- fsisa fSpec, s == cpt],
        cptJSONdirectSpecs = map (text1ToText . idWithoutType) $ L.nub [s | (s, g) <- fsisa fSpec, g == cpt],
        cptJSONaffectedConjuncts = maybe [] (map (text1ToText . rc_id)) . lookup cpt . allConjsPerConcept $ fSpec,
        cptJSONinterfaces = fmap (text1ToText . tName) . filter hasAsSourceCpt . interfaceS $ fSpec,
        cptJSONdefaultViewId = fmap (text1ToText . tName) . getDefaultViewForConcept fSpec $ cpt,
        cptJSONconceptTable = fromAmpersand env fSpec cpt,
        cptJSONlargestConcept = text1ToText . idWithoutType . largestConcept fSpec $ cpt
      }
    where
      hasAsSourceCpt :: Interface -> Bool
      hasAsSourceCpt ifc = (source . objExpression . ifcObj) ifc `elem` cpts
      cpts = cpt : largerConcepts (vgens fSpec) cpt

instance JSON A_Concept TableCols where
  fromAmpersand _ fSpec cpt =
    TableCols
      { tclJSONname = (text1ToText . tName) cptTable,
        tclJSONcols = case L.nub . map fst $ cols of
          [t] ->
            if t == cptTable
              then map (text1ToText . sqlColumNameToText1 . attSQLColName . snd) cols
              else fatal $ "Table names should match: " <> (text1ToText . tName) t <> " " <> (text1ToText . tName) cptTable <> "."
          _ -> fatal "All concepts in a typology should be in exactly one table."
      }
    where
      cols = concatMap (lookupCpt fSpec) $ cpt : largerConcepts (vgens fSpec) cpt
      cptTable = case lookupCpt fSpec cpt of
        [(table, _)] -> table
        [] -> fatal ("Concept `" <> (text1ToText . tName) cpt <> "` not found in a table.")
        _ -> fatal ("Concept `" <> (text1ToText . tName) cpt <> "` found in multiple tables.")

instance JSON ViewDef View where
  fromAmpersand env fSpec vd =
    View
      { vwJSONlabel = (text1ToText . tName) vd,
        vwJSONisDefault = vdIsDefault vd,
        vwJSONhtmlTemplate = fmap templateName . vdhtml $ vd,
        vwJSONsegments = fmap (fromAmpersand env fSpec) . vdats $ vd
      }
    where
      templateName (ViewHtmlTemplateFile fn) = fn

instance JSON ViewSegment Segment where
  fromAmpersand _ fSpec seg =
    Segment
      { segJSONseqNr = vsmSeqNr seg,
        segJSONlabel = text1ToText <$> vsmlabel seg,
        segJSONsegType = case vsmLoad seg of
          ViewExp {} -> "Exp"
          ViewText {} -> "Text",
        segJSONexpADL = case vsmLoad seg of
          ViewExp expr -> Just . showA $ expr
          _ -> Nothing,
        segJSONexpSQL = case vsmLoad seg of
          ViewExp expr -> Just $ sqlQuery fSpec expr
          _ -> Nothing,
        segJSONtext = case vsmLoad seg of
          ViewText str -> Just str
          _ -> Nothing
      }
