{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Concepts (Concepts, Segment) where

import Ampersand.ADL1
import Ampersand.FSpec
import Ampersand.Output.ToJSON.JSONutils
import qualified RIO.List as L
import qualified RIO.Set as Set

newtype Concepts = Concepts [Concept] deriving (Generic, Show)

data Concept = Concept
  { cptJSONname :: Text,
    cptJSONlabel :: Text,
    cptJSONtype :: Text,
    cptJSONgeneralizations :: [Text],
    cptJSONspecializations :: [Text],
    cptJSONdirectGens :: [Text],
    cptJSONdirectSpecs :: [Text],
    cptJSONaffectedConjuncts :: [Text],
    cptJSONinterfaces :: [Text],
    cptJSONdefaultViewName :: Maybe Text,
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
  fromAmpersand env fSpec _ = Concepts (map (fromAmpersand env fSpec) (filter isUsed . toList $ concs fSpec))
    where
      isUsed :: A_Concept -> Bool
      isUsed cpt = cpt `Set.member` concs (instanceList fSpec :: [Relation])

instance JSON A_Concept Concept where
  fromAmpersand env fSpec cpt =
    Concept
      { cptJSONname = fullName cpt,
        cptJSONlabel = label cpt,
        cptJSONtype = tshow . cptTType fSpec $ cpt,
        cptJSONgeneralizations = map (text1ToText . idWithoutType') . largerConcepts (vgens fSpec) $ cpt,
        cptJSONspecializations = map (text1ToText . idWithoutType') . smallerConcepts (vgens fSpec) $ cpt,
        cptJSONdirectGens = map (text1ToText . idWithoutType') $ L.nub [g | (s, g) <- fsisa fSpec, s == cpt],
        cptJSONdirectSpecs = map (text1ToText . idWithoutType') $ L.nub [s | (s, g) <- fsisa fSpec, g == cpt],
        cptJSONaffectedConjuncts = maybe [] (map (text1ToText . rc_id)) . lookup cpt . allConjsPerConcept $ fSpec,
        cptJSONinterfaces = fmap fullName . filter hasAsSourceCpt . interfaceS $ fSpec,
        cptJSONdefaultViewName = fmap fullName . getDefaultViewForConcept fSpec $ cpt,
        cptJSONconceptTable = fromAmpersand env fSpec cpt,
        cptJSONlargestConcept = text1ToText . idWithoutType' . largestConcept fSpec $ cpt
      }
    where
      hasAsSourceCpt :: Interface -> Bool
      hasAsSourceCpt ifc = (source . objExpression . ifcObj) ifc `elem` cpts
      cpts = cpt : largerConcepts (vgens fSpec) cpt

instance JSON A_Concept TableCols where
  fromAmpersand _ fSpec cpt =
    TableCols
      { tclJSONname = tshow (sqlname cptTable),
        tclJSONcols = case L.nub . map fst $ cols of
          [t] ->
            if t == cptTable
              then map (text1ToText . sqlColumNameToText1 . attSQLColName . snd) cols
              else fatal $ "Table names should match: " <> tshow (sqlname t) <> " " <> tshow (sqlname cptTable) <> "."
          _ -> fatal "All concepts in a typology should be in exactly one table."
      }
    where
      cols = concatMap (lookupCpt fSpec) $ cpt : largerConcepts (vgens fSpec) cpt
      cptTable = case lookupCpt fSpec cpt of
        [(table, _)] -> table
        [] -> fatal ("Concept `" <> fullName cpt <> "` not found in a table.")
        _ -> fatal ("Concept `" <> fullName cpt <> "` found in multiple tables.")

instance JSON ViewDef View where
  fromAmpersand env fSpec vd =
    View
      { vwJSONlabel = fullName vd,
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
