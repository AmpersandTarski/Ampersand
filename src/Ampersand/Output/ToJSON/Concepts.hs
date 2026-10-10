{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Concepts (Concepts, Segment) where

import Ampersand.ADL1
import qualified Ampersand.Basics.Name as Name
import Ampersand.Core.AbstractSyntaxTree (largerConcepts, smallerConcepts)
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
    -- | Nothing for a concept that has no concept table of its own: ONE, and
    --   any concept whose atoms no generated query enumerates (issue #1672).
    cptJSONconceptTable :: Maybe TableCols,
    -- | Every table that holds a row for an atom of this concept, with the
    --   columns of that row that name the atom (issue #1716). In a typology
    --   with a MULTITABLE concept an atom has a row in the table of its own
    --   concept and in the table of each generalisation that is stored apart;
    --   a runtime that adds or deletes an atom must touch every one of these.
    --   For an unmarked typology this is the one table of `conceptTable`.
    cptJSONconceptTables :: [TableCols],
    -- | For a concept without a table of its own that is the union of its
    --   members (`CLASSIFY C IS A \/ B` with `REPRESENT C TYPE MULTITABLE`,
    --   issue #1716): the query that lists its atoms, one column `atomId`,
    --   as the union of the tables of its members. Nothing otherwise.
    cptJSONallAtomsQuery :: Maybe Text,
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

-- TableCols is reached as `Maybe TableCols`, so it needs a ToJSON of its own
-- rather than one borrowed from a JSON instance.
instance ToJSON TableCols where
  toJSON = genericToJSON ampersandDefault

instance JSON FSpec Concepts where
  fromAmpersand env fSpec _ = (Concepts . map (fromAmpersand env fSpec) . filter isUsed . toList . concs) fSpec
    where
      isUsed :: A_Concept -> Bool
      isUsed ONE = True -- ONE is always there, even if not explicitly mentioned in the FSpec.
      isUsed cpt = cpt `Set.member` (concs (instanceList fSpec :: [Relation]) `Set.union` concs (instanceList fSpec :: [AClassify]))

instance JSON A_Concept Concept where
  fromAmpersand _env fSpec cpt =
    Concept
      { cptJSONname = fullName cpt,
        cptJSONlabel = case conceptLabel fSpec cpt of
          Nothing -> localNameOf cpt
          Just (Name.Label t) -> t,
        cptJSONtype = tshow . cptTType fSpec $ cpt,
        cptJSONgeneralizations = map (text1ToText . idWithoutType') . largerConcepts $ cpt,
        cptJSONspecializations = map (text1ToText . idWithoutType') . smallerConcepts $ cpt,
        cptJSONdirectGens = map (text1ToText . idWithoutType') $ L.nub [g | (s, g) <- fsisa fSpec, s == cpt],
        cptJSONdirectSpecs = map (text1ToText . idWithoutType') $ L.nub [s | (s, g) <- fsisa fSpec, g == cpt],
        cptJSONaffectedConjuncts = maybe [] (map (text1ToText . rc_id)) . lookup cpt . allConjsPerConcept $ fSpec,
        cptJSONinterfaces = fmap fullName . filter hasAsSourceCpt . interfaceS $ fSpec,
        cptJSONdefaultViewName = fmap fullName . getDefaultViewForConcept fSpec $ cpt,
        cptJSONconceptTable = case conceptTablesOf fSpec cpt of
          [] -> Nothing -- this concept has no concept table; see issue #1672
          own : _ -> Just own,
        cptJSONconceptTables = conceptTablesOf fSpec cpt,
        cptJSONallAtomsQuery = case unionMembersIn (conceptUnions fSpec) cpt of
          Nothing -> Nothing
          Just _ -> Just (allAtomsQueryOf fSpec cpt),
        cptJSONlargestConcept = text1ToText . idWithoutType' . largestConcept fSpec $ cpt
      }
    where
      hasAsSourceCpt :: Interface -> Bool
      hasAsSourceCpt ifc = (source . objExpression . ifcObj) ifc `elem` cpts
      cpts = cpt : largerConcepts cpt

-- | The query that lists the atoms of a concept without a table of its own
--   (issue #1716), in one column `atomId`, as the runtime's `getAllAtoms`
--   expects it.
allAtomsQueryOf :: FSpec -> A_Concept -> Text
allAtomsQueryOf fSpec cpt =
  "SELECT DISTINCT \"src\" AS \"atomId\" FROM (" <> sqlQuery fSpec (EDcI cpt) <> ") AS \"members\""

-- | The tables that hold a row for an atom of the concept, the concept's own
--   table first, each with the columns of that row that name the atom: the
--   column of the concept itself and of every generalisation that shares the
--   table. A generalisation without a table of its own contributes no column
--   (issue #1672); a generalisation stored apart (issue #1716) contributes a
--   table of its own.
conceptTablesOf :: FSpec -> A_Concept -> [TableCols]
conceptTablesOf fSpec cpt =
  [ TableCols
      { tclJSONname = tshow (sqlname t),
        tclJSONcols = [text1ToText . sqlColumNameToText1 . attSQLColName $ att | (t', att) <- cols, t' == t]
      }
    | t <- ownFirst (L.nub (map fst cols))
  ]
  where
    cols = concatMap (lookupCpt fSpec) $ cpt : largerConcepts cpt
    ownFirst ts = case lookupCpt fSpec cpt of
      (own, _) : _ -> own : filter (/= own) ts
      [] -> ts

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
