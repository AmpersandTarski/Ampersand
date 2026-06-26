{-# LANGUAGE DeriveGeneric #-}

-- | Generic abstract syntax for an EXPRESS schema (ISO 10303-11).
--
-- This module is deliberately free of any IFC- or Ampersand-specific
-- assumptions. It models /any/ EXPRESS schema. The mapping to Ampersand's
-- @P_Context@ happens later (WP3), in @Ampersand.Input.IFC@.
--
-- See the handoff §3a for the interface contract that downstream work
-- packages bind against.
module Ampersand.Input.Express.Schema
  ( ExpressSchema (..),
    Entity (..),
    Attr (..),
    DefinedType (..),
    Aggregate (..),
    AggregateKind (..),
    primConcept,
    targetConcept,
    fullAttrs,
  )
where

import Ampersand.Basics
import qualified RIO.Map as Map
import RIO.Text (Text)

-- | A whole EXPRESS schema.
data ExpressSchema = ExpressSchema
  { -- | Schema name, e.g. @"IFC4X3_ADD2"@.
    esName :: Text,
    -- | Entities, keyed by their (PascalCase) name.
    esEntities :: Map Text Entity,
    -- | Defined TYPEs, keyed by their (PascalCase) name.
    esTypes :: Map Text DefinedType
  }
  deriving (Show, Eq, Generic)

-- | A single @ENTITY@ declaration.
data Entity = Entity
  { enName :: Text,
    -- | Single supertype from @SUBTYPE OF (X)@ (EXPRESS allows multiple
    -- inheritance, but IFC-style schemas use single inheritance; we keep
    -- only the first listed supertype).
    enSupertype :: Maybe Text,
    -- | Only the /explicit/ attributes, in declaration order.
    enAttrs :: [Attr],
    -- | Raw @WHERE@ rule lines (deferred analysis; may be empty).
    enWhere :: [Text],
    -- | Raw @INVERSE@ lines (deferred analysis; may be empty).
    enInverse :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | A single explicit attribute of an entity.
data Attr = Attr
  { atName :: Text,
    -- | Bare element type, after stripping @OPTIONAL@ and any aggregate
    -- wrapper(s). Primitives are mapped to safe concept names (see
    -- 'primConcept').
    atTarget :: Text,
    atOptional :: Bool,
    -- | Aggregate wrapper, if the attribute is a @LIST@/@SET@/@ARRAY@/@BAG@.
    -- For nested aggregates (e.g. @LIST OF LIST OF X@) only the outermost
    -- wrapper is recorded.
    atAggregate :: Maybe Aggregate
  }
  deriving (Show, Eq, Generic)

-- | A defined TYPE.
data DefinedType
  = -- | @SELECT@ members.
    TSelect [Text]
  | -- | @ENUMERATION@ values.
    TEnum [Text]
  | -- | Alias for an underlying type (possibly a primitive concept name).
    TAlias Text
  deriving (Show, Eq, Generic)

-- | An aggregate wrapper with its kind and (optional) bounds.
data Aggregate = Aggregate
  { agKind :: AggregateKind,
    -- | Lower bound, if stated and numeric.
    agLo :: Maybe Integer,
    -- | Upper bound, if stated and numeric (@?@ means unbounded -> 'Nothing').
    agHi :: Maybe Integer
  }
  deriving (Show, Eq, Generic)

data AggregateKind = AggList | AggSet | AggArray | AggBag
  deriving (Show, Eq, Generic)

-- | EXPRESS primitive type names mapped to safe Ampersand concept names.
-- Identical to @ifc_express.py::PRIM_CONCEPT@.
primConcept :: Map Text Text
primConcept =
  Map.fromList
    [ ("REAL", "IfcRawReal"),
      ("INTEGER", "IfcRawInteger"),
      ("NUMBER", "IfcRawNumber"),
      ("STRING", "IfcRawString"),
      ("BOOLEAN", "IfcRawBoolean"),
      ("LOGICAL", "IfcRawLogical"),
      ("BINARY", "IfcRawBinary")
    ]

-- | Concept name for an attribute element type. Primitives get their safe
-- concept name; everything else passes through unchanged.
targetConcept :: Text -> Text
targetConcept t = Map.findWithDefault t t primConcept

-- | The full, ordered attribute list for an entity, supertype attributes
-- first (matching @ifc_express.py::full_attrs@). Returns @[]@ for an
-- unknown entity name.
fullAttrs :: ExpressSchema -> Text -> [Attr]
fullAttrs schema = go []
  where
    go seen name
      | name `elem` seen = [] -- guard against cyclic supertypes
      | otherwise =
          case Map.lookup name (esEntities schema) of
            Nothing -> []
            Just ent ->
              maybe [] (go (name : seen)) (enSupertype ent)
                <> enAttrs ent
