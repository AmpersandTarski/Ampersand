-- | Schema-agnostic data model for ISO 10303-21 (STEP / Part-21 / SPF) instances.
--
--   This module knows nothing about EXPRESS or IFC; it only describes the shape of
--   the data that 'Ampersand.Input.Step.Parser' produces from a Part-21 @DATA@ section.
--   The types here form the interface contract (handoff §3b) consumed by the IFC binder (WP3).
module Ampersand.Input.Step.Types
  ( StepInstance (..),
    StepValue (..),
  )
where

import Ampersand.Basics

-- | One Part-21 instance statement: @#id = TYPE(args)@.
data StepInstance = StepInstance
  { -- | Instance reference, including the leading @#@, e.g. @"#42"@.
    siId :: !Text,
    -- | Entity type name in UPPERCASE, exactly as it appears in the file, e.g. @"IFCWALL"@.
    siType :: !Text,
    -- | Positional arguments, in file order.
    siArgs :: ![StepValue]
  }
  deriving (Show, Eq)

-- | A single Part-21 argument value.
data StepValue
  = -- | An instance reference, e.g. @"#42"@ (leading @#@ kept).
    SVRef !Text
  | -- | A string literal, with surrounding quotes removed and @''@ unescaped to @'@.
    SVStr !Text
  | -- | An enumeration value, e.g. @.T.@ becomes @"T"@ (dots stripped).
    SVEnum !Text
  | -- | An integer literal.
    SVInt !Integer
  | -- | A real (floating point) literal.
    SVReal !Double
  | -- | A STEP binary literal, e.g. @"0FF..."@ (surrounding double quotes removed).
    SVBin !Text
  | -- | A parenthesised list of values.
    SVList ![StepValue]
  | -- | A typed value, e.g. @IFCBOOLEAN(.T.)@ becomes @SVTyped "IFCBOOLEAN" [SVEnum "T"]@.
    SVTyped !Text ![StepValue]
  | -- | An omitted (@$@) or derived/redeclared (@*@) value.
    SVNull
  deriving (Show, Eq)
