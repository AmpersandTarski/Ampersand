{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics 
  ( module DatabaseDesign.Ampersand.Basics.Auxiliaries
  , module DatabaseDesign.Ampersand.Basics.Collection
  , module DatabaseDesign.Ampersand.Basics.String
  , module DatabaseDesign.Ampersand.Basics.UTF8
  , module DatabaseDesign.Ampersand.Basics.Version
  , Identified(..)
  ) where
import DatabaseDesign.Ampersand.Basics.Auxiliaries
import DatabaseDesign.Ampersand.Basics.Collection
import DatabaseDesign.Ampersand.Basics.String
import DatabaseDesign.Ampersand.Basics.UTF8
import DatabaseDesign.Ampersand.Basics.Version


class Identified a where
  name :: a->String

