{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics (module X, Identified(..)) where
import DatabaseDesign.Ampersand.Basics.Auxiliaries as X
import DatabaseDesign.Ampersand.Basics.Collection as X
import DatabaseDesign.Ampersand.Basics.String as X
import DatabaseDesign.Ampersand.Basics.UTF8 as X
import DatabaseDesign.Ampersand.Basics.Version as X


class Identified a where
  name :: a->String

