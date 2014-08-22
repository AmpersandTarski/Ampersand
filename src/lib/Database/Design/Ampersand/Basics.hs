{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand.Basics
  ( module Database.Design.Ampersand.Basics.Auxiliaries
  , module Database.Design.Ampersand.Basics.Collection
  , module Database.Design.Ampersand.Basics.String
  , module Database.Design.Ampersand.Basics.UTF8
  , module Database.Design.Ampersand.Basics.Version
  , Identified(..)
  ) where
import Database.Design.Ampersand.Basics.Auxiliaries
import Database.Design.Ampersand.Basics.Collection
import Database.Design.Ampersand.Basics.String
import Database.Design.Ampersand.Basics.UTF8
import Database.Design.Ampersand.Basics.Version

class Identified a where
  name :: a->String

