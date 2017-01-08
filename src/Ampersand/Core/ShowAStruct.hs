module Ampersand.Core.ShowAStruct
  (AStruct(..))
where

--import Ampersand.Core.ParseTree

class AStruct a where
 showA :: a -> String
