{-# OPTIONS_GHC -Wall #-}
module Adl.Expression (Expression)
where
data Expression rel
instance Eq (Expression r)