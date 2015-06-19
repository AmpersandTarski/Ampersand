{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.ADL1.Pair
                    ( Paire(..),Pairs
                    , mkPair
                    )
where
import Data.Typeable
import Database.Design.Ampersand.Basics

type Pairs = [Paire]
--srcPaire :: Paire -> String
--trgPaire :: Paire -> String
--type Paire = (String,String)
--mkPair :: String -> String -> Paire
--mkPair a b = (a,b)
--srcPaire = fst
--trgPaire = snd
data Paire = Paire {srcPaire ::String  --TODO introduction of AtomValue, and replace these messy Strings
                   ,trgPaire ::String
                   } deriving (Show ,Eq,Ord, Typeable)
mkPair :: String -> String -> Paire
mkPair = Paire
instance Unique Paire where
  showUnique p = "("++srcPaire p++","++trgPaire p++")"

instance Flippable Paire where
  flp (Paire a b) = (Paire b a)