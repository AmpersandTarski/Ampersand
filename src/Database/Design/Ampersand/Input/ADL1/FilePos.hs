{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.Design.Ampersand.Input.ADL1.FilePos
         (FilePos(..), Origin(..), Pos(Pos) , Traced(..)) where

import Database.Design.Ampersand.Input.ADL1.UU_Scanner (Pos(Pos))
import Database.Design.Ampersand.Basics
import Data.Typeable
import GHC.Generics (Generic)
import Data.Hashable

--fatal :: Int -> String -> a
--fatal = fatalMsg "Input.ADL1.FilePos"

--Traced a have an origin, which may be unknown.
data FilePos = FilePos (String, Pos, String) deriving (Eq, Ord, Generic)
instance Hashable FilePos where
  hashWithSalt s (FilePos (fn,Pos l c, sym))
    = s  `hashWithSalt`
      fn `hashWithSalt`
      l  `hashWithSalt`
      c  `hashWithSalt`
      sym
data Origin = OriginUnknown | Origin String | FileLoc FilePos | DBLoc String deriving (Eq, Ord, Typeable, Generic)
instance Unique Origin where
  showUnique = show
instance Hashable Origin

instance Show FilePos where
  show (FilePos (fn,Pos l c,_))
    = "line " ++ show l++":"++show c
         ++ ", file " ++ show fn

instance Show Origin where
  show (FileLoc pos) = show pos
  show (DBLoc str)   = "Database location: "++str
  show (Origin str)  = str
  show OriginUnknown = "Unknown origin"
class Traced a where
  origin :: a -> Origin
  filenm :: a -> String
  linenr :: a -> Int
  colnr :: a -> Int
  filenm x = case origin x of
               FileLoc (FilePos (nm, _, _)) -> nm
               _ -> ""
  linenr x = case origin x of
               FileLoc (FilePos (_,Pos l _,_)) -> l
               _ -> 0
  colnr x  = case origin x of
               FileLoc (FilePos (_,Pos _ c,_)) -> c
               _ -> 0

instance Traced Origin where
  origin = id

