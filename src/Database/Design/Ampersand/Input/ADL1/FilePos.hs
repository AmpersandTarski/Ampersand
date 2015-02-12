{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Input.ADL1.FilePos
         (FilePos(..), Origin(..), SourcePos, Traced(..)) where

import Text.Parsec.Pos
import Database.Design.Ampersand.Basics
import Data.Typeable

--fatal :: Int -> String -> a
--fatal = fatalMsg "Input.ADL1.FilePos"

--Traced a have an origin, which may be unknown.
data FilePos = FilePos (String, SourcePos, String) deriving (Eq, Ord)
data Origin = OriginUnknown | Origin String | FileLoc FilePos | DBLoc String deriving (Eq, Ord, Typeable)
instance Unique Origin where
  showUnique = show

instance Show FilePos where
  show (FilePos (fn, src, _))
    = "line " ++ show (sourceLine src)++":"++show (sourceColumn src)
         ++ ", file " ++ fn

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
               FileLoc (FilePos (_,src,_)) -> sourceLine src
               _ -> 0
  colnr x  = case origin x of
               FileLoc (FilePos (_,src,_)) -> sourceColumn src
               _ -> 0

instance Traced Origin where
  origin = id

