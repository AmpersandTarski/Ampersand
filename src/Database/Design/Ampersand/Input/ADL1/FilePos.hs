{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.Design.Ampersand.Input.ADL1.FilePos (
    FilePos(..), Origin(..), Traced(..),
    addPos, initPos, updatePos
) where

import Database.Design.Ampersand.Basics
import Data.Typeable
import GHC.Generics (Generic)
import Data.Hashable

--fatal :: Int -> String -> a
--fatal = fatalMsg "Input.ADL1.FilePos"

type Line = Int
type Column = Int
type SymbolName = String

initPos :: FilePath -> FilePos
initPos fn = FilePos fn 1 1

updatePos :: FilePos -> Char -> FilePos
updatePos pos '\n' = addLn 1 pos
updatePos pos '\t' = addTab pos
updatePos pos   _  = addPos 1 pos

addTab :: FilePos -> FilePos
addTab pos@(FilePos _ _ col) = addPos tabWidth pos
    where tabWidth = 8 - ((col-1) `mod` 8)

addPos :: Int -> FilePos -> FilePos 
addPos n (FilePos fn line col) = FilePos fn line (col + n)

addLn :: Int -> FilePos -> FilePos 
addLn n (FilePos fn line col) = FilePos fn (line+n) col

--Traced a have an origin, which may be unknown.
data FilePos = FilePos FilePath Line Column deriving (Eq, Ord, Generic)

instance Hashable FilePos where
  hashWithSalt s (FilePos fn l c) = s `hashWithSalt` fn `hashWithSalt` l `hashWithSalt` c

data Origin = OriginUnknown | Origin String | FileLoc FilePos SymbolName | DBLoc String
    deriving (Eq, Ord, Typeable, Generic)

instance Unique Origin where
  showUnique = show
instance Hashable Origin

instance Show FilePos where
  show (FilePos fn l c) = "line " ++ show l ++ ":" ++ show c ++ ", file " ++ fn

instance Show Origin where
  show (FileLoc pos _) = show pos
  show (DBLoc str)     = "Database location: "++str
  show (Origin str)    = str
  show OriginUnknown   = "Unknown origin"

class Traced a where
  origin :: a -> Origin
  filenm :: a -> String
  linenr :: a -> Int
  colnr :: a -> Int
  filenm x = case origin x of
               FileLoc (FilePos nm _ _) _ -> nm
               _ -> ""
  linenr x = case origin x of
               FileLoc (FilePos _ l _) _ -> l
               _ -> 0
  colnr x  = case origin x of
               FileLoc (FilePos _ _ c) _ -> c
               _ -> 0

instance Traced Origin where
  origin = id

