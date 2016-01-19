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
import Data.Data
import Codec.Xlsx.Types
import qualified Data.Text as T

-- | The line number
type Line = Int
-- | The column number
type Column = Int
-- | The name of a symbol
type SymbolName = String

-- | Makes the initial position for a file
initPos :: FilePath -- ^ The file path
        -> FilePos  -- ^ The initial position
initPos fn = FilePos fn 1 1

-- | Increases the position of a lexer monad with the given character
updatePos :: FilePos -- ^ The file position
          -> Char    -- ^ The character to add
          -> FilePos -- ^ The new position
updatePos pos '\n' = addLn 1 pos
updatePos pos '\t' = addTab pos
updatePos pos   _  = addPos 1 pos

-- | Adds a tab character to the given position
addTab :: FilePos -- ^ The old position
       -> FilePos -- ^ The new position
addTab pos@(FilePos _ _ col) = addPos tabWidth pos
    where tabWidth = 8 - ((col-1) `mod` 8)

-- | Adds one column to the file position
addPos :: Int -> FilePos -> FilePos 
addPos n (FilePos fn line col) = FilePos fn line (col + n)

-- | Adds one line to the file position, resetting the column number
addLn :: Int -> FilePos -> FilePos 
addLn n (FilePos fn line _) = FilePos fn (line+n) 1

-- | Represents a position within a file, including the file path, line and column numbers
data FilePos = FilePos FilePath Line Column deriving (Eq, Ord, Generic,Typeable, Data)

instance Hashable FilePos where
  hashWithSalt s (FilePos fn l c) = s `hashWithSalt` fn `hashWithSalt` l `hashWithSalt` c

data Origin = OriginUnknown
            | Origin String 
            | FileLoc FilePos SymbolName 
            | XLSXLoc FilePath String (Int,Int) 
            | DBLoc String
    deriving (Eq, Ord, Typeable, Generic, Data)

instance Unique Origin where
  showUnique = show
instance Hashable Origin

instance Show FilePos where
  show (FilePos fn l c) = "line " ++ show l ++ ":" ++ show c ++ ", file " ++ fn

instance Show Origin where
  show (FileLoc pos _) = show pos
  show (XLSXLoc filePath sheet (row,col)) 
                       = show filePath++":"++
                         "\n   Sheet: "++sheet++", "++T.unpack (int2col col)++show row
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

