{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Input.ADL1.FilePos (
    FilePos(..), Origin(..), Traced(..),
    isFuzzyOrigin, maybeOrdering, sortWithOrigins,
    addPos, initPos, updatePos
) where

import           Ampersand.Basics
import           Codec.Xlsx.Types
import           Data.Hashable
import           GHC.Generics (Generic)
import qualified RIO.List as L
import qualified RIO.Text as T
-- | The line number
type Line = Int
-- | The column number
type Column = Int
-- | The name of a symbol
type SymbolName = Text

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
            | Origin Text 
            | PropertyRule Text Origin -- Constructor is used to hold the origin of a propertyrule.
            | FileLoc FilePos SymbolName 
            | XLSXLoc FilePath Text (Int,Int) 
            | MeatGrinder -- Constructor is used to specify stuff that originates from meatgrinder
    deriving (Typeable, Generic, Data)
-- Eq and Ord have been removed by desing on Origin. See issue #1035
-- | A fuzzy origin has a constructor that breaks tracability. They should be used as little as possible.
isFuzzyOrigin :: Origin -> Bool
isFuzzyOrigin OriginUnknown = True
isFuzzyOrigin Origin{}      = True
isFuzzyOrigin MeatGrinder   = True
isFuzzyOrigin _             = False
sortWithOrigins :: Traced a => [a] -> [a]
sortWithOrigins xs = sortedNonFuzzy <> fuzzy
  where (fuzzy, nonfuzzy) = L.partition (isFuzzyOrigin . origin) xs
        sortedNonFuzzy = L.sortBy nonFuzzyOrdering nonfuzzy
        nonFuzzyOrdering :: Traced a => a -> a -> Ordering
        nonFuzzyOrdering x y = case maybeOrdering (origin x) (origin y) of
                                 Just ordering -> ordering
                                 Nothing -> fatal "nonFuzzyOrdering must only be used on list containing non-fuzzy origins"

-- | Not all Origins have an ordering. This function serves as a replacement
--   for ordering of Origins in cases where that can be done. 
maybeOrdering :: Origin -> Origin -> Maybe Ordering
maybeOrdering x y = case x of
-- FileLoc{} > XLSXLoc{} > PropertyRule{}
  FileLoc fpx _ 
      -> case y of
           FileLoc fpy _ -> Just $ compare fpx fpy
           XLSXLoc{}       -> Just GT
           PropertyRule{}  -> Just GT
           _ -> if isFuzzyOrigin y then Nothing 
                else fatal $ "All cases for non-fuzzy orderings must be implemented.\n"<>tshow y
  XLSXLoc fpx wbx (rowx,colx) 
      -> case y of
           FileLoc{}       -> Just LT
           XLSXLoc fpy wby (rowy,coly) 
             -> Just $ compare (fpx, wbx, (rowx,colx))
                               (fpy, wby, (rowy,coly))
           PropertyRule{}  -> Just GT
           _ -> if isFuzzyOrigin y then Nothing 
                else fatal $ "All cases for non-fuzzy orderings must be implemented.\n"<>tshow y
  PropertyRule _ ox 
      -> case y of
           FileLoc{}       -> Just LT
           XLSXLoc{}       -> Just LT
           PropertyRule _ oy -> maybeOrdering ox oy
           _ -> if isFuzzyOrigin y then Nothing 
                else fatal $ "All cases for non-fuzzy orderings must be implemented.\n"<>tshow y
  _ -> if isFuzzyOrigin x then Nothing 
       else fatal $ "All cases for non-fuzzy orderings must be implemented.\n"<>tshow x   
instance Hashable Origin

instance Show FilePos where
  show (FilePos fn l c) = fn <> ":" <> show l <> ":" <> show c

instance Show Origin where
  -- The vscode extension expects errors and warnings
  -- to be in a standardized format. The show function
  -- complies to that. Iff for whatever reason 
  -- this function is changed, please verify 
  -- the proper working of the ampersand-language-extension
  show (FileLoc pos _) = show pos
  show (XLSXLoc filePath sheet (row,col)) 
                       = filePath<>":"<>
                         "\n   Sheet: "<>T.unpack sheet<>", Cell: "<>T.unpack (int2col col)<>show row<>". "
  show (PropertyRule dcl o) = "PropertyRule for "<>T.unpack dcl<>" which is defined at "<>show o
  show (Origin str)    = T.unpack str
  show OriginUnknown   = "Unknown origin"
  show MeatGrinder     = "MeatGrinder"

class Traced a where
  origin :: a -> Origin
  filenm :: a -> String
  linenr :: a -> Int
  colnr  :: a -> Int
  filenm x = case origin x of
               FileLoc (FilePos nm _ _) _ -> nm
               XLSXLoc filePath _ _       -> filePath
               _ -> ""
  linenr x = case origin x of
               FileLoc (FilePos _ l _) _ -> l
               XLSXLoc _        _     (row,_  ) -> row
               _ -> 0
  colnr x  = case origin x of
               FileLoc (FilePos _ _ c) _ -> c
               XLSXLoc _        _     (_  ,col) -> col
               _ -> 0

instance Traced Origin where
  origin = id

