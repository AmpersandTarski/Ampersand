{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.FilePos
       ( FilePos(..), Origin(..), Pos(Pos) , Traced(..),posIn)
where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner (Pos(Pos))
--   import DatabaseDesign.Ampersand.Basics      (fatalMsg)

--   fatal :: Int -> String -> a
--   fatal = fatalMsg "Input.ADL1.FilePos"

   --Traced a have an origin, which may be unknown.
   data FilePos = FilePos ( String, Pos, String) deriving (Eq, Ord)
   data Origin = OriginUnknown | Origin String | FileLoc FilePos | DBLoc String deriving (Eq, Ord)
--line column pos

   posIn :: Traced a => Origin -> a -> Origin -> Bool
   posIn (FileLoc (FilePos (f , Pos bl bc, _)))
         x
         (FileLoc (FilePos (f', Pos el ec, _)))
         | f/=f' = False
         | bl==el = bc < colnr x && colnr x < ec
         | otherwise = bl < linenr x && linenr x < el
   posIn _ _ _ = False
 
   instance Show FilePos where
     show (FilePos (fn,Pos l c,_))
       = "line " ++ show l++":"++show c
         --   ++ ", file " ++ show fn

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
    colnr x = case origin x of 
                     FileLoc (FilePos (_,Pos _ c,_)) -> c
                     _ -> 0

   instance Traced Origin where
    origin = id


