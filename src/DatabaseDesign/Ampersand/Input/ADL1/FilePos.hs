{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.FilePos
       ( FilePos(..), Origin(..), Pos(Pos) , Traced(..))
where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner (Pos(Pos))
--   import DatabaseDesign.Ampersand.Basics      (fatalMsg)

--   fatal :: Int -> String -> a
--   fatal = fatalMsg "Input.ADL1.FilePos"

   --Traced a have an origin, which may be unknown.
   data FilePos = FilePos ( String, Pos, String) deriving Eq
   data Origin = OriginUnknown | Origin String | FileLoc FilePos | DBLoc String deriving Eq 

   instance Show FilePos where
     show (FilePos (fn,Pos l _,_))
       = "line " ++ show l
         ++ ", file " ++ show fn

   instance Show Origin where
     show (FileLoc pos) = show pos
     show (DBLoc str)   = "Database location: "++str
     show (Origin str)  = str
     show OriginUnknown = "Unknown origin"

   class Traced a where
    origin :: a -> Origin
    lineNumber :: a -> Int
    lineNumber x = case origin x of 
                     FileLoc (FilePos (_,Pos l _,_)) -> l
                     _ -> 0

   instance Traced Origin where
    origin = id
    lineNumber x = case x of 
                     FileLoc (FilePos (_,Pos l _,_)) -> l
                     _ -> 0


