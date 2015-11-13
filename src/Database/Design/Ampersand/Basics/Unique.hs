{-# LANGUAGE DeriveDataTypeable #-}
{- The purpose of class Unique is to identify a Haskell object by means of a string.
E.g.
instance Unique Pattern where
 showUnique = name
-}

module Database.Design.Ampersand.Basics.Unique 
  (Unique(..),Named(..))
where
import Data.Typeable
import Data.List
import Data.Char
import Database.Design.Ampersand.Basics.Version

-- | anything could have some label, can't it?
class Named a where
  name :: a->String

-- | In the context of the haskell code, things can be Unique. 
class (Typeable e, Eq e) => Unique e where 
  -- | a representation of a unique thing
  self :: e -> UniqueObj e
  self a = UniqueObj { theThing = a
                     , theShow  = showUnique
                     }
  -- | representation of a Unique thing into a string.  
  uniqueShow :: Bool ->  --  Should the type show too? 
              e    ->  --  the thing to show
              String
  uniqueShow includeType x = typePrefix ++ (showUnique . theThing . self) x
    where
      typePrefix = if includeType then show $ typeOf x else ""
  -- | A function to show a unique instance. It is the responsability
  --   of the instance definition to make sure that for every a, b of 
  --   an individual type:
  --        a == b  <==> showUnique a == showUnique b
  showUnique :: e -> String
  {-# MINIMAL showUnique #-}
  

-- | this is the implementation of the abstract data type. It mustn't be exported
data UniqueObj a = 
       UniqueObj { theThing :: a
                 , theShow  :: (a -> String)
                 } deriving (Typeable)

instance Unique a => Unique [a] where
   showUnique [] = fatal 74 $ "empty list is not unique"
   showUnique xs = "["++intercalate ", " (map showUnique xs)++"]"

instance Unique Bool where
 showUnique = map toLower . show 
