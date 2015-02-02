{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Basics.Unique 
  (Unique(..),Named(..))
where
import Data.Typeable

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
  showSelf :: Bool ->        -- ^ Should the type show too? 
              UniqueObj e -> -- ^ the thing to show
              String
  showSelf includeType x = typePrefix ++ (showUnique . theThing) x
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
