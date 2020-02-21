{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{- The purpose of class Unique is to identify a Haskell object by means of a string.
E.g.
instance Unique Pattern where
 showUnique = name
-}

module Ampersand.Basics.Unique 
  (Unique(..),Named(..))
where
import           Ampersand.Basics.Prelude
import           Ampersand.Basics.String(escapeIdentifier)
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Data.Hashable
import           Data.Typeable

-- | anything could have some label, can't it?
class Named a where
  name :: a->Text

-- | In the context of the haskell code, things can be Unique. 
class (Typeable e, Eq e) => Unique e where 
  -- | a representation of a unique thing
  self :: e -> UniqueObj e
  self a = UniqueObj { theThing = a
                 --    , theShow  = showUnique
                     }
  -- | representation of a Unique thing into a Text.  
  uniqueShowWithType :: e -> Text
  uniqueShowWithType x = tshow (typeOf x) <>"_" <> showUnique x

  -- | A function to show a unique instance. It is the responsability
  --   of the instance definition to make sure that for every a, b of 
  --   an individual type:
  --        a == b  <==> showUnique a == showUnique b
  showUnique :: e -> Text
  {-# MINIMAL showUnique #-}

  idWithoutType :: e -> Text
  idWithoutType = uniqueButNotTooLong -- because it could be stored in an SQL database
                . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
                . showUnique
  
  idWithType :: e -> Text
  idWithType e = uniqueButNotTooLong -- because it could be stored in an SQL database
               . addType e
               . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
               $ showUnique e
  
  addType :: e -> Text -> Text
  addType x string = tshow (typeOf x) <> "_" <> string

uniqueButNotTooLong :: Text -> Text
uniqueButNotTooLong txt =
  let (prfx,rest) = T.splitAt safeLength txt
  in if T.null rest
        then txt
        else prfx<>"#"<>tshow (hash txt)<>"#"
  where safeLength = 50 -- HJO, 20170812: Subjective value. This is based on the 
                          -- limitation that DirtyId's are stored in an sql database
                          -- in a field that is normally 255 long. We store the
                          -- prefix of the string but make sure we still have space
                          -- left over for the hash. While theoretically this is a 
                          -- crappy solution, in practice this will prove to be well 
                          -- enough.
  

-- | this is the implementation of the abstract data type. It mustn't be exported
data UniqueObj a = 
       UniqueObj { theThing :: a
                 } deriving (Typeable)

instance Unique a => Unique [a] where
   showUnique [] = "[]"
   showUnique xs = "["<>T.intercalate ", " (map showUnique xs)<>"]"
instance Unique a => Unique (Set.Set a) where
   showUnique = showUnique . Set.elems

instance Unique Bool where
 showUnique = T.toLower . tshow 
