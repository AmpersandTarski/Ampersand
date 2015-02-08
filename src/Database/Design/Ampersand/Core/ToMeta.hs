module Database.Design.Ampersand.Core.ToMeta 
  (toMeta)
where
import Database.Design.Ampersand.Core.ParseTree

-- | When dealing with meta-stuff for Ampersand, (Like makeGenerics, makeRAP), 
--   the names of Concepts should be different than 'normal', user-defined Concepts. 
--   This function modifies everything in the context to reflect that.  
toMeta :: (P_Context -> P_Context)
toMeta = makeMeta

class Meta1 a where
  makeMeta :: a -> a

instance Meta1 P_Context where
  makeMeta ctx = ctx