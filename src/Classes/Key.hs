{-# OPTIONS_GHC -Wall #-}
module Classes.Key            (Key(keys)) 
where
   import Adl.Concept         (Concept,Association(..))
   import Adl.ObjectDef       (ObjectDef)
   import Adl.KeyDef          (KeyDef(..))
   import Adl.Pattern         (Pattern)
   import Adl.Context         (Context(..))
   import Classes.Morphical   (keyDefs)
   
   class Key a where
    keys :: a->[(Concept,String,[ObjectDef])]
   instance Key KeyDef where
    keys kd = [theKey]
      where theKey = (target (kdctx kd),(kdlbl kd),(kdats kd))
 
   instance Key Context where
    keys context
     = ( concat [keys p| p<-ctxpats context] ++
         concat (map keys (keyDefs context))
       )
   instance Key Pattern where
    keys pat = concat (map keys (keyDefs pat))
 
    