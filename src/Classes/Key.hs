{-# OPTIONS_GHC -Wall #-}
module Classes.Key            (Key(keys))
-- WAAROM bestaat deze module? het idee is om keys uit elk datatype te halen door middel van de functie keydefs uit de class ViewPoint.
-- De class 'Key' lijkt dubbelop. 'Key' en 'keys' worden nergens gebruikt. Opruimen?
where
   import Adl.Concept         (Concept)
   import Adl.ObjectDef       (ObjectDef)
   import Adl.KeyDef          (KeyDef(..))
   import Adl.Pattern         (Pattern)
   import Adl.Context         (Context(..))
   import Classes.ViewPoint   (keyDefs)
   
   class Key a where
    keys :: a->[(Concept,String,[ObjectDef])]
   instance Key KeyDef where
    keys kd = [theKey]
      where theKey = (kdcpt kd, kdlbl kd, kdats kd)
 
   instance Key Context where
    keys context
     = concat (map keys (keyDefs context))

   instance Key Pattern where
    keys pat = concat (map keys (keyDefs pat))
 
    