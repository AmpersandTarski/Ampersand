{-# OPTIONS_GHC -Wall #-}
module Classes.Object        (Object( concept
                                    , attributes
                                    , ctx
                                    , populations
                                    , extends
                                    )
                             ) 
where
   import Adl.Concept        (Concept,cptAnything,Association(..))
   import Adl.ObjectDef      (ObjectDef(..))
   import Adl.Expression     (Expression)
   import Adl.Population     (Population)
   import Adl.Context        (Context(..))

   class Object a where
    concept :: a -> Concept                 -- the type of the object
    attributes :: a -> [ObjectDef]          -- the objects defined within the object
    ctx :: a -> Expression                  -- the context expression
    populations :: a -> [Population]        -- the populations in the object (for now: use for contexts only)
    extends :: a -> [String]                -- the objects of which this is is extension (for now: use for contexts only)
    extends _ = []                          -- empty unless specified otherwise.

   instance Object Context where
    concept _      = cptAnything
    attributes c   = ctxos c
    ctx        _   = error ("!Fatal (module Classes.Object 27): Cannot evaluate the context expression of the current context (yet)")
    populations  c = ctxpops c
    extends c      = ctxon c

   instance Object ObjectDef where
    concept obj = target (objctx obj)
    attributes obj = objats obj
    ctx obj = objctx obj
    populations  _ = []
