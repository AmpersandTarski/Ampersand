
module Classes.Object where
   import Adl.Concept
   import Adl.ObjectDef
   import Adl.Expression
   import Adl.Population
   import Adl.Context

   class Object a where
    concept :: a -> Concept                 -- the type of the object
    attributes :: a -> [ObjectDef]          -- the objects defined within the object
    ctx :: a -> Expression                  -- the context expression
    populations :: a -> [Population]        -- the populations in the object (for now: use for contexts only)
    extends :: a -> [String]                -- the objects of which this is is extension (for now: use for contexts only)
    extends _ = []                          -- empty unless specified otherwise.

   instance Object Context where
    concept _    = cptAnything
    attributes ctx = ctxos ctx
    ctx        _ = error ("Cannot evaluate the context expression of the current context (yet)")
    populations  ctx = ctxpops ctx
    extends ctx = ctxon ctx

   instance Object ObjectDef where
    concept obj = target (objctx obj)
    attributes obj = objats obj
    ctx obj = objctx obj
    populations  _ = []

