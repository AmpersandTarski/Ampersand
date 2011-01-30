{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Object
        (Object( concept
               , attributes
               , ctx
               , populations
               , extends
               , foldedattributes
        )      )
where
import DatabaseDesign.Ampersand.ADL1.Concept                (Concept,cptAnything)
import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration (Relation(..),Association(..))
import DatabaseDesign.Ampersand.ADL1.ObjectDef              (ObjectDef(..),Service(..))
import DatabaseDesign.Ampersand.ADL1.Expression             (Expression(..))
import DatabaseDesign.Ampersand.ADL1.Population             (Population)
import DatabaseDesign.Ampersand.ADL1.Context                (Context(..))

class Object a where
 concept :: a -> Concept                 -- the type of the object
 attributes :: a -> [ObjectDef]          -- the objects defined within the object    
 ctx :: a -> Expression (Relation Concept) -- the context expression
 populations :: a -> [Population Concept]        -- the populations in the object (for now: use for contexts only)
 extends :: a -> [String]                -- the objects of which this is is extension (for now: use for contexts only)
 extends _ = []                          -- empty unless specified otherwise.
 foldedattributes ::  a -> [Expression (Relation Concept)] --the attributes of obj as a list of expressions with source = concept obj
 foldedattributes obj 
      = (ctx obj):[f (ctx obj) x |xs<-map foldedattributes (attributes obj),x<-xs]
      where f::Expression r -> Expression r -> Expression r
            f x (F xs) = F (x:xs)
            f x x' = F [x,x'] 
instance Object Context where
 concept _      = cptAnything
 attributes c   = [svObj s| s<-ctxsvcs c]
 ctx        _   = error ("!Fatal (module Classes.Object 28): Cannot evaluate the context expression of the current context (yet)")
 populations  c = ctxpops c
 extends c      = ctxon c

instance Object ObjectDef where
 concept obj = target (objctx obj)
 attributes obj = objats obj
 ctx obj = objctx obj
 populations  _ = []



