{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Object
        (Object(..)      )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Basics (fatalMsg)

fatal :: Int -> String -> a
fatal = fatalMsg "Classes.Object"

class Object a where
 concept :: a -> A_Concept                 -- the type of the object
 attributes :: a -> [ObjectDef]          -- the objects defined within the object    
 contextOf :: a -> Expression -- the context expression
 foldedattributes ::  a -> [Expression] --the attributes of obj as a list of expressions with source = concept obj
 foldedattributes obj 
      = contextOf obj:[f (contextOf obj) x |xs<-map foldedattributes (attributes obj),x<-xs]
      where f::Expression -> Expression -> Expression
            f x (ECps xs) = ECps (x:xs)
            f x x' = ECps [x,x'] 
instance Object A_Context where
 concept _      = fatal 29 "this used to be 'Anything' but that has become history in ticket #104."
 attributes c   = [ifcObj s | s<-ctxifcs c]
 contextOf  _   = fatal 38 "Cannot evaluate the context expression of the current context (yet)"
 
instance Object ObjectDef where
 concept obj = target (objctx obj)
 attributes  = objAts
 contextOf   = objctx
