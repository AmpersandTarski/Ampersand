
module Classes.Key where
   import Adl.Concept
   import Adl.ObjectDef
   import Adl.KeyDef
   import Adl.Pattern
   import Adl.Context
   import Classes.Morphical
   
   class Key a where
    keys :: a->[(Concept,String,[ObjectDef])]
   instance Key KeyDef where
    keys (Kd _ lbl ctx ats) = [(target ctx,lbl,ats)]

   instance Key Context where
    keys context
     = ( concat [keys p| p<-ctxpats context] ++
         [(target (kdctx kd),(kdlbl kd),(kdats kd))|kd<-keyDefs context]
       )
   instance Key Pattern where
    keys pat = [(target (kdctx kd),(kdlbl kd),(kdats kd))|kd<-keyDefs pat]
 
    