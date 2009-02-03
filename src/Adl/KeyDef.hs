
module Adl.KeyDef where
   import Adl.FilePos
   import Adl.Concept
   import Adl.Expression
   import Adl.ObjectDef
   import CommonClasses(Identified(name,typ))   
   type KeyDefs = [KeyDef]
   data KeyDef = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
                    , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                    , kdctx :: Expression   -- ^ this expression describes the instances of this object, related to their context
                    , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                    } deriving Eq
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************
--   instance Eq KeyDef where       -- WAAROM :TODO Stef, deze Eq mistte zijn where clause. Wil jij dit valideren? 
--    kd == kd' = kdpos kd == kdpos kd' &&
--                kdlbl kd == kdlbl kd' &&
--                kdctx kd == kdctx kd' &&
--                kdats kd == kdats kd'
    
   instance Show KeyDef
   instance Identified KeyDef where
    name kd = kdlbl kd
    typ kd = "KeyDef_"

   class Key a where
    keys :: a->[(Concept,String,[ObjectDef])]
   instance Key KeyDef where
    keys (Kd pos lbl ctx ats) = [(target ctx,lbl,ats)]


                    