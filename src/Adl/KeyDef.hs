{-# OPTIONS_GHC -Wall #-}
module Adl.KeyDef (KeyDef(..),KeyDefs)
where
   import Adl.FilePos       (FilePos)
   import Adl.Expression    (Expression)
   import Adl.ObjectDef     (ObjectDefs)
   import CommonClasses     (Identified(..))
      
   type KeyDefs = [KeyDef]
   data KeyDef = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
                    , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                    , kdctx :: Expression   -- ^ this expression describes the instances of this object, related to their context
                    , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                    } deriving (Eq,Show)
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************
--   instance Eq KeyDef where       -- WAAROM :TODO Stef, deze Eq mistte zijn where clause. Wil jij dit valideren? 
--    kd == kd' = kdpos kd == kdpos kd' &&
--                kdlbl kd == kdlbl kd' &&
--                kdctx kd == kdctx kd' &&
--                kdats kd == kdats kd'
    
--   instance Show KeyDef
   instance Identified KeyDef where
    name kd = kdlbl kd
    typ _ = "KeyDef_"


                    