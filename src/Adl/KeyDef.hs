{-# OPTIONS_GHC -Wall #-}
module Adl.KeyDef (KeyDef(..),KeyDefs)
where
   import Adl.FilePos       (FilePos,Numbered(..))
   import Adl.Expression    (Expression)
   import Adl.ObjectDef     (ObjectDefs)
   import Adl.Concept       (Concept)
   import CommonClasses     (Identified(..))
      
   type KeyDefs = [KeyDef]
   data KeyDef = Kd { kdpos :: FilePos      -- ^ position of this definition in the text of the ADL source file (filename, line number and column number).
                    , kdlbl :: String       -- ^ the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                    , kdcpt :: Concept      -- ^ this expression describes the instances of this object, related to their context
                    , kdats :: ObjectDefs   -- ^ the constituent attributes (i.e. name/expression pairs) of this key.
                    } deriving (Eq,Show)
   instance Identified KeyDef where
    name kd = kdlbl kd

   instance Numbered KeyDef where
    pos kd = kdpos kd


                    