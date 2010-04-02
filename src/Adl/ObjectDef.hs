{-# OPTIONS_GHC -Wall #-}
module Adl.ObjectDef (ObjectDef(..),ObjectDefs,actions)
where
   import Adl.FilePos       (FilePos,Numbered(..))
   import Adl.Expression    (Expression)
   import CommonClasses     (Identified(..)) 
   import TypeInference.InfLibAG (InfTree)
   
     
   type ObjectDefs = [ObjectDef]
   data ObjectDef = Obj { objnm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                        , objpos  :: FilePos        -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
                        , objctx  :: Expression     -- ^ this expression describes the instances of this object, related to their context. 
                        , objctx_proof :: Maybe (InfTree,Expression)
                        , objats  :: ObjectDefs     -- ^ the attributes, which are object definitions themselves.
                        , objstrs :: [[String]]     -- ^ directives that specify the interface.
                        } deriving (Eq, Show)  -- voor debugging (zie ook instance Show ObjectDef)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

  -- instance Show ObjectDef
   instance Identified ObjectDef where
    name obj = objnm obj
   instance Numbered ObjectDef where
    pos obj = objpos obj


   --Actions can be defined in objstrs by "Action=<action>" p.e. Action=Select
   --If no actions are specified then actions defaults to ["Select","Edit","Delete","New"]
   actions :: ObjectDef -> [String]
   actions obj = if null xs then ["Select","Edit","Delete","New"] else xs
      where xs = [x | strs<-objstrs obj,('A':'c':'t':'i':'o':'n':'=':x)<-strs]
   
