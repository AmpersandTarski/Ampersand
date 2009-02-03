
module Adl.ObjectDef where
   import Adl.FilePos
   import Adl.Expression
   import CommonClasses(Identified(name,typ))   
   type ObjectDefs = [ObjectDef]
   data ObjectDef = Obj { objnm   :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
                        , objpos  :: FilePos        -- ^ position of this definition in the text of the ADL source file (filename, line number and column number)
                        , objctx  :: Expression     -- ^ this expression describes the instances of this object, related to their context. 
                        , objats  :: ObjectDefs     -- ^ the attributes, which are object definitions themselves.
                        , objstrs :: [[String]]     -- ^ directives that specify the interface.
                        } deriving Eq

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************
--   instance Eq ObjectDef where  -- WAAROM :TODO Stef, deze Eq mistte zijn where clause. Wil jij dit valideren? 
--      obj == obj' = name obj == name obj'
   instance Show ObjectDef
   instance Identified ObjectDef where
    name obj = objnm obj
    typ obj = "ObjectDef_"
   instance Numbered ObjectDef where
    pos obj = objpos obj

   objdefNew e = Obj "" posNone e [] []    -- de constructor van een object. Er is geen default waarde voor expression, dus die moeten we dan maar meegeven. 8-((


   
