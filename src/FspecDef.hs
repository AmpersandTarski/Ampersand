  module FspecDef
         ( module Data.Fspec
         , module CommonClasses
         , module Auxiliaries
         , module Strings 
         , Fidentified(..)
     --    , fspc_patterns
     --    , themesOfPatterns
         )

  where

   import Adl
   import CommonClasses(Identified(name,typ))
   import Auxiliaries(showL,haskellIdentifier)
   import Strings(chain)
   import Data.Fspec

   --fspc_patterns :: Fspc -> Patterns
   --fspc_patterns spec = themesOfPatterns (themes spec)     
   --themesOfPatterns :: [Ftheme] -> [Pattern]
   --themesOfPatterns themes = [ftpat tm | tm <-themes]





   instance Fidentified Morphism where
     fsid m = FS_id (name m++name (source m)++name(target m))  --Hier moet nog goed naar worden gekeken....
 --        where 
 --          source (Mph nm pos atts (a,b) _ s) = a
 --          source m = error ("FspecDef.lhs : Cannot evaluate the source expression of the current morphism (yet)")
 --          target (Mph nm pos atts (a,b) _ s) = b    
 --          target m = error ("FspecDef.lhs : Cannot evaluate the target expression of the current morphism (yet)")
  --   typ m  = "f_morph"
   instance Fidentified Concept where
     fsid c = FS_id (name c)
  --   typ m  = "f_cpt"
   instance Fidentified ObjectDef where
     fsid o = FS_id (name o)
  --   typ m  = "f_objdef"


   class Fidentified a where
     fsid :: a -> FSid
   --  typ  :: a -> String



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Identified Fspc where
     name fspc = name (fsid fspc)
     typ   _   = "Fspc_"
   
   instance Fidentified Fspc where
    fsid    spec = fsfsid spec
  --  typ     _ = "f_Ctx"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************
   
   instance Identified Ftheme where
     name theme = name (ftsid theme)
     typ  _     = "f_Thm"
   instance Fidentified Ftheme where
     fsid theme = ftsid theme
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************
     
   instance Identified Funit where
     name funit = name (fusid funit)
     typ _      = "f_Unit"

   instance Fidentified Funit where
     fsid funit = fusid funit 


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************
     
   instance Identified Fservice where
     name fservice = name (objectdef fservice)
     typ fservice = "f_Service"
   instance Fidentified Fservice where
     fsid fservice = fsid (objectdef fservice)
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ServiceSpec                   ***
-- \***********************************************************************
   instance Identified ServiceSpec where
    typ sspc = "f_svc"
    name sspc = name (ssid sspc)
    
   instance Fidentified ServiceSpec where
      fsid ss = ssid ss 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance Identified FSid where
    name (FS_id nm) = nm
    typ _ = "f_Id"

