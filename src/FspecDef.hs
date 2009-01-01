  module FspecDef
         ( module Data.Fspec
         , module CommonClasses
         , module Auxiliaries
         , module Strings 
         , Fidentified(..)
         , fspc_patterns
         , themesOfPatterns)

  where

   import ADLdataDef
   import CommonClasses(Identified(name,typ))
   import Auxiliaries(showL,haskellIdentifier)
   import Strings(chain)

   import Data.Fspec

   fspc_patterns :: Fspc -> Patterns
   fspc_patterns spec = themesOfPatterns (themes spec)     
   themesOfPatterns :: [Ftheme] -> [Pattern]
   themesOfPatterns themes = [ftpat tm | tm <-themes]


   instance Fidentified Ftheme where
    fsid theme = ftsid theme
   



   instance Identified Dataset where
     name (DS c pths) = name c
     name (BR m) = name m
     typ  (DS c pths) = "f_DS"
     typ  (BR m) = "f_DS"


   instance Fidentified Dataset where
     fsid (DS c pths) = fsid c
     fsid (BR m) = fsid m

   instance Fidentified Morphism where
     fsid m = FS_id (name m++name (source m)++name(target m))  --Hier moet nog goed naar worden gekeken....
         where 
           source (Mph nm pos atts (a,b) _ s) = a
           source m = error ("FspecDef.lhs : Cannot evaluate the source expression of the current morphism (yet)")
           target (Mph nm pos atts (a,b) _ s) = b    
           target m = error ("FspecDef.lhs : Cannot evaluate the target expression of the current morphism (yet)")
  --   typ m  = "f_morph"
   instance Fidentified Concept where
     fsid c = FS_id (name c)
  --   typ m  = "f_cpt"
   instance Fidentified ObjectDef where
     fsid o = FS_id (name o)
  --   typ m  = "f_objdef"

   instance Eq Dataset where  -- opletten: een dataset moet één vast concept hebben waaraan het wordt herkend.
    DS c _ == DS d _ = c==d
    BR m   == BR m'  = m==m'
    _      == _      = False

   instance Fidentified Frule where
    fsid frul = FS_id (name (rule frul))   

   rule:: Frule -> Rule
   rule (Frul r) = r

   instance Fidentified Fview where
    fsid fview = fsid (objectdef(fview))
   
   class Fidentified a where
     fsid :: a -> FSid
   --  typ  :: a -> String




-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Identified Fspc where
     name fspc = name (fsid fspc)
     typ fspc = "Fspc_"
   
   instance Fidentified Fspc where
    fsid    spec = fsfsid spec
  --  typ     a = "f_Ctx"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************
   
   instance Identified Ftheme where
    typ  f = "f_Thm"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************
     
   instance Identified Funit where
    typ funit = "f_Unit"

   instance Fidentified Funit where
    fsid funit = fusid funit 


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fview                         ***
-- \***********************************************************************
     
   instance Identified Fview where
    typ fview = "f_View"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Frule                         ***
-- \***********************************************************************
   
   instance Identified Frule where
    typ frul = "f_rule"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ServiceSpec                   ***
-- \***********************************************************************
   instance Identified ServiceSpec where
    typ sspc = "f_svc"

   instance Fidentified ServiceSpec where
      fsid ss = ssid ss 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Dataset                       ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance Identified FSid where
    name (FS_id nm) = nm
    typ _ = "f_Id"

