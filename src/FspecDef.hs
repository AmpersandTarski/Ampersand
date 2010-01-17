  module FspecDef
         ( module Data.Fspec
         , module CommonClasses
         , module Auxiliaries
         , module Strings 
         , Fidentified(..)
         )

  where

   import Adl
   import CommonClasses(Identified(..))
   import Auxiliaries(showL)
   import Strings(chain)
   import Data.Fspec

   --fspc_patterns :: Fspc -> Patterns
   --fspc_patterns spec = themesOfPatterns (themes spec)     
   --themesOfPatterns :: [Ftheme] -> [Pattern]
   --themesOfPatterns themes = [ftpat tm | tm <-themes]





   instance Fidentified Morphism where
     fsid m = FS_id (name m++name (source m)++name(target m))

   instance Fidentified Concept where
     fsid c = FS_id (name c)
 
   instance Fidentified ObjectDef where
     fsid o = FS_id (name o)

   class Fidentified a where
     fsid :: a -> FSid

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Fidentified Fspc where  -- WAAROM moet er een tweede vorm van Identified zijn? Dit is in tegenspraak met het principe van code-ontdubbeling.
    fsid    fSpec = FS_id (fsName fSpec)


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************
     
   instance Fidentified Fservice where
     fsid fservice = fsid (fsv_objectdef fservice)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

