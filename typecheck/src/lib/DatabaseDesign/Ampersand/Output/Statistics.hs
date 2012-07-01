{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.Statistics (Statistics(..)) where

   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.FPA
   import DatabaseDesign.Ampersand.Fspec.Plug ()
   import DatabaseDesign.Ampersand.Basics (fatalMsg)

   fatal :: Int -> String -> a
   fatal = fatalMsg "Output.Statistics"

 -- TODO Deze module moet nog verder worden ingekleurd...
 
   class Statistics a where
    nInterfaces :: a -> Int      -- ^ The number of interfaces in a
    nPatterns   :: a -> Int      -- ^ The number of patterns in a
    nFpoints    :: a -> Int      -- ^ The number of function points in a
    
    
   instance Statistics a => Statistics [a] where
    nInterfaces xs = sum (map nInterfaces xs)
    nPatterns   xs = sum (map nPatterns xs)
    nFpoints    xs = sum (map nFpoints xs)


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Statistics Fspc where
    nInterfaces fSpec = length (fActivities fSpec) --TODO -> check correctness
    nPatterns   fSpec = nPatterns (patterns fSpec)
    nFpoints    fSpec = sum [nFpoints act       | act<-fActivities fSpec] +
                        sum [fPoints (fpa plug) | InternalPlug plug <- plugInfos fSpec]
-- TODO Deze module moet nog verder worden ingekleurd...
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************

   instance Statistics Pattern where
    nInterfaces _ = 0 --TODO -> check correctness
    nPatterns   _ = 1
    nFpoints   _  = fatal 43 "function points are not defined for patterns at all."

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Activity                         ***
-- \***********************************************************************
   instance Statistics Activity where
    nInterfaces _ = 1
    nPatterns   _ = 0
    nFpoints act  = fPoints (actFPA act) --TODO -> implement correct FPA qualification

-- \***********************************************************************
-- \*** Properties with respect to: Dataset                       ***
-- \*** TODO: both datasets and interfaces are represented as ObjectDef. This does actually make a difference for the function point count, so we have work....
   instance Statistics ObjectDef where
    nInterfaces (Obj{objmsub=Nothing}) = 2 -- this is an association, i.e. a binary relation --TODO -> check correctness
    nInterfaces _ = 4 -- this is an entity with one or more attributes. --TODO -> check correctness
    nPatterns   _ = 0
    nFpoints    _ = fatal 60 "function points are not defined for ObjectDefs at all."
