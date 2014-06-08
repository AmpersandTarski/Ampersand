{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.CoreImporter 
    ( module DatabaseDesign.Ampersand
    )
where

import DatabaseDesign.Ampersand hiding (fatalMsg) -- we wrote our own version for the prototype
--   ( -- Data Constructors:
--     A_Context
--   , P_Context(..)
--   , PRef2Obj(..), PPurpose(..)
--   , A_Concept(..), newAcpt
--   , ConceptDef(..), ConceptStructure(..)
--   , Pattern(..)
--   , Declaration(..)
--   , Expression(..)
--   , P_Population(..)
--   , Fspc(..)
--   , ObjectDef(..)
--   , PlugSQL(..), SqlField(..), SqlType(..), PlugInfo(..)
--   , Relation(..)
--   , Rule(..)
--   , Prop(..)
--   , Lang(..)
--   , Options(..), DocTheme(..)
--   , Picture(..), writePicture
--   , Origin(..)
--   , FPA(..), FPcompl(..)
--   , mkPair
--   , P_Populations
--   -- * Classes:
--   , Association(..), flp
--   , Collection(..)
--   , Identified(..)
--   , ProcessStructure(..)
--   , Relational(..)
--   , Interface(..)
--   , Object(..)
--   , Plugable(..)
--   , Traced(..)
--   , SpecHierarchy(..)
--   , Language(..)
--   , makePicture
--   , FPAble(..)
--   , ShowHS(..), haskellIdentifier
--   , ADL1Importable(..)
--   -- * Functions on relsDefdIn
--   , makeRelation
--   -- * Functions on rules
--   , normExpr
--   , showexpression
--   -- * Functions on expressions:
--   , conjNF, disjNF, simplify
--   , v, notCpl, isPos, isNeg
--   -- * Functions with plugs:
--   , tblfields, tblcontents, plugpath, fldauto, requires, requiredFields, isPlugIndex
--   -- * Parser related stuff
--   , ParserVersion(..)
--   , parseFile
--   , parseADL1Rule
--   , parseADL1
--   , parseADL1Pop
--   -- * typechecking
--   , typecheckAdl1
--   -- * Prettyprinters
--   , showADL, showADLcode, showSQL
--   -- * Generators
--   , makeFspec
--   , generate
--   -- * Functions with Options
--   , getOptions
--   , verboseLn, verbose
--   , ImportFormat(..),helpNVersionTexts
--   -- * Other functions
--   , eqCl, naming
--   , versionNumber
--   , putStr, readFile, writeFile
--   -- * Stuff that should not be in the prototype
--   , explainContent2String, RuleMeaning(..)
--   )
   