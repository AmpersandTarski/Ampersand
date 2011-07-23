{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.CoreImporter 
    ( module DatabaseDesign.Ampersand
    )
where

import DatabaseDesign.Ampersand
   ( -- Data Constructors:
     A_Context
   , P_Context(..)
   , PExplObj(..), PExplanation(..)
   , Architecture(..)
   , Concept(..), cptnew
   , ConceptDef(..), ConceptStructure(..)
   , Pattern(..)
   , Declaration(..)
   , Expression(..)
   , P_Population(..)
   , Fspc(..)
   , ObjectDef(..)
   , PlugSQL(..), SqlField(..), SqlType(..), PlugInfo(..)
   , Relation(..)
   , Rule(..)
   , Prop(..)
   , Lang(..)
   , Options(..), DocTheme(..)
   , Picture(..), writePicture, DrawingType(..)
   , Origin(..), IOLocation(..)
   , FPA(..), FPcompl(..)
   , mkPair
   , P_Populations
   -- * Classes:
   , Association(..)
   , Collection(..)
   , Identified(..)
   , ProcessStructure(..)
   , Relational(..)
   , Interface(..)
   , Object(..)
   , Plugable(..)
   , Traced(..)
   , SpecHierarchy(..)
   , Language(..)
   , Dotable(..)
   , FPAble(..)
   , ShowHS(..), haskellIdentifier
   , ADL1Importable(..)
   -- * Functions on declarations
   , makeRelation
   -- * Functions on rules
   , normExpr
   , showexpression
   -- * Functions on expressions:
   , conjNF, disjNF, simplify
   , v, notCp, isPos, isNeg
   , isI
   , mapExpression, mapRelation
   -- * Functions with plugs:
   , tblfields, tblcontents, plugpath, fldauto, requires, requiredFields, iskey
   -- * Parser related stuff
   , ParserVersion(..)
   , parseFile
   , parseADL1Rule
   , parseADL1
   , parseADL1Pop
   -- * typechecking
   , typecheckAdl1
   -- * Prettyprinters
   , showADL, showADLcode, showSQL
   -- * Generators
   , calculate
   , generate
   -- * Functions with Options
   , getOptions
   , verboseLn, verbose
   , ImportFormat(..),helpNVersionTexts
   -- * Other functions
   , eqCl, naming
   , versionNumber
   , putStr, readFile, writeFile
   -- * Stuff that should not be in the prototype
   , explainContent2String, RuleMeaning(..)
   )
   