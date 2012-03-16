{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand
   ( -- Data Constructors:
     A_Context
   , P_Context(..), P_Relation(..), P_Population(..), PairView(..), PairViewSegment(..), SrcOrTgt(..), P_Rule(..), P_Expression(..), P_Sign(..), P_Concept(..), P_Declaration(..), P_Pattern(..)
   , P_Markup(..), PRef2Obj(..), PPurpose(..), PMeaning(..)
   , A_Concept(..), A_Gen, newAcpt, atomsOf
   , Sign(..), ConceptDef(..), ConceptStructure(..)
   , Pattern(..)
   , Declaration(..)
   , KeyDef(..)
   , KeySegment(..)
   , Expression(..)
   , Population(..)
   , Fspc(..)
   , PlugSQL(..), SqlField(..), SqlType(..), PlugInfo(..)
   , Relation(..)
   , Rule(..), ruleviolations, violationsexpr
   , Prop(..)
   , Lang(..)
   , Options(..), DocTheme(..)
   , Picture(..), writePicture, DrawingType(..)
   , Origin(..)
   , FPA(..), FPcompl(..)
   , mkPair
   -- * Classes:
   , Association(..), flp
   , Collection(..)
   , Identified(..)
   , ProcessStructure(..)
   , Relational(..)
   , ObjectDef(..)
   , objAts, objatsLegacy
   , Interface(..)
   , SubInterface(..)
   , Object(..)
   , Plugable(..)
   , Motivated(..)
   , Traced(..)
   , Language(..)
   , Dotable(..)
   , FPAble(..)
   , ShowHS(..), ShowHSName(..), haskellIdentifier
   -- * Functions on concepts
   , (<==>),meet,order,join
   -- * Functions on declarations
   , makeRelation
   -- * Functions on rules
   -- * Functions on expressions:
   , conjNF, disjNF, simplify
   , notCpl, isCpl, isPos, isNeg
   , isI
   -- * Functions with plugs:
   , tblfields, tblcontents, plugpath, fldauto, requires, requiredFields, iskey
   -- * Parser related stuff
   , CtxError, nocxe, cxes
   , parseCtxM_
   , parsePopsM_
   , parseADL1pExpr
    -- * Type checking and calculus
   , typeCheck
   , makeFspec
    -- * Generators of output
   , interfaceGen
   , prove
   , doGenHaskell
   , doGenXML
   , doGenDocument
   -- * Prettyprinters
   , ShowADL(..), showSQL
   -- * Functions with Options
   , getOptions
   , verboseLn, verbose
   , ImportFormat(..),helpNVersionTexts
   -- * Other functions
   , eqCl
   , ampersandVersionStr, ampersandVersionWithoutBuildTimeStr
   , DatabaseDesign.Ampersand.Basics.putStr
   , DatabaseDesign.Ampersand.Basics.readFile
   , DatabaseDesign.Ampersand.Basics.writeFile
   , DatabaseDesign.Ampersand.Basics.trim
   , spaces
   -- * Stuff that should probably not be in the prototype
   , A_Markup(..), blocks2String, aMarkup2String, PandocFormat(..), Meaning(..)
   , rulefromProp, allprops, endoprops
   , Populated(..), Paire, Purpose(..), ExplObj(..), PictType(..)
   )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.FPA
import DatabaseDesign.Ampersand.ADL1 
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Basics 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Input
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Components 
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms (isI)
import DatabaseDesign.Ampersand.Output.AdlExplanation

