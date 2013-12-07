{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand
   ( -- Data Constructors:
     A_Context
   , P_Context(..), P_Population(..), PairView(..), PairViewSegment(..), SrcOrTgt(..), P_Rule(..), Term(..), TermPrim(..), P_Sign(..), P_Concept(..), P_Declaration(..), P_Pattern(..), P_Gen(..)
   , P_Markup(..), PRef2Obj(..), PPurpose(..), PMeaning(..), RelConceptDef(..)
   , A_Concept(..), A_Gen
   , Sign(..), ConceptDef(..), ConceptStructure(..)
   , Activity(..)
   , AMeaning(..)
   , Quad(..), RuleClause(..)
   , ECArule(..)
   , Pattern(..)
   , Declaration(..)
   , IdentityDef(..)
   , ViewDef(..)
   , IdentitySegment(..)
   , ViewSegment(..)
   , Expression(..)
   , UserDefPop(..)
   , Fspc(..)
   , PlugSQL(..), SqlField(..), SqlType(..), PlugInfo(..)
   , Rule(..), ruleviolations, violationsexpr
   , Process(..) , FProcess(..)
   , Prop(..), RuleOrigin(..)
   , Lang(..)
   , SqlFieldUsage(..)
   , DnfClause(..), Clauses(..)
   , Options(..), DocTheme(..)
   , Picture(..), writePicture, DrawingType(..)
   , FilePos(..), Origin(..), Pos(Pos)
   , FPA(..), FPcompl(..)
   , mkPair
   -- * Classes:
   , Association(..), flp
   , Collection(..)
   , Identified(..)
   , ProcessStructure(..)
   , ObjectDef(..)
   , Relational(..)
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
   , (<==>),meet,join,sortWith,atomsOf
   , smallerConcepts, largerConcepts, rootConcepts
   -- * Functions on declarations
   , decusr
   -- * Functions on rules
   -- * Functions on expressions:
   , conjNF, disjNF, simplify
   , cfProof,dfProof,nfProof,normPA
   , lookupCpt
   , showPrf
   , notCpl, isCpl, isPos, isNeg
      , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
   , deMorganERad, deMorganECps, deMorganEUni, deMorganEIsc
   , exprUni2list, exprIsc2list, exprCps2list, exprRad2list
   -- * Functions with plugs:
   , plugFields, tblcontents, plugpath, fldauto, requires, requiredFields, isPlugIndex
   -- * Parser related stuff
   , parseADL1pExpr, CtxError 
   , createFspec
   , getGeneralizations, getSpecializations
    -- * Type checking and calculus
   , Guarded(..), pCtx2aCtx
   , makeFspec
    -- * Generators of output
   , generateAmpersandOutput
   -- * Prettyprinters
   , ShowADL(..), showSQL, showSign
   -- * Functions with Options
   , getOptions
   , verboseLn, verbose
   , FileFormat(..),helpNVersionTexts
   -- * Other functions
   , eqCl, showErr, unCap,upCap,escapeNonAlphaNum, fatalMsg
   , ampersandVersionStr, ampersandVersionWithoutBuildTimeStr
   , DatabaseDesign.Ampersand.Basics.putStr
   , DatabaseDesign.Ampersand.Basics.hGetContents
   , DatabaseDesign.Ampersand.Basics.hPutStr
   , DatabaseDesign.Ampersand.Basics.hPutStrLn
   , DatabaseDesign.Ampersand.Basics.readFile
   , DatabaseDesign.Ampersand.Basics.writeFile
   -- * Stuff that should probably not be in the prototype
   , A_Markup(..), blocks2String, aMarkup2String, PandocFormat(..), Meaning(..)
   , rulefromProp
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
import DatabaseDesign.Ampersand.ADL1.Expression (isPos,isNeg)
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
import DatabaseDesign.Ampersand.InputProcessing
import DatabaseDesign.Ampersand.ADL1.P2A_Converters