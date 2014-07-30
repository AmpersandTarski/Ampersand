{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand
   ( -- Data Constructors:
     A_Context
   , P_Context(..), P_Population(..), PairView(..), PairViewSegment(..), SrcOrTgt(..), P_Rule(..), Term(..), TermPrim(..), P_Sign(..), P_Concept(..), P_Declaration(..), P_Pattern(..), P_Gen(..)
   , P_Markup(..), PRef2Obj(..), PPurpose(..), PMeaning(..), Meta(..), MetaObj(..)
   , A_Concept(..), A_Gen(..)
   , Sign(..), ConceptDef(..), ConceptStructure(..)
   , Activity(..)
   , AMeaning(..)
   , Quad(..), RuleClause(..)
   , Fswitchboard(..), ECArule(..), Event(..), InsDel(..) -- (required for --haskell output)
   , Pattern(..)
   , Declaration(..)
   , IdentityDef(..)
   , ViewDef(..)
   , IdentitySegment(..)
   , ViewSegment(..)
   , Expression(..)
   , Population(..)
   , Fspc(..), concDefs
   , PlugSQL(..), SqlField(..), SqlType(..), PlugInfo(..)
   , PAclause(..)
   , Rule(..)
   , Process(..) , FProcess(..)
   , Prop(..), RuleOrigin(..)
   , Lang(..)
   , SqlFieldUsage(..)
   , DnfClause(..), Clauses(..)
   , Options(..), DocTheme(..)
   , Picture(..), writePicture , PictureReq(..), makePicture
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
   , Interface(..)
   , SubInterface(..)
   , Object(..)
   , Plugable(..)
   , Motivated(..)
   , Traced(..)
   , Language(..)
   , FPAble(..)
   , ShowHS(..), ShowHSName(..), haskellIdentifier
   -- * Functions on concepts
   , (<==>),meet,join,sortWith,atomsOf
   , smallerConcepts, largerConcepts, rootConcepts
   -- * Functions on relations
   -- * Functions on rules
   -- * Functions on expressions:
   , conjNF, disjNF, simplify
   , cfProof,dfProof,normPA
   , lookupCpt
   , showPrf
   , notCpl, isCpl, isPos, isNeg, foldrMapExpression
      , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.)
   , deMorganERad, deMorganECps, deMorganEUni, deMorganEIsc
   , exprUni2list, exprIsc2list, exprCps2list, exprRad2list, exprPrd2list
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
   , Database.Design.Ampersand.Basics.putStr
   , Database.Design.Ampersand.Basics.hGetContents
   , Database.Design.Ampersand.Basics.hPutStr
   , Database.Design.Ampersand.Basics.hPutStrLn
   , Database.Design.Ampersand.Basics.readFile
   , Database.Design.Ampersand.Basics.writeFile
   -- * Stuff that should probably not be in the prototype
   , A_Markup(..), blocks2String, aMarkup2String, PandocFormat(..), Meaning(..)
   , rulefromProp
   , fullContents, Paire, srcPaire,trgPaire
   , Purpose(..), ExplObj(..)
   )
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Fspec.Fspec
import Database.Design.Ampersand.Fspec.FPA
import Database.Design.Ampersand.ADL1 
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics 
import Database.Design.Ampersand.Fspec
import Database.Design.Ampersand.Input
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Components 
import Database.Design.Ampersand.ADL1.Expression (isPos,isNeg,foldrMapExpression)
import Database.Design.Ampersand.Fspec.ToFspec.NormalForms
import Database.Design.Ampersand.InputProcessing
import Database.Design.Ampersand.ADL1.P2A_Converters