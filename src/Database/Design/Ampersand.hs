module Database.Design.Ampersand
   ( -- Data Constructors:
     A_Context
   , P_Context(..), P_Population(..), PairView(..), PairViewSegment(..), SrcOrTgt(..), P_Rule(..), Term(..), TermPrim(..), P_NamedRel(..), P_Sign(..), P_Concept(..), P_Declaration(..), P_Pattern(..), P_Gen(..)
   , P_Markup(..), PRef2Obj(..), PPurpose(..), PMeaning(..), Meta(..), MetaObj(..)
   , A_Concept(..), A_Gen(..)
   , Signature(..), ConceptDef(..), ConceptStructure(..)
   , Activity(..)
   , AMeaning(..)
   , Quad(..), Conjunct(..)
   , Fswitchboard(..), ECArule(..), Event(..), InsDel(..) -- (required for --haskell output)
   , Pattern(..)
   , Declaration(..)
   , IdentityDef(..)
   , ViewDef(..)
   , IdentitySegment(..)
   , ViewSegment(..)
   , Expression(..)
   , Population(..)
   , FSpec(..), concDefs
   , PlugSQL(..), SqlAttribute(..), SqlTType(..), PlugInfo(..)
   , PAclause(..)
   , Rule(..)
   , Prop(..), RuleOrigin(..)
   , Lang(..)
   , SqlAttributeUsage(..)
   , DnfClause(..)
   , Options(..)
   , FilePos(..), Origin(..)
   , mkPair
   -- * Classes:
   , Association(..), flp
   , Collection(..)
   , Named(..)
   , ObjectDef(..)
   , Relational(..)
   , Interface(..)
   , getInterfaceByName
   , SubInterface(..)
   , Object(..)
   , Plugable(..)
   , Motivated(..)
   , Traced(..)
   , Language(..)
   , ShowHS(..), ShowHSName(..), haskellIdentifier
   -- * Functions on concepts
   , (<==>),sortWith,atomValuesOf
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
   , plugAttributes, fldauto
   -- * Parser related stuff
   , parseADL1pExpr, CtxError
   , createFSpec
    -- * Type checking and calculus
   , Guarded(..), pCtx2aCtx
   , makeFSpec
    -- * Generators of output
   , generateAmpersandOutput
   -- * Prettyprinters
   , ShowADL(..), showSQL, showSign
   -- * Functions with Options
   , getOptions
   , verboseLn, verbose
   ,helpNVersionTexts
   -- * Other functions
   , eqCl, showErr, unCap,upCap,escapeNonAlphaNum
   , ampersandVersionStr, ampersandVersionWithoutBuildTimeStr
   , Database.Design.Ampersand.Basics.putStr
   , Database.Design.Ampersand.Basics.hGetContents
   , Database.Design.Ampersand.Basics.hPutStr
   , Database.Design.Ampersand.Basics.hPutStrLn
   , Database.Design.Ampersand.Basics.readFile
   , Database.Design.Ampersand.Basics.writeFile
   , fst3, snd3, thd3
   , blockParenthesize, addToLastLine, indent 
   , trace, showTrace, showTraceTag
   -- * Stuff that should probably not be in the prototype
   , A_Markup(..), blocks2String, aMarkup2String, PandocFormat(..), Meaning(..)
   , rulefromProp
   , fullContents, AAtomPair, apLeft,apRight
   , Purpose(..), ExplObj(..)
   , ContextInfo,AAtomValue
   )
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Input
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Components
import Database.Design.Ampersand.ADL1.Expression (isPos,isNeg,foldrMapExpression)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
import Database.Design.Ampersand.ADL1.P2A_Converters