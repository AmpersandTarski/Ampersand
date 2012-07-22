{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters 
     ( pCtx2aCtx
     , disambiguate
     )
where
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.ADL1.Pair
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Core.Poset hiding (sortWith)
import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Data.GraphViz hiding (addExtension, C)
import Data.GraphViz.Attributes.Complete hiding (Box)
import Data.Maybe
import Data.List
import Data.Char
import Data.Array

-- TODO: this module should import Database.Ampersand.Core.ParseTree directly, and it should be one 
--       of the very few modules that imports it. (might require some refactoring due to shared stuff)

fatal :: Int -> String -> a
fatal = fatalMsg "P2A_Converters"

{-The data structure Type is used to represent an expression inside the type checker.
TypExpr e flipped o origExpr is read as:
  "the source type of e, with e equal to (if flipped then PFlp origExpr else origExpr), and origExpr occurs in the P_Context p_context on location o."
origExpr (in conjunction with Origin o) is kept for the purpose of generating messages in terms of the original expression written by the user.
TypLub a b o e is read as:
  "the least upper bound of types a and b, in the context of expression e, which is located at o.
-}
data Type =  TypExpr P_Expression Bool Origin P_Expression
           | TypLub Type Type Origin P_Expression
           | TypGlb Type Type Origin P_Expression
            deriving Show

-- Equality of type expressions is based on occurrence. Only I[c] and V[a,b] have equality irrespective of their occurrence.
{-
instance Eq Type where
 TypExpr (Pid   _ []) _ o _ == TypExpr (Pid   _ [])  _ o' _  =  o==o'
 TypExpr (Pid   _ cs) _ o _ == TypExpr (Pid   _ cs') _ o' _  =  cs==cs' && o==o'
 TypExpr (Pfull _ []) _ o _ == TypExpr (Pfull _ [])  _ o' _  =  o==o'
 TypExpr (Pfull _ cs) _ o _ == TypExpr (Pfull _ cs') _ o' _  =  cs==cs' && o==o'
 TypExpr p _ o _ == TypExpr p' _  o' _  =  p `p_eq` p'    && o==o'
 TypLub  a b o _ == TypLub  a' b' o' _  =  a==a' && b==b' && o==o'
 TypGlb  a b o _ == TypGlb  a' b' o' _  =  a==a' && b==b' && o==o'
 _ == _ = False
-}

showType :: Type -> String
showType (TypExpr _ _       orig origExpr@(Pid _ []))   = showADL origExpr ++"("++ shOrig orig++")"
showType (TypExpr expr@(Pid _ []) _ orig _)             = showADL expr     ++"("++ shOrig orig++")"
showType (TypExpr expr@(Pid _ _ ) _ _ _)                = showADL expr
showType (TypExpr _ _       _    origExpr@(Pid _ _))    = showADL origExpr
showType (TypExpr _ _       orig origExpr@(Pfull _ [])) = showADL origExpr ++"("++ shOrig orig++")"
showType (TypExpr expr@(Pfull _ []) _ orig _)           = showADL expr     ++"("++ shOrig orig++")"
showType (TypExpr expr@(Pfull _ _ ) _ _ _)              = showADL expr
showType (TypExpr _ _       _    origExpr@(Pfull _ _))  = showADL origExpr
showType (TypExpr _ flipped orig origExpr)              = showADL (if flipped then p_flp origExpr else origExpr) ++"("++ shOrig orig++")"
showType (TypLub a b _ _)                               = showType a++" ./\\. "++showType b
showType (TypGlb a b _ _)                               = showType a++" .\\/. "++showType b

showOrig :: Type -> String
showOrig (TypExpr _ _       orig origExpr@(Pid _ []))   = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pid _ []) _ orig _)             = showADL expr     ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pid _ cs) _ _ _)                = showADL expr
showOrig (TypExpr _ _       orig origExpr@(Pid _ _))    = showADL origExpr
showOrig (TypExpr _ _       orig origExpr@(Pfull _ [])) = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pfull _ []) _ orig _)           = showADL expr     ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pfull _ cs) _ _ _)              = showADL expr
showOrig (TypExpr _ _       orig origExpr@(Pfull _ _))  = showADL origExpr
showOrig (TypExpr _ flipped orig origExpr)              = showADL (if flipped then p_flp origExpr else origExpr) ++"("++ shOrig orig++")"
showOrig (TypLub _ _ orig origExpr)                     = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypGlb _ _ orig origExpr)                     = showADL origExpr ++"("++ shOrig orig++")"

shOrig :: Origin -> String
shOrig (FileLoc (FilePos (fn,DatabaseDesign.Ampersand.ADL1.Pos l c,_))) = "line " ++ show l++":"++show c
shOrig (DBLoc str)   = "Database location: "++str
shOrig (Origin str)  = str
shOrig OriginUnknown = "Unknown origin"

instance Traced Type where
  origin (TypExpr _ _ o _) = o
  origin (TypLub _ _ o _) = o
  origin (TypGlb _ _ o _) = o

instance Eq Type where
  t==t' = t `t_eq` t'
t_eq :: Type -> Type -> Bool
t_eq (TypExpr (Pid _ [c])    _ _ _) (TypExpr (Pid _ [c'])     _ _ _) = c==c'
t_eq (TypExpr (Pnid c)       _ _ _) (TypExpr (Pnid c')        _ _ _) = c==c'
t_eq (TypExpr (Patm _ x [c]) _ _ _) (TypExpr (Patm _ x' [c']) _ _ _) = x==x' && c==c'
t_eq (TypExpr (Pfull _ cs)   _ _ _) (TypExpr (Pfull _ cs')    _ _ _) = cs==cs'
t_eq e@(TypExpr (PTyp _ _ (P_Sign [])) _ _ _) e'@(TypExpr (PTyp _ _ (P_Sign [])) _ _ _) = e==e'
t_eq (TypExpr (PTyp _ a sgn) _ _ _) (TypExpr (PTyp _ a' sgn') _ _ _) | sgn==sgn' = a `p_eq` a'
t_eq (TypExpr x _ _ _) (TypExpr y _ _ _) = x == y
t_eq (TypLub l r _ _) (TypLub l' r' _ _) =  l `t_eq` l' && r `t_eq` r'
t_eq (TypGlb l r _ _) (TypGlb l' r' _ _) =  l `t_eq` l' && r `t_eq` r'
t_eq _ _ = False

p_eq :: P_Expression -> P_Expression -> Bool
p_eq (Pid _ cs)     (Pid _ cs')      = cs==cs'
p_eq (Pnid c)       (Pnid c')        = c==c'
p_eq (Patm _ x cs)  (Patm _ x' cs')  = x==x' && cs==cs'
p_eq Pnull          Pnull            = True
p_eq (Pfull _ cs)   (Pfull _ cs')    = cs==cs'
p_eq (Prel _ a)     (Prel _ a')      = a==a'
p_eq (Pflp _ a)     (Pflp _ a')      = a==a'
p_eq (Pequ _ a b)   (Pequ _ a' b')   = p_eq a a' && p_eq b b'
p_eq (Pimp _ a b)   (Pimp _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PIsc _ a b)   (PIsc _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PUni _ a b)   (PUni _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PDif _ a b)   (PDif _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PLrs _ a b)   (PLrs _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PRrs _ a b)   (PRrs _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PCps _ a b)   (PCps _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PRad _ a b)   (PRad _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PPrd _ a b)   (PPrd _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PKl0 _ a)     (PKl0 _ a')      = p_eq a a'
p_eq (PKl1 _ a)     (PKl1 _ a')      = p_eq a a'
p_eq (PFlp _ a)     (PFlp _ a')      = p_eq a a'
p_eq (PCpl _ a)     (PCpl _ a')      = p_eq a a'
p_eq (PBrk _ a)     (PBrk _ a')      = p_eq a a'
p_eq (PTyp _ a sgn) (PTyp _ a' sgn') = p_eq a a' && sgn==sgn'
p_eq _ _ = False

p_flp :: P_Expression -> P_Expression
p_flp a@(Pid{})    = a
p_flp a@(Pnid{})   = a
p_flp a@(Patm{})   = a
p_flp Pnull        = Pnull
p_flp (Pfull o cs)   = Pfull o (reverse cs)
p_flp (Prel o a)     = Pflp o a
p_flp (Pflp o a)     = Prel o a
p_flp (Pequ o a b)   = Pequ o (p_flp a) (p_flp b)
p_flp (Pimp o a b)   = Pimp o (p_flp a) (p_flp b)
p_flp (PIsc o a b)   = PIsc o (p_flp a) (p_flp b)
p_flp (PUni o a b)   = PUni o (p_flp a) (p_flp b)
p_flp (PDif o a b)   = PDif o (p_flp a) (p_flp b)
p_flp (PLrs o a b)   = PRrs o (p_flp b) (p_flp a)
p_flp (PRrs o a b)   = PLrs o (p_flp b) (p_flp a)
p_flp (PCps o a b)   = PCps o (p_flp b) (p_flp a)
p_flp (PRad o a b)   = PRad o (p_flp b) (p_flp a)
p_flp (PPrd o a b)   = PPrd o (p_flp b) (p_flp a)
p_flp (PKl0 o a)     = PKl0 o (p_flp a)
p_flp (PKl1 o a)     = PKl1 o (p_flp a)
p_flp (PFlp _ a)     = p_flp (p_flp a) -- ensures that inner PFlp is removed too
p_flp (PCpl o a)     = PCpl o (p_flp a)
p_flp (PBrk o a)     = PBrk o (p_flp a)
p_flp (PTyp o a sgn) = PTyp o (p_flp a) (let P_Sign cs=sgn in P_Sign (reverse cs))

t_complement :: Type -> Type
t_complement (TypExpr e flipped orig origExpr) = TypExpr (complement e) flipped orig (PCpl (origin origExpr) origExpr)
t_complement (TypLub a b orig origExpr)        = TypGlb (t_complement a) (t_complement b) orig (PCpl (origin origExpr) origExpr)
t_complement (TypGlb a b orig origExpr)        = TypLub (t_complement a) (t_complement b) orig (PCpl (origin origExpr) origExpr)

complement :: P_Expression -> P_Expression
complement a@(Pid o _)    = PCpl o a
complement (Pnid a)       = Pid OriginUnknown [a]
complement (Patm o a cs)  = PCpl o (Patm o a cs)
complement Pnull          = PCpl OriginUnknown Pnull
complement (Pfull _ _)    = Pnull
complement (Prel o r)     = PCpl o (Prel o r)
complement (Pflp o r)     = PCpl o (Pflp o r)
complement (Pequ o a b)   = complement (PIsc o (Pimp o a b) (Pimp o b a))
complement (Pimp o a b)   = PIsc o a (complement b)
complement (PIsc o a b)   = PUni o (complement a) (complement b)
complement (PUni o a b)   = PIsc o (complement a) (complement b)
complement (PDif o a b)   = PUni o (complement a) b
complement (PLrs o a b)   = PCps o (complement a) (p_flp b)
complement (PRrs o a b)   = PCps o (p_flp a) (complement b)
complement (PCps o a b)   = PRad o (complement a) (complement b)
complement (PRad o a b)   = PCps o (complement a) (complement b)
complement (PPrd o a b)   = PCpl o (PPrd o a b)
complement a@(PKl0{})     = PCpl (origin a) a
complement a@(PKl1{})     = PCpl (origin a) a
complement (PFlp o a)     = PFlp o (complement a)
complement (PCpl _ a)     = a
complement (PBrk o a)     = PBrk o (complement a)
complement (PTyp o a sgn) = PTyp o (complement a) sgn

-- p_simplify is used to make the type graphs smaller, by simplifying only the most evident things.
p_simplify :: P_Expression -> P_Expression
p_simplify (PFlp _ (PFlp _ a)) = p_simplify a
p_simplify (PFlp _ (Prel o r)) = Pflp o r
p_simplify (PFlp _ (Pflp o r)) = Prel o r
p_simplify (PFlp o a)          = PFlp o (p_simplify a)
p_simplify (PBrk _ a)          = p_simplify a
p_simplify (Pequ o a b)        = Pequ o (p_simplify a) (p_simplify b)
p_simplify (Pimp o a b)        = Pimp o (p_simplify a) (p_simplify b)
p_simplify (PIsc o a b)        = PIsc o (p_simplify a) (p_simplify b)
p_simplify (PUni o a b)        = PUni o (p_simplify a) (p_simplify b)
p_simplify (PDif o a b)        = PDif o (p_simplify a) (p_simplify b)
p_simplify (PLrs o a b)        = PLrs o (p_simplify a) (p_simplify b)
p_simplify (PRrs o a b)        = PRrs o (p_simplify a) (p_simplify b)
p_simplify (PCps o a b)        = PCps o (p_simplify a) (p_simplify b)
p_simplify (PRad o a b)        = PRad o (p_simplify a) (p_simplify b)
p_simplify (PPrd o a b)        = PPrd o (p_simplify a) (p_simplify b)
p_simplify (PKl0 _ (PKl0 o a)) = PKl0 o (p_simplify a)
p_simplify (PKl0 _ (PKl1 o a)) = PKl1 o (p_simplify a)
p_simplify (PKl1 _ (PKl0 o a)) = PKl1 o (p_simplify a)
p_simplify (PKl1 _ (PKl1 o a)) = PKl1 o (p_simplify a)
p_simplify (PKl0 o a)          = PKl0 o (p_simplify a)
p_simplify (PKl1 o a)          = PKl1 o (p_simplify a)
p_simplify (PCpl o a)          = PCpl o (p_simplify a)
p_simplify (PTyp o a sgn)      = PTyp o (p_simplify a) sgn
p_simplify a                   = a
          
mSpecific, mGeneric :: Type -> Type -> Origin -> P_Expression -> Type
mSpecific a b o e = TypLub a b o e
mGeneric  a b o e = TypGlb a b o e
{- obsolete:
mSpecific (TypExpr a flippedA oa origA) (TypExpr b flippedB ob origB) o origExpr
 = TypExpr (PIsc o (if flippedA then p_flp a else a) (if flippedB then p_flp b else b)) False o origExpr
mGeneric  (TypExpr a flippedA oa origA) (TypExpr b flippedB ob origB) o origExpr
 = TypExpr (PUni o (if flippedA then p_flp a else a) (if flippedB then p_flp b else b)) False o origExpr
-}

{-
-- The following function, computepredicates, computes a number of predicates to be resolved with a simplex method,
-- for the purpose of disambiguating relations in P_Expressions.
-- This function only computes the predicates.
-- Linking a declaration (PTyp oD (PRel oD' relNameD) [s,t]) to an occurrence (PRel o relName) is denoted as:  (PTyp o (PRel o relName) [s,t]), which has type P_Expression
-- correspondence between two such links is denoted as a pair (of type (P_Expression,P_Expression))
computePredicates :: [P_Declaration] ->             -- A list of all declarations in this script
                     P_Expression ->                -- an expression that must be disambiguated.
                     [(P_Expression,P_Expression)]  -- a list of term-tuples of the form (x@(PTyp (PRel oX relNameX) [srcX, trgX]),y@(PTyp (PRel oY relNameY) [srcY, trgY]))
                                                    --   each of which represents a predicate x=y.
computePredicates decls expr
 = [(x,y) |(x,y)<-predics, x/=y]
   where
     (_,_,predics) = preds expr
     preds :: P_Expression -> ( [P_Expression]                 -- a list of terms of the form PTyp (PRel o relName) [src, trg], being the untyped relations on the left hand side, typed with a possible type
                              , [P_Expression]                 -- a list of terms of the form PTyp (PRel o relName) [src, trg], being the untyped relations on the right hand side, typed with a possible type
                              , [(P_Expression,P_Expression)]  -- a list of term-tuples of the form PTyp (PRel o relName) [src, trg],
                              )                                --   each of which represents a predicate
     preds (Pequ _ a b)                                                      -- a=b    equality
          = ( nub [e| (x,y)<-tuplesL,e<-[x,y]]
            , nub [e| (x,y)<-tuplesR,e<-[x,y]]
            , tuplesL++tuplesR++predicatesA++predicatesB                                 
            )
            where (lTermsA,rTermsA,predicatesA) = preds a; (lTermsB,rTermsB,predicatesB) = preds b
                  tuplesL = [(x,y) | x@(PTyp _ _ (P_Sign [xSrc, _]))<-lTermsA, y@(PTyp _ _ (P_Sign [ySrc, _]))<-lTermsB, xSrc==ySrc]
                  tuplesR = [(x,y) | x@(PTyp _ _ (P_Sign [_, xTrg]))<-rTermsA, y@(PTyp _ _ (P_Sign [_, yTrg]))<-rTermsB, xTrg==yTrg]
     preds (Pimp o a b)            = preds (Pequ o a (PIsc o a b))           -- a|-b    (subset)
     preds (PIsc _ (Pfull _ []) b) = preds b                                 -- V/\b    (intersection with full relation)
     preds (PIsc _ a (Pfull _ [])) = preds a                                 -- a/\V    (intersection with full relation)
     preds (PIsc o a b)            = preds (Pequ o a b)                      -- a/\b    (intersection)
     preds (PUni _ Pnull b)        = preds b                                 -- emptySet\/b    (union)
     preds (PUni _ a Pnull)        = preds a                                 -- a\/emptySet    (union)
     preds (PUni o a b)            = preds (Pequ o a b)                      -- a\/b    (union)
     preds (PDif o a b)            = preds (Pequ o a b)                      -- a-b     (difference)
     preds (PLrs o a b)            = preds (PRad o a (complement (p_flp b))) -- a/b     (left residual)      
     preds (PRrs o a b)            = preds (PRad o (complement (p_flp a)) b) -- a\b     (right residual) 
     preds (PCps _ (Pid _ []) b)   = preds b                                 -- I;b      composition
     preds (PCps _ a (Pid _ []))   = preds a                                 -- a;I      composition
     preds (PCps _ a b)                                                      -- a;b      composition
          = ( lTermsA>-thrownOutA
            , rTermsB>-thrownOutB
            , tuples++predicatesA++predicatesB                                 
            )
            where (lTermsA,rTermsA,predicatesA) = preds a; (lTermsB,rTermsB,predicatesB) = preds b
                  tuples = [(l,r) | l@(PTyp _ _ (P_Sign [_, lTrg]))<-rTermsA, r@(PTyp _ _ (P_Sign [rSrc, _]))<-lTermsB, lTrg==rSrc]
                  thrownOutA = rTermsA>-[l | (l,_)<-tuples]                    -- the relation-declaration assignments from rTermsA that were not used
                  thrownOutB = lTermsB>-[r | (_,r)<-tuples]                    -- the relation-declaration assignments from lTermsB that were not used
     preds (PRad o a b) = preds (PCps o a b)                                 -- a!b relative addition (dagger)
     preds (PPrd _ a b)                                                      -- a*b cartesian product
          = ( lTermsA                                                       
            , rTermsB
            , predicatesA++predicatesB                                 
            )
            where (lTermsA,_,predicatesA) = preds a; (_,rTermsB,predicatesB) = preds b
     preds (PKl0 _ e)   = preds e
     preds (PKl1 _ e)   = preds e
     preds (PFlp _ e)                                                 -- e~ inverse (wok, flip)
          = ( map p_flp rTerms
            , map p_flp lTerms
            , predicates
            )
            where (lTerms,rTerms,predicates) = preds e
     preds (PCpl _ e)             = preds e                           -- -e  complement
     preds (PBrk _ e)             = preds e                           -- (e) brackets
     preds (PTyp _ e (P_Sign [])) = preds e
     preds (PTyp _ e (P_Sign cs))                                     -- e[A*B]  type-annotation
          = ( [l | l@(PTyp _ _ (P_Sign [lSrc, _]))<-lTerms, lSrc==head cs]                            -- the untyped terms at the left of x
            , [r | r@(PTyp _ _ (P_Sign [_, rTrg]))<-rTerms, rTrg==last cs]                            -- the untyped terms at the right of x
            , predicates
            )
            where (lTerms,rTerms,predicates) = preds e
     preds (Pid _ _)      = ( [], [], [])                             -- I[C]
     preds (Pnid _)       = ( [], [], [])                             -- -I[C]
     preds (Patm _ _ _)   = ( [], [], [])                             -- 'Piet'   (an untyped singleton)
     preds  Pnull         = ( [], [], [])                             -- -V     (the empty set)
     preds (Pfull _ [])   = ( [], [], [])                             -- V     (the untyped full set)
     preds (Pfull _ _ )   = ( [], [], [])                             -- V[A*B] (the typed full set)
     preds x@(Prel o relName)                                           -- r      a relation
          = ( [PTyp o x (dec_sign d)| d<-decls, relName==dec_nm d]  -- the untyped terms at the left of x, with possible types
            , [PTyp o x (dec_sign d)| d<-decls, relName==dec_nm d]  -- the untyped terms at the right of x, with possible types
            , []                                                    -- no predicates can be derived from x                                
            )
     preds (Pflp o nm) = preds (PFlp o (Prel o nm))                   -- r~     a flipped relation
-}

anything = TypExpr (Pfull OriginUnknown []) False OriginUnknown (Pfull OriginUnknown [])
thing c  = TypExpr (Pid OriginUnknown [c]) False OriginUnknown (Pid OriginUnknown [c])
isAnything (TypExpr (Pfull _ []) _ _ (Pfull _ [])) = True
isAnything _ = False
{- The purpose of 'typing' is to analyse the domains and codomains of an expression in a context.
As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
For any two P_Expressions a and b,  if dom(a) is a subset of dom(b), this is represented as a tuple (TypExpr a _ _ _,TypExpr b _ _ _) in st.
In the code below, this shows up as  dom a.<.dom b
The function typing does a recursive scan through all subexpressions, collecting all tuples on its way.
Besides expression expr, this function requires a universe in which to operate.
Specify 'anything anything' if there are no restrictions.
If the source and target of expr is restricted to concepts c and d, specify (thing c) (thing d).
-}
typing :: P_Context -> Type -> Type -> P_Expression -> [(Type, Type)] -- subtypes (.. is subset of ..)
typing p_context universeSource universeTarget expr
 = nub (uType expr universeSource universeTarget expr ++  -- should we use '(p_simplify expr)' instead of just 'expr'?
        [ st| g<-isas
            , let spc=Pid (origin g) [gen_spc g]
            , let gen=Pid (origin g) [gen_gen g]
            , let x=Pimp (origin g) spc gen
            , st<-dom spc.<.dom gen
        ])
   where
     isas     = p_gens p_context
     decls    = p_declarations p_context
     pDecls   = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) | d<-decls]
     u = OriginUnknown
     uType :: P_Expression    -- x:    the original expression from the script, meant for representation in the graph.
           -> Type            -- uLft: the type of the universe for the domain of x 
           -> Type            -- uRt:  the type of the universe for the codomain of x
           -> P_Expression    -- z:    the expression to be analyzed
           -> [(Type, Type)]  -- a list of subset pairs, which is the result of analysing expression x.
     uType x _    _     (Pid{})               = dom x.=.cod x                                                              -- I[C]
     uType _ _    _     (Pnid _)              = fatal 136 "Pnid has no representation"
--   uType x _    _     (Pnid _)              = dom x.=.dom (Pid [c]) .+. cod x.=.cod (Pid [c])                      -- These rules apply for  -I[C] (i.e. Pnid, if it were represented)
     uType x _    _     (Patm _ _ [])         = dom x.=.cod x                                                        -- 'Piet'   (an untyped singleton)
     uType x _    _     (Patm o _ cs)         = dom x.<.dom (Pid u [head cs]) .+. cod x.<.cod (Pid u [head cs])      -- 'Piet'[Persoon]  (a typed singleton)
     uType _ _    _      Pnull                = nothing                                                              -- -V     (the empty set)
     uType _ _    _     (Pfull _ [])          = nothing                                                              --  V     (the untyped full set)
     uType x _    _     (Pfull o cs)          = dom x.=.dom (Pid u [head cs]) .+. cod x.=.cod (Pid u [last cs])      --  V[A*B] (the typed full set)
     uType x _    _     (Prel _ nm)           = foldr (.+.) [] [ dom x.<.dom decl .+. cod x.<.cod decl               --  r      a relation
                                                               | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
     uType x _    _     (Pflp _ nm)           = foldr (.+.) [] [ dom x.<.cod decl .+. cod x.<.dom decl               --  r~     a flipped relation
                                                               | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
     uType x uLft uRt   (Pequ _ a b)          = dom a.=.dom x .+. cod a.=.cod x .+. dom b.=.dom x .+. cod b.=.cod x  --  a=b    equality
                                                 .+. uType a uLft uRt a .+. uType b uLft uRt b 
{- A direct way, which requires proof
     uType x uLft uRt   (Pimp _ a b)          = dom a.<.dom x .+. cod a.<.cod x .+.                                  --  a|-b   implication (aka: subset)
                                                dom b.<.dom x .+. cod b.<.cod x .+.
                                                uType uLft uRt a .+. uType uLft uRt b
-}
-- A more indirect way, which requires no proof
     uType x uLft uRt   (Pimp o a b)          = uType x uLft uRt e                 --  a|-b   implication (aka: subset)
                                                where e = Pequ o a (PIsc o a b)
--
     uType x  _    _    (PIsc o a b)          = dom x.<.dom a .+. cod x.<.cod a .+. dom x.<.dom b .+. cod x.<.cod b    --  intersect ( /\ )
                                                .+. interDom.<.dom a .+. interCod.<.cod a .+. interDom.<.dom b .+. interCod.<.cod b
                                                .+. uType a interDom interCod a .+. uType b interDom interCod b
                                                where interDom = mSpecific (dom a) (dom b) o x
                                                      interCod = mSpecific (cod a) (cod b) o x
     uType x  _    _    (PUni o a b)          = dom a.<.dom x .+. cod a.<.cod x .+. dom b.<.dom x .+. cod b.<.cod x    --  union     ( \/ )
                                                .+. dom a.<.interDom .+. cod a.<.interCod .+. dom b.<.interDom .+. cod b.<.interCod
                                                .+. uType a interDom interCod a .+. uType b interDom interCod b
                                                where interDom = mGeneric (dom a) (dom b) o x
                                                      interCod = mGeneric (cod a) (cod b) o x
     uType x uLft uRt   (PDif _ a b)          = dom x.<.dom a .+. cod x.<.cod a                                        --  a-b    (difference)
                                                 .+. uType a uLft uRt a .+. uType b (dom a) (cod a) b
     uType x uLft uRt   (PLrs o a b)          = dom x.=.dom e .+. cod x.=.cod e .+. uType x uLft uRt e                 -- a!b      relative addition
                                                where e = PRad o a (complement (p_flp b))
     uType x uLft uRt   (PRrs o a b)          = dom x.=.dom e .+. cod x.=.cod e .+. uType x uLft uRt e                 -- a!b      relative addition
                                                where e = PRad o (complement (p_flp a)) b
     uType x uLft uRt   (PCps _ (Pid _ []) b) = uType x uLft uRt b                                                     -- I;b
     uType x  _   uRt   (PCps o a@(Pid{}) b)  = dom x.=.between .+. cod x.<.cod b .+. between.<.dom b .+.              -- I[C];b   composition
                                                uType x between uRt b
                                                where between = mSpecific (cod a) (dom b) o x
     uType x uLft uRt   (PCps _ a (Pid _ [])) = uType x uLft uRt a                                                     -- a;I      composition
     uType x uLft  _    (PCps o a b@(Pid{}))  = dom x.<.dom a .+. cod x.=.between .+. between.<.cod a .+.              -- a;I[C]   composition
                                                uType x uLft between a
                                                where between = mSpecific (cod a) (dom b) o x
     uType x uLft uRt   (PCps o a b)          = dom x.<.dom a .+. cod x.<.cod b .+.                                  -- a;b      composition
                                                between.<.cod a .+. between.<.dom b .+.
                                          --    between.=.TypExpr betweenExpr False o betweenExpr .+.
                                                uType a uLft between a .+. uType b between uRt b
                                                where between = mSpecific (cod a) (dom b) o x
                                          --          betweenExpr = PIsc o (p_flp a) b
     uType x uLft uRt   (PRad o a b)          = dom x.=.dom e .+. cod x.=.cod e .+. uType x uLft uRt e                 -- a!b      relative addition
                                                where e = PCps o (complement a) (complement b)
{- the elaborated version of uType for PRad
     uType x uLft uRt   (PRad _ a@(Pnid{}) b) = dom x.=.mGeneric (cod a) (dom b) o x .+. cod x.<.cod b .+. st_b          -- -I[C]!b  relative addition
                                                where st_b = uType a (mGeneric (cod a) (dom b) o x) uRt a
     uType x uLft uRt   (PRad _ a b@(Pnid{})) = dom x.<.dom a .+. cod x.=.mGeneric (cod a) (dom b) o x .+. st_a          -- a!-I[C]  relative addition
                                                where st_a = uType b uLft (mGeneric (cod a) (dom b) o x) b
     uType x uLft uRt   (PRad o a b)          = dom x.<.dom a .+. cod x.<.cod b                                      -- a!b      relative addition
                                                mGeneric (cod a) (dom b) o x.<.cod a .+.
                                                mGeneric (cod a) (dom b) o x.<.dom b .+.
                                                uType a uLft (mGeneric (cod a) (dom b) o x) a .+.
                                                uType b (mGeneric (cod a) (dom b) o x) uRt b
-}
     uType x uLft uRt   (PPrd _ a b)          = dom a.=.dom x .+. cod b.=.cod x                                      -- a*b cartesian product
                                                .+. uType a uLft anything a .+. uType b anything uRt b
     uType x uLft uRt   (PKl0 _ e)            = dom e.<.dom x .+. cod e.<.cod x .+. uType e uLft uRt e
     uType x uLft uRt   (PKl1 _ e)            = dom e.<.dom x .+. cod e.<.cod x .+. uType e uLft uRt e
     uType x uLft uRt   (PFlp _ (Prel o nm))  = uType x uLft uRt (Pflp o nm)                                            -- r~  flip
     uType x uLft uRt   (PFlp _ (Pflp o nm))  = uType x uLft uRt (Prel o nm)                                            -- r~~
     uType x uLft uRt   (PFlp _ e)            = cod e.=.dom x .+. dom e.=.cod x .+. uType e uRt uLft e
{- the abstract version of uType for PCpl
     uType x uLft uRt   (PCpl o e)            = dom x.=.dom ec .+. cod x.=.cod ec .+. uType x uLft uRt ec                 -- -a  complement
                                                where ec = PDif o (Pfull o [uLft, uRt]) e
-}
{- the abstract version of uType for PCpl
     uType x uLft uRt   (PCpl _ e)            = ( case (isAnything uLft, isAnything uRt) of
                                                   (False, False) -> dom x.<.uLft .+. dom e.<.uLft .+. cod x.<.uRt .+. cod e.<.uRt
                                                   (False, True ) -> dom x.<.uLft .+. dom e.<.uLft .+. cod x.=.cod e
                                                   (True , False) -> dom x.=.dom e .+. cod x.<.uRt .+. cod e.<.uRt
                                                   (True , True ) -> dom x.=.dom e .+. cod x.=.cod e
                                                ) .+. uType e uLft uRt e
-}
     uType x uLft uRt   (PCpl _ e)            = dom x.=.dom e .+. cod x.=.cod e .+. uType e uLft uRt e
     uType x uLft uRt   (PBrk _ e)            = uType x uLft uRt e                                                     -- (e) brackets
     uType _  _    _    (PTyp _ _ (P_Sign []))= fatal 196 "P_Sign is empty"
     uType x uLft uRt   (PTyp o e (P_Sign cs))= dom x.<.iSrc  .+. cod x.<.iTrg  .+.                                  -- e[A*B]  type-annotation
                                                iSrc .<.uLft  .+. iTrg .<.uRt .+.
                                                if o `elem` [origin d| d<-decls]
                                                then nothing
                                                else dom x.<.dom e .+. cod x.<.cod e .+.
                                                     uType e iSrc iTrg e
                                                where iSrc = TypExpr (Pid u [head cs]) False o x
                                                      iTrg = TypExpr (Pid u [last cs])  True o x
     nothing :: [(Type,Type)]
     nothing = []
     infixl 2 .+.   -- concatenate two lists of types
     infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
     infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
     (.<.) :: Type -> Type -> [(Type,Type)]
     _ .<. TypExpr (Pfull _ []) _ _ _= []
     TypExpr Pnull _ _ _ .<. _ = []
     a .<. b  = [(a, b)] -- a tuple meaning that a is a subset of b.
     (.=.) :: Type -> Type -> [(Type,Type)]
     a .=. b  = [(a, b),(b, a)]
     (.+.) :: [(Type,Type)] -> [(Type,Type)] -> [(Type,Type)]
     a .+. b  = a `uni` b
     dom, cod :: P_Expression -> Type
     dom x    = TypExpr x         False (origin x) x -- the domain of x, and make sure to check subexpressions of x as well
     cod x    = TypExpr (p_flp x) True  (origin x) x 

{- The following table is a data structure that is meant to facilitate drawing type graphs and creating the correct messages for users.
This table is organized as follows:
Int             : a vertex (number) in the stGraph, which contains the raw tuples from function 'typing'
Int             : a vertex (number) in the sccGraph, which is a condensed form of the stGraph, leaving the semantics identical
Type            : a type expression, containing a P_Expression, which is represented by a number in the type graphs. Different expressions may carry the same number in the sccGraph.
[P_Expression]  : the original expression, as it came out of the script, if there is one.
-}
tableOfTypes :: [(Type,Type)] -> ([(Int,Int,Type,[P_Concept])], Graph.Graph, Graph.Graph, Graph.Graph)
tableOfTypes st = (table, stGraph, sccGraph, ambGraph) -- to debug:  error (intercalate "\n  " (map show (take 10 eqClasses)++[show (length eqClasses), show ((sort.nub) [classNr | (exprNr,classNr,_)<-table]>-[0..length eqClasses])]++[show x | x<-take 25 table++drop (length table-10) table])) --  
 where
{- stGraph is a graph whose edges are precisely st, but each element in st is replaced by a pair of integers. The reason is that datatype Graph expects integers.
   The list st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by dom t is a subset of the set of atoms contained by dom t'.
-}
     typeExpressions   :: [Type]     -- a list of all type expressions in st.
     typeExpressions = nub (map fst st++map snd st)
     expressionTable :: [(Int, Type)]
     (expressionTable,numberOfNodes) = ([(i,typeExpr) | (i,cl)<-zip [0..] eqExpressions, typeExpr<-cl ], length eqExpressions)
      where eqExpressions = eqClass (==) typeExpressions     -- WHY do we need the following definition of t_Eq? What is the problem to use t_eq instead?
            TypExpr (Pid   _ cs) _ _ _ `t_Eq` TypExpr (Pid   _ cs') _ _ _  =  cs==cs'
            TypExpr (Pfull _ cs) _ _ _ `t_Eq` TypExpr (Pfull _ cs') _ _ _  =  cs==cs'
            x `t_Eq` y                                                     =   x==y
     expressionNr   :: Type -> Int
     expressionNr t  = head ([i | (i,v)<-expressionTable, t == v]++[fatal 178 ("Type Expression "++show t++" not found by expressionNr")])
-- stGraph is computed for debugging purposes. It shows precisely which edges are computed by uType.
     stGraph :: Graph.Graph
     stGraph = Graph.buildG (0, numberOfNodes-1) stEdges
     stEdges :: [(Int,Int)]
     stEdges = nub [(i,i') | (t,t')<-st, let i=expressionNr t, let i'=expressionNr t', i/=i']
{- sccGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph, which are computed by Graph.scc
-}
     eqClasses :: [[Int]]             -- The strongly connected components are computed in the form of trees (by Graph.scc)
     eqClasses = map Tree.flatten stronglyConnected    -- We are only interested in the elements of each component.
      where stronglyConnected :: [Tree.Tree Int]       -- For that reason we flatten the trees.
            stronglyConnected = Graph.scc stGraph      -- Each equivalence class contains integers, each of which represents a type expression.
     exprClass  :: Int -> Int
     exprClass i = head ([classNr | (exprNr,classNr)<-classNumbers, i==exprNr]++[fatal 191 ("Type Expression "++show i++" not found by exprClass")])
     sccGraph :: Graph.Graph
     sccGraph
      = Graph.buildG (0, length eqClasses-1) edges
        where edges = nub [(c,c') | (i,i')<-stEdges, let c=exprClass i, let c'=exprClass i', c/=c']
        --    verts = nub [n | (c,c')<-edges, n<-[c,c']]
     classNumbers = sort [ (exprNr,classNr) | (classNr,eClass)<-zip [0..] eqClasses, exprNr<-eClass]
{-  The following table is made by merging expressionTable and classNumbers into one list.
    In this case it might be done simply with zip, because the left column of classNumbers is identical to the left column of expressionTable.
    However, the following (more elaborate) way has been chosen to make sure that future mistakes will be caught..
-}
     table = f expressionTable classNumbers
       where f [(i,typeExpr)] [(j,classNr)]
              | i==j = [(i,classNr,typeExpr, ambConcepts classNr)]
             f exprTable@((i,typeExpr):exprTable') classNrs@((j,classNr):classNrs')
              | i==j = (i,classNr,typeExpr, ambConcepts classNr) : f exprTable' classNrs
              | i>j  = f exprTable classNrs'
              | i<j  = fatal 425 "mistake in table"
             f et ct = fatal 249 ("Remaining elements in table\n"++intercalate "\n" (map show et++map show ct))
             ambConcepts classNr = [c |cl<-eqCl fst ambiguities, fst (head cl)==classNr, TypExpr (Pid _ [c]) _ _ _<-map (lookupType table.snd) cl]
     ambGraph :: Graph.Graph
     ambGraph = Graph.buildG (0, length eqClasses-1) ambiguities
      where
       ag = Graph.buildG (0, length eqClasses-1) 
                             [ (i,j)| (i,j)<-clos1 (Graph.edges sccGraph), Graph.outdegree sccGraph!i>1, Graph.outdegree sccGraph!j==0 ]
     ambiguities :: [(Int,Int)]
     ambiguities = [ (i,j)| (i,j)<-Graph.edges ag, Graph.outdegree ag!i>1 ]
      where
       ag = Graph.buildG (0, length eqClasses-1) 
                             [ (i,j)| (i,j)<-clos1 (Graph.edges sccGraph), Graph.outdegree sccGraph!i>1, Graph.outdegree sccGraph!j==0 ]
                             
calcTypeErrors :: P_Context -> [(Int,Int,Type,[P_Concept])] -> [(Type, Type)] -> ([CtxError])
calcTypeErrors p_context typeTable st = typeErrors
   where
    typeErrors :: [CtxError]
    typeErrors
     | (not.null) err1 = err1
     | (not.null) err2 = err2
     | otherwise       = err3
     where err1 = derivedEquals++ambiguousRelations
           err2 = undeclaredRelations
           err3 = untypableExpressions++ambiguousExpressions
    (_{-typeTable-},_,sccGraph,_) = tableOfTypes st
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the expressions derived from the script).
     = [ newcxe ("The following concepts were proven equal:\n  "++intercalate ", " [show c| (_,_,TypExpr (Pid _ [c]) _ _ _)<-diffs] )
       | diffs<-eqCl (\(_,classNr,_) -> classNr) (map head (eqClass tripleEq conceptTypes))
       , length diffs>1]
       where (_,_,t) `tripleEq` (_,_,t') = t `t_eq` t'
--    predics = [pred | expr<-expressions p_context, pred<-computePredicates (p_declarations p_context) expr ]
    conceptTypes :: [(Int,Int,Type)]
    conceptTypes = [ (exprNr, classNr, e) | (exprNr, classNr, e@(TypExpr (Pid{}) _ _ _), _)<-typeTable ]
    conceptClasses = [ classNr | (_, classNr, _)<-conceptTypes ]
{- The expressions with constructor Pid are basic types. Each one should be in precisely one equivalence class.
   It is useful to have a small table with the numbers that represent the expression in the stGraph and the number that represents the equivalence class -}
    -- type errors come in three kinds:
    -- 1. Two named types are equal.
    --    This is usually unintended: user should give equal types equal names.
    -- 2. The type of a relation cannot be determined.
    --    This means that there is no named type in which it is contained.
    -- 3. The type of a term has no name??
    --    I don't know if these can be considered as type-errors
    --    perhaps if this type is too high up, or too low under
    ambiguousRelations :: [CtxError]
    ambiguousRelations
     = [ CxeOrig (newcxe ("Relation \""++relName++"\" is ambiguous. It can be typed by:\n   "
                  ++intercalate "\n   " ["declaration ("++show (origin decl)++") of \""++showADL decl++"\"" | decl<-map snd cl]))
                 "relation"
                 relName
                 orig
       | cl<-eqCl fst declaredExprs, length cl>1
       , let (Prel orig relName,_) = head cl
       ]
    pDecls = [origin d| d<-p_declarations p_context]
    declaredExprs = [ (r,decl) | ( TypExpr r@(Prel _ relName) _ _ _, TypExpr decl@(PTyp _ (Prel _ rnm) _) _ orig _ )<-st, relName==rnm, orig `elem` pDecls ]
    undeclaredRelations :: [CtxError]
    undeclaredRelations
     = [ CxeOrig (newcxe ("Relation \""++relName++"\" is not declared."))
                 "relation"
                 relName
                 orig
       | Prel orig relName<-subexpressions p_context, relName `notElem` nub (map name (p_declarations p_context))
       ] where 
    ambiguousExpressions :: [CtxError]
    ambiguousExpressions
     = [ CxeAmbExpr {cxeExpr   = expr
                    ,cxeSrcT   = srcTypes
                    ,cxeTrgT   = trgTypes
                    ,cxeSign   = []
                    }       
       | cl<-eqCl (\(e,_,_)->origin e) separateSrcTrg
       , let (expr,_,_) = head cl
       , let srcTypes = nub [a | (_,as,_)<-cl, a<-as]
       , let trgTypes = nub [a | (_,_,as)<-cl, a<-as]
       , length srcTypes>1 || length trgTypes>1
       ]++
       [ CxeAmbBetween {cxeExpr   = origExpr
                       ,cxeSrcE   = pExpr a
                       ,cxeSrcB   = fExpr a
                       ,cxeTrgE   = pExpr b
                       ,cxeTrgB   = fExpr b
                       ,cxeCpts   = as
                       }       
       | (TypLub a b _ origExpr, as) <- ambiguity
       ]
        where
         ambiguity
          = [ (typExpr, ambConcepts)
            | (_, _, typExpr, ambConcepts)<-typeTable, (not.null) ambConcepts
            ]
         separateSrcTrg -- In order to qualify ambiguity of a particular expression, we distinguish source- and target ambiguity for the purpose of assembling feedback in CxeAmbExpr.
          = [ x
            | (TypExpr _ flipped _ expr,as) <- ambiguity
            , x<-[(expr, [a | not flipped, a<-as], []), (p_flp expr, [], [a | flipped, a<-as])]
            ]
         pExpr :: Type -> P_Expression
         pExpr (TypExpr _ _ _ expr) = expr
         pExpr x = fatal 549 ("May not call pExpr with "++showType x)
         fExpr :: Type -> Bool
         fExpr (TypExpr _ flipped _ _) = flipped
         fExpr x = fatal 552 ("May not call fExpr with "++showType x)
    untypableExpressions :: [CtxError]
    untypableExpressions
     = [ CxeOrig (newcxe ("Nonexistent type for src("++showVertex typeTable v++")." ))
                 "expression"
                 ""
                 (origin (lookupType typeTable v))
       | v <- Graph.vertices sccGraph, Graph.indegree sccGraph!v==0, tree<-Graph.dfs sccGraph [v]
       , let as=[a|a<-apples tree, a `elem` conceptClasses]
       , null as
       ]
    apples :: Tree.Tree Int -> [Int]
    apples t = if null (Tree.subForest t) then [Tree.rootLabel t] else concat (map apples (Tree.subForest t))

instance Show CtxError where
    showsPrec _ err = showString (showErr err)

showErr :: CtxError -> String
showErr (Cxes xs) = intercalate "\n" (map showErr xs)
showErr err@(CxeAmbExpr{})
 = concat
     ( [show (origin (cxeExpr err))++": Ambiguous expression\n   "++showADL (cxeExpr err)++"\n   "]++
       ["can be typed by either "++commaEng "or" (map show (cxeSign err))++"." | not (null (cxeSign err)) ]++
       ["has either "++commaEng "or" (map showADL (cxeSrcT err))++" as its source concept." | not (null (cxeSrcT err)) ]++
       ["has either "++commaEng "or" (map showADL (cxeTrgT err))++" as its target concept." | not (null (cxeTrgT err)) ]
     )
showErr err@(CxeAmbBetween{})
 = concat
     ( [show (origin (cxeExpr err))]++["\n"]++
       ["    Inside expression   "++showADL (cxeExpr err)++",\n"]++
       ["between the "++f (cxeSrcB err)++" of   "++showADL (cxeSrcE err)]++["\n"]++
       ["    and the "++f (cxeTrgB err)++" of   "++showADL (cxeTrgE err)]++["\n"]++
       ["there is a conflict between concepts "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++["."]
     )
   where f flipped = if flipped then "target" else "source"
showErr (CxeOrig cxe t nm o)
 | nocxe cxe                                    = "The " ++ t ++ " at "++ shOrig o ++ " is correct."
 | t `elem` ["pattern", "process", "interface"] = "The " ++ t ++ " named \""++ nm ++ "\" contains errors " ++ showErr cxe
 | otherwise                                    = "in the " ++ t ++ " at "++ shOrig o ++ ":\n" ++ showErr cxe
showErr (Cxe cxe x) = x ++ "\n" ++ showErr cxe
showErr CxeNone =  ""
showErr (PE msg) = "Parse error:\n"++ show (case msg of 
                                                 [] -> fatal 35 "No messages??? The impossible happened!" 
                                                 x:_ -> x)

showStVertex :: [(Int,Int,Type,[P_Concept])] -> Int -> String
showStVertex typeTable i
 = head ([ showType e | (exprNr, _, e,_)<-typeTable, i==exprNr ]++fatal 506 ("No expression numbered "++show i++" found by showStVertex"))
showVertex :: [(Int,Int,Type,[P_Concept])] -> Int -> String
showVertex typeTable i
 = (intercalate "\n".nub) [ showType (head cl)
                          | cl<-eqCl original [ typExpr | (_, classNr, typExpr,_)<-typeTable, i==classNr ]
                          ]
vertex2Concept :: [(Int,Int,Type,[P_Concept])] -> Int -> P_Concept
vertex2Concept typeTable i
 = head ([ c | (_, classNr, TypExpr (Pid _ [c]) _ _ _, _)<-typeTable , i==classNr]
         ++fatal 603 ("No concept numbered "++show i++" found by vertex2Concept typeTable.\n  "++intercalate "\n  " [ show tti | tti@(_, classNr, _, _)<-typeTable , i==classNr])
        )
unFlip :: Type -> P_Expression
unFlip (TypExpr _ flipped _ e) = if flipped then p_flp e else e
unFlip x = fatal 607 ("May not call 'original' with "++showType x)
original :: Type -> P_Expression
original (TypExpr _ _ _ e) = e
original (TypLub  _ _ _ e) = e
original (TypGlb  _ _ _ e) = e
lookupType :: [(Int,Int,Type,[P_Concept])] -> Int -> Type
lookupType typeTable i
 = head ([ typExpr | (_, classNr, typExpr, _)<-typeTable, i==classNr ]++fatal 562 ("No expression numbered "++show i++" found by tExpr"))

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: [(Type, Type)] -> (DotGraph String,DotGraph String,DotGraph String)
typeAnimate st = (stTypeGraph, condensedGraph, ambiguityGraph)
   where
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
    (typeTable,stGraph,sccGraph,ambGraph) = tableOfTypes st
    stTypeGraph :: DotGraph String
    stTypeGraph = toDotGraph (showStVertex typeTable) stGraph
    condensedGraph :: DotGraph String
    condensedGraph = toDotGraph showVtx sccGraph
     where showVtx n = (intercalate "\n".nub)
                       [ case head cl of
                           t@(TypExpr (Pid _ [])   _ _ _ ) -> showType t
                           (TypExpr t@(Pid{})      _ _ _ ) -> showADL t
                           t@(TypExpr (Pfull _ []) _ _ _ ) -> showType t
                           (TypExpr t@(Pfull{})    _ _ _ ) -> showADL t
                           (TypExpr t@(Pnid{})     _ _ _ ) -> showADL t
                           (TypExpr t@(Pnull{})    _ _ _ ) -> showADL t
                           t                               -> showType t
                       | cl<-eqCl original [ typExpr| (_, classNr, typExpr,_)<-typeTable, n==classNr ]
                       ]
    ambiguityGraph :: DotGraph String
    ambiguityGraph = toDotGraph showVtx ambGraph
     where showVtx n = (intercalate "\n".nub)
                       [ case head cl of
                           t@(TypExpr (Pid _ [])   _ _ _ ) -> showType t
                           (TypExpr t@(Pid{})      _ _ _ ) -> showADL t
                           t@(TypExpr (Pfull _ []) _ _ _ ) -> showType t
                           (TypExpr t@(Pfull{})    _ _ _ ) -> showADL t
                           (TypExpr t@(Pnid{})     _ _ _ ) -> showADL t
                           (TypExpr t@(Pnull{})    _ _ _ ) -> showADL t
                           t                               -> showType t
                       | cl<-eqCl original [ typExpr| (_, classNr, typExpr,_)<-typeTable, n==classNr ]
                       ]
class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
  p_concs :: a -> [P_Concept]
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  expressions :: a -> [P_Expression]
  subexpressions :: a -> [P_Expression]

instance Expr P_Context where
 p_gens pContext
  = concat [ p_gens pat | pat<-ctx_pats  pContext] ++
    concat [ p_gens prc | prc<-ctx_PPrcs pContext] ++
    ctx_gs pContext
 p_concs pContext
  = nub (p_concs (ctx_pats  pContext) ++
         p_concs (ctx_PPrcs pContext) ++
         p_concs (ctx_rs    pContext) ++
         p_concs (ctx_ds    pContext) ++
         p_concs (ctx_ks    pContext) ++
         p_concs (ctx_ifcs  pContext) ++
         p_concs (ctx_sql   pContext) ++
         p_concs (ctx_php   pContext)
        )
 p_declarations pContext
  = concat [ p_declarations pat | pat<-ctx_pats  pContext] ++
    concat [ p_declarations prc | prc<-ctx_PPrcs pContext] ++
    ctx_ds pContext
 expressions pContext
  = nub (expressions (ctx_pats  pContext) ++
         expressions (ctx_PPrcs pContext) ++
         expressions (ctx_rs    pContext) ++
         expressions (ctx_ds    pContext) ++
         expressions (ctx_ks    pContext) ++
         expressions (ctx_ifcs  pContext) ++
         expressions (ctx_sql   pContext) ++
         expressions (ctx_php   pContext)
        )
 subexpressions pContext
  = subexpressions (ctx_pats  pContext) ++
    subexpressions (ctx_PPrcs pContext) ++
    subexpressions (ctx_rs    pContext) ++
    subexpressions (ctx_ds    pContext) ++
    subexpressions (ctx_ks    pContext) ++
    subexpressions (ctx_ifcs  pContext) ++
    subexpressions (ctx_sql   pContext) ++
    subexpressions (ctx_php   pContext)

instance Expr P_Pattern where
 p_gens pPattern
  = pt_gns pPattern
 p_concs pPattern
  = nub (p_concs (pt_rls pPattern) ++
         p_concs (pt_dcs pPattern) ++
         p_concs (pt_kds pPattern)
        )
 p_declarations pPattern
  = pt_dcs pPattern
 expressions pPattern
  = nub (expressions (pt_rls pPattern) ++
         expressions (pt_dcs pPattern) ++
         expressions (pt_kds pPattern)
        )
 subexpressions pPattern
  = subexpressions (pt_rls pPattern) ++
    subexpressions (pt_dcs pPattern) ++
    subexpressions (pt_kds pPattern)

instance Expr P_Process where
 p_gens pProcess
  = procGens pProcess
 p_concs pProcess
  = nub (p_concs (procRules pProcess) ++
         p_concs (procDcls  pProcess) ++
         p_concs (procKds   pProcess)
        )
 p_declarations pProcess
  = procDcls pProcess
 expressions pProcess
  = nub (expressions (procRules pProcess) ++
         expressions (procDcls  pProcess) ++
         expressions (procKds   pProcess)
        )
 subexpressions pProcess
  = subexpressions (procRules pProcess) ++
    subexpressions (procDcls  pProcess) ++
    subexpressions (procKds   pProcess)

instance Expr P_Rule where
 p_concs r = p_concs (rr_exp r)
 expressions r = expressions (rr_exp r)
 subexpressions r = subexpressions (rr_exp r)
instance Expr P_Sign where
 p_concs (P_Sign cs) = nub cs
 expressions r = []
 subexpressions r = []
instance Expr P_Declaration where
 p_concs d = p_concs (dec_sign d)
 expressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
-- expressions d = [PCps orig (Pid orig [head sgn]) (PCps orig (Prel orig (dec_nm d)) (Pid orig [last sgn]))] where P_Sign sgn = dec_sign d; orig = origin d
 subexpressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
instance Expr P_KeyDef where
 p_concs k = p_concs [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 expressions k = expressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 subexpressions k = subexpressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
instance Expr P_Interface where
 p_concs k = p_concs (ifc_Obj k)
 expressions k = expressions (ifc_Obj k)
 subexpressions k = subexpressions (ifc_Obj k)
instance Expr P_ObjectDef where
 p_concs o = p_concs (obj_ctx o) `uni` p_concs (expressions (obj_msub o))
 expressions o = [obj_ctx o | null (expressions (obj_msub o))]++expressions [PCps (origin o) (obj_ctx o) e | e<-expressions (obj_msub o)]
 subexpressions o = subexpressions (obj_ctx o)++subexpressions (obj_msub o)
instance Expr P_SubInterface where
 p_concs x = p_concs (si_box x)
 expressions x = expressions (si_box x)
 subexpressions x = subexpressions (si_box x)
instance Expr a => Expr (Maybe a) where
 p_concs Nothing = []
 p_concs (Just x) = p_concs x
 expressions Nothing = []
 expressions (Just x) = expressions x
 subexpressions Nothing = []
 subexpressions (Just x) = subexpressions x
instance Expr a => Expr [a] where
 p_concs = concat.map p_concs
 expressions = concat.map expressions
 subexpressions = concat.map subexpressions
instance Expr P_Expression where
 p_concs e@(Pid _ cs)     = nub cs
 p_concs e@(Pnid c)       = [c]
 p_concs e@(Patm _ _ cs)  = nub cs
 p_concs e@Pnull          = []
 p_concs e@(Pfull _ cs)   = nub cs
 p_concs e@(Prel{})       = []
 p_concs e@(Pflp{})       = []
 p_concs e@(Pequ _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(Pimp _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PIsc _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PUni _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PDif _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PLrs _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PRrs _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PCps _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PRad _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PPrd _ a b)   = p_concs a `uni` p_concs b
 p_concs e@(PKl0 _ a)     = p_concs a
 p_concs e@(PKl1 _ a)     = p_concs a
 p_concs e@(PFlp _ a)     = p_concs a
 p_concs e@(PCpl _ a)     = p_concs a
 p_concs e@(PBrk _ a)     = p_concs a
 p_concs e@(PTyp _ a sgn) = p_concs a `uni` p_concs sgn
 expressions e = [e]
 subexpressions e@(Pid{})      = [e]
 subexpressions e@(Pnid{})     = [e]
 subexpressions e@(Patm{})     = [e]
 subexpressions e@Pnull        = [e]
 subexpressions e@(Pfull{})    = [e]
 subexpressions e@(Prel{})     = [e]
 subexpressions e@(Pflp{})     = [e]
 subexpressions e@(Pequ _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(Pimp _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PIsc _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PUni _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PDif _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PLrs _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PRrs _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PCps _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PRad _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PPrd _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PKl0 _ a)   = [e]++subexpressions a
 subexpressions e@(PKl1 _ a)   = [e]++subexpressions a
 subexpressions e@(PFlp _ a)   = [e]++subexpressions a
 subexpressions e@(PCpl _ a)   = [e]++subexpressions a
 subexpressions e@(PBrk _ a)   = [e]++subexpressions a
 subexpressions e@(PTyp _ a _) = [e]++subexpressions a


--  The following is for drawing graphs.

toDotGraph :: (Graph.Vertex->String) -- ^ a show-function for printing vertices.
             -> Graph.Graph
             -> DotGraph String        -- ^ The resulting DotGraph
toDotGraph showVtx graph
       = DotGraph { strictGraph = False
                  , directedGraph = True
                  , graphID = Nothing
                  , graphStatements 
                        = DotStmts { attrStmts = [GraphAttrs [Splines SplineEdges, RankDir FromLeft]]
                                   , subGraphs = []
                                   , nodeStmts = map constrNode (Graph.vertices graph)
                                   , edgeStmts = map constrEdge (Graph.edges graph)
                                   }
                  }
   where
    constrNode :: Graph.Vertex -> DotNode String
    constrNode v
      = DotNode { nodeID = show v
                , nodeAttributes = [ toLabel (showVtx v)]
                }
 
    constrEdge :: Graph.Edge -> DotEdge String
    constrEdge (v, v')
      = DotEdge { fromNode = show v
                , toNode   = show v'
                , edgeAttributes = []
                }
 
pCtx2aCtx :: P_Context -> (A_Context,CtxError,DotGraph String,DotGraph String,DotGraph String)
pCtx2aCtx p_context
 = (contxt
   ,cxelist ( cxerrs++if nocxe(cxelist cxerrs) then postchks else [])
   ,stTypeGraph,condensedGraph,ambiguityGraph)
   where
    contxt = 
         ACtx{ ctxnm     = name p_context     -- The name of this context
             , ctxpos    = ctx_pos p_context
             , ctxlang   = fromMaybe Dutch (ctx_lang p_context)
             , ctxmarkup = fromMaybe ReST  (ctx_markup p_context) -- The default markup format for free text in this context
             , ctxpo     = makePartialOrder hierarchy    -- The base hierarchy for the partial order of concepts (see makePartialOrder)
             , ctxthms   = ctx_thms p_context -- The patterns/processes to be printed in the functional specification. (for making partial documentation)
             , ctxpats   = pats          -- The patterns defined in this context
                                         -- Each pattern contains all user defined rules inside its scope
             , ctxprocs  = procs         -- The processes defined in this context
             , ctxrs     = ctxrules
             , ctxds     = adecs         -- The declarations defined in this context, outside the scope of patterns
             , ctxcds    = acds          -- All concept definitions
             , ctxks     = keys          -- The key definitions defined in this context, outside the scope of patterns
             , ctxgs     = agens         -- The gen definitions defined in this context, outside the scope of patterns
             , ctxifcs   = ifcs          -- The interfaces defined in this context, outside the scope of patterns
             , ctxps     = apurp         -- The purposes defined in this context, outside the scope of patterns
             , ctxsql    = sqlPlugs      -- user defined sqlplugs, taken from the Ampersand script
             , ctxphp    = phpPlugs      -- user defined phpplugs, taken from the Ampersand script
             , ctxenv    = (ERel(V (Sign ONE ONE)) ,[])
             , ctxmetas  = [ Meta pos metaObj nm val | P_Meta pos metaObj nm val <- ctx_metas p_context ]
             , ctxexperimental = ctx_experimental p_context
             , ctxatoms  = allexplicitatoms
             }
    st = (nub . concat . map (typing p_context anything anything) . expressions) p_context
    (typeTable,stGraph,sccGraph,ambGraph) = tableOfTypes st
    typeErrors = calcTypeErrors p_context typeTable st
    (stTypeGraph,condensedGraph,ambiguityGraph) = typeAnimate st
    cxerrs = typeErrors++patcxes++rulecxes++keycxes++interfacecxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes++declnmchk++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ procnmchk ++ cyclicInterfaces
    hierarchy = 
        let ctx_gens = p_gens p_context
        in [(a (gen_spc g),a (gen_gen g)) | g<-ctx_gens]
        where a pc = C {cptnm = p_cptnm pc
                       ,cptgE = fatal 63 "do not refer to this concept"
                       ,cptos = fatal 64 "do not refer to this concept"
                       ,cpttp = fatal 65 "do not refer to this concept"
                       ,cptdf = fatal 66 "do not refer to this concept"
                       }
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecs,   deccxes)   = (unzip . map (pDecl2aDecl allpops "NoPattern") . ctx_ds) p_context
    (apurp,   xplcxes)   = (unzip . map  pPurp2aPurp                      . ctx_ps   ) p_context
    (pats,    patcxes)   = (unzip . map (pPat2aPat   allpops)             . ctx_pats ) p_context
    (procs,   proccxes)  = (unzip . map (pProc2aProc allpops)             . ctx_PPrcs) p_context
    (ctxrules,rulecxes)  = (unzip . map (pRul2aRul   "NoPattern")         . ctx_rs   ) p_context
    (keys,    keycxes)   = (unzip . map  pKDef2aKDef                      . ctx_ks   ) p_context
    (ifcs,interfacecxes) = (unzip . map  pIFC2aIFC                        . ctx_ifcs ) p_context
    (sqlPlugs,sPlugcxes) = (unzip . map (pODef2aODef [] NoCast)           . ctx_sql  ) p_context
    (phpPlugs,pPlugcxes) = (unzip . map (pODef2aODef [] NoCast)           . ctx_php  ) p_context
    (allmbpops, popcxes) = (unzip . map  pPop2aPop                        . pops     ) p_context
    allpops    = [pop | Just pop<-allmbpops]
    allexplicitatoms = [cptos' | P_CptPopu cptos'<-pops p_context]
    pops pc
     = ctx_pops pc ++
       [ pop | pat<-ctx_pats pc,  pop<-pt_pop pat] ++
       [ pop | prc<-ctx_PPrcs pc, pop<-procPop prc]
    themeschk = case orphans of
                 []   -> []
                 [nm] -> [newcxe ("Theme '"++nm++"' is selected for output, but is not defined.")]
                 _    -> [newcxe ("The following themes are selected for output, but are not defined:\n   "++intercalate ", " orphans)]
                where orphans = ctxthms contxt>-themenames
                      themenames=[name p |p<-pats]++[name p |p<-procs]
    rulenmchk = nub [newcxe ("Rules with identical names at positions "++show(map origin rs))
                    |r<-rules contxt, let rs=[r' |r'<-rules contxt,name r==name r'],length rs>1]
    ifcnmchk  = nub [newcxe ("Interfaces with identical names at positions "++show(map origin xs))
                    |ifc<-ifcs, let xs=[ifc' |ifc'<-ifcs,name ifc==name ifc'],length xs>1]
    patnmchk  = nub [newcxe ("Patterns with identical names at positions "++show(map origin xs))
                    |p<-pats, let xs=[p' |p'<-pats,name p==name p'],length xs>1]
    procnmchk = nub [newcxe ("Processes with identical names at positions "++show(map origin xs))
                    |p<-procs, let xs=[p' |p'<-procs,name p==name p'],length xs>1]
    declnmchk = nub [newcxe ("Declarations with comparable signatures at positions "++show(map origin ds))
                    | d<-declarations contxt, decusr d
                    , let ds=[d' | d'<-declarations contxt, decusr d'
                                 , name d==name d'
                                 , sign d <==> sign d']
                    , length ds>1]
    cyclicInterfaces = [ newcxe $ "These interfaces form a reference cycle:\n" ++
                                  unlines [ "- " ++ show ifcNm ++ " at " ++ show (origin $ lookupInterface ifcNm)
                                          | ifcNm <- iCycle ]
                       | iCycle <- getCycles refsPerInterface ]
      where refsPerInterface = [(ifcName ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ifcs ]
            getDeepIfcRefs obj = case objmsub obj of
                                   Nothing                -> []
                                   Just (InterfaceRef nm) -> [nm]
                                   Just (Box objs)        -> concatMap getDeepIfcRefs objs
            lookupInterface nm = case [ ifc | ifc <- ifcs, ifcName ifc == nm ] of
                                   [ifc] -> ifc
                                   _     -> fatal 124 "Interface lookup returned zero or more than one result"

    pPat2aPat :: [Population] -> P_Pattern -> (Pattern, CtxError)
    pPat2aPat pops ppat 
     = (A_Pat { ptnm  = name ppat    -- Name of this pattern
              , ptpos = pt_pos ppat  -- the position in the file in which this pattern was declared.
              , ptend = pt_end ppat  -- the position in the file in which this pattern was declared.
              , ptrls = prules       -- The user defined rules in this pattern
              , ptgns = agens        -- The generalizations defined in this pattern
              , ptdcs = adecs        -- The declarations declared in this pattern
              , ptkds = keys         -- The key definitions defined in this pattern
              , ptxps = xpls         -- The purposes of elements defined in this pattern
              }
       ,CxeOrig (cxelist (rulecxes++keycxes++deccxes++xplcxes)) "pattern" (name ppat) (origin ppat) )
       where
        (prules,rulecxes) = unzip arls
        arls  = map (pRul2aRul (name ppat)) (pt_rls ppat)
        agens = map (pGen2aGen (name ppat)) (pt_gns ppat)
        (keys,keycxes) = unzip akds
        akds  = map pKDef2aKDef (pt_kds ppat)
        (adecs,deccxes) = (unzip . map (pDecl2aDecl pops (name ppat)) . pt_dcs) ppat
        (xpls,xplcxes) = (unzip . map pPurp2aPurp . pt_xps) ppat
    
    pProc2aProc :: [Population] -> P_Process -> (Process,CtxError)
    pProc2aProc pops pproc
     = (Proc { prcNm    = procNm pproc
             , prcPos   = procPos pproc
             , prcEnd   = procEnd pproc
             , prcRules = prules
             , prcGens  = agens          -- The generalizations defined in this pattern
             , prcDcls  = adecs          -- The declarations declared in this pattern
             , prcRRuls = arruls         -- The assignment of roles to rules.
             , prcRRels = arrels         -- The assignment of roles to Relations.
             , prcKds   = keys           -- The key definitions defined in this process
             , prcXps   = expls          -- The purposes of elements defined in this process
             }
       ,CxeOrig (cxelist (rulecxes++keycxes++deccxes++rrcxes++editcxes++explcxes)) "process" (name pproc) (origin pproc) )
       where
        (prules,rulecxes) = (unzip . map (pRul2aRul (name pproc)) . procRules) pproc
        arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
        (rrels,editcxes)  = (unzip . map pRRel2aRRel            . procRRels) pproc
        agens  = map (pGen2aGen (name pproc)) (procGens pproc)
        arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
        (adecs,deccxes)   = (unzip . map (pDecl2aDecl pops (name pproc)) . procDcls) pproc
        (rruls,rrcxes)    = (unzip . map  pRRul2aRRul                    . procRRuls) pproc
        (keys,keycxes)    = (unzip . map  pKDef2aKDef                    . procKds) pproc
        (expls,explcxes)  = (unzip . map  pPurp2aPurp                    . procXps) pproc
 
    pRRul2aRRul :: RoleRule -> (RoleRule,CtxError)
    pRRul2aRRul prrul
     = ( prrul, CxeOrig (cxelist rrcxes) "role rule" "" (origin prrul))
       where
         rrcxes = [ newcxe ("Rule '"++r++" does not exist.")
                  | r<-mRules prrul, null [rul | rul<-rules contxt, name rul==r]]
         
    pRRel2aRRel :: P_RoleRelation -> (RoleRelation,CtxError)
    pRRel2aRRel prrel
     = ( RR { rrRoles = rr_Roles prrel
            , rrRels  = rels
            , rrPos   = rr_Pos prrel
            }
       , CxeOrig (cxelist editcxes) "role relation" "" (origin prrel))
       where
         (rels,editcxes) = unzip [ pRel2aRel (psign sgn) r
                                 | PTyp _ r@(Prel{}) sgn<-rr_Rels prrel
                                                          ++fatal 547 ("Untyped relation(s) "++ intercalate ", " [nm | Prel _ nm<-rr_Rels prrel])
                                 ]
    
    p2aPairView :: Sign -> P_PairView -> (PairView,CtxError)
    p2aPairView sgn (P_PairView ppvs) = (PairView pvs, cxelist errs) 
     where (pvs, errs) = unzip $ map (p2aPairViewSegment sgn) ppvs
    
    p2aPairViewSegment :: Sign -> P_PairViewSegment -> (PairViewSegment,CtxError)
    p2aPairViewSegment  _  (P_PairViewText str)          = (PairViewText str, cxenone)
    p2aPairViewSegment sgn (P_PairViewExp srcOrTgt pexp) = (PairViewExp srcOrTgt aexpr, exprcxe)
        where (aexpr,exprcxe) = pExpr2aExpr (SourceCast $ segSrcType sgn srcOrTgt) pexp
              segSrcType (Sign srcType _) Src = srcType 
              segSrcType (Sign _ tgtType) Tgt = tgtType
               
    pRul2aRul :: String -> P_Rule -> (Rule,CtxError)
    pRul2aRul patname prul        -- for debugging the parser, this is a good place to put     error (show (rr_exp prul))
     = (Ru { rrnm  = rr_nm prul                 -- Name of this rule
           , rrexp = aexpr                      -- The rule expression
           , rrfps = rr_fps prul                -- Position in the Ampersand file
           , rrmean = meanings (rr_mean prul)   -- Ampersand generated meaning (for all known languages)
           , rrmsg = map (pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt)) $ rr_msg prul
           , rrviol = mviol
           , rrtyp = sign aexpr                 -- Allocated type
           , rrdcl = Nothing                    -- The property, if this rule originates from a property on a Declaration
           , r_env = patname                    -- Name of pattern in which it was defined.
           , r_usr = True                       -- True if this rule was specified explicitly as a rule in the Ampersand script;
                                                -- False if it follows implicitly from the Ampersand script and generated by a computer
           , r_sgl = or [rr_nm prul `elem` map (name.snd) (prcRRuls p) | p<-ctxprocs contxt]  -- True if this is a signal; False if it is an ALWAYS rule
           , srrel = -- the signal relation
                     Sgn { decnm = rr_nm prul
                         , decsgn = sign aexpr
                         , decprps = []
                         , decprps_calc = []
                         , decprL = ""
                         , decprM = ""
                         , decprR = ""
                         , decMean = meanings (rr_mean prul)
                         , decConceptDef = Nothing
                         , decpopu = []
                         , decfpos = rr_fps prul
                         , deciss = True
                         , decusr = False
                         , decpat = ""
                         , decplug = True
                         }
           }
       , CxeOrig (cxelist [exprcxe, mviolcxe]) "rule" "" (origin prul)
       )
       where (aexpr,exprcxe) = pExpr2aExpr NoCast (rr_exp prul)
             (mviol, mviolcxe) = case fmap (p2aPairView $ sign aexpr) $ rr_viol prul of
                                   Nothing              -> (Nothing, cxenone)
                                   Just (viol, violcxe) -> (Just viol, violcxe)
             meanings = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) 
    pMeanings2aMeaning :: Lang          -- The default language
                      -> PandocFormat  -- The default format
                      -> [PMeaning]
                      -> AMeaning
    pMeanings2aMeaning ldefLang defFormat pms
       = AMeaning (map (pMeaning2Amarkup ldefLang defFormat) pms)
         where  pMeaning2Amarkup l f (PMeaning pm)
                 = pMarkup2aMarkup l f pm
                   
    pMarkup2aMarkup :: Lang          -- The default language
                    -> PandocFormat  -- The default format
                    -> P_Markup
                    -> A_Markup
    pMarkup2aMarkup defLang defFormat pm
       = A_Markup { amLang   = fromMaybe defLang (mLang pm)
                  , amFormat = fmt
                  , amPandoc = string2Blocks fmt (mString pm)
                  }
               where fmt = fromMaybe defFormat (mFormat pm)
               
    -- | pKDef2aKDef checks compatibility of composition with key concept on equality
    pKDef2aKDef :: P_KeyDef -> (KeyDef, CtxError)
    pKDef2aKDef pkdef
     = (Kd { kdpos = kd_pos pkdef
           , kdlbl = kd_lbl pkdef
           , kdcpt = c
           , kdats = segs
                        }
       , CxeOrig (cxelist (nmchk:kdcxe:duplicateKeyErrs:multipleKeyErrs:segscxes)) "key definition" "" (origin pkdef) )
       where
        (segs, segscxes) = unzip . map (pKeySeg2aKeySeg c) $ kd_ats pkdef
        c  = pCpt2aCpt (kd_cpt pkdef)
        -- check equality
        ats = [ expr | KeyExp expr <- segs ]
        kdcxe = newcxeif (nocxe (cxelist segscxes) && length (nub (c:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of expression " ++ showADL (objctx x) 
                                            ++" ("++showADL (source (objctx x))++") is compatible, but not equal to the key concept ("++ showADL c ++ ")."
                                           |x<-ats,source (objctx x)/=c])
        nmchk  = cxelist$nub [ newcxe ("Sibling objects with identical names at positions "++show(map origin xs))
                             | P_KeyExp at<-kd_ats pkdef, let xs=[ at' | P_KeyExp at'<-kd_ats pkdef,name at==name at' ],length xs>1]
        duplicateKeyErrs = newcxeif (length (filter (\k -> name k == kd_lbl pkdef) $ keyDefs contxt) > 1) $
                             "Duplicate key name \""++kd_lbl pkdef++"\" at "++show (origin pkdef)  
        multipleKeyErrs = newcxeif (length (filter (\k -> name (kdcpt k) == name c) $ keyDefs contxt) > 1) $
                             "Multiple keys for concept  \""++name c++"\" at "++show (origin pkdef)  
    
    -- (ats,atscxes)  = (unzip . map (pODef2aODef (SourceCast c)) . kd_ats) pkdef
    pKeySeg2aKeySeg :: A_Concept -> P_KeySegment -> (KeySegment, CtxError)
    pKeySeg2aKeySeg _      (P_KeyText str)   = (KeyText str, cxenone)
    pKeySeg2aKeySeg _      (P_KeyHtml str)   = (KeyHtml str, cxenone)
    pKeySeg2aKeySeg concpt (P_KeyExp keyExp) = let (objDef, cxe) = pODef2aODef [] (SourceCast concpt) keyExp
                                                    in ( KeyExp objDef, cxe)
      
    
    -- TODO -> Does pIFC2aIFC require more checks? What is the intention of params, viols, args i.e. the interface data type?
    pIFC2aIFC :: P_Interface -> (Interface,CtxError)
    pIFC2aIFC pifc 
     = (Ifc { ifcName   = ifc_Name pifc
            , ifcParams = prms
            , ifcViols  = fatal 206 "not implemented ifcViols"
            , ifcArgs   = ifc_Args pifc
            , ifcRoles  = ifc_Roles pifc
            , ifcObj    = obj
            , ifcPos    = ifc_Pos pifc
            , ifcExpl   = ifc_Expl pifc
            }
       , CxeOrig (cxelist (objcxe:prmcxes++duplicateRoleErrs++undeclaredRoleErrs)) "interface" (name pifc) (origin pifc) )
       where
        parentIfcRoles = if null $ ifc_Roles pifc then roles contxt else ifc_Roles pifc -- if no roles are specified, the interface supports all roles
        (obj,objcxe)  = pODef2aODef parentIfcRoles NoCast (ifc_Obj pifc)
        (prms,prmcxes)  = unzip [pRel2aRel (psign sgn) r
                                | PTyp _ r@(Prel{}) sgn<-ifc_Params pifc -- Todo: link untyped relations to their type!
                                                         ++fatal 669 ("Untyped relation(s) "++ intercalate ", " [nm | (Prel _ nm)<-ifc_Params pifc])
                                ]
        duplicateRoleErrs = [newcxe $ "Duplicate interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, length (filter (==role) $ ifc_Roles pifc) > 1 ]
        undeclaredRoleErrs = if null duplicateRoleErrs then [newcxe $ "Undeclared interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, role `notElem` roles contxt ]
                                                       else []
        -- we show the line nr for the interface, which may be slightly inaccurate, but roles have no position 
        -- and the implementation of error messages makes it difficult to give a nice one here
        
    -- | pODef2aODef checks compatibility of composition of expressions on equality
    pODef2aODef :: [String] -> AutoCast -> P_ObjectDef -> (ObjectDef,CtxError)
    pODef2aODef parentIfcRoles cast podef 
     = (Obj { objnm   = obj_nm podef
            , objpos  = obj_pos podef
            , objctx  = expr
            , objmsub = msub
            , objstrs = obj_strs podef
            }
       , CxeOrig (cxelist (nmchk : exprcxe : msubcxes)) "object definition" "" (origin podef) )
       where
        nmchk  = cxelist$nub [newcxe ("Sibling objects with identical names at positions "++show(map origin xs))
                             |at<-getSubPObjs podef, let xs=[at' |at'<-getSubPObjs podef,name at==name at'],length xs>1]
        getSubPObjs P_Obj { obj_msub = Just (P_Box objs) } = objs
        getSubPObjs _                                      = []
        -- Step1: check obj_ctx
        (expr,exprcxe)  = pExpr2aExpr cast (obj_ctx podef)
        -- Step2: check obj_ats in the context of expr
        (msub,msubcxes) = p2a_MaybeSubInterface parentIfcRoles (target expr) $ obj_msub podef
        -- Step3: compute type error messages
        {- SJ 4th jan 2012: I have disabled odcxe in order to find out why it is necessary to check equality. We should run in trouble if this check is indeed necessary...
        odcxe
         | nocxe exprcxe && nocxe atscxes = eqcxe   -- equality check disabled (see below)
         | nocxe exprcxe && not(nocxe atscxes) 
              -- the nature of an atscxe is unknown and may be caused by the SourceCast. If so, a note on the type of expr is useful .
            = cxelist [atscxes,newcxe ("Note that the type of "++ showADL expr ++ " at " ++ show(origin podef) ++ " is "++ show (sign expr) ++ ".")]
         | otherwise     = exprcxe
        -- Step4: check equality  -- Why is this necessary? Compatible should be enough...
        eqcxe = newcxeif (length (nub (target expr:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of expression " 
                                            ++ showADL (objctx x) ++" ("++showADL (source (objctx x))++")"
                                            ++ " is compatible, but not equal to the target of expression "
                                            ++ showADL expr       ++" ("++showADL (target expr) ++ ")."
                                           |x<-ats,source (objctx x)/=target expr])
        -}
        
    p2a_MaybeSubInterface :: [String] -> A_Concept -> Maybe P_SubInterface -> (Maybe SubInterface, [CtxError])
    p2a_MaybeSubInterface _              _    Nothing = (Nothing, [])
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_Box p_objs)) =
      let (objs, errs) = unzip [pODef2aODef parentIfcRoles (SourceCast conc) p_obj | p_obj<-p_objs] 
      in  (Just $ Box objs, errs)
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_InterfaceRef pos nm)) =
      (Just $ InterfaceRef nm, [err])
     where err = case [ifc | ifc <- interfaces contxt, name ifc == nm ] of
                   []                                     -> newcxe $ "Undeclared interface \""++nm++"\" at " ++show pos ++ "."
                   (_:_:_)                                -> fatal 350 $ "Multiple interfaces for ref "++nm
                   [Ifc { ifcObj = Obj {objctx= ifcExp}, ifcRoles = thisIfcRoles }] ->
                     if source ifcExp < conc
                     then newcxe $ "Incompatible interface "++show nm++" at "++show pos++":"++
                                   "\nInterface source concept "++name (source ifcExp)++" is not equal to or a supertype of "++name conc
                     else let unsupportedRoles = if null thisIfcRoles
                                                 then [] -- no roles specified means all roles are supported
                                                 else parentIfcRoles \\ thisIfcRoles
                          in  newcxeif (not $ null unsupportedRoles) $
                             "Interface "++show nm++", referenced at "++show pos++", does not support all roles of the containing interface. "++
                             "Unsupported roles: "++ intercalate ", " unsupportedRoles ++"."
      
    pPurp2aPurp :: PPurpose -> (Purpose, CtxError)
    pPurp2aPurp pexpl
     = ( Expl { explPos      = pexPos   pexpl
              , explObj      = explobs
              , explMarkup   = pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt) (pexMarkup pexpl)
              , explRefId    = pexRefID pexpl
              , explUserdefd = True
             -- , explCont  = string2Blocks (ctxmarkup contxt) (pexExpl  pexpl)
              }
       , CxeOrig xplcxe "explanation" "" (origin pexpl))
       where (explobs,xplcxe) = pExOb2aExOb (pexObj   pexpl)
             
    
    pExOb2aExOb :: PRef2Obj -> (ExplObj, CtxError)
    pExOb2aExOb (PRef2ConceptDef str  ) = (ExplConceptDef (head cds), newcxeif(null cds)("No concept definition for '"++str++"'"))
                                          where cds = [cd | cd<-conceptDefs contxt, cdcpt cd==str ]
    pExOb2aExOb (PRef2Declaration x@(PTyp o (Prel _ nm) sgn))
                                        = ( ExplDeclaration (head decls)
                                          , if null decls
                                            then CxeOrig (newcxe ("No declaration for '"++showADL x++"'")) "relation" nm o
                                            else CxeNone
                                          )
                                          where decls = [d | d<-declarations contxt, name d==nm, sign d==pSign2aSign sgn ]
    pExOb2aExOb (PRef2Rule str        ) = (ExplRule (head ruls), newcxeif(null ruls)("No rule named '"++str++"'") )
                                          where ruls = [rul | rul<-rules contxt, name rul==str ]
    pExOb2aExOb (PRef2KeyDef str      ) = (ExplKeyDef (head kds), newcxeif(null kds)("No key definition named '"++str++"'") )
                                          where kds = [kd | kd<-keyDefs contxt, name kd==str]
    pExOb2aExOb (PRef2Pattern str     ) = (ExplPattern str,   newcxeif(null[pat |pat<-patterns   contxt,   name pat==str])("No pattern named '"++str++"'") )
    pExOb2aExOb (PRef2Process str     ) = (ExplProcess str,   newcxeif(null[prc |prc<-processes  contxt,  name prc==str]) ("No process named '"++str++"'") )
    pExOb2aExOb (PRef2Interface str   ) = (ExplInterface str, newcxeif(null[ifc |ifc<-interfaces contxt, name ifc==str])  ("No interface named '"++str++"'") )
    pExOb2aExOb (PRef2Context str     ) = (ExplContext str,   newcxeif(name contxt/=str) ("No context named '"++str++"'") )  
    pExOb2aExOb (PRef2Fspc str        ) = (ExplFspc str,      newcxeif(name contxt/=str) ("No specification named '"++str++"'") )
    
    pPop2aPop :: P_Population -> (Maybe Population,CtxError)
    pPop2aPop (P_CptPopu{}) = (Nothing,cxenone)
    pPop2aPop p@(P_Popu{})
     = ( Just (Popu { popm  = aRel
                    , popps = p_popps p
                    })
       , relcxe
       )
       where (ERel aRel, relcxe) = pExpr2aExpr NoCast (PTyp (origin p) (Prel (origin p) (name p)) (p_type p))
    
    pGen2aGen :: String -> P_Gen -> A_Gen
    pGen2aGen patNm pg
       = Gen{genfp  = gen_fp  pg
            ,gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            ,genpat = patNm
            }
              
    pSign2aSign :: P_Sign -> Sign
    pSign2aSign (P_Sign cs) = Sign (head ts) (last ts)
      where ts = map pCpt2aCpt cs
            
    pCpt2aCpt :: P_Concept -> A_Concept
    pCpt2aCpt pc
        = case pc of
            PCpt{} -> c 
            P_Singleton -> ONE
          where 
          c = C {cptnm = p_cptnm pc
                ,cptgE = genE contxt
                ,cptos = nub$[srcPaire p | d<-declarations contxt,decusr d,p<-contents d, source d <= c]
                           ++[trgPaire p | d<-declarations contxt,decusr d,p<-contents d, target d <= c]
                           ++[v | r<-rules contxt,Mp1 v c'<-mors r,c'<=c]
                           ++[x | (cnm,xs)<-initialatoms contxt, cnm==p_cptnm pc, x<-xs]
                ,cpttp = head ([cdtyp cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]++[""])
                ,cptdf = [cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]
                }
    
    pDecl2aDecl :: [Population] -> String -> P_Declaration -> (Declaration, CtxError)
    pDecl2aDecl pops patname pd =
     ( Sgn { decnm   = dec_nm pd
           , decsgn  = pSign2aSign (dec_sign pd)
           , decprps = dec_prps pd
           , decprps_calc = dec_prps pd --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
           , decprL  = dec_prL pd
           , decprM  = dec_prM pd
           , decprR  = dec_prR pd
           , decMean = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) (dec_Mean pd)
           , decConceptDef = dec_conceptDef pd
           , decpopu = nub$    -- All populations from the P_structure will be assembled in the decpopu field of the corresponding declaratio
                       dec_popu pd ++ 
                       concat [popps pop | pop<-pops, let ad=popm pop
                                         , name ad==name pd
                                         , relsgn ad==pSign2aSign (dec_sign pd)
                                         ]
           , decfpos = dec_fpos pd 
           , deciss  = True
           , decusr  = True
           , decpat  = patname
           , decplug = dec_plug pd
           }
      , case dec_conceptDef pd of 
          Just (RelConceptDef srcOrTgt _) | relConceptName (dec_nm pd) `elem` map name (concs contxt) -> 
            CxeOrig (newcxe ("Illegal DEFINE "++showSRCorTGT++" for relation "++show (dec_nm pd)++". Concept "++
                             relConceptName (dec_nm pd)++" already exists."))
                    "declaration" "" (origin pd)
             where showSRCorTGT = if srcOrTgt == Src then "SRC" else "TGT"
                   relConceptName ""     = fatal 472 "empty concept"
                   relConceptName (c:cs) = toUpper c : cs
          _ -> cxenone
      )
      
    -- | p2a for isolated references to relations. Use pExpr2aExpr instead if relation is used in an expression.
    pRel2aRel :: [P_Concept] -> P_Expression -> (Relation,CtxError)
    pRel2aRel _ (Pfull orig pConcepts)
     = case pConcepts of
        [] -> (fatal 326 "Ambiguous universal relation."
                              , CxeOrig (newcxe "Ambiguous universal relation.") 
                                        "relation" "" orig )
        [c] -> (V (Sign (pCpt2aCpt c) (pCpt2aCpt c)), CxeOrig cxenone "relation" "" orig)
        [s,t] -> (V (Sign (pCpt2aCpt s) (pCpt2aCpt t)), CxeOrig cxenone "relation" "" orig)
        _   -> fatal 328 "Encountered a Sign with more than two elements. This should be impossible."
    pRel2aRel _ (Pid orig pConcepts)
     = case pConcepts of
        [] -> (fatal 331 "Ambiguous identity relation."
                              , CxeOrig (newcxe
                                        "Ambiguous identity relation.") 
                                        "relation" "" orig )
        [c] -> (I (pCpt2aCpt c), CxeOrig cxenone "relation" "" orig)
        _   -> fatal 341 "Encountered a Sign with more than one element. This should be impossible."
    pRel2aRel _ (Patm orig atom pConcepts) 
     = case pConcepts of
        [] -> (fatal 343 "Ambiguous value."
                              , CxeOrig (newcxe
                                        "Ambiguous value.") 
                                        "relation" "" orig )
        [c] -> (Mp1 atom (pCpt2aCpt c), CxeOrig cxenone "relation" "" orig)
        _   -> fatal 354 "Encountered a Sign with more than one element. This should be impossible."
    pRel2aRel sgn (Prel orig nm)
     = case (ds,dts,sgn,unknowncpts) of
        ( _ , _ , _ ,c:cs) -> ( fatal 324 ("Unknown concept in a relation named '"++nm++".")
                              , cxelist [ newcxeif (null cs)      ("Unknown concept: '"++name c++"'.")
                                        , newcxeif (not(null cs)) ("Unknown concepts: '"++name c++"' and '"++name (head cs)++"'." )
                                        ]
                              )
                              --        "relation" "" (origin prel) )
        ([] , _ , _ , _  ) -> ( fatal 329 ("Relation undeclared: '"++nm++".")
                              , newcxe ("Relation undeclared: '"++nm++"'.")
                              )
                              --        "relation" "" (origin prel) )
        ([d],[] ,[] , _  ) -> (makeRelation d, CxeOrig cxenone "relation" "" orig)
        ([d],[] , _ , _  ) -> ( fatal 334 ("Relation undeclared: '"++nm++".")
                              , newcxe ("Relation undeclared: '"++nm++show sgn++"'."
                                        ++".\nDo you intend the one with type "++(show.sign) d++"?")
                              )
                              --        "relation" "" (origin prel) )
        ( _ ,[d], _ , _  ) -> (makeRelation d, CxeOrig cxenone "relation" "" orig)
        ( _ ,[] ,[] , _  ) -> ( fatal 340 ("Ambiguous reference to a relation named: '"++nm++".")
                              , newcxe ("Ambiguous relation: '"++nm++"'.\nUse the full relation signature."
                                        ++"\nPossible types are "++concatMap (show.sign) ds++".")
                              )
                              --        "relation" "" (origin prel) )
        ( _ ,[] , _ , _  ) -> ( fatal 345 ("Illegal reference to a relation named '"++nm++".")
                              , newcxe ("Relation undeclared: '"++nm++show sgn++"'."
                                        ++"\nPossible types are "++concatMap (show.sign) ds++".")
                              )
                              --        "relation" "" (origin prel) )
        (_ : (_ : _), _ : (_ : _), [], []) -> fatal 350 "dts should be empty because dts=[..|.., not(null sgn), ..]"
        (_ : (_ : _), _ : (_ : _), _ : _, []) -> fatal 351 ("length dts should be at most 1 when not(null sgn)\n"++show dts)
        ([_], _ : (_ : _), _, []) -> fatal 352 "More ds than dts should be impossible due to implementation of dts i.e. dts=[d |d<-ds,condition]"
       where
        unknowncpts = nub[c |c<-sgn, pCpt2aCpt c `notElem` concs contxt]
        ds  = [d | d<-declarations contxt, name d==nm]
        dts = [d | d<-ds, not(null sgn)
                        , name (head sgn)==name (source d) &&
                          name (last sgn)==name (target d)   ]

    pExpr2aExpr :: AutoCast -> P_Expression -> (Expression,CtxError)
    pExpr2aExpr typecast pExpr = (f pExpr, CxeNone)
       where
         f :: P_Expression -> Expression
         f (PTyp _ (Pid _ _) (P_Sign (c:_))) = ERel (I (pCpt2aCpt c))
         f x@(PTyp o (Pid _ _) (P_Sign _))  = fatal 983 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (Pid _ [c])     = ERel (I (pCpt2aCpt c))
         f x@(Pid o _ )    = fatal 985 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (Pnid c)        = ECpl (ERel (I (pCpt2aCpt c)))
         f (Patm _ atom [c]) = ERel (Mp1 atom (pCpt2aCpt c))
         f x@Pnull         = fatal 988 ("pExpr2aExpr cannot transform "++show x++" to an expression.")
         f (Pfull _ [s,t]) = ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt t)))
         f (Pfull _ [s])   = ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt s)))
         f x@(Pfull o [])  = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (Prel o a)      = ERel (Rel{relnm=a, relpos=o})
         f (Pflp o a)      = EFlp (f (Prel o a))
         f (Pequ _ a b)    = EEqu (f a, f b)
         f (Pimp _ a b)    = EImp (f a, f b)
         f (PIsc o (PIsc _ a b) c) = EIsc (a_b_s++[f c]) where EIsc a_b_s = f (PIsc o a b)
         f (PIsc o a (PIsc _ b c)) = EIsc ([f a]++b_c_s) where EIsc b_c_s = f (PIsc o b c)
         f (PIsc _ a b)            = EIsc [f a, f b]
         f (PUni o (PUni _ a b) c) = EUni (a_b_s++[f c]) where EUni a_b_s = f (PUni o a b)
         f (PUni o a (PUni _ b c)) = EUni ([f a]++b_c_s) where EUni b_c_s = f (PUni o b c)
         f (PUni _ a b)            = EUni [f a, f b]
         f (PDif _ a b)    = EDif (f a, f b)
         f (PLrs _ a b)    = ELrs (f a, f b)
         f (PRrs _ a b)    = ERrs (f a, f b)
         f (PCps o (PCps _ a b) c) = ECps (a_b_s++[f c]) where ECps a_b_s = f (PCps o a b)
         f (PCps o a (PCps _ b c)) = ECps ([f a]++b_c_s) where ECps b_c_s = f (PCps o b c)
         f (PCps _ a b)            = ECps [f a, f b]
         f (PRad o (PRad _ a b) c) = ERad (a_b_s++[f c]) where ERad a_b_s = f (PRad o a b)
         f (PRad o a (PRad _ b c)) = ERad ([f a]++b_c_s) where ERad b_c_s = f (PRad o b c)
         f (PRad _ a b)            = ERad [f a, f b]
         f (PPrd o (PPrd _ a b) c) = EPrd (a_b_s++[f c]) where EPrd a_b_s = f (PPrd o a b)
         f (PPrd o a (PPrd _ b c)) = EPrd ([f a]++b_c_s) where EPrd b_c_s = f (PPrd o b c)
         f (PPrd _ a b)            = EPrd [f a, f b]
         f (PKl0 _ a)      = EKl0 (f a)
         f (PKl1 _ a)      = EKl1 (f a)
         f (PFlp _ a)      = EFlp (f a)
         f (PCpl _ a)      = ECpl (f a)
         f (PBrk _ a)      = EBrk (f a)
         f x@(PTyp o _ (P_Sign [])) = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (PTyp _ a sgn)  = ETyp (f a) (Sign (pCpt2aCpt s) (pCpt2aCpt t))
                             where P_Sign cs = sgn; s=head cs; t=last cs

    --the type checker always returns an expression with sufficient type casts, it should remove redundant ones.
    --applying the type checker on an complete, explicitly typed expression is equivalent to disambiguating the expression

disambiguate :: Fspc -> Expression -> Expression
disambiguate fSpec x = x -- temporarily disabled (19 july 2012), in order to get the type checker correct first...
{-
 | nocxe errs = expr 
 | otherwise  = fatal 428 ("an expression must be type correct, but this one is not:\n" ++ show errs)
 where
   (expr,errs) = pExpr2aExpr fSpec{vrels=vrels fSpec++deltas} NoCast (f x)
   -- f transforms x to a P_Expression using full relation signatures
   f (EEqu (l,r)) = Pequ OriginUnknown (f l) (f r)
   f (EImp (l,r)) = Pimp OriginUnknown (f l) (f r)
   f (EIsc [l])   = f l
   f (EIsc [l,r]) = PIsc OriginUnknown (f l) (f r)
   f (EIsc (l:rs))= PIsc OriginUnknown (f l) (f (EIsc rs))
   f (EIsc _)     = Pfull OriginUnknown []
   f (EUni [l])   = f l
   f (EUni [l,r]) = PUni OriginUnknown (f l) (f r)
   f (EUni (l:rs))= PUni OriginUnknown (f l) (f (EUni rs))
   f (EUni _)     = Pnull
   f (EDif (l,r)) = PDif OriginUnknown (f l) (f r)
   f (ELrs (l,r)) = PLrs OriginUnknown (f l) (f r)
   f (ERrs (l,r)) = PRrs OriginUnknown (f l) (f r)
   f (ECps es)    = foldr1 (PCps OriginUnknown) (map f es)
   f (ERad es)    = foldr1 (PRad OriginUnknown) (map f es)
   f (EPrd es)    = foldr1 (PPrd OriginUnknown) (map f es)
   f (EKl0 e)     = PKl0 OriginUnknown (f e)
   f (EKl1 e)     = PKl1 OriginUnknown (f e)
   f (EFlp e)     = PFlp OriginUnknown (f e)
   f (ECpl e)     = PCpl OriginUnknown (f e)
   f (EBrk e)     = PBrk OriginUnknown (f e)
   f (ETyp e _)   = f e
   f (ERel rel@(Rel{})) = PTyp OriginUnknown (Prel OriginUnknown (name rel))
                               (P_Sign [g (source rel),g (target rel)])
   f (ERel rel@(I{}))   = Pid OriginUnknown [g (source rel)]
   f (ERel rel@(V{}))   = Pfull OriginUnknown [g (source rel),g (target rel)]
   f (ERel rel@(Mp1{})) = Patm OriginUnknown (relval rel) [g (source rel)]
   g c@(C{}) = PCpt (name c) 
   g ONE     = P_Singleton
   deltas    = [ makeDeclaration r | r<-mors x, name r=="Delta" ]
-}

-- | An InfExpression yields a list of alternatives that are type correct (type: [Expression]) and a list of error messages (type: [String]).
--type InfExpression  = AutoCast -> ([Expression],[String])
-- | internal type to push down the type as far as known on the ERel, thus possibly with wild cards on source or target
data AutoCast = NoCast | SourceCast A_Concept | TargetCast A_Concept | Cast A_Concept A_Concept deriving (Show,Eq)
-- | AutoCast is not of class Association, but it should be flippable

-- The ordering of AutoCast is from less determined to more determined.
instance Poset AutoCast where
 Cast s t `compare` Cast s' t' = Sign s t `compare` Sign s' t'
 SourceCast s `compare` Cast s' _     = s `compare` s'
 SourceCast s `compare` SourceCast s' = s `compare` s'
 TargetCast t `compare` Cast _ t'     = t `compare` t'
 TargetCast t `compare` TargetCast t' = t `compare` t'
 NoCast       `compare` _             = CP
 _            `compare` NoCast        = CP
 _            `compare` _             = NC

{-
flpcast :: AutoCast -> AutoCast
flpcast NoCast = NoCast
flpcast (SourceCast x) = TargetCast x
flpcast (TargetCast x) = SourceCast x
flpcast (Cast x y) = Cast y x
-}

