{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters 
     ( pGen2aGen
     , pCpt2aCpt
     , pSign2aSign
     , pExpr2aExpr
     , pDecl2aDecl
 --    , pRel2aRel
     , pCtx2aCtx
     , pPat2aPat
     , pRul2aRul
     , pKDef2aKDef
     , pIFC2aIFC
     , pProc2aProc
     , pODef2aODef
     , disambiguate
     )
where
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.ADL1
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
The constructor TypExpr e flipped o origExpr is read as:
  "the source type of e, with e equal to (if flipped then PFlp origExpr else origExpr), and origExpr occurs in the P_Context p_context on location o."
origExpr (in conjunction with Origin o) is kept for the purpose of generating messages in terms of the original expression written by the user.
-}
data Type =  TypExpr P_Expression Bool Origin P_Expression
           | TypLub Type Type Origin P_Expression
           | TypGlb Type Type Origin P_Expression
            deriving (Eq, Show)

showType :: Type -> String
showType (TypExpr _ orig _       origExpr@(Pid _ [])) = showADL origExpr
showType (TypExpr _ orig _       origExpr@(Pid _ _))  = showADL origExpr
showType (TypExpr _ flipped orig expr)                = showADL (if flipped then PFlp orig expr else expr) ++"("++ show (origin expr)++")"
showType (TypLub a b orig _)                          = showType a++" ./\\. "++showType b
showType (TypGlb a b orig _)                          = showType a++" .\\/. "++showType b

instance Traced Type where
  origin (TypExpr _ _ o _) = o
  origin (TypLub _ _ o _) = o
  origin (TypGlb _ _ o _) = o


t_eq :: Type -> Type -> Bool
t_eq (TypExpr x _ _ _) (TypExpr y _ _ _) = x `p_eq` y
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
p_simplify (PLrs o a b)        = PRrs o (p_simplify a) (p_simplify b)
p_simplify (PRrs o a b)        = PLrs o (p_simplify a) (p_simplify b)
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
mSpecific = TypLub
mGeneric  = TypGlb

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

{- The purpose of 'typing' is to build a relation st::Type*Type, which represents a domain analysis.
For any two P_Expressions a and b,  if dom(a) is a subset of dom(b), this is represented as a tuple (TypExpr a _ _ _,TypExpr b _ _ _) in st.
In the code below, this shows up as  dom a.<.dom b
The function typing does a recursive scan through all expressions in a script, collecting all tuples on its way.
-}
typing :: [P_Gen] -> [P_Declaration] -> [P_Expression] -> [(Type, Type)] -- subtypes (.. is subset of ..)
typing isas decls exprs
 = nub ([ tuple 
        | expr<-map p_simplify exprs
        , tuple<-uType anything anything expr
        ]++
        [ st| g<-isas
            , let spc=Pid (origin g) [gen_spc g]
            , let gen=Pid (origin g) [gen_gen g]
            , let x=Pimp (origin g) spc gen
            , st<-dom x spc.<.dom x gen
        ])
   where
     anything = TypExpr (Pfull OriginUnknown []) False OriginUnknown (Pfull OriginUnknown [])
     pDecls = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) | d<-decls]
     uType :: Type -> Type -> P_Expression -> [(Type, Type)]
     uType _    _     (Pid{})               = nothing                                                              -- I[C]
     uType _    _     (Pnid _)              = fatal 136 "Pnid has no representation"
                                            -- dom x x.=.dom x (Pid [c]) .+. cod x x.=.cod x (Pid [c])                     -- These rules apply for  -I[C] (i.e. Pnid, if it were represented)
     uType _    _     (Patm _ _ [])         = nothing                                                              -- 'Piet'   (an untyped singleton)
     uType _    _   x@(Patm o _ cs)         = dom x x.<.dom x (Pid o [head cs]) .+. cod x x.<.cod x (Pid o [head cs])      -- 'Piet'[Persoon]  (a typed singleton)
     uType _    _      Pnull                = nothing                                                              -- -V     (the empty set)
     uType _    _     (Pfull _ [])          = nothing                                                              --  V     (the untyped full set)
     uType _    _   x@(Pfull o cs)          = dom x x.=.dom x (Pid o [head cs]) .+. cod x x.=.cod x (Pid o [last cs])      --  V[A*B] (the typed full set)
     uType _    _   x@(Prel _ nm)           = foldr (.+.) [] [ dom x x.<.dom x decl .+. cod x x.<.cod x decl | decl<-dcs ] --  r      a relation
                                              where dcs = [ decl | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
     uType _    _   x@(Pflp _ nm)           = foldr (.+.) [] [ dom x x.<.cod x decl .+. cod x x.<.dom x decl | decl<-dcs ] --  r~     a flipped relation
                                              where dcs = [ decl | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
     uType uLft uRt x@(Pequ _ a b)          = dom x a.=.dom x x .+. cod x a.=.cod x x .+. dom x b.=.dom x x .+. cod x b.=.cod x x  --  a=b    equality
                                               .+. uType uLft uRt a .+. uType uLft uRt b 
{- A direct way, which requires proof
     uType uLft uRt x@(Pimp _ a b)          = dom x a.<.dom x x .+. cod x a.<.cod x x .+.                                  --  a|-b   implication (aka: subset)
                                              dom x b.<.dom x x .+. cod x b.<.cod x x .+.
                                              uType uLft uRt a .+. uType uLft uRt b
-}
-- A more indirect way, which requires no proof
     uType uLft uRt x@(Pimp o a b)          = dom x x.=.dom x e .+. cod x x.=.cod x e .+. uType uLft uRt e                 --  a|-b   implication (aka: subset)
                                              where e = Pequ o a (PIsc o a b)
--
     uType  _    _  x@(PIsc o a b)          = dom x x.<.dom x a .+. cod x x.<.cod x a .+. dom x x.<.dom x b .+. cod x x.<.cod x b  --  intersect ( /\ )
                                              .+. uType (mSpecific (dom x a) (dom x b) o x) (mSpecific (cod x a) (cod x b) o x) a
                                              .+. uType (mSpecific (dom x a) (dom x b) o x) (mSpecific (cod x a) (cod x b) o x) b
     uType  _    _  x@(PUni o a b)          = dom x a.<.dom x x .+. cod x a.<.cod x x .+. dom x b.<.dom x x .+. cod x b.<.cod x x  --  union     ( \/ )
                                              .+. uType (mGeneric (dom x a) (dom x b) o x) (mGeneric (cod x a) (cod x b) o x) a
                                              .+. uType (mGeneric (dom x a) (dom x b) o x) (mGeneric (cod x a) (cod x b) o x) b
     uType uLft uRt x@(PDif _ a b)          = dom x x.<.dom x a .+. cod x x.<.cod x a                                      --  a-b    (difference)
                                               .+. uType uLft uRt a .+. uType (dom x a) (cod x a) b
     uType uLft uRt x@(PLrs o a b)          = dom x x.=.dom x e .+. cod x x.=.cod x e .+. uType uLft uRt e                 -- a!b      relative addition
                                              where e = PRad o a (complement (p_flp b))
     uType uLft uRt x@(PRrs o a b)          = dom x x.=.dom x e .+. cod x x.=.cod x e .+. uType uLft uRt e                 -- a!b      relative addition
                                              where e = PRad o (complement (p_flp a)) b
     uType uLft uRt   (PCps _ (Pid _ []) b) = uType uLft uRt b                                                     -- I;b
     uType  _   uRt x@(PCps o a@(Pid{}) b)  = dom x x.=.(mSpecific (cod x a) (dom x b) o x) .+. cod x x.<.cod x b .+.              -- I[C];b   composition
                                              uType (mSpecific (cod x a) (dom x b) o x) uRt b
     uType uLft uRt   (PCps _ a (Pid _ [])) = uType uLft uRt a                                                     -- a;I      composition
     uType uLft  _  x@(PCps o a b@(Pid{}))  = dom x x.<.dom x a .+. cod x x.=.mSpecific (cod x a) (dom x b) o x .+.              -- a;I[C]   composition
                                              uType uLft (mSpecific (cod x a) (dom x b) o x) a
     uType uLft uRt x@(PCps o a b)          = dom x x.<.dom x a .+. cod x x.<.cod x b .+.                                  -- a;b      composition
                                              mSpecific (cod x a) (dom x b) o x.<.cod x a .+.
                                              mSpecific (cod x a) (dom x b) o x.<.dom x b .+.
                                              uType uLft (mSpecific (cod x a) (dom x b) o x) a .+. uType (mSpecific (cod x a) (dom x b) o x) uRt b
     uType uLft uRt x@(PRad o a b)          = dom x x.=.dom x e .+. cod x x.=.cod x e .+. uType uLft uRt e                 -- a!b      relative addition
                                              where e = PCps o (complement a) (complement b)
{- the elaborated version of uType for PRad
     uType uLft uRt x@(PRad _ a@(Pnid{}) b) = dom x x.=.mGeneric (cod x a) (dom x b) o x .+. cod x x.<.cod x b .+. st_b          -- -I[C]!b  relative addition
                                              where st_b = uType (mGeneric (cod x a) (dom x b) o x) uRt a
     uType uLft uRt x@(PRad _ a b@(Pnid{})) = dom x x.<.dom x a .+. cod x x.=.mGeneric (cod x a) (dom x b) o x .+. st_a          -- a!-I[C]  relative addition
                                              where st_a = uType uLft (mGeneric (cod x a) (dom x b) o x) a
     uType uLft uRt x@(PRad o a b)          = dom x x.<.dom x a .+. cod x x.<.cod x b                                      -- a!b      relative addition
                                              mGeneric (cod x a) (dom x b) o x.<.cod x a .+.
                                              mGeneric (cod x a) (dom x b) o x.<.dom x b .+.
                                              uType uLft (mGeneric (cod x a) (dom x b) o x) a .+.
                                              uType (mGeneric (cod x a) (dom x b) o x) uRt b
-}
     uType uLft uRt x@(PPrd _ a b)          = dom x a.=.dom x x .+. cod x b.=.cod x x                                      -- a*b cartesian product
                                              .+. uType uLft anything a .+. uType anything uRt b
     uType uLft uRt x@(PKl0 _ e)            = dom x e.<.dom x x .+. cod x e.<.cod x x .+. uType uLft uRt e
     uType uLft uRt x@(PKl1 _ e)            = dom x e.<.dom x x .+. cod x e.<.cod x x .+. uType uLft uRt e
     uType uLft uRt   (PFlp _ (Prel o nm))  = uType uLft uRt (Pflp o nm)                                            -- r~  flip
     uType uLft uRt   (PFlp _ (Pflp o nm))  = uType uLft uRt (Prel o nm)                                            -- r~~
     uType uLft uRt x@(PFlp _ e)            = cod x e.=.dom x x .+. dom x e.=.cod x x .+. uType uRt uLft e
{- the abstract version of uType for PCpl
     uType uLft uRt x@(PCpl o e)            = dom x x.=.dom x e .+. cod x x.=.cod x e .+. uType uLft uRt e                 -- -a  complement
                                              where e = PDif o (Pfull o [uLft, uRt]) b
-}
     uType uLft uRt x@(PCpl _ e)            = dom x x.<.uLft .+. cod x x.<.uRt .+.                                     -- -e  complement
                                              dom x e.<.uLft .+. cod x e.<.uRt .+.
                                              uType uLft uRt e
     uType uLft uRt   (PBrk _ e)            = uType uLft uRt e                                                     -- (e) brackets
     uType  _    _    (PTyp _ _ (P_Sign []))= fatal 196 "P_Sign is empty"
     uType uLft uRt x@(PTyp o e (P_Sign cs))= dom x x.<.iSrc  .+. cod x x.<.iTrg  .+.                                  -- e[A*B]  type-annotation
                                              iSrc .<.uLft  .+. iTrg .<.uRt .+.
                                              if o `elem` [origin d| d<-decls]
                                              then nothing
                                              else dom x x.<.dom x e .+. cod x x.<.cod x e .+.
                                                   uType iSrc iTrg e
                                              where iSrc = TypExpr (Pid o [head cs]) False o x
                                                    iTrg = TypExpr (Pid o [last cs])  True o x
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
     dom, cod :: P_Expression -> P_Expression -> Type
     dom e x    = TypExpr x         False (origin x) e -- the domain of x, and make sure to check subexpressions of x as well
     cod e x    = TypExpr (p_flp x) True  (origin x) e 

{- The following table is a data structure that is meant to facilitate drawing type graphs and creating the correct messages for users.
This table is organized as follows:
Int             : a vertex (number) in the stGraph, which contains the raw tuples from function 'typing'
Int             : a vertex (number) in the sccGraph, which is a condensed form of the stGraph, leaving the semantics identical
Type            : a type expression, containing a P_Expression, which is represented by a number in the type graphs. Different expressions may carry the same number in the sccGraph.
[P_Expression]  : the original expression, as it came out of the script, if there is one.
-}
tableOfTypes :: P_Context -> [(Type,Type)] -> ([(Int,Int,Type)], Graph.Graph, Graph.Graph)
tableOfTypes p_context st = (table, stGraph, sccGraph) -- to debug:  error (intercalate "\n  " (map show (take 10 eqClasses)++[show (length eqClasses), show ((sort.nub) [classNr | (exprNr,classNr,_)<-table]>-[0..length eqClasses])]++[show x | x<-take 25 table++drop (length table-10) table])) --  
 where
{- stGraph is a graph whose edges are precisely st, but each element in st is replaced by a pair of integers. The reason is that datatype Graph expects integers.
   The list st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by dom t is a subset of the set of atoms contained by dom t'.
-}
     typeExpressions   :: [Type]     -- a list of all type expressions in st.
     typeExpressions = nub (map fst st++map snd st)
     expressionTable :: [(Int, Type)]
     (expressionTable,numberOfNodes) = ([(i,typeExpr) | (i,cl)<-zip [0..] eqExpressions, typeExpr<-cl ], length eqExpressions)
      where eqExpressions = eqClass t_Eq typeExpressions     -- WHY do we need the following definition of t_Eq? What is the problem to use t_eq instead?
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
              | i==j = [(i,classNr,typeExpr)]
             f exprTable@((i,typeExpr):exprTable') classNrs@((j,classNr):classNrs')
              | i==j = (i,classNr,typeExpr) : f exprTable' classNrs
              | i>j  = f exprTable classNrs'
              | i<j  = fatal 425 "mistake in table"
             f et ct = fatal 249 ("Remaining elements in table\n"++intercalate "\n" (map show et++map show ct))

calcTypes :: P_Context -> [(Type, Type)] -> ([CtxError])
calcTypes p_context st = typeErrors
   where
    typeErrors :: [CtxError]
    typeErrors
     | (not.null) err1 = err1
     | (not.null) err2 = err2
     | otherwise       = err3
     where err1 = derivedEquals++ambiguousRelations
           err2 = unTypableRelations
           err3 = ambiguousAndUntypableExpressions
    (typeTable,_,sccGraph) = tableOfTypes p_context st
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the expressions derived from the script).
     = [ newcxe ("The following concepts were proven equal:\n  "++intercalate ", " [show c| (_,_,TypExpr (Pid _ [c]) _ _ _)<-diffs] )
       | diffs<-eqCl (\(_,classNr,_) -> classNr) (map head (eqClass tripleEq conceptTypes))
       , length diffs>1]
       where (_,_,t) `tripleEq` (_,_,t') = t `t_eq` t'
--    predics = [pred | expr<-expressions p_context, pred<-computePredicates (p_declarations p_context) expr ]
    conceptTypes :: [(Int,Int,Type)]
    conceptTypes = [ (exprNr, classNr, e) | (exprNr, classNr, e@(TypExpr (Pid{}) _ _ _))<-typeTable ]
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
    unTypableRelations :: [CtxError]
    unTypableRelations
     = [ CxeOrig (newcxe ("Relation \""++relName++"\" is not declared."))
                 "relation"
                 relName
                 orig
       | Prel orig relName<-subexpressions p_context, relName `notElem` nub (map name (p_declarations p_context))
       ] where 
    ambiguousAndUntypableExpressions :: [CtxError]
    ambiguousAndUntypableExpressions
     = [ CxeOrig (newcxe (case as of
                           []  -> "Nonexistent type for src("++sh v++")."
                           _   -> "Ambiguous type for src("++sh v++")\n The type might be "++intercalate ", " (map sh as)++"."
                 )       )
                 "expression"
                 ""
                 (originVtx typeTable v)
       | v <- Graph.vertices sccGraph, Graph.indegree sccGraph!v==0, tree<-Graph.dfs sccGraph [v]
       , let as=[a|a<-apples tree, a `elem` conceptClasses]
       , length as/=1
       ] where sh i = showVertex typeTable i
    apples :: Tree.Tree Int -> [Int]
    apples t = if null (Tree.subForest t) then [Tree.rootLabel t] else concat (map apples (Tree.subForest t))

showStVertex :: [(Int,Int,Type)] -> Int -> String
showStVertex typeTable i
 = head ([ showType e | (exprNr, _, e)<-typeTable, i==exprNr ]++fatal 506 ("No expression numbered "++show i++" found by showStVertex"))
showVertex :: [(Int,Int,Type)] -> Int -> String
showVertex typeTable i
 = (intercalate "\n".nub) [ showType (head cl)
                          | cl<-eqCl original [ typExpr | (_, classNr, typExpr)<-typeTable, i==classNr ]
                          ]
originVtx :: [(Int,Int,Type)] -> Int -> Origin
originVtx typeTable i
 = head ([ origin e | (_, classNr, e)<-typeTable, i==classNr ]++fatal 512 ("No expression numbered "++show i++" found by originVtx"))
original :: Type -> P_Expression
original (TypExpr _ flipped _ e) = if flipped then p_flp e else e

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: P_Context -> [(Type, Type)] -> (DotGraph String,DotGraph String)
typeAnimate p_context st = (stTypeGraph,condensedGraph)
   where
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
    (typeTable,stGraph,sccGraph) = tableOfTypes p_context st
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
                       | cl<-eqCl original [ typExpr| (_, classNr, typExpr)<-typeTable, n==classNr ]
                       ]
class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  expressions :: a -> [P_Expression]
  subexpressions :: a -> [P_Expression]

instance Expr P_Context where
 p_gens pContext
  = concat [ p_gens pat | pat<-ctx_pats  pContext] ++
    concat [ p_gens prc | prc<-ctx_PPrcs pContext] ++
    ctx_gs pContext
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
 expressions r = expressions (rr_exp r)
 subexpressions r = subexpressions (rr_exp r)
instance Expr P_Declaration where
 expressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
-- expressions d = [PCps orig (Pid orig [head sgn]) (PCps orig (Prel orig (dec_nm d)) (Pid orig [last sgn]))] where P_Sign sgn = dec_sign d; orig = origin d
 subexpressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
instance Expr P_KeyDef where
 expressions k = expressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 subexpressions k = subexpressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
instance Expr P_Interface where
 expressions k = expressions (ifc_Obj k)
 subexpressions k = subexpressions (ifc_Obj k)
instance Expr P_ObjectDef where
 expressions o = [obj_ctx o | null (expressions (obj_msub o))]++expressions [PCps (origin o) (obj_ctx o) e | e<-expressions (obj_msub o)]
 subexpressions o = subexpressions (obj_ctx o)++subexpressions (obj_msub o)
instance Expr P_SubInterface where
 expressions x = expressions (si_box x)
 subexpressions x = subexpressions (si_box x)
instance Expr a => Expr (Maybe a) where
 expressions Nothing = []
 expressions (Just x) = expressions x
 subexpressions Nothing = []
 subexpressions (Just x) = subexpressions x
instance Expr a => Expr [a] where
 expressions = concat.map expressions
 subexpressions = concat.map subexpressions
instance Expr P_Expression where
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
 
pCtx2aCtx :: P_Context -> (A_Context,CtxError,DotGraph String,DotGraph String)
pCtx2aCtx p_context
 = (contxt
   ,cxelist ( cxerrs++if nocxe(cxelist cxerrs) then postchks else [])
   ,stTypeGraph,condensedGraph)
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
    st = typing (p_gens p_context) (p_declarations p_context) (expressions p_context)
    (typeErrors) = calcTypes p_context st
    (stTypeGraph,condensedGraph) = typeAnimate p_context st
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
    agens = map (pGen2aGen contxt "NoPattern") (ctx_gs p_context)
    (adecs,   deccxes)   = (unzip . map (pDecl2aDecl contxt allpops "NoPattern") . ctx_ds) p_context
    (apurp,   xplcxes)   = (unzip . map (pPurp2aPurp contxt)             . ctx_ps   ) p_context
    (pats,    patcxes)   = (unzip . map (pPat2aPat   contxt allpops)     . ctx_pats ) p_context
    (procs,   proccxes)  = (unzip . map (pProc2aProc contxt allpops)     . ctx_PPrcs) p_context
    (ctxrules,rulecxes)  = (unzip . map (pRul2aRul   contxt "NoPattern") . ctx_rs   ) p_context
    (keys,    keycxes)   = (unzip . map (pKDef2aKDef contxt)             . ctx_ks   ) p_context
    (ifcs,interfacecxes) = (unzip . map (pIFC2aIFC   contxt)             . ctx_ifcs ) p_context
    (sqlPlugs,sPlugcxes) = (unzip . map (pODef2aODef contxt [] NoCast)   . ctx_sql  ) p_context
    (phpPlugs,pPlugcxes) = (unzip . map (pODef2aODef contxt [] NoCast)   . ctx_php  ) p_context
    (allmbpops, popcxes) = (unzip . map (pPop2aPop   contxt)             . pops ) p_context
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

pPat2aPat :: A_Context -> [Population] -> P_Pattern -> (Pattern, CtxError)
pPat2aPat contxt pops ppat 
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
    arls  = map (pRul2aRul contxt (name ppat)) (pt_rls ppat)
    agens = map (pGen2aGen contxt (name ppat)) (pt_gns ppat)
    (keys,keycxes) = unzip akds
    akds  = map (pKDef2aKDef contxt) (pt_kds ppat)
    (adecs,deccxes) = (unzip . map (pDecl2aDecl contxt pops (name ppat)) . pt_dcs) ppat
    (xpls,xplcxes) = (unzip . map (pPurp2aPurp contxt) . pt_xps) ppat

pProc2aProc :: A_Context -> [Population] -> P_Process -> (Process,CtxError)
pProc2aProc contxt pops pproc
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
    (prules,rulecxes) = (unzip . map (pRul2aRul contxt (name pproc)) . procRules) pproc
    arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
    (rrels,editcxes)  = (unzip . map (pRRel2aRRel contxt)            . procRRels) pproc
    agens  = map (pGen2aGen contxt (name pproc)) (procGens pproc)
    arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
    (adecs,deccxes)   = (unzip . map (pDecl2aDecl contxt pops (name pproc)) . procDcls) pproc
    (rruls,rrcxes)    = (unzip . map (pRRul2aRRul contxt)            . procRRuls) pproc
    (keys,keycxes)    = (unzip . map (pKDef2aKDef contxt)            . procKds) pproc
    (expls,explcxes)  = (unzip . map (pPurp2aPurp contxt)            . procXps) pproc

pRRul2aRRul :: (Language l, ConceptStructure l, Identified l) => l -> RoleRule -> (RoleRule,CtxError)
pRRul2aRRul contxt prrul
 = ( prrul, CxeOrig (cxelist rrcxes) "role rule" "" (origin prrul))
   where
     rrcxes = [ newcxe ("Rule '"++r++" does not exist.")
              | r<-mRules prrul, null [rul | rul<-rules contxt, name rul==r]]
     
pRRel2aRRel :: (Language l, ConceptStructure l, Identified l) => l -> P_RoleRelation -> (RoleRelation,CtxError)
pRRel2aRRel contxt prrel
 = ( RR { rrRoles = rr_Roles prrel
        , rrRels  = rels
        , rrPos   = rr_Pos prrel
        }
   , CxeOrig (cxelist editcxes) "role relation" "" (origin prrel))
   where
     (rels,editcxes) = unzip [ pRel2aRel contxt (psign sgn) r
                             | PTyp _ r@(Prel{}) sgn<-rr_Rels prrel
                                                      ++fatal 547 ("Untyped relation(s) "++ intercalate ", " [nm | Prel _ nm<-rr_Rels prrel])
                             ]

p2aPairView :: A_Context -> Sign -> P_PairView -> (PairView,CtxError)
p2aPairView contxt sgn (P_PairView ppvs) = (PairView pvs, cxelist errs) 
 where (pvs, errs) = unzip $ map (p2aPairViewSegment contxt sgn) ppvs

p2aPairViewSegment :: A_Context -> Sign -> P_PairViewSegment -> (PairViewSegment,CtxError)
p2aPairViewSegment _       _  (P_PairViewText str)          = (PairViewText str, cxenone)
p2aPairViewSegment contxt sgn (P_PairViewExp srcOrTgt pexp) = (PairViewExp srcOrTgt aexpr, exprcxe)
    where (aexpr,exprcxe) = pExpr2aExpr contxt (SourceCast $ segSrcType sgn srcOrTgt) pexp
          segSrcType (Sign srcType _) Src = srcType 
          segSrcType (Sign _ tgtType) Tgt = tgtType
           
pRul2aRul :: A_Context -> String -> P_Rule -> (Rule,CtxError)
pRul2aRul contxt patname prul        -- for debugging the parser, this is a good place to put     error (show (rr_exp prul))
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
   where (aexpr,exprcxe) = pExpr2aExpr contxt NoCast (rr_exp prul)
         (mviol, mviolcxe) = case fmap (p2aPairView contxt $ sign aexpr) $ rr_viol prul of
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
pKDef2aKDef :: (Language l, ProcessStructure l, ConceptStructure l, Identified l) => l -> P_KeyDef -> (KeyDef, CtxError)
pKDef2aKDef contxt pkdef
 = (Kd { kdpos = kd_pos pkdef
       , kdlbl = kd_lbl pkdef
       , kdcpt = c
       , kdats = segs
                    }
   , CxeOrig (cxelist (nmchk:kdcxe:duplicateKeyErrs:multipleKeyErrs:segscxes)) "key definition" "" (origin pkdef) )
   where
    (segs, segscxes) = unzip . map (pKeySeg2aKeySeg contxt c) $ kd_ats pkdef
    c  = pCpt2aCpt contxt (kd_cpt pkdef)
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

-- (ats,atscxes)  = (unzip . map (pODef2aODef contxt (SourceCast c)) . kd_ats) pkdef
pKeySeg2aKeySeg :: (Language l, ProcessStructure l, ConceptStructure l, Identified l) => l -> A_Concept -> P_KeySegment -> (KeySegment, CtxError)
pKeySeg2aKeySeg _    _      (P_KeyText str)   = (KeyText str, cxenone)
pKeySeg2aKeySeg _    _      (P_KeyHtml str)   = (KeyHtml str, cxenone)
pKeySeg2aKeySeg contxt concpt (P_KeyExp keyExp) = let (objDef, cxe) = pODef2aODef contxt [] (SourceCast concpt) keyExp
                                                in ( KeyExp objDef, cxe)
  

-- TODO -> Does pIFC2aIFC require more checks? What is the intention of params, viols, args i.e. the interface data type?
pIFC2aIFC :: (Language l, ProcessStructure l, ConceptStructure l, Identified l) => l -> P_Interface -> (Interface,CtxError)
pIFC2aIFC contxt pifc 
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
    (obj,objcxe)  = pODef2aODef contxt parentIfcRoles NoCast (ifc_Obj pifc)
    (prms,prmcxes)  = unzip [pRel2aRel contxt (psign sgn) r
                            | PTyp _ r@(Prel{}) sgn<-ifc_Params pifc -- Todo: link untyped relations to their type!
                                                     ++fatal 669 ("Untyped relation(s) "++ intercalate ", " [nm | (Prel _ nm)<-ifc_Params pifc])
                            ]
    duplicateRoleErrs = [newcxe $ "Duplicate interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, length (filter (==role) $ ifc_Roles pifc) > 1 ]
    undeclaredRoleErrs = if null duplicateRoleErrs then [newcxe $ "Undeclared interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, role `notElem` roles contxt ]
                                                   else []
    -- we show the line nr for the interface, which may be slightly inaccurate, but roles have no position 
    -- and the implementation of error messages makes it difficult to give a nice one here
    
-- | pODef2aODef checks compatibility of composition of expressions on equality
pODef2aODef :: (Language l, ProcessStructure l, ConceptStructure l, Identified l) => l -> [String] -> AutoCast -> P_ObjectDef -> (ObjectDef,CtxError)
pODef2aODef contxt parentIfcRoles cast podef 
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
    (expr,exprcxe)  = pExpr2aExpr contxt cast (obj_ctx podef)
    -- Step2: check obj_ats in the context of expr
    (msub,msubcxes) = p2a_MaybeSubInterface contxt parentIfcRoles (target expr) $ obj_msub podef
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
    
p2a_MaybeSubInterface :: (Language l, ProcessStructure l, ConceptStructure l, Identified l) => 
                         l -> [String] -> A_Concept -> Maybe P_SubInterface -> (Maybe SubInterface, [CtxError])
p2a_MaybeSubInterface _    _              _    Nothing = (Nothing, [])
p2a_MaybeSubInterface contxt parentIfcRoles conc (Just (P_Box p_objs)) =
  let (objs, errs) = unzip [pODef2aODef contxt parentIfcRoles (SourceCast conc) p_obj | p_obj<-p_objs] 
  in  (Just $ Box objs, errs)
p2a_MaybeSubInterface contxt parentIfcRoles conc (Just (P_InterfaceRef pos nm)) =
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
  
pPurp2aPurp :: A_Context -> PPurpose -> (Purpose, CtxError)
pPurp2aPurp contxt pexpl
 = ( Expl { explPos      = pexPos   pexpl
          , explObj      = explobs
          , explMarkup   = pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt) (pexMarkup pexpl)
          , explRefId    = pexRefID pexpl
          , explUserdefd = True
         -- , explCont  = string2Blocks (ctxmarkup contxt) (pexExpl  pexpl)
          }
   , CxeOrig xplcxe "explanation" "" (origin pexpl))
   where (explobs,xplcxe) = pExOb2aExOb contxt (pexObj   pexpl)
         

pExOb2aExOb :: A_Context -> PRef2Obj -> (ExplObj, CtxError)
pExOb2aExOb contxt (PRef2ConceptDef str  ) = (ExplConceptDef (head cds), newcxeif(null cds)("No concept definition for '"++str++"'"))
                                             where cds = [cd | cd<-conceptDefs contxt, cdcpt cd==str ]
pExOb2aExOb contxt (PRef2Declaration x@(PTyp o (Prel _ nm) sgn))
                                           = ( ExplDeclaration (head decls)
                                             , if null decls
                                               then CxeOrig (newcxe ("No declaration for '"++showADL x++"'")) "relation" nm o
                                               else CxeNone
                                             )
                                             where decls = [d | d<-declarations contxt, name d==nm, sign d==pSign2aSign contxt sgn ]
pExOb2aExOb contxt (PRef2Rule str        ) = (ExplRule (head ruls), newcxeif(null ruls)("No rule named '"++str++"'") )
                                             where ruls = [rul | rul<-rules contxt, name rul==str ]
pExOb2aExOb contxt (PRef2KeyDef str      ) = (ExplKeyDef (head kds), newcxeif(null kds)("No key definition named '"++str++"'") )
                                             where kds = [kd | kd<-keyDefs contxt, name kd==str]
pExOb2aExOb contxt (PRef2Pattern str     ) = (ExplPattern str,   newcxeif(null[pat |pat<-patterns   contxt,   name pat==str])("No pattern named '"++str++"'") )
pExOb2aExOb contxt (PRef2Process str     ) = (ExplProcess str,   newcxeif(null[prc |prc<-processes  contxt,  name prc==str]) ("No process named '"++str++"'") )
pExOb2aExOb contxt (PRef2Interface str   ) = (ExplInterface str, newcxeif(null[ifc |ifc<-interfaces contxt, name ifc==str])  ("No interface named '"++str++"'") )
pExOb2aExOb contxt (PRef2Context str     ) = (ExplContext str,   newcxeif(name contxt/=str) ("No context named '"++str++"'") )  
pExOb2aExOb contxt (PRef2Fspc str        ) = (ExplFspc str,      newcxeif( name contxt/=str)("No specification named '"++str++"'") )

pPop2aPop :: (Language l, ConceptStructure l, Identified l) => l -> P_Population -> (Maybe Population,CtxError)
pPop2aPop _        (P_CptPopu{}) = (Nothing,cxenone)
pPop2aPop contxt p@(P_Popu{})
 = ( Just (Popu { popm  = aRel
                , popps = p_popps p
                })
   , relcxe
   )
   where (ERel aRel, relcxe) = pExpr2aExpr contxt NoCast (PTyp (origin p) (Prel (origin p) (name p)) (p_type p))

pGen2aGen :: (Language l, ConceptStructure l, Identified l) => l -> String -> P_Gen -> A_Gen
pGen2aGen contxt patNm pg
   = Gen{genfp  = gen_fp  pg
        ,gengen = pCpt2aCpt contxt (gen_gen pg)
        ,genspc = pCpt2aCpt contxt (gen_spc pg)
        ,genpat = patNm
        }
          
pSign2aSign :: (Language l, ConceptStructure l, Identified l) => l -> P_Sign -> Sign
pSign2aSign contxt (P_Sign cs) = Sign (head ts) (last ts)
  where ts = map (pCpt2aCpt contxt) cs
        
pCpt2aCpt :: (Language l, ConceptStructure l, Identified l) => l -> P_Concept -> A_Concept
pCpt2aCpt contxt pc
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

pDecl2aDecl :: A_Context -> [Population] -> String -> P_Declaration -> (Declaration, CtxError)
pDecl2aDecl contxt pops patname pd =
 ( Sgn { decnm   = dec_nm pd
       , decsgn  = pSign2aSign contxt (dec_sign pd)
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
                                     , relsgn ad==pSign2aSign contxt (dec_sign pd)
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
pRel2aRel :: (Language l, ConceptStructure l, Identified l) => l -> [P_Concept] -> P_Expression -> (Relation,CtxError)
pRel2aRel contxt _ (Pfull orig pConcepts)
 = case pConcepts of
    [] -> (fatal 326 "Ambiguous universal relation."
                          , CxeOrig (newcxe "Ambiguous universal relation.") 
                                    "relation" "" orig )
    [c] -> (V (Sign (pCpt2aCpt contxt c) (pCpt2aCpt contxt c)), CxeOrig cxenone "relation" "" orig)
    [s,t] -> (V (Sign (pCpt2aCpt contxt s) (pCpt2aCpt contxt t)), CxeOrig cxenone "relation" "" orig)
    _   -> fatal 328 "Encountered a Sign with more than two elements. This should be impossible."
pRel2aRel contxt _ (Pid orig pConcepts)
 = case pConcepts of
    [] -> (fatal 331 "Ambiguous identity relation."
                          , CxeOrig (newcxe
                                    "Ambiguous identity relation.") 
                                    "relation" "" orig )
    [c] -> (I (pCpt2aCpt contxt c), CxeOrig cxenone "relation" "" orig)
    _   -> fatal 341 "Encountered a Sign with more than one element. This should be impossible."
pRel2aRel contxt _ (Patm orig atom pConcepts) 
 = case pConcepts of
    [] -> (fatal 343 "Ambiguous value."
                          , CxeOrig (newcxe
                                    "Ambiguous value.") 
                                    "relation" "" orig )
    [c] -> (Mp1 atom (pCpt2aCpt contxt c), CxeOrig cxenone "relation" "" orig)
    _   -> fatal 354 "Encountered a Sign with more than one element. This should be impossible."
pRel2aRel contxt sgn (Prel orig nm)
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
    unknowncpts = nub[c |c<-sgn, pCpt2aCpt contxt c `notElem` concs contxt]
    ds  = [d | d<-declarations contxt, name d==nm]
    dts = [d | d<-ds, not(null sgn)
                    , name (head sgn)==name (source d) &&
                      name (last sgn)==name (target d)   ]

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



--the type checker always returns an expression with sufficient type casts, it should remove redundant ones.
--applying the type checker on an complete, explicitly typed expression is equivalent to disambiguating the expression
disambiguate :: Fspc -> Expression -> Expression
disambiguate fSpec x
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


pExpr2aExpr :: (Language l, ConceptStructure l, Identified l) => l -> AutoCast -> P_Expression -> (Expression,CtxError)
pExpr2aExpr contxt typecast pExpr = (f pExpr, CxeNone)
   where
     f :: P_Expression -> Expression
     f (PTyp _ (Pid _ _) (P_Sign (c:_))) = ERel (I (pCpt2aCpt contxt c))
     f x@(PTyp o (Pid _ _) (P_Sign _))  = fatal 983 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
     f (Pid _ [c])     = ERel (I (pCpt2aCpt contxt c))
     f x@(Pid o _ )    = fatal 985 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
     f (Pnid c)        = ECpl (ERel (I (pCpt2aCpt contxt c)))
     f (Patm _ atom [c]) = ERel (Mp1 atom (pCpt2aCpt contxt c))
     f x@Pnull         = fatal 988 ("pExpr2aExpr cannot transform "++show x++" to an expression.")
     f (Pfull _ [s,t]) = ERel (V (Sign (pCpt2aCpt contxt s) (pCpt2aCpt contxt t)))
     f (Pfull _ [s])   = ERel (V (Sign (pCpt2aCpt contxt s) (pCpt2aCpt contxt s)))
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
     f (PLrs _ a b)    = ERrs (f a, f b)
     f (PRrs _ a b)    = ELrs (f a, f b)
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
     f (PTyp _ a sgn)  = ETyp (f a) (Sign (pCpt2aCpt contxt s) (pCpt2aCpt contxt t))
                         where P_Sign cs = sgn; s=head cs; t=last cs
