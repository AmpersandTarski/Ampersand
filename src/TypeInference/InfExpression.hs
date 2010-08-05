module TypeInference.InfExpression where
--this module is based on code generated with uuagc
--uuagc is not used because you can't (as far as I know) define DATA PExpression Morphism (Maybe Sign)
--you can define the polymorph DATA PExpression a b, but it does not match our expectations

import Adl

--the type containing inh and syn atts
type InfExpression  = 
     --inherited
     [Declaration] ->  --decls
     [(Concept,Concept)] -> --isas
     (Maybe Sign) --autocast
      --synthesized
      -> InfAtts
type InfAtts =
     (PExpression Declaration Sign --typedexpr
     ,Either Sign TErr --trytype 
                               --if trytype subexpression==ambiguous => autocast if one alternative possible 
                               --else remove impossible alts and return trytype==ambiguous
                               --An ambiguous error contains all alternatives
     ,Either Sign TErr --reltype (if reltype subexpression==ambiguous => error)
     ,[ITree] --proof
     ,Bool --homogeneous?
     )
type TErr = String
type ITree = String
------------------------------------------------------------
--uuagc: *_Syn_PExpression functions
typedexpr :: InfAtts -> PExpression Declaration Sign
typedexpr (infex,_,_,_,_) = infex
trytype :: InfAtts -> Either Sign TErr
trytype (_,ttype,_,_,_) = ttype
reltype :: InfAtts -> Either Sign TErr
reltype (_,_,rtype,_,_) = rtype
prooftree :: InfAtts -> [ITree]
prooftree (_,_,_,proof,_) = proof
ishomo :: InfAtts -> Bool
ishomo (_,_,_,_,hm) = hm

--all the type-correct exprs given a context of decls and isas
typedexprs :: [Declaration] -> [(Concept,Concept)] -> [PExpression Morphism (Maybe Sign)] -> [PExpression Declaration Sign]
typedexprs decls isas xs = [typedexpr ix|ix <-[infer x decls isas Nothing | x<-xs],let Left _ = reltype ix]
--all the type errors given a context of decls and isas
typeerrors :: [Declaration] -> [(Concept,Concept)] -> [PExpression Morphism (Maybe Sign)] -> [TErr]
typeerrors decls isas xs = [err|ix <-[infer x decls isas Nothing | x<-xs],let Right err = reltype ix]
------------------------------------------------------------
--uuagc: infer=sem_PExpression
--stop de attributen van een expr in de infer functie met gelijke structuur
--een attr x van het type PExpression gaat er in als (infer x) = inh atts tuple
infer :: PExpression Morphism (Maybe Sign) -> InfExpression 
infer (MulPExp mop subs usercast )  = infer_MulPExp mop (map infer subs) usercast
infer (UnPExp uop sub usercast )  = infer_UnPExp uop (infer sub) usercast 
infer (TPExp subm usercast )  = infer_TPExp subm usercast 

-- inh: *_  (decls and isa do not change)
-- syn: _* 
infer_UnPExp :: (UnOp) -> InfExpression  ->  (Maybe Sign) ->  InfExpression 
infer_UnPExp uop sub usercast     decls_ isa_ autocast_ =
    let --push inherited on subexpression
        subtuple = sub decls_ isa_ castsub
        --calculate synthesized
        ttype = rmap redefine (lmap dir(trytype subtuple)) --no UnOp can solve ambiguity, so pass up
        rtype = if cc then lmap dir(reltype subtuple) else Right "No match with user cast"
        infex = UnPExp uop (typedexpr subtuple) (thetype rtype)
        proof = []
        hm =ishomo subtuple
    in  (infex,ttype,rtype,proof,hm) --return synthesized
    where
    cc = castcondition usercast autocast_ isa_
    castsub =  fmap dir (cast usercast autocast_)
    dir (a,b) | uop==Co   = (b,a)
              | otherwise = (a,b)
    redefine "AMB" = "typerules applied to AMB" 
    redefine err = err

infer_MulPExp :: MulOp -> [InfExpression]  ->  (Maybe Sign) ->  InfExpression 
infer_MulPExp mop subs usercast     decls_ isa_ autocast_ =
    let --push inherited on subexpressions
        trysubstuples = [sub decls_ isa_ Nothing | sub<-subs]
        --if trysubstuples contains ambiguities, then solve them by finding a suitable autocast
        --such that substuples does not contain ambiguities anymore
        --for a list of all possible autocasts there should be exactly one substuple without errors in the subs
        autocast = Nothing
        substuples = [sub decls_ isa_ autocast | sub<-subs]
        --calculate synthesized
        ttype = testy usercast
        rtype = testy usercast
        infex = MulPExp mop (map typedexpr substuples) (thetype rtype)
        proof = []
        hm = foldr (||) False (map ishomo substuples)
             && elem mop [Re,Ri,Fi,Fu]
    in  (infex,ttype,rtype,proof,hm) --return synthesized 
    

infer_TPExp :: Morphism  ->  (Maybe Sign) ->  InfExpression 
infer_TPExp subm usercast     decls_ isa_ autocast_ =
    let --calculate synthesized
        ttype = case length(ds usercast) of
            1 -> dt usercast --TODO -> check+infer homo
            0 -> Right "No decl"
            _ -> Right "AMB"
        rtype = if cc
            then case length(ds ct) of
              1 -> dt ct --TODO -> check not Anything; check+infer homo
              0 -> Right "No decl"
              _ -> Right "AMB"
            else Right "No match with user cast"
        infex = case rtype of
            Left t -> TPExp (head (ds ct)) t
            _ -> error "only infex if there is a type"
        proof = []
        --iff all (ds usercast) are homogeneous, then hm=True 
        --i.e. hm applies to ttype, not rtype.
        --implemented: not(d is heterogeneous)
        hm = not$foldr (||) False [ isIdent d || null[()|p<-decprps d,elem p [Asy,Rfx,Sym,Trn]] |d<-ds usercast]
    in  (infex,ttype,rtype,proof,hm) --return synthesized
    where
    cc = castcondition usercast autocast_ isa_
    ct = cast usercast autocast_
    dt Nothing = Left ((sign.head.ds) Nothing)
    dt (Just cx) = Left cx
    --TODO -> I and V must be added to decls_
    ds (Just cx) = [d|d<-decls_, name subm==name d, (sign d \\-\\ cx) isa_]
    ds Nothing = [d|d<-decls_, name subm==name d]
------------------------------------------------------------
------------------------------------------------------------

type OnIsa a = [(Concept,Concept)] -> a

--cptgE of Concept is still (==), so I can't use the functions order,glb,lub
(\-\) :: Concept -> Concept -> OnIsa Bool
(\-\) a b = (\isas -> elem (a,b) isas || elem (b,a) isas)
(\\-\\) :: Sign -> Sign -> OnIsa Bool
(\\-\\) (a,b) (c,d) = (\isas -> (a\-\c) isas && (b\-\d) isas)

lmap :: (a -> a) -> Either a b -> Either a b
lmap f (Left x) = Left (f x)
lmap _ y = y
rmap :: (b -> b) -> Either a b -> Either a b
rmap f (Right x) = Right (f x)
rmap _ y = y

castcondition :: Maybe Sign -> Maybe Sign -> OnIsa Bool
castcondition (Just x) (Just y) = x \\-\\ y
castcondition _ _ = (\_ -> True)
cast :: Maybe Sign -> Maybe Sign -> Maybe Sign
cast usercast@(Just _) _ = usercast
cast _ autocast = autocast

-- (\-/) :: Concept -> Concept -> OnIsa Concept
-- (\-/) a b | elem (
-- (/-\) :: Concept -> Concept -> OnIsa Concept



testy (Just x) = Left x
testy _ = Right "Anything"

thetype :: Either Sign TErr -> Sign
thetype (Left t) = t
thetype _ = error "no type"



