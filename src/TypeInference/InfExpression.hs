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
     ,Either Sign TErr --reltype (an ambiguous error contains all alternatives and inftrees, ambiguity in a subexpression is 'allowed' because it can turn out not to be ambiguous in a larger expression)
     ,[ITree] --proof
     ,Bool --homogeneous?
     )
type TErr = String
type ITree = String
------------------------------------------------------------
--uuagc: *_Syn_PExpression functions
typedexpr :: InfAtts -> PExpression Declaration Sign
typedexpr (infex,_,_,_) = infex
reltype :: InfAtts -> Either Sign TErr
reltype (_,rtype,_,_) = rtype
prooftree :: InfAtts -> [ITree]
prooftree (_,_,proof,_) = proof
ishomo :: InfAtts -> Bool
ishomo (_,_,_,hm) = hm

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
        autocast = Nothing
        subtuple = sub decls_ isa_ autocast
        --calculate synthesized
        rtype = testy usercast
        infex = UnPExp uop (typedexpr subtuple) (thetype rtype)
        proof = []
        hm =ishomo subtuple
    in  (infex,rtype,proof,hm) --return synthesized

infer_TPExp :: Morphism  ->  (Maybe Sign) ->  InfExpression 
infer_TPExp subm usercast     decls_ isa_ autocast_ =
    let --calculate synthesized
        rtype = testy usercast
        infex = TPExp (makeDeclaration subm) (thetype rtype)
        proof = []
        hm = False
    in  (infex,rtype,proof,hm) --return synthesized

infer_MulPExp :: MulOp -> [InfExpression]  ->  (Maybe Sign) ->  InfExpression 
infer_MulPExp mop subs usercast     decls_ isa_ autocast_ =
    let --push inherited on subexpressions
        autocast = Nothing
        substuples = [sub decls_ isa_ autocast | sub<-subs]
        --calculate synthesized
        rtype = testy usercast
        infex = MulPExp mop (map typedexpr substuples) (thetype rtype)
        proof = []
        hm = foldr (||) False (map ishomo substuples)
             && elem mop [Re,Ri,Fi,Fu]
    in  (infex,rtype,proof,hm) --return synthesized
------------------------------------------------------------
------------------------------------------------------------
testy (Just x) = Left x
testy _ = Right "Anything"

thetype :: Either Sign TErr -> Sign
thetype (Left t) = t
thetype _ = error "no type"


