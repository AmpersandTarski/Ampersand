module TypeInference.InfLibAGFuncs where
{- a module to infer types of expressions of a heterogeneous relation algebra with ISA hierarchy 
 - expressions must be normalised with the function normalise
 - the relation variables in the expression are bound to a declaration with a type
 - more than one declaration can exist for a relation variable
 - I and V are relation constants with a type (Universe,Universe)
 - a relation in an expression can be given a (user-defined) type to solve ambiguities
 - expressions cannot be given a user-defined type
 - relations have a property indicating whether they have a homogeneous type or not.
 -
 - recall the attributes of the expression:
 - ATTR RelAlgExpr ISectList UnionList
   [ {-inherited attributes: are copied down by default-}
     env_decls:{[RelDecl]}
     env_isa:Isa
     type_down:RelAlgType
     listof:ListOf
   | {-chain attributes-}
   | {-synthesized attributes-}
     me:SELF --copy of myself to investigate me
     env_in:AltList
     rtype:InfType
     env_mph:{[(RelAlgExpr,InfType,RelDecl)]}
   ]
 - 
 - env_decls and env_isa need to be derived from the script
 - the initial type_down is usually (Universe,Universe) i.e. the expression is in the context of the universe
 - listOf is a helper attribute to persist the list type for unions and intersections. It can initially be set to NoListOf.
 -
 - env_in contains all the possible types, given env_decls and env_isa, before pushing down the type, or a type error
 - rtype contains the inferred type of the expression or a type error in the expression
 - env_mph contains a (relation,inferred type+declaration) binding for all relations in the type if there is no type error (in rtype)
 - me is a helper attribute containing <this> node
 -}

fatal :: Int -> String -> a
fatal regel msg = error ("!Fatal (module InfLibAG+Funcs "++show regel++"): "++msg )

{-GENERATED with uuagc -d -}
-- ISectList ---------------------------------------------------
type ISectList  = [(RelAlgExpr)]
-- RelAlgExpr --------------------------------------------------
data RelAlgExpr  = Comp (RelAlgExpr) (RelAlgExpr) 
                 | Compl (RelAlgExpr) 
                 | Conv (RelAlgExpr) 
                 | Equiv (RelAlgExpr) (RelAlgExpr) 
                 | ISect (ISectList) 
                 | Implic (RelAlgExpr) (RelAlgExpr) 
                 | Morph (RelAlgMorph) (RelAlgType) (Int) 
                 | RAdd (RelAlgExpr) (RelAlgExpr) 
                 | Union (UnionList) 
                 deriving ( Show)
-- UnionList ---------------------------------------------------
type UnionList  = [(RelAlgExpr)]
{-END GENERATED with uuagc -d -}

data RelAlgMorph = 
   DRel {rname::String}
  |IdRel
  |VRel
data RelAlgObj = Universe | EmptyObject | Object String deriving (Eq)
type RelAlgType = (RelAlgObj, RelAlgObj)
type Isa = [(RelAlgObj, RelAlgObj)]
data RelDecl = RelDecl {dname::String, dtype::RelAlgType, ishomo::Bool} | IDecl | VDecl

type InfType = Either RelAlgType TError
type AltList = Either [RelAlgType] TError
data ListOf = ListOfUnion | ListOfISect | NoListOf deriving(Eq)

instance Show RelAlgMorph where
  show (DRel x) = x
  show IdRel = "I"
  show VRel = "V"
instance Show RelDecl where
  show (RelDecl x _ _) = x
  show IDecl = "I"
  show VDecl = "V"
instance Show RelAlgObj where
  show Universe = "Universal set"
  show EmptyObject = "Empty set"
  show (Object x) = x

data TError =
   TErrorAmb ETitle [RelAlgType] -- the type of the root expression is ambiguous
  |TErrorU ETitle  -- the source or target of the type of the root expression is the universe
  |TErrorUC ETitle RelAlgExpr RelAlgExpr -- the composition is over the universe
  |TError0 ETitle RelAlgObj --the concept is not defined in isas or part of the type of any env_decls
  |TError1 ETitle RelAlgExpr --the relation expression is not defined in the env_decls
  |TError2 ETitle (RelAlgExpr,[RelAlgType]) ([RelAlgExpr],[RelAlgType]) --(ababab) there is no type in the first list matching a type in the second
  |TError3 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) --(abbcac) there is no b in the first list matching a b in the second
  |TError4 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) [RelAlgObj] --(abbcac) there is more than one b in the first list 
                                                         --matching a b in the second
  |TError5 ETitle RelDecl --The declaration has an heteogeneous type and an homogeneous property
  |TError6 ETitle RelAlgType RelAlgExpr RelDecl --The declaration bound to the relation expression 
                                                --has an homogeneous property, but the type inferred is heterogeneous
  deriving (Show)
 
type ETitle = String

----------------------------------------------------------------------------
--type inference rules
----------------------------------------------------------------------------
--DESCR -> general and specific can only be used on isarelated objects i.e. 
--         the expression must have a type (no type error) to be able to infer that type
--DESCR -> not_universe is an inference rule to prefer inferring a user-defined object above the universe object
isarelated,isspecific,isgeneral::RelAlgObj->RelAlgObj->Isa->Bool
not_universe::RelAlgObj->RelAlgObj->RelAlgObj
general,specific::RelAlgObj->RelAlgObj->Isa->RelAlgObj
isspecific x y isas = elem (x,y) isas
isgeneral x y isas = elem (y,x) isas
isarelated x y isas = isspecific x y isas || isgeneral x y isas
--REMARK: pattern "not_universe Universe Universe" is not added while the conclusion of an error for inferring the Universe is best taken elsewhere 
--        if the root expression infers a type containing the Universe or a composition over the Universe then there will be an error.
not_universe Universe x = x
not_universe x _ = x

general x y isas = if elem (x,y) isas 
                   then not_universe y x 
                   else if elem (y,x) isas 
                        then not_universe x y
                        else fatal 167 $ "A type can only be inferred if there is no type error. "
                                         ++show x++" is not a "++show y++" given the is-a hierarchy " ++ show isas ++"."
specific x y isas = if elem (x,y) isas 
                    then not_universe x y 
                    else if elem (y,x) isas 
                         then not_universe y x                          
                         else fatal 173 $ "A type can only be inferred if there is no type error. "
                                          ++show x++" is not a "++show y++" given the is-a hierarchy " ++ show isas ++"."

data InfRuleType = ISect_cs | ISect_ncs | ISect_mix
                  |Union_mix
                  |Comp_ncs | Comp_c1 | Comp_c2 | Comp_cs
                  |RAdd_ncs | RAdd_c1 | RAdd_c2 | RAdd_cs deriving (Show,Eq)

--DESCR -> match pattern of the expression to an inference rule
inferencerule_abbcac :: RelAlgExpr -> InfRuleType
inferencerule_abbcac (Comp (Compl{}) (Compl{})) = Comp_cs
inferencerule_abbcac (Comp (Compl{}) _ ) = Comp_c1
inferencerule_abbcac (Comp _  (Compl{})) = Comp_c2
inferencerule_abbcac (Comp{}) = Comp_ncs
inferencerule_abbcac (RAdd (Compl{}) (Compl{})) = RAdd_cs
inferencerule_abbcac (RAdd (Compl{}) _) = RAdd_c1
inferencerule_abbcac (RAdd _ (Compl{})) = RAdd_c2
inferencerule_abbcac (RAdd{}) = RAdd_ncs
inferencerule_abbcac _ = fatal 191 "inferencerule_abbcac is a function for composition or relative addition only."
inferencerule_ababab :: ListOf -> RelAlgExpr -> [RelAlgExpr] -> InfRuleType
inferencerule_ababab (ListOfUnion) _ _ = Union_mix
inferencerule_ababab (ListOfISect) _ [] = ISect_mix
inferencerule_ababab (ListOfISect) headx [tailx]
  | iscomplement headx && iscomplement tailx = ISect_cs
  | not(iscomplement headx && iscomplement tailx) = ISect_ncs
  | otherwise = ISect_mix
inferencerule_ababab (ListOfISect) headx _ 
  | iscomplement headx = ISect_mix
  | otherwise = ISect_ncs
inferencerule_ababab _ _ _ = fatal 202 "inferencerule_ababab is a function for union or intersection only"

iscomplement :: RelAlgExpr -> Bool
iscomplement (Compl{}) = True
iscomplement _ = False

--DESCR -> the expression is type checked, and must return one type, else fatal error
--         given an env_isa and an (abab=>ab)-inference rule to apply
--         given the two types, infer the type of the expression
--         remark that the expression must have a type (no type error) to be able to infer that type
infer_ababab :: Isa -> InfRuleType -> RelAlgType -> RelAlgType -> RelAlgType
infer_ababab isas irule (a,b) (a',b') = case irule of
   ISect_mix -> infer_ab specific 
   ISect_cs -> infer_ab general
   ISect_ncs -> infer_ab specific
   Union_mix -> infer_ab general
   _ -> fatal 218 "infer_ababab is a function for Union_* or ISect_* inference rules only"
   where
   infer_ab f = (f a a' isas, f b b' isas)

--DESCR -> the expression is type checked, and must return one type, else fatal error
--         given an env_isa and an (abbc=>ac)-inference rule to apply
--         given b from ab and b from bc, infer the b
--         remark that the expression must have a type (no type error) to be able to infer b
infer_abbcac_b :: Isa -> InfRuleType -> RelAlgObj -> RelAlgObj -> RelAlgObj
infer_abbcac_b isas irule lb rb = case irule of
   Comp_ncs -> specific lb rb isas
   Comp_c1 -> not_universe rb lb
   Comp_c2 -> not_universe lb rb
   Comp_cs -> general lb rb isas
   RAdd_ncs -> general lb rb isas
   RAdd_c1 -> not_universe rb lb
   RAdd_c2 -> not_universe lb rb
   RAdd_cs -> specific lb rb isas
   _ -> fatal 236 "infer_abbcac_b is a function for Comp_* or RAdd_* inference rules only"
----------------------------------------------------------------------------
--synthesize env_in
--bind the declarations to the relations by name
--and check if there are alternatives for the expression given env_decls
--if there are no alternatives then we can already return those errors
--if there is more than one alternative on the root node then the type of the (root) expression is ambiguous
--if there is one alternative, then
--we need to push_down the type to infer the type of all relations and the b's in expressions of type abbc=>ac
--type errors can still arise as a result of ambiguous b's or properties on relations like homogenity
----------------------------------------------------------------------------
--DESCR -> it is possible to have isarelated alternatives, only keep the most specific types as alternatives
--         types are isarelated if and only if both sources and targets are isarelated in the same direction
--         if we allow isarelated alternatives, then the type checker could infer a type in case it should
--         infer a type error. For example we could add the more general type (Universe,Universe) to all
--         alternative lists => then there will be no type errors.
--         rdisa is used in alts_* functions to infer the most specific alternatives only.
rdisa :: Isa -> [RelAlgType] -> [RelAlgType]
rdisa _ []        = []
rdisa isas ((x,y):xs) 
 | xy_genof_t_in_xs    =  rdisa isas xs --remove (x,y) if (x,y) is general of something in xs
 | otherwise = ((x,y):(rdisa isas xs))
  where xy_genof_t_in_xs = (not.null) [()|(x',y')<-xs,isgeneral x x' isas, isgeneral y y' isas]

alts_ababab :: (RelAlgExpr,[RelAlgExpr]) -> Isa -> AltList -> AltList -> AltList
alts_ababab _ _ ts (Left []) = ts --function pattern needed for recursion while we defined union/intersection lists
alts_ababab (hd,tl) isas (Left ts) (Left ts') = 
   if null alts 
   then Right$TError2 "Incompatible comparison" (hd,ts) (tl,ts')  
   else Left alts
   where alts = rdisa isas [(specific a a' isas,specific b b' isas)
                             |(a,b)<-ts,(a',b')<-ts',isarelated a a' isas, isarelated b b' isas]
alts_ababab _ _ _ (Right err)  = Right err
alts_ababab _ _ (Right err) _  = Right err

alts_abbcac :: (RelAlgExpr,RelAlgExpr) -> Isa -> AltList -> AltList -> AltList
alts_abbcac (l,r) isas (Left lts) (Left rts) = 
   if null alts 
   then Right$TError3 "Incompatible composition" (l,lts) (r,rts)
   else Left alts
   where alts = rdisa isas [(a,c)|(a,b)<-lts,(b',c)<-rts, isarelated b b' isas]
alts_abbcac _ _ _ (Right err)  = Right err
alts_abbcac _ _ (Right err) _  = Right err

alts_abba :: AltList -> AltList
alts_abba (Left ts) = Left [(b,a)|(a,b)<-ts]
alts_abba err = err

alts_compl :: RelAlgExpr -> AltList -> AltList
alts_compl me xs = case me of
     Compl (Morph{}) -> xs
     _ -> fatal 286 "Complements on relations only -> normalize"

alts_mph :: [RelDecl] -> Isa -> RelAlgExpr -> AltList
alts_mph reldecls isas me = 
   if null alts
   then Right$TError1 "Relation undefined" me
   else if null declerrs
        then Left (rdisa isas [x|Left x<-alts])
        else Right (head declerrs)
   where 
   homo_alt d 
      |ishomo d = if isarelated a b isas
                  then Left (specific a b isas, specific a b isas)
                  else Right$TError5 "Homogeneous property on heterogeneous relation" d
      |otherwise = Left (a,b)
      where (a,b) = dtype d
   declerrs = [x|Right x<-alts]
   alts' nm = [homo_alt d|d@(RelDecl{})<-reldecls, dname d==nm]
   alts = case me of
     Morph (DRel nm) ut@(ua,ub) _ -> [Right x|Right x<-alts' nm] ++
                                     if ut==(Universe,Universe)
                                          --get all declarations by name equivalence 
                                     then [Left x|Left x<-alts' nm]
                                          --get the declarations matching the user-defined type
                                     else [Left (specific ua c1 isas,specific ub c2 isas)
                                           |Left (c1,c2)<-alts' nm, isarelated c1 ua isas, isarelated c2 ub isas]
     --constant relations (I and V) have no declaration, use there type as alternative iff concepts are in isas
     Morph _ (ua,ub) _ -> [Right$TError0 "Concept undefined" c|c<-[ua,ub],not(isdef c)]
                          ++ [Left (ua,ub)|isdef ua, isdef ub]
           where      
           --an object is defined if it is in a relation declaration or isa declaration 
           isdef c = if isarelated c c isas 
                     then True
                     else if elem c [x|d@(RelDecl{})<-reldecls,x<-[fst(dtype d),snd(dtype d)]]
                          then fatal 270 ("Missing identity of defined object in isas: "++show (c,c))
                          else False
     _ -> fatal 312 "function alts_mph expects relation expressions only."

--------------------------------------------------------------------------------
--inherit push_type
--There is one alternative in the env_in of the root expression
--we need to push_down the type to infer the type of all relations and the b's in expressions of type abbc=>ac
--type errors can arise as a result of ambiguous b's or properties on relations like homogenity
--the b's must be inferred while pushing the type down
--other type objects must be inferred from the rtype of its subexpressions
--------------------------------------------------------------------------------
--DESCR -> given the type of the expression, push a type on the direct subexpressions
push_type_ababab,push_type_abba,push_type_abab :: RelAlgType -> RelAlgType
push_type_ababab t = t
push_type_abba (x,y) = (y,x)
push_type_abab t = t

push_type_abbcac :: (RelAlgExpr,RelAlgExpr) -> Isa -> InfRuleType -> RelAlgType -> AltList -> AltList -> Either (RelAlgType,RelAlgType) TError
push_type_abbcac _ _ _ _ _ (Left []) = fatal 329 "the AltList cannot be Left []."
push_type_abbcac _ _ _ _ (Left []) _ = fatal 330 "the AltList cannot be Left []."
push_type_abbcac _ _ _ _ _ (Right err) = Right err --error in rsub
push_type_abbcac _ _ _ _ (Right err) _ = Right err --error in lsub
push_type_abbcac (l,r) isas irule (inh_a,inh_c) (Left lalts) (Left ralts) = 
  if length final_t==1
       --REMARK: compositions are inferred from the right resulting in unnecessary composition ambiguities 
       --        p.e. r;I;V results in ambiguity of composition I;V while I;V is inferred to push down a type and not (r;I);V
  then if notuniverse then Left (fst(head final_t)) else Right$TErrorUC "Composition over the universal set" l r
  else if null final_t
            --this should not be possible while there are no user-defined type on expressions, only on relations
       then fatal 338 "the expression has a type error, there cannot be a type for this composition expression."
       else Right$TError4 "Ambiguous composition" (l,lalts) (r,ralts) [b|(((_,b),_),_)<-final_t]
  where  --final_t is like alts_abbcac only with an inferred b, given the inherited type
  notuniverse = snd(head final_t)
  final_t = [(infer_t (not_universe inh_a a, b) (b',not_universe inh_c c),not(b==Universe && b'==Universe))
            |(a,b)<-lalts,(b',c)<-ralts, isarelated a inh_a isas, isarelated c inh_c isas, isarelated b b' isas]
  infer_t (a,b) (b',c)  = ( (a,final_b) , (final_b ,c) )
    where final_b = infer_abbcac_b isas irule b b' 

lefttype :: Either (RelAlgType,RelAlgType) TError -> RelAlgType
lefttype (Left (x,_)) = x
lefttype (Right _) = fatal 348 "no Left, check is_b_error first before using function lefttype."

righttype :: Either (RelAlgType,RelAlgType) TError -> RelAlgType
righttype (Left (_,x)) = x
righttype (Right _) = fatal 352 "no Left, check is_b_error first before using function righttype."

b_error :: Either (RelAlgType,RelAlgType) TError -> TError 
b_error (Right err) = err
b_error _ = fatal 433 "no Right, check is_b_error first before using function b_error."
is_b_error :: Either (RelAlgType,RelAlgType) TError -> Bool 
is_b_error (Right _) = True
is_b_error _ = False


----------------------------------------------------------------------------
--synthesize rtype and env_mph
--There is one alternative in the env_in of the root expression
--This type has been pushed down to the relations without type errors while inferring the b's.
--final_infer_mph can now determine the final type of all relations, by matching the alternatives
--of the relation to the inherited type. This type will be added to the relation type binding env_mph.
--Like all expressions the type of the relation expression will be represented by rtype.
--More complex expressions can infer there final type based on the rtype of their subexpressions.
--
--In final_infer_mph we can still discover one sort of type error as a result of a violation of the homogenity
--property. This is only possible with the abbcac-like inference rules, while a and c have different bindings.
--in all other inference rules there are only a's and b's. 
--p.e. B isa A, C isa A, I[B];I[A];I[C] => isarelated B C isas = False
--Consider also B isa A, C isa A, I[B]\/I[A]\/I[C], here a type error of type AB->AB->AB should have already been
--discovered during checking of the possible alternatives of the expression.
----------------------------------------------------------------------------

final_infer_mph :: [RelDecl] -> RelAlgExpr -> Isa -> RelAlgType -> AltList -> InfType
final_infer_mph _ _ _ _ (Left []) = fatal 377 "the AltList cannot be Left []."
final_infer_mph _ _ _ _ (Right err) = Right err
final_infer_mph reldecls me isas (inh_a,inh_b) (Left alts) = final_t
   where
   alts' = [(a,b)|(a,b)<-alts, isarelated a inh_a isas && isarelated b inh_b isas]
   final_t = if null alts' 
             then fatal 383 "the expression has a type error, there cannot be a type for this relation."
                  --this should not be possible while there are no user-defined type on expressions, only on relations
             else if length alts'==1 
                  then check_infer_homo (not_universe inh_a (fst$head alts')
                                        ,not_universe inh_b (snd$head alts')) 
                  else fatal 388 $ "the expression has a type error, there cannot be a type for this relation."
                                   ++ show (alts',alts,me,(inh_a,inh_b))
   check_infer_homo (a,b) 
       | case me of
          (Morph (DRel{}) _ _)->ishomo d
          (Morph IdRel _ _)->True
          _ -> False
                   = if isarelated a b isas
                     then Left (specific a b isas,specific a b isas)
                     else Right$TError6 "Type is not homogeneous" (a,b) me d 
       | otherwise = Left (a,b)
       where d = thedecl reldecls isas me (Left(a,b))
   

--DESCR -> remark that the expression must have a type (no type error) to be able to get the declaration of a relation
thedecl :: [RelDecl] -> Isa -> RelAlgExpr -> InfType -> RelDecl
thedecl _ _ _ (Right _) = fatal 402 "the expression has a type error, there is no declaration for this relation."
thedecl reldecls isas me (Left(a,b)) = 
   if null alts
   then fatal 405 "the expression has a type error, there is no declaration for this relation."
   else if length alts==1   
        --SJO: Gerard, ik heb deze test tijdelijk uitgezet, omdat er dubbelen voorkomen in alts! Er zit dus een fout in.
        --GMI: Ik heb hem weer aangezet, want als de fatal voorkomt dan zit er een bug in InfLibAG. 
        then head alts
        else fatal 408 $ "the expression has a type error, there cannot be more than one declaration for this relation."
                          ++ show (alts,me,(a,b))
   where 
   alts = case me of
     Morph (DRel nm) _ _ -> [d|d@(RelDecl{dtype=(c1,c2)})<-reldecls
                              , dname d==nm, isarelated a c1 isas, isarelated b c2 isas]
     Morph IdRel _ _ -> [IDecl]
     Morph VRel _ _ -> [VDecl]
     _ -> fatal 415 "function thedecl expects relation expressions only."

final_infer_conv :: InfType -> InfType
final_infer_conv (Left (a,b)) = Left (b,a)
final_infer_conv err = err

final_infer_abbcac:: Isa->InfType->InfType->InfType
final_infer_abbcac isas (Left (a,b)) (Left (b',c))
  | isarelated b b' isas = Left (a,c)
  | otherwise = fatal 424 "the expression has a type error, there cannot be a type for this composition expression."
final_infer_abbcac _ _ (Right err)  = (Right err)
final_infer_abbcac _ (Right err) _  = (Right err)

--the @lhs.pushed_type is processed in the rtype of the most right conjunct/disjunct
final_infer_ababab :: Isa -> InfRuleType -> InfType -> InfType -> InfType
final_infer_ababab _ _ _ (Right err) = Right err --error in the head
final_infer_ababab _ _ (Right err) _ = Right err --error in the tail
final_infer_ababab isas irule (Left (a,b)) (Left (a',b')) 
   | isarelated a a' isas && isarelated b b' isas = Left$infer_ababab isas irule (a,b) (a',b') 
   | otherwise = fatal 434 "or there should be an error, or only one matching alternative"


----------------------------------------------------------------------------
--Normalise
----------------------------------------------------------------------------

normalise :: RelAlgExpr -> RelAlgExpr
--rules
normalise (Implic ant cons) = normalise$ compl ant \/ cons
normalise (Equiv ant cons) = normalise(ant |- cons) /\ normalise(cons |- ant)
normalise (Compl (Implic ant cons)) = normalise$compl(compl ant \/ cons)
normalise (Compl (Equiv ant cons)) = normalise$compl((ant |- cons) /\ (cons |- ant))
--demorgan
normalise (Compl (ISect xs)) = ISect$foldISect$map (normalise.compl) xs
normalise (Compl (Union xs)) = Union$foldUnion$map (normalise.compl) xs
normalise (Compl (Comp x y)) = normalise(compl x) *!* normalise(compl y)
normalise (Compl (RAdd x y)) = normalise(compl x) *.* normalise(compl y)
--compl/conv
normalise (Compl (Conv x)) = conv$normalise (compl x)
--double complement
normalise (Compl (Compl x)) = normalise x
--recursion
normalise (ISect xs) = ISect$foldISect$map normalise xs
normalise (Union xs) = Union$foldUnion$map normalise xs
normalise (Comp x y) = normalise x *.* normalise y
normalise (RAdd x y) = normalise x *!* normalise y
normalise x@(Conv (Morph{})) = x
normalise (Conv x) = conv$normalise x
normalise x@(Compl (Morph{})) = x
normalise x@(Morph{}) = x

(/\),(\/),(*.*),(*!*),(|-)::RelAlgExpr->RelAlgExpr->RelAlgExpr
compl,conv::RelAlgExpr->RelAlgExpr
(/\) x y = ISect [x,y]
(\/) x y = Union [x,y]
(*.*) x y = Comp x y
(*!*) x y = RAdd x y
(|-) x y = Implic x y
compl x = Compl x
conv x = Conv x

foldISect::[RelAlgExpr] -> [RelAlgExpr]
foldISect [] = []
foldISect ((ISect sxs):xs) = sxs ++ foldISect xs
foldISect (x:xs) = (x:foldISect xs)
foldUnion::[RelAlgExpr] -> [RelAlgExpr]
foldUnion [] = []
foldUnion ((Union sxs):xs) = sxs ++ foldUnion xs
foldUnion (x:xs) = (x:foldUnion xs)

