

-- UUAGC 0.9.10 (src/TypeInference/InfLibAG.ag)


module TypeInference.InfLibAG 
  (infer
  ,RelAlgExpr(..)
  ,RelAlgMorph(..)
  ,RelDecl(..) 
  ,RelAlgObj(..)
  ,RelAlgType
  ,Isa
  ,TError(..)
  ) where
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
fatal regel msg = error ("!Fatal (module InfLibAG "++show regel++"): "++msg )

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
{-
 -
 - InfAdlExpr errors: 
   showsPrec _ (ErrCode 1) = showString $ "[1] Type mismatch in rule"
   showsPrec _ (ErrCode 2) = showString $ "[2] Ambiguous type"

   InfLibAG errors:
   showsPrec _ (ErrCode 3) = showString $ "[3] Relation undefined"
   showsPrec _ (ErrCode 4) = showString $ "[4] Incompatible comparison" --union and disjunction
   showsPrec _ (ErrCode 5) = showString $ "[5] Incompatible composition"
   showsPrec _ (ErrCode 6) = showString $ "[6] Ambiguous composition"
   showsPrec _ (ErrCode 7) = showString $ "[7] Homogeneous property on heterogeneous relation"
   showsPrec _ (ErrCode 8) = showString $ "[8] Type is not homogeneous" 
   --showsPrec _ (ErrCode 9) = showString $ "[9] Type is not homogeneous" --merged with 8
   
   Isa errors:
   error $ show ["Concept "++show c1++" cannot be the specific of both "++show c2++" and "++show c3
                       ++ " if the order of "++show c2++" and "++show c3 ++ 
                       " is not specified. Specify the order with a GEN .. ISA .."
                       |(c1,c2,c3)<-checkrels]
 -}
data TError =
   TErrorAmb ETitle [RelAlgType] -- the type of the root expression is ambiguous
  |TErrorU ETitle RelAlgType -- the source or target of the type of the root expression is the universe
  |TError1 ETitle RelAlgExpr [RelDecl] --the relation expression is not defined in the env_decls
  |TError2 ETitle (RelAlgExpr,[RelAlgType]) ([RelAlgExpr],[RelAlgType]) --(ababab) there is no type in the first list matching a type in the second
  |TError3 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) --(abbcac) there is no b in the first list matching a b in the second
  |TError4 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) [RelAlgObj] --(abbcac) there is more than one b in the first list 
                                                         --matching a b in the second
  |TError5 ETitle RelDecl --The declaration has an heteogeneous type and an homogeneous property
  |TError6 ETitle RelAlgType RelAlgExpr RelDecl --The declaration bound to the relation expression 
                                                --has an homogeneous property, but the type inferred is heterogeneous
  deriving (Show)
 
type ETitle = String
type UserDefOp = String --the string representation of the user defined binary expression operator

infer:: [RelDecl] -> Isa -> RelAlgExpr -> Either (RelAlgType,[(RelAlgExpr,RelAlgType,RelDecl)]) TError
infer reldecls isas root_expr =
  case rtype of
    Left (x,y) -> if x==Universe || y==Universe 
                  then Right$TErrorU "Source or target is the universal set" (x,y) 
                  else Left ((x,y), env_mph )
    Right err -> Right err
  where
  env_mph = [(m,t,d)|(m,Left t,d)<-env_mph_Syn_RelAlgExpr (inftree (head alltypes))]
  env_in = env_in_Syn_RelAlgExpr (inftree (Universe,Universe))
  rtype = if length alltypes==1 
          then rtype_Syn_RelAlgExpr (inftree (head alltypes)) --finalize by pushing the type down again
          else if null alltypes
               then Right env_in_err
               else Right$TErrorAmb "Ambiguous type" alltypes
  alltypes = case env_in of
     Left xs -> if null xs 
                then fatal 214 "the AltList cannot be Left []."
                else xs 
     _ -> []
  Right env_in_err = env_in
  inftree push = wrap_RelAlgExpr (sem_RelAlgExpr$normalise$root_expr)$Inh_RelAlgExpr reldecls isas NoListOf push


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
not_universe Universe x = x
not_universe x _ = x

general x y isas = if elem (x,y) isas 
                   then not_universe y x 
                   else if elem (y,x) isas 
                        then not_universe x y
                        else fatal 167 $ "A type can only be inferred if there is no type error. "
                                         ++show x++" is not a "++show y++" given the is-a hierarchy " ++ show isas ++"."
specific x y isas = if elem (x,y) isas 
                    then x 
                    else if elem (y,x) isas 
                         then y                          
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
   Comp_c1 -> rb
   Comp_c2 -> lb
   Comp_cs -> general lb rb isas
   RAdd_ncs -> general lb rb isas
   RAdd_c1 -> rb
   RAdd_c2 -> lb
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
   then Right$TError1 "Relation undefined" me reldecls
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
     Morph _ ut _ -> [Left ut] --constant relations (I and V) have no declaration, use there type as alternative
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
  then Left (head final_t)
  else if null final_t
            --this should not be possible while there are no user-defined type on expressions, only on relations
       then fatal 338 "the expression has a type error, there cannot be a type for this composition expression."
       else Right$TError4 "Ambiguous composition" (l,lalts) (r,ralts) [b|((_,b),_)<-final_t]
  where  --final_t is like alts_abbcac only with an inferred b, given the inherited type
  final_t = [infer_t (not_universe inh_a a, b) (b',not_universe inh_c c)
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
                                 --  ++ show (alts',alts,me,(inh_a,inh_b))
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
        then head alts
         else fatal 408 "the expression has a type error, there cannot be more than one declaration for this relation."
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


----------------------------------------------------------------------------
--TEST
----------------------------------------------------------------------------
{-
main :: IO ()
main = print$show$[case result1 test of Left t -> (result1 test,result2 test); x -> (x,[]); |test<-tests]
--main = print$show [result0 test|test<-tests]

--testTree :: Tree
--testTree = Node (Tip 1) (Node (Tip 2) (Tip 3))
r0 = Morph (DRel "r0" ) (Universe,Universe) 1
r = Morph (DRel "r" ) (Universe,Universe) 1
s = Morph (DRel "s" ) (Universe,Universe) 1
ramb = Morph (DRel "ramb" ) (Universe,Universe) 1
ramb1 = Morph (DRel "ramb") (Object "A1",Object "B") 1
ramb2 = Morph (DRel "ramb") (Object "A2",Object "B") 1

testcpts = [Object "A",Object "A1",Object "A2",Object "B",Object "C",Object "D"]
--testisa = [(x,Universe)|x<-testcpts]++[(Universe,x)|x<-testcpts]++[(x,x)|x<-testcpts]
--testdecls = [RelDecl "ramb" (Object "A1",Object "B")
  --          ,RelDecl "ramb" (Object "A2",Object "B")
    --        ,RelDecl "s" (Object "C",Object "D")
      --      ,RelDecl "r" (Object "A",Object "B")]

testdecls = [RelDecl {dname = "item", dtype = (Object "Order",Object "Product")},RelDecl {dname = "item", dtype = (Object "Delivery",Object "Product")},RelDecl {dname = "of", dtype = (Object "Delivery",Object "Order")},RelDecl {dname = "provided", dtype = (Object "Provider",Object "Delivery")},RelDecl {dname = "accepted", dtype = (Object "Provider",Object "Order")},RelDecl {dname = "addressedTo", dtype = (Object "Order",Object "Provider")},RelDecl {dname = "deliveredTo", dtype = (Object "Delivery",Object "Client")},RelDecl {dname = "from", dtype = (Object "Order",Object "Client")},RelDecl {dname = "sentTo", dtype = (Object "Invoice",Object "Client")},RelDecl {dname = "delivery", dtype = (Object "Invoice",Object "Delivery")},RelDecl {dname = "from", dtype = (Object "Invoice",Object "Provider")},RelDecl {dname = "", dtype = (Universe,Universe)}]

testisa = [(Universe,Universe),(Object "Client",Universe),(Object "Client",Object "Client"),(Object "Delivery",Universe),(Object "Delivery",Object "Delivery"),(Object "Invoice",Universe),(Object "Invoice",Object "Invoice"),(Object "Order",Universe),(Object "Order",Object "Order"),(Object "Product",Universe),(Object "Product",Object "Product"),(Object "Provider",Universe),(Object "Provider",Object "Provider"),(EmptyObject,Object "Client"),(EmptyObject,Object "Delivery"),(EmptyObject,Object "Invoice"),(EmptyObject,Object "Order"),(EmptyObject,Object "Product"),(EmptyObject,Object "Provider"),(EmptyObject,EmptyObject),(EmptyObject,Object "S"),(Object "S",Universe),(Object "S",Object "S")]


tests = [test99]
tests0 = [test0a,test0b]
tests1 = [test1]
tests2 = [test2a,test2b]
tests3 = [test3a,test3b,test3c]
tests6 = [test6a,test6b]
tests7 = [test7a,test7b,test7c,test7d]
--test :: T_Tree
--test = sem_Tree testTree
test1::T_RelAlgExpr
test0a = sem_RelAlgExpr r
test0b= sem_RelAlgExpr ramb
test1= sem_RelAlgExpr (compl r /\ r)
test2a= sem_RelAlgExpr (s /\ r /\ r)
test2b= sem_RelAlgExpr (r /\ r /\ s)
test3a= sem_RelAlgExpr ((conv r) /\ (conv r) /\ (conv r))
test3b= sem_RelAlgExpr ((conv r) /\ (conv r) /\ ( r))
test3c= sem_RelAlgExpr (( r) /\ (conv r) /\ (conv r))
test4= sem_RelAlgExpr (r /\ r /\ r)
test5= sem_RelAlgExpr (r /\ r /\ r0)
test6a= sem_RelAlgExpr (r *.* (conv r))
test6b= sem_RelAlgExpr (r *.* r)
test7a= sem_RelAlgExpr (ramb *.* (conv ramb))
test7b= sem_RelAlgExpr ((conv ramb) *.* ramb1)
test7c= sem_RelAlgExpr ((conv ramb) *.* ramb2)
test7d= sem_RelAlgExpr (ramb *.* r)
test99 = sem_RelAlgExpr$
             ISect[
                  Union [Morph (DRel {rname = "sentTo"}) (Universe,Universe) 1,
                      Comp ( (Morph (DRel {rname = "delivery"}) (Universe,Universe) 2)) 
                            (Comp ( (Morph (DRel {rname = "of"}) (Universe,Universe) 3)) 
                                  ( (Morph (DRel {rname = "from"}) (Universe,Universe) 4))
                             )
                      ]
               ,Union [Compl (Morph (DRel {rname = "sentTo"}) (Universe,Universe) 5)
                      ,Comp (Morph (DRel {rname = "delivery"}) (Universe,Universe) 6) 
                            (Comp (Morph (DRel {rname = "of"}) (Universe,Universe) 7) 
                                  (Morph (DRel {rname = "from"}) (Universe,Universe) 8))]]


result0 :: T_RelAlgExpr -> AltList
result1 :: T_RelAlgExpr -> InfType
result2 :: T_RelAlgExpr -> [(RelAlgExpr, InfType, RelDecl)]
--result :: [Int]
--result = front_Syn_Tree (wrap_Tree test Inh_Tree)
result0 test = env_in_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 
result1 test = rtype_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 
result2 test = env_mph_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 
-}

-- ISectList ---------------------------------------------------
type ISectList  = [(RelAlgExpr)]
-- cata
sem_ISectList :: ISectList  ->
                 T_ISectList 
sem_ISectList list  =
    (Prelude.foldr sem_ISectList_Cons sem_ISectList_Nil (Prelude.map sem_RelAlgExpr list) )
-- semantic domain
type T_ISectList  = ([RelDecl]) ->
                    Isa ->
                    ListOf ->
                    RelAlgType ->
                    ( AltList,([(RelAlgExpr,InfType,RelDecl)]),ISectList,InfType)
data Inh_ISectList  = Inh_ISectList {env_decls_Inh_ISectList :: [RelDecl],env_isa_Inh_ISectList :: Isa,listof_Inh_ISectList :: ListOf,type_down_Inh_ISectList :: RelAlgType}
data Syn_ISectList  = Syn_ISectList {env_in_Syn_ISectList :: AltList,env_mph_Syn_ISectList :: [(RelAlgExpr,InfType,RelDecl)],me_Syn_ISectList :: ISectList,rtype_Syn_ISectList :: InfType}
wrap_ISectList :: T_ISectList  ->
                  Inh_ISectList  ->
                  Syn_ISectList 
wrap_ISectList sem (Inh_ISectList _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_ISectList _lhsOenv_in _lhsOenv_mph _lhsOme _lhsOrtype ))
sem_ISectList_Cons :: T_RelAlgExpr  ->
                      T_ISectList  ->
                      T_ISectList 
sem_ISectList_Cons hd_ tl_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: ISectList
              _hdOenv_decls :: ([RelDecl])
              _hdOenv_isa :: Isa
              _hdOlistof :: ListOf
              _hdOtype_down :: RelAlgType
              _tlOenv_decls :: ([RelDecl])
              _tlOenv_isa :: Isa
              _tlOlistof :: ListOf
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: AltList
              _hdIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _hdIme :: RelAlgExpr
              _hdIrtype :: InfType
              _tlIenv_in :: AltList
              _tlIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _tlIme :: ISectList
              _tlIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_ababab (_hdIme,_tlIme) _lhsIenv_isa _hdIenv_in _tlIenv_in
              _lhsOrtype =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIrtype
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          _tlIrtype _hdIrtype
              _lhsOenv_mph =
                  _hdIenv_mph ++ _tlIenv_mph
              _me =
                  (:) _hdIme _tlIme
              _lhsOme =
                  _me
              _hdOenv_decls =
                  _lhsIenv_decls
              _hdOenv_isa =
                  _lhsIenv_isa
              _hdOlistof =
                  _lhsIlistof
              _hdOtype_down =
                  _lhsItype_down
              _tlOenv_decls =
                  _lhsIenv_decls
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOlistof =
                  _lhsIlistof
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIenv_mph,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_decls _hdOenv_isa _hdOlistof _hdOtype_down )
              ( _tlIenv_in,_tlIenv_mph,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_decls _tlOenv_isa _tlOlistof _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_ISectList_Nil :: T_ISectList 
sem_ISectList_Nil  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: ISectList
              _lhsOenv_in =
                  Left []
              _lhsOrtype =
                  fatal 42 "undefined rtype on Nil of ISect-/UnionList"
              _lhsOenv_mph =
                  []
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
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
-- cata
sem_RelAlgExpr :: RelAlgExpr  ->
                  T_RelAlgExpr 
sem_RelAlgExpr (Comp _lsub _rsub )  =
    (sem_RelAlgExpr_Comp (sem_RelAlgExpr _lsub ) (sem_RelAlgExpr _rsub ) )
sem_RelAlgExpr (Compl _sub )  =
    (sem_RelAlgExpr_Compl (sem_RelAlgExpr _sub ) )
sem_RelAlgExpr (Conv _sub )  =
    (sem_RelAlgExpr_Conv (sem_RelAlgExpr _sub ) )
sem_RelAlgExpr (Equiv _lsub _rsub )  =
    (sem_RelAlgExpr_Equiv (sem_RelAlgExpr _lsub ) (sem_RelAlgExpr _rsub ) )
sem_RelAlgExpr (ISect _sublst )  =
    (sem_RelAlgExpr_ISect (sem_ISectList _sublst ) )
sem_RelAlgExpr (Implic _lsub _rsub )  =
    (sem_RelAlgExpr_Implic (sem_RelAlgExpr _lsub ) (sem_RelAlgExpr _rsub ) )
sem_RelAlgExpr (Morph _rel _usertype _locid )  =
    (sem_RelAlgExpr_Morph _rel _usertype _locid )
sem_RelAlgExpr (RAdd _lsub _rsub )  =
    (sem_RelAlgExpr_RAdd (sem_RelAlgExpr _lsub ) (sem_RelAlgExpr _rsub ) )
sem_RelAlgExpr (Union _sublst )  =
    (sem_RelAlgExpr_Union (sem_UnionList _sublst ) )
-- semantic domain
type T_RelAlgExpr  = ([RelDecl]) ->
                     Isa ->
                     ListOf ->
                     RelAlgType ->
                     ( AltList,([(RelAlgExpr,InfType,RelDecl)]),RelAlgExpr,InfType)
data Inh_RelAlgExpr  = Inh_RelAlgExpr {env_decls_Inh_RelAlgExpr :: [RelDecl],env_isa_Inh_RelAlgExpr :: Isa,listof_Inh_RelAlgExpr :: ListOf,type_down_Inh_RelAlgExpr :: RelAlgType}
data Syn_RelAlgExpr  = Syn_RelAlgExpr {env_in_Syn_RelAlgExpr :: AltList,env_mph_Syn_RelAlgExpr :: [(RelAlgExpr,InfType,RelDecl)],me_Syn_RelAlgExpr :: RelAlgExpr,rtype_Syn_RelAlgExpr :: InfType}
wrap_RelAlgExpr :: T_RelAlgExpr  ->
                   Inh_RelAlgExpr  ->
                   Syn_RelAlgExpr 
wrap_RelAlgExpr sem (Inh_RelAlgExpr _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_RelAlgExpr _lhsOenv_in _lhsOenv_mph _lhsOme _lhsOrtype ))
sem_RelAlgExpr_Comp :: T_RelAlgExpr  ->
                       T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_Comp lsub_ rsub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lsubOtype_down :: RelAlgType
              _rsubOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  push_type_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  if not(is_b_error _t) then lefttype _t else fatal 65 "There should be an ambiguous b error"
              _rsubOtype_down =
                  if not(is_b_error _t) then righttype _t else fatal 66 "There should be an ambiguous b error"
              _lhsOrtype =
                  if not(is_b_error _t)
                  then final_infer_abbcac _lhsIenv_isa _lsubIrtype _rsubIrtype
                  else Right(b_error _t)
              _lhsOenv_mph =
                  _lsubIenv_mph ++ _rsubIenv_mph
              _me =
                  Comp _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_decls =
                  _lhsIenv_decls
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOlistof =
                  _lhsIlistof
              _rsubOenv_decls =
                  _lhsIenv_decls
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOlistof =
                  _lhsIlistof
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Compl :: T_RelAlgExpr  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Compl sub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _subOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _subOenv_decls :: ([RelDecl])
              _subOenv_isa :: Isa
              _subOlistof :: ListOf
              _subIenv_in :: AltList
              _subIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _subIme :: RelAlgExpr
              _subIrtype :: InfType
              _lhsOenv_in =
                  alts_compl _me _subIenv_in
              _subOtype_down =
                  push_type_abab _lhsItype_down
              _lhsOrtype =
                  case _me of
                     Compl (Morph{}) -> _subIrtype
                     _ -> error "complements on mphs only -> normalize"
              _lhsOenv_mph =
                  _subIenv_mph
              _me =
                  Compl _subIme
              _lhsOme =
                  _me
              _subOenv_decls =
                  _lhsIenv_decls
              _subOenv_isa =
                  _lhsIenv_isa
              _subOlistof =
                  _lhsIlistof
              ( _subIenv_in,_subIenv_mph,_subIme,_subIrtype) =
                  (sub_ _subOenv_decls _subOenv_isa _subOlistof _subOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Conv :: T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_Conv sub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _subOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _subOenv_decls :: ([RelDecl])
              _subOenv_isa :: Isa
              _subOlistof :: ListOf
              _subIenv_in :: AltList
              _subIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _subIme :: RelAlgExpr
              _subIrtype :: InfType
              _lhsOenv_in =
                  alts_abba _subIenv_in
              _subOtype_down =
                  push_type_abba _lhsItype_down
              _lhsOrtype =
                  final_infer_conv _subIrtype
              _lhsOenv_mph =
                  _subIenv_mph
              _me =
                  Conv _subIme
              _lhsOme =
                  _me
              _subOenv_decls =
                  _lhsIenv_decls
              _subOenv_isa =
                  _lhsIenv_isa
              _subOlistof =
                  _lhsIlistof
              ( _subIenv_in,_subIenv_mph,_subIme,_subIrtype) =
                  (sub_ _subOenv_decls _subOenv_isa _subOlistof _subOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Equiv :: T_RelAlgExpr  ->
                        T_RelAlgExpr  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Equiv lsub_ rsub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _lsubOtype_down :: RelAlgType
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _rsubOtype_down :: RelAlgType
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  error "no equivalence rule symbols -> normalize"
              _lhsOrtype =
                  error "no equivalence rule symbols -> normalize"
              _lhsOenv_mph =
                  error "no equivalence rule symbols -> normalize"
              _me =
                  Equiv _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_decls =
                  _lhsIenv_decls
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOlistof =
                  _lhsIlistof
              _lsubOtype_down =
                  _lhsItype_down
              _rsubOenv_decls =
                  _lhsIenv_decls
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOlistof =
                  _lhsIlistof
              _rsubOtype_down =
                  _lhsItype_down
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_ISect :: T_ISectList  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_ISect sublst_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _sublstOlistof :: ListOf
              _lhsOenv_in :: AltList
              _sublstOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _sublstOenv_decls :: ([RelDecl])
              _sublstOenv_isa :: Isa
              _sublstIenv_in :: AltList
              _sublstIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _sublstIme :: ISectList
              _sublstIrtype :: InfType
              _sublstOlistof =
                  ListOfISect
              _lhsOenv_in =
                  _env
              _env =
                  _sublstIenv_in
              _sublstOtype_down =
                  push_type_ababab _lhsItype_down
              _lhsOrtype =
                  _sublstIrtype
              _lhsOenv_mph =
                  _sublstIenv_mph
              _me =
                  ISect _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_decls =
                  _lhsIenv_decls
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIenv_mph,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_decls _sublstOenv_isa _sublstOlistof _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Implic :: T_RelAlgExpr  ->
                         T_RelAlgExpr  ->
                         T_RelAlgExpr 
sem_RelAlgExpr_Implic lsub_ rsub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _lsubOtype_down :: RelAlgType
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _rsubOtype_down :: RelAlgType
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  error "no implication rule symbols -> normalize"
              _lhsOrtype =
                  error "no implication rule symbols -> normalize"
              _lhsOenv_mph =
                  error "no implication rule symbols -> normalize"
              _me =
                  Implic _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_decls =
                  _lhsIenv_decls
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOlistof =
                  _lhsIlistof
              _lsubOtype_down =
                  _lhsItype_down
              _rsubOenv_decls =
                  _lhsIenv_decls
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOlistof =
                  _lhsIlistof
              _rsubOtype_down =
                  _lhsItype_down
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Morph :: RelAlgMorph ->
                        RelAlgType ->
                        Int ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Morph rel_ usertype_ locid_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _lhsOenv_in =
                  _env
              _env =
                  alts_mph _lhsIenv_decls _lhsIenv_isa _me
              _t =
                  final_infer_mph _lhsIenv_decls _me _lhsIenv_isa _lhsItype_down _env
              _lhsOrtype =
                  _t
              _lhsOenv_mph =
                  [( _me, _t, thedecl _lhsIenv_decls _lhsIenv_isa _me _t)]
              _me =
                  Morph rel_ usertype_ locid_
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_RAdd :: T_RelAlgExpr  ->
                       T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_RAdd lsub_ rsub_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lsubOtype_down :: RelAlgType
              _rsubOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  push_type_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  if not(is_b_error _t) then lefttype _t else fatal 65 "There should be an ambiguous b error"
              _rsubOtype_down =
                  if not(is_b_error _t) then righttype _t else fatal 66 "There should be an ambiguous b error"
              _lhsOrtype =
                  if not(is_b_error _t)
                  then final_infer_abbcac _lhsIenv_isa _lsubIrtype _rsubIrtype
                  else Right(b_error _t)
              _lhsOenv_mph =
                  _lsubIenv_mph ++ _rsubIenv_mph
              _me =
                  RAdd _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_decls =
                  _lhsIenv_decls
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOlistof =
                  _lhsIlistof
              _rsubOenv_decls =
                  _lhsIenv_decls
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOlistof =
                  _lhsIlistof
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Union :: T_UnionList  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Union sublst_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _sublstOlistof :: ListOf
              _lhsOenv_in :: AltList
              _sublstOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: RelAlgExpr
              _sublstOenv_decls :: ([RelDecl])
              _sublstOenv_isa :: Isa
              _sublstIenv_in :: AltList
              _sublstIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _sublstIme :: UnionList
              _sublstIrtype :: InfType
              _sublstOlistof =
                  ListOfUnion
              _lhsOenv_in =
                  _env
              _env =
                  _sublstIenv_in
              _sublstOtype_down =
                  push_type_ababab _lhsItype_down
              _lhsOrtype =
                  _sublstIrtype
              _lhsOenv_mph =
                  _sublstIenv_mph
              _me =
                  Union _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_decls =
                  _lhsIenv_decls
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIenv_mph,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_decls _sublstOenv_isa _sublstOlistof _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
-- UnionList ---------------------------------------------------
type UnionList  = [(RelAlgExpr)]
-- cata
sem_UnionList :: UnionList  ->
                 T_UnionList 
sem_UnionList list  =
    (Prelude.foldr sem_UnionList_Cons sem_UnionList_Nil (Prelude.map sem_RelAlgExpr list) )
-- semantic domain
type T_UnionList  = ([RelDecl]) ->
                    Isa ->
                    ListOf ->
                    RelAlgType ->
                    ( AltList,([(RelAlgExpr,InfType,RelDecl)]),UnionList,InfType)
data Inh_UnionList  = Inh_UnionList {env_decls_Inh_UnionList :: [RelDecl],env_isa_Inh_UnionList :: Isa,listof_Inh_UnionList :: ListOf,type_down_Inh_UnionList :: RelAlgType}
data Syn_UnionList  = Syn_UnionList {env_in_Syn_UnionList :: AltList,env_mph_Syn_UnionList :: [(RelAlgExpr,InfType,RelDecl)],me_Syn_UnionList :: UnionList,rtype_Syn_UnionList :: InfType}
wrap_UnionList :: T_UnionList  ->
                  Inh_UnionList  ->
                  Syn_UnionList 
wrap_UnionList sem (Inh_UnionList _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_UnionList _lhsOenv_in _lhsOenv_mph _lhsOme _lhsOrtype ))
sem_UnionList_Cons :: T_RelAlgExpr  ->
                      T_UnionList  ->
                      T_UnionList 
sem_UnionList_Cons hd_ tl_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: UnionList
              _hdOenv_decls :: ([RelDecl])
              _hdOenv_isa :: Isa
              _hdOlistof :: ListOf
              _hdOtype_down :: RelAlgType
              _tlOenv_decls :: ([RelDecl])
              _tlOenv_isa :: Isa
              _tlOlistof :: ListOf
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: AltList
              _hdIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _hdIme :: RelAlgExpr
              _hdIrtype :: InfType
              _tlIenv_in :: AltList
              _tlIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _tlIme :: UnionList
              _tlIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_ababab (_hdIme,_tlIme) _lhsIenv_isa _hdIenv_in _tlIenv_in
              _lhsOrtype =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIrtype
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          _tlIrtype _hdIrtype
              _lhsOenv_mph =
                  _hdIenv_mph ++ _tlIenv_mph
              _me =
                  (:) _hdIme _tlIme
              _lhsOme =
                  _me
              _hdOenv_decls =
                  _lhsIenv_decls
              _hdOenv_isa =
                  _lhsIenv_isa
              _hdOlistof =
                  _lhsIlistof
              _hdOtype_down =
                  _lhsItype_down
              _tlOenv_decls =
                  _lhsIenv_decls
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOlistof =
                  _lhsIlistof
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIenv_mph,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_decls _hdOenv_isa _hdOlistof _hdOtype_down )
              ( _tlIenv_in,_tlIenv_mph,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_decls _tlOenv_isa _tlOlistof _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))
sem_UnionList_Nil :: T_UnionList 
sem_UnionList_Nil  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOme :: UnionList
              _lhsOenv_in =
                  Left []
              _lhsOrtype =
                  fatal 42 "undefined rtype on Nil of ISect-/UnionList"
              _lhsOenv_mph =
                  []
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))