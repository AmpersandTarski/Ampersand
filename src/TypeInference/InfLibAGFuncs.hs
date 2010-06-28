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
data RelDecl = RelDecl {dname::String, dtype::RelAlgType, ishomo::Bool} | IDecl | VDecl deriving (Eq)

type InfType = Either RelAlgType TError
type GlbType = RelAlgType --the most specific type (the type if all type decisions decide to infer the most specific concept)
data AltList = AltList [(RelAlgType,GlbType)] | AltListError TError
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
  |TErrorU ETitle InfTree -- the source or target of the type of the root expression is the universe
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

data InfTree = InfExprs InfRuleType (RelAlgType,RelAlgObj) [InfTree] | InfRel DeclRuleType RelAlgType RelDecl Int
               deriving (Show,Eq)
data DeclRuleType = D_rel|D_rel_h|D_rel_c|D_rel_c_h|D_id|D_v|D_id_c|D_v_c
                    deriving (Eq)
data InfRuleType = ISect_cs | ISect_ncs | ISect_mix
                  |Union_mix
                  |Comp_ncs | Comp_c1 | Comp_c2 | Comp_cs
                  |RAdd_ncs | RAdd_c1 | RAdd_c2 | RAdd_cs 
                  |Conv_nc | Conv_c
                   deriving (Eq)
instance Show DeclRuleType where
   show D_rel = "D-Rel"
   show D_rel_h = "D-RelH"
   show D_rel_c = "D-Rel-c"
   show D_rel_c_h = "D-RelH-c"
   show D_id = "D-Id"
   show D_v = "D-V"
   show D_id_c =  "D-Id"
   show D_v_c  = "D-V"
instance Show InfRuleType where
   show ISect_cs = "T-Intersect-cs"
   show ISect_ncs = "T-Intersect-ncs"
   show ISect_mix = "T-Intersect"
   show Union_mix = "T-Union"
   show Comp_ncs = "T-Comp"
   show Comp_c1 = "T-Comp-c1"
   show Comp_c2 =  "T-Comp-c2"
   show Comp_cs  = "T-Comp-cs"
   show RAdd_ncs = "T-RelAdd"
   show RAdd_c1 = "T-RelAdd-c1"
   show RAdd_c2 =  "T-RelAdd-c2"
   show RAdd_cs  = "T-RelAdd-cs"
   show Conv_nc =  "T-Conv"
   show Conv_c  = "T-Conv-c"
inferred :: InfType -> RelAlgType
inferred (Left tp) = tp
inferred _ = (EmptyObject,EmptyObject)

--the imaginairy unary union/intersection
headofaxiomlist :: InfRuleType -> InfType -> InfTree -> InfTree
headofaxiomlist rt tp t = InfExprs rt (inferred tp,EmptyObject) [t]
--combine the trees of the head and tail
--the new tree gets the rule of the head
axiomlist :: InfType -> InfTree -> InfTree -> InfTree
axiomlist tp (InfExprs hdrule _ hdts) (InfExprs _ _ tlts) = InfExprs hdrule (inferred tp,EmptyObject) (hdts++tlts)
--axiomlist tp (InfExprs Union_mix _ hdts) (InfExprs Union_mix _ tlts) = InfExprs Union_mix (inferred tp,EmptyObject) (hdts++tlts)
--axiomlist tp (InfExprs hdrule _ hdts) (InfExprs tlrule _ tlts) 
--  | hdrule==tlrule = InfExprs hdrule (inferred tp,EmptyObject) (hdts++tlts) --REMARK: assuming ISect_*
--  | otherwise = InfExprs ISect_mix (inferred tp,EmptyObject) (hdts++tlts)
axiomlist _ _ _ = fatal 148 "These axioms cannot be merged into one inf tree."

complement_rule :: InfType -> InfTree -> InfTree
complement_rule _ (InfExprs Conv_nc tp t) =  InfExprs Conv_c {-(inferred tp,EmptyObject)-} tp t
complement_rule _ (InfRel dtype tp r i) = case dtype of
   D_rel -> InfRel D_rel_c {-(inferred tp)-} tp r i
   D_rel_h -> InfRel D_rel_c_h {-(inferred tp)-} tp r i
   D_id -> InfRel D_id_c {-(inferred tp)-} tp r i
   D_v -> InfRel D_v_c {-(inferred tp)-} tp r i
   _ -> fatal 162 "double complements not allowed -> normalize"
complement_rule _ _ = fatal 163 "complements on relations and conversions only -> normalize"

--DESCR -> match pattern of the expression to an inference rule
inferencerule_abbcac :: RelAlgExpr -> InfRuleType
inferencerule_abbcac (Comp x y)
  |iscomplement x && iscomplement y = Comp_cs
  |iscomplement x = Comp_c1
  |iscomplement y = Comp_c2
  |otherwise = Comp_ncs
inferencerule_abbcac (RAdd x y)
  |iscomplement x && iscomplement y = RAdd_cs
  |iscomplement x = RAdd_c1
  |iscomplement y = RAdd_c2
  |otherwise = RAdd_ncs
inferencerule_abbcac _ = fatal 191 "inferencerule_abbcac is a function for composition or relative addition only."
--the inference rule of a union or intersection if the list (R:S) is considered an expression like R\/S or R/\S where R is the head of the list and S the tail
inferencerule_ababab :: ListOf -> RelAlgExpr -> [RelAlgExpr] -> InfRuleType
inferencerule_ababab (ListOfUnion) _ _ = Union_mix
inferencerule_ababab (ListOfISect) headx []  
  | iscomplement headx = ISect_cs
  | otherwise = ISect_ncs
inferencerule_ababab (ListOfISect) headx (tailx:tailxs) =
  case (inferencerule_ababab (ListOfISect) tailx tailxs) of
    ISect_ncs -> if iscomplement headx then ISect_mix else ISect_ncs
    x -> if iscomplement headx then x else fatal 217 "sort intersection expressions, first complements then non-complements"
inferencerule_ababab _ _ _ = fatal 202 "inferencerule_ababab is a function for union or intersection only"

iscomplement :: RelAlgExpr -> Bool
iscomplement (Compl{}) = True
iscomplement (Conv(Compl{})) = True
iscomplement _ = False

--DESCR -> the expression is type checked, and must return one type, else fatal error
--         given an env_isa and an (abab=>ab)-inference rule to apply
--         given the two types, infer the type of the expression
--         remark that the expression must have a type (no type error) to be able to infer that type
infer_ababab :: Isa -> InfRuleType -> RelAlgType -> RelAlgType -> RelAlgType
infer_ababab isas irule (a,b) (a',b') = case irule of
   ISect_mix -> (not_universe a' a,not_universe b' b) --infer right (left is complement) (the right is an intersection expression with the tail as conjuncts)
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
   Comp_c1 -> not_universe rb lb --the b of +S
   Comp_c2 -> not_universe lb rb --the b of +R
   Comp_cs -> general lb rb isas
   RAdd_ncs -> general lb rb isas
   RAdd_c1 -> not_universe lb rb --the b of -r
   RAdd_c2 -> not_universe rb lb --the b of -s
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
{-
rdisa :: Isa -> [RelAlgType] -> [RelAlgType]
rdisa _ []        = []
rdisa isas ((x,y):xs) 
 | xy_genof_t_in_xs    =  rdisa isas xs --remove (x,y) if (x,y) is general of something in xs
 | otherwise = ((x,y):(rdisa isas xs))
  where xy_genof_t_in_xs = (not.null) [()|(x',y')<-xs,isgeneral x x' isas, isgeneral y y' isas]
-}

alttypes :: AltList -> [(RelAlgType,GlbType)]
alttypes (AltList xs) = xs -- [x|Left x<-xs] ++ [((a,a),(sa,sa))|Right (a,sa)<-xs]
alttypes (AltListError{}) = []

alterror :: AltList -> TError
alterror (AltListError x) = x
alterror _ = fatal 282 "only use alterror if there is an error"

--the decision for a and b must also be made here
--the decision for a and b expresses itself in the type, and thus the alternatives, of the intersection/union
--partiality is checked on the glb_a and glb_b
--IF there is not an error in hdalts or tlalts, but there will be one in hd.rtype or tl.rtype, then this is handled when calculating lhs.rtype.
alts_ababab :: InfRuleType -> (RelAlgExpr,[RelAlgExpr]) -> Isa -> AltList -> AltList -> AltList
alts_ababab _ (_,[]) _ ts _ = ts --function pattern needed for recursion while we defined union/intersection lists
alts_ababab _ _ _ _ (AltListError err)  = AltListError err
alts_ababab _ _ _ (AltListError err) _  = AltListError err
alts_ababab rt (hd,tl) isas hdalts tlalts 
--  |ishomoababab && not(null altsh) = AltListH altsh
  |not(null alts) = AltList alts
  |otherwise = AltListError$TError2 "Incompatible comparison" (hd,map snd (alttypes hdalts)) (tl,map snd (alttypes tlalts))  
   where
   --if one of the subexpressions is homogeneous then so is the ababab expression
--   ishomoababab = ishomoalt hdalts || ishomoalt tlalts
   alts :: [(RelAlgType,GlbType)]
   alts = {-rdisa isas-} 
          [(alt,salt)
          |((a,b),(sa,sb))<-alttypes hdalts
          ,((a',b'),(sa',sb'))<-alttypes tlalts
          --check if there is a glb_a and glb_b, i.e. infer_ababab can be applied
          ,isarelated sa sa' isas, isarelated sb sb' isas  
          --everything that can be inferred will be added to alts', keeping track of the most specific type 
          ,let alt=infer_ababab isas rt (a,b) (a',b') 
          ,let salt = (specific sa sa' isas,specific sb sb' isas)
          ]
{-   altslh :: [(RelAlgTypeH,GlbTypeH)]
-   altslh = {-rdisa isas-} 
-            [(alt,salt)
-            |Right(a,sa)<-hdalts
-            ,Left ((a',b'),(sa',sb'))<-tlalts
-            --check if there is a glb_a and glb_b, i.e. infer_ababab can be applied
-            ,isarelated sa sa' isas, isarelated sa sb' isas  
-            --everything that can be inferred will be added to alts', keeping track of the most specific type 
-            ,let (alta,altb)=infer_ababab isas (inferencerule_ababab lo hd tl) (a,a) (a',b')
-            ,let salt = specific sa (specific sa' sb' isas) isas 
-            ]
-   altsh :: [(RelAlgTypeH,GlbTypeH)]
-   altsh = {-rdisa isas-} 
-          [(alt,salt)
-          |((a,b),(sa,sb))<-alttypes hdalts
-          ,((a',b'),(sa',sb'))<-alttypes tlalts
-          --check if there is a glb_h, i.e. infer_ababab can be applied
-          ,isarelated sa sa' isas, isarelated sb sb' isas  
-          ,isarelated sa sb' isas, isarelated sb sa' isas
-          --everything that can be inferred will be added to alts', keeping track of the most specific type 
-          ,let ah = specific a b isas
-          ,let ah' = specific a' b' isas
-          ,let alt=infer_ababab isas (inferencerule_ababab lo hd tl) ah ah'
-          ,let salt = specific (specific sa sa' isas) (specific sb sb' isas) isas 
-          ] 
-}


--the decision for b is not made yet, only the partiality of the composition/addition is checked
--the decision for b does not express itself in the type of the composition/addition expression
--only in the calculated relation set of the expression
alts_abbcac :: InfRuleType -> (RelAlgExpr,RelAlgExpr) -> Isa -> AltList -> AltList -> AltList
alts_abbcac _ _ _ _ (AltListError err)  = AltListError err
alts_abbcac _ _ _ (AltListError err) _  = AltListError err
alts_abbcac rt (l,r) isas lalts ralts 
--  |ishomoabbcac && not(null altsh) = AltListH altsh
  |not(null alts) = AltList alts
  |otherwise = AltListError abbcacerror
   where 
   --if both of the subexpressions are homogeneous then so is the abbcac expression
--   ishomoabbcac = ishomoalt lalts & ishomoalt ralts
--   altsh = [(a,sa)|((a,a'),(sa,sa'))<-alts,a==a',sa==sa']
--iedere (a,c) met hooguit één b /= Universe, dat is een alternatief
   alts = [((a,c),(sa,sc))|((a,c),(sa,sc),b)<-rawalts,nootherb (a,c,b)]
   nootherb (a,c,b) =  null [b'|((a',c'),_,b')<-rawalts,a==a',c==c',b/=b']
--Als ik geen alternatief heb dan heb ik een fout
-- OF een lijst
--     + iedere (a,c) met meerdere b /= Universe <- Ambigue
--     + iedere (a,c) met hooguit één b==Universe <- compositie over Universe
-- OF als geen (a,c) i.e. null rawalts <- incompatibel  r s
   abbcacerror = if null rawalts 
                 then TError3 "Incompatible composition" (l,map snd (alttypes lalts)) (r,map snd (alttypes ralts))
                 else if unierror
                      then TErrorUC "Composition over the universal set" l r
                      else TError4 "Ambiguous composition" (l,map fst (alttypes lalts)) (r,map fst (alttypes ralts)) ambb 
   ambb = [b|b<-errorbs,b/=Universe]
   unierror = not(null errorbs) && null ambb
   errorbs = [b|((a',c'),_,b)<-rawalts,a==a',c==c']
        where ((a,c),_,_) = head rawalts    --TODO -> one error with all ambiguous and universe errors instead of just one
   rawalts = {-rdisa isas-} 
             [((a,c),(sa,sc),altb)
             |((a,b),(sa,sb))<-alttypes lalts
             ,((b',c),(sb',sc))<-alttypes ralts
             --check if there is a glb_b, i.e. infer_abbcac can be applied
             ,isarelated sb sb' isas
             ,let altb = infer_abbcac_b isas rt b b'] 
--    | ishomoalt lalts & ishomoalt ralts
--        =  {-rdisa isas-} 
--           [((alta,alta),(salta,salta),alta)
--           |(a,sa)<-(\AltListH xs->xs) lalts
--           ,(a',sa')<-(\AltListH xs->xs) ralts
--           --check if there is a glb_a, i.e. infer_abbcac can be applied
--           ,isarelated sa sa' isas 
--           ,let alta = infer_abbcac isas (inferencerule_abbcac orig) a a'
--           ,let salta = specific sa sa' isas] 
--    | ishomoalt lalts
--         = {-rdisa isas-} 
--           [((altb,c),(saltb,sc),altb)
--           |(b,sb)<-(\AltListH xs->xs) lalts
--           ,((b',c),(sb',sc))<-alttypes ralts
--           --check if there is a glb_b, i.e. infer_abbcac can be applied
--           ,isarelated sb sb' isas
--           ,let altb = infer_abbcac isas (inferencerule_abbcac orig) b b'
--           ,let saltb = specific sb sb' isas]
--    | ishomoalt ralts
--         = {-rdisa isas-} 
--           [((a,altb),(sa,saltb),altb)
--           |((a,b),(sa,sb))<-alttypes lalts
--           ,(b',sb')<-(\AltListH xs->xs) ralts
--           --check if there is a glb_b, i.e. infer_abbcac can be applied
--           ,isarelated sb sb' isas
--           ,let altb = infer_abbcac isas (inferencerule_abbcac orig) b b'
--           ,let saltb = specific sb sb' isas]
--    | otherwise

--infer_abbcac_b :: Isa -> InfRuleType -> RelAlgObj -> RelAlgObj -> RelAlgObj

alts_abba :: AltList -> AltList
alts_abba (AltList ts) = AltList [((b,a),(sb,sa))|((a,b),(sa,sb))<-ts]
alts_abba err = err 

--(article) -> D-Rel-c + D-RelH-c
alts_compl :: RelAlgExpr -> AltList -> AltList
alts_compl me xs = case me of
     Compl (Morph{}) -> xs
     _ -> fatal 286 "Complements on relations only -> normalize"

alts_mph :: [RelDecl] -> Isa -> RelAlgExpr -> AltList
alts_mph reldecls isas me = 
   if null alts
   then AltListError$TError1 "Relation undefined" me
   else if null declerrs
        then AltList [x|Left x<-alts]
        else AltListError (head declerrs)
   where 
   checkhomo d = if not(ishomo d) || a==b --REMARK -> a==b and not a `isarelated` b
                 then Left (a,b)
                 else Right$TError5 "Homogeneous property on heterogeneous relation" d
                 where (a,b) = dtype d
   declerrs = [x|Right x<-alts]
   --(article) -> D-Rel + D-RelH
   --the homochecked alts with a certain name
   --TODO -> can I do this?:
   --If there are multiple declarations with isarelated types, then the most specific is used.
   --the others can be ignored.
   alts' nm = xs -- [Right x|Right x<-xs] ++ (rdisa isas [y|Left (x,y)<-xs])
      where xs = [checkhomo d|d@(RelDecl{})<-reldecls, dname d==nm] 
   --DESCR -> the alternatives in reldecls of a relation optionally with user-defined type
   alts = case me of
     Morph (DRel nm) ut@(ua,ub) _ -> [Right x|Right x<-alts' nm] ++
                                     if ut==(Universe,Universe)
                                     --get all declarations by name equivalence 
                                     then [Left (x,x)|Left x<-alts' nm]
                                     --get the declarations matching the user-defined type
                                     else [Left ((ua,ub),(specific ua c1 isas,specific ub c2 isas))
                                           |Left (c1,c2)<-alts' nm, isarelated c1 ua isas, isarelated c2 ub isas]
     --(article) -> D-Id + D-V
     --constant relations (I and V) have no declaration, use there type as alternative if concepts are defined
     Morph m (ua,ub) _ -> [Right$TError0 "Concept undefined" c|c<-[ua,ub],not(isdef c)]
                          ++ [Left ((ua,ub),(ua,ub))|isdef ua, isdef ub]
           where      
           --(article) -> D-Cpt-Rel + D-Cpt-Isa
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
push_type_abba :: RelAlgType -> RelAlgType
push_type_abba (x,y) = (y,x)

{-
is_type_error :: InfType -> Bool
is_type_error (Right _) = True
is_type_error _ = False
thetype :: InfType -> RelAlgType
thetype (Left x) = x
thetype (Right _) = fatal 356 "no Left, check is_type_error first before using function thetype."
-}

push_type_abbcac :: (RelAlgExpr,RelAlgExpr) -> Isa -> InfRuleType -> RelAlgType -> AltList -> AltList -> RelAlgObj
push_type_abbcac _ _ _ _ _ (AltListError err) = fatal 478 "error should have been detected earlier"
push_type_abbcac _ _ _ _ (AltListError err) _ = fatal 479 "error should have been detected earlier"
push_type_abbcac (l,r) isas rt (inh_a,inh_c) lalts ralts = 
  if length bs==1
  then head bs
  else fatal 485 "there must be only one b, because alts_abbcbc should have precalculated if there is only one b, otherwise it should have returned an error"
  where
  --filter the alternatives given the pushed type, GlbTypes are not needed anymore
  --there must be only one b, because alts_abbcbc has precalculated if there is only one b, otherwise it returned an error
  flalts = [(a,b)|((a,b),_)<-alttypes lalts,isarelated a inh_a isas]
  fralts = [(b,c)|((b,c),_)<-alttypes ralts,isarelated c inh_c isas]
  bs = [infer_abbcac_b isas rt b b' |(a,b)<-flalts,(b',c)<-fralts ,isarelated b b' isas] 
--       --TODO CHECK THIS STATEMENT: compositions are inferred from the right resulting in unnecessary composition ambiguities 
--       --        p.e. r;I;V results in ambiguity of composition I;V while I;V is inferred to push down a type and not (r;I);V
 
{-
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
-}

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

final_infer_mph :: [RelDecl] -> RelAlgExpr -> Isa -> RelAlgType -> AltList -> (RelAlgType,InfTree,RelDecl)
final_infer_mph _ _ _ _ (AltListError err) = fatal 532 "error should have been detected earlier"
final_infer_mph reldecls me isas (inh_a,inh_b) alts = ((fa,fb),tr,d) 
   where
   --filter the alternatives given the pushed type, GlbTypes are not needed anymore
   falts = [(a,b)|((a,b),_)<-alttypes alts, isarelated a inh_a isas, isarelated b inh_b isas]
   (fa,fb) = if length falts==1
             then (not_universe (fst$head falts) inh_a
                  ,not_universe (snd$head falts) inh_b) --the declared/userdefined type /= Universe
             else fatal 540 $ "error should have been detected earlier"
   d = if null ds
       then fatal 405 "the expression has a type error, there is no declaration for this relation."
       else head ds --one declaration is enough, there may be more...
   ds = case me of
        Morph (DRel nm) _ _ -> [d|d@(RelDecl{dtype=(c1,c2)})<-reldecls
                                 ,dname d==nm, isarelated fa c1 isas, isarelated fb c2 isas]
        Morph IdRel _ _ -> [IDecl]
        Morph VRel _ _ -> [VDecl]
        _ -> fatal 415 "function thedecl expects relation expressions only."
   tr = case d of
            RelDecl{} -> InfRel D_rel (fa,fb) d i 
            IDecl -> InfRel D_id (fa,fb) IDecl i
            VDecl -> InfRel D_v (fa,fb) VDecl i
        where 
        i = case me of Morph _ _ i' -> i'; _ -> fatal 434 "This is not a simple expression.";
   
{-
final_infer_conv :: InfType -> InfType
final_infer_conv (Left (a,b)) = Left (b,a)
final_infer_conv err = err

final_infer_abbcac:: Isa->InfType->InfType->InfType
final_infer_abbcac isas (Left (a,b)) (Left (b',c))
  | isarelated b b' isas && not(b==Universe && b'==Universe) = Left (a,c)
  | otherwise = fatal 424 "the expression has a type error, there cannot be a type for this composition expression."
final_infer_abbcac _ _ (Right err)  = (Right err)
final_infer_abbcac _ (Right err) _  = (Right err)
-}

----------------------------------------------------------------------------
--Normalise
----------------------------------------------------------------------------

normalise :: RelAlgExpr -> RelAlgExpr
--done
normalise x@(Morph{}) = x
normalise x@(Conv (Morph{})) = x
normalise x@(Compl (Morph{})) = x
normalise (Compl (Conv s@(Morph{}))) = conv(compl s)
--rules
normalise (Implic ant cons) = normalise$ compl ant \/ cons
normalise (Equiv ant cons) = normalise(ant |- cons) /\ normalise(cons |- ant)
normalise (Compl (Implic ant cons)) = normalise$compl(compl ant \/ cons)
normalise (Compl (Equiv ant cons)) = normalise$compl((ant |- cons) /\ (cons |- ant))
--demorgan
normalise (Compl (ISect xs)) = ISect$complsfirst$foldISect$map (normalise.compl) xs
normalise (Compl (Union xs)) = Union$foldUnion$map (normalise.compl) xs
normalise (Compl (Comp x y)) = normalise(compl x) *!* normalise(compl y)
normalise (Compl (RAdd x y)) = normalise(compl x) *.* normalise(compl y)
--double complement
normalise (Compl (Compl x)) = normalise x
--compl/conv
normalise (Compl (Conv x)) = conv$normalise (compl x)
--recursion
normalise (ISect xs) = ISect$complsfirst$foldISect$map normalise xs
normalise (Union xs) = Union$foldUnion$map normalise xs
normalise (Comp x y) = normalise x *.* normalise y
normalise (RAdd x y) = normalise x *!* normalise y
normalise (Conv x) = conv$normalise x

complsfirst xs = [x|x<-xs,iscomplement x]++[x|x<-xs,not(iscomplement x)]

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

