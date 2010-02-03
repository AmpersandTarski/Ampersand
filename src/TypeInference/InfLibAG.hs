

-- UUAGC 0.9.10 (src/TypeInference/InfLibAG.ag)


module TypeInference.InfLibAG where
--import Adl
--the heterogeneous relation algebra with a possibility to explicitly declare types on morphisms in an expression
--a is a data structure for storing language statement meta data like file position of a declaration

data RelAlgMorph = 
   DRel {rname::String}
  |IdRel
  |VRel
  deriving (Show)
data RelAlgObj = Universe | EmptyObject | Object String deriving (Show,Eq)
type RelAlgType = (RelAlgObj, RelAlgObj)
type Isa = [(RelAlgObj, RelAlgObj)]
data RelDecl = RelDecl {dname::String, dtype::RelAlgType} | IDecl | VDecl deriving (Show)

type InfType = Either RelAlgType String
type AltList = Either [RelAlgType] String
data ListOf = ListOfUnion | ListOfISect | NoListOf 

isarelated x y isas = elem (x,y) isas || elem (y,x) isas
general Universe y _ = y
general x Universe _ = x
general x y isas = if elem (x,y) isas then y else if elem (y,x) isas then x else EmptyObject 
specific Universe y isas = y
specific x Universe isas = x
specific x y isas = if elem (x,y) isas then x else if elem (y,x) isas then y else EmptyObject

----------------------------------------------------------------------------
--type inference rules
----------------------------------------------------------------------------
data InfRuleType = ISect_cs | ISect_ncs | ISect_mix
                  |Union_mix
                  |Comp_ncs | Comp_c1 | Comp_c2 | Comp_cs
                  |RAdd_ncs | RAdd_c1 | RAdd_c2 | RAdd_cs deriving (Show,Eq)

inferencerule_abbcac :: RelAlgExpr -> InfRuleType
inferencerule_abbcac (Comp (Compl{}) (Compl{})) = Comp_cs
inferencerule_abbcac (Comp (Compl{}) _ ) = Comp_c1
inferencerule_abbcac (Comp _  (Compl{})) = Comp_c2
inferencerule_abbcac (Comp{}) = Comp_ncs
inferencerule_abbcac (RAdd (Compl{}) (Compl{})) = RAdd_cs
inferencerule_abbcac (RAdd (Compl{}) _) = RAdd_c1
inferencerule_abbcac (RAdd _ (Compl{})) = RAdd_c2
inferencerule_abbcac (RAdd{}) = RAdd_ncs
inferencerule_abbcac _ = error "not a composition or relative addition"
inferencerule_ababab :: ListOf -> RelAlgExpr -> [RelAlgExpr] -> InfRuleType
inferencerule_ababab (ListOfUnion) _ _ = Union_mix
inferencerule_ababab (ListOfISect) headx [] = ISect_mix
inferencerule_ababab (ListOfISect) headx [tailx]
  | iscomplement headx && iscomplement tailx = ISect_cs
  | not(iscomplement headx && iscomplement tailx) = ISect_ncs
  | otherwise = ISect_mix
inferencerule_ababab (ListOfISect) headx tailx
  | iscomplement headx = ISect_mix
  | otherwise = ISect_ncs
inferencerule_ababab _ _ _ = error "not a union or isect list"


iscomplement :: RelAlgExpr -> Bool
iscomplement (Compl{}) = True
iscomplement _ = False

--DESCR -> the expression is type checked, and must return one type, else fatal error
infer_ababab :: Isa -> InfRuleType -> RelAlgType -> RelAlgType -> RelAlgType
infer_ababab isas irule (inh_tx,inh_ty) (syn_tx,syn_ty) = case irule of
   ISect_mix -> infer_ab specific 
   ISect_cs -> infer_ab general
   ISect_ncs -> infer_ab specific
   Union_mix -> infer_ab general
   _ -> error "fatal: use another infer function."
   where
   infer_ab f = (f inh_tx syn_tx isas
                ,f inh_ty syn_ty isas)

--DESCR -> the expression is type checked, and must return one type, else fatal error
infer_abbcac_b :: Isa -> InfRuleType -> RelAlgObj -> RelAlgObj -> RelAlgObj
infer_abbcac_b isas irule ly rx = case irule of
   Comp_ncs -> specific ly rx isas
   Comp_c1 -> rx
   Comp_c2 -> ly
   Comp_cs -> general ly rx isas
   RAdd_ncs -> general ly rx isas
   RAdd_c1 -> rx
   RAdd_c2 -> ly
   RAdd_cs -> specific ly rx isas
   _ -> error "fatal: use another infer function."
----------------------------------------------------------------------------
--synthesize env_in
--get the declarations on the morphisms
--and check if there are alternatives for the expression
--if there are no alternatives then we can already return those errors
----------------------------------------------------------------------------
--only keep the most specific types as alternatives
rdisa :: Isa -> [RelAlgType] -> [RelAlgType]
rdisa _ []        = []
rdisa isas ((x,y):xs) -- = ((x,y):xs)
 | xy_spec_in_xs    =  ((x,y):(rdisa isas xs))
 | otherwise = rdisa isas xs
  where xy_spec_in_xs = foldr (&&) True [True|(x',y')<-xs, x==specific x x' isas, x/=x', y==specific y y' isas, y/=y']

not_universe x y = if x==Universe then y else x

alts_ababab :: Isa -> AltList -> AltList -> AltList
alts_ababab _ xs (Left []) = xs --needed for recursion while we defined union/intersection lists
alts_ababab isas (Left xs) (Left ys) = if null alts then Right "AB->AB->AB type error" else Left alts
   where alts = rdisa isas [(specific x x' isas,specific y y' isas)
                             |(x,y)<-xs,(x',y')<-ys,isarelated x x' isas, isarelated y y' isas]
alts_ababab _ (Left xs) err  = err
alts_ababab _ err (Left ys)  = err
alts_ababab _ lerr rerr  = lerr --TODO -> combine instead of just the first

alts_abbcac :: Isa -> AltList -> AltList -> AltList
alts_abbcac isas (Left xs) (Left ys) = if null alts then Right "AB->BC->AC type error" else Left alts
   where alts = rdisa isas [(x,z)|(x,y)<-xs,(y',z)<-ys, isarelated y y' isas]
alts_abbcac _ (Left xs) err  = err
alts_abbcac _ err (Left ys)  = err
alts_abbcac _ lerr rerr  = lerr --TODO -> combine instead of just the first

alts_abba :: AltList -> AltList
alts_abba (Left xs) = Left [(y,x)|(x,y)<-xs]
alts_abba err = err

alts_compl :: RelAlgExpr -> AltList -> AltList
alts_compl me xs = case me of
     Compl (Morph{}) -> xs
     _ -> error "complements on mphs only -> normalize"

alts_mph :: [RelDecl] -> Isa -> RelAlgExpr -> AltList
alts_mph reldecls isas me = 
   if null alts
   then Right "Undeclared morphism" 
   else Left (rdisa isas alts)
   where 
   alts' nm = [dtype d|d@(RelDecl{})<-reldecls, dname d==nm]
   alts = case me of
     Morph (DRel nm) ut@(ux,uy) _ -> if ut==(Universe,Universe) 
                                     then alts' nm
                                     else [(ux,uy)|(x,y)<-alts' nm, isarelated x ux isas, isarelated y uy isas]
     Morph _ ut _ -> [ut]
     _ -> error "alts_mph expects morphism expression only."
thedecl :: [RelDecl] -> Isa -> RelAlgExpr -> InfType -> RelDecl
thedecl _ _ _ (Right _) = error "fatal: the expression has a type error, there is no declaration."
thedecl reldecls isas me (Left (x,y)) = 
   if null alts
   then error "fatal: the expression should have discovered a type error earlier, there is no declaration."
   else head alts --REMARK -> I could throw a fatal if there is more than one alts
   where 
   alts = case me of
     Morph (DRel nm) _ _ -> [d|d@(RelDecl{dtype=(c1,c2)})<-reldecls
                              , dname d==nm, isarelated x c1 isas, isarelated y c2 isas]
     Morph IdRel _ _ -> [IDecl]
     Morph VRel _ _ -> [VDecl]
     _ -> error "alts_mph expects morphism expression only."
--------------------------------------------------------------------------------
--inherit push_type
--if there are morphisms with more than one alternative then we need to push
--a type down starting with (Universe,Universe) 
--the type of compositions can already be finalized, while the b can be inferred
--------------------------------------------------------------------------------
--DESCR -> given the type of the expression, push a type on the direct subexpressions
push_type_ababab,push_type_abba,push_type_abab :: RelAlgType -> RelAlgType
push_type_ababab t = t
push_type_abba (x,y) = (y,x)
push_type_abab t = t

final_infer_abbcac :: Isa -> InfRuleType -> RelAlgType -> AltList -> AltList -> Either (RelAlgType,RelAlgType) String
final_infer_abbcac _ _ _ _ (Left []) = error "fatal: the AltList cannot be Left []."
final_infer_abbcac _ _ _ (Left []) _ = error "fatal: the AltList cannot be Left []."
final_infer_abbcac _ _ _ _ (Right err) = Right err --error in rsub
final_infer_abbcac _ _ _ (Right err) _ = Right err --error in lsub
final_infer_abbcac isas irule (tx,ty) (Left lalts) (Left ralts) = if null alts
  then error $ "fatal: is it possible that the type pushed on a composition does not match any alternative?"++show (lalts,ralts,tx,ty)
                  --there were alts and with the type pushed down there aren't
                  --this should not be possible while there are no mphats on expressions
  else if length final_t==1 
       then head final_t
       else if null final_t
            then Right err
            else Right$ "Ambiguous composition: "
  where
  final_t = [Left x|Left x<-alts]
  err = concat [x|Right x<-alts]
  alts = [infer_t (lx,ly) (rx,ry)|(lx,ly)<-lalts,(rx,ry)<-ralts, isarelated lx tx isas, isarelated ry ty isas]
  infer_t (lx,ly) (rx,ry) 
    | isarelated ly rx isas = Left$((if tx==Universe then lx else tx,b)
                                   ,(b ,if tx==Universe then ry else ty) )
    | otherwise = Right "AB->BC->AC type error" 
    where b = infer_abbcac_b isas irule ly rx 

lefttype :: Either (RelAlgType,RelAlgType) String -> RelAlgType
lefttype (Left (x,_)) = x
lefttype (Right _) = error "fatal: no Left, check Right first before using function lefttype."
lefterror :: Either (RelAlgType,RelAlgType) String -> String 
lefterror (Right err) = err
lefterror _ = ""

righttype :: Either (RelAlgType,RelAlgType) String -> RelAlgType
righttype (Left (_,x)) = x
righttype (Right _) = error "fatal: no Left, check Right first before using function righttype."
righterror :: Either (RelAlgType,RelAlgType) String -> String 
righterror (Right err) = err
righterror _ = ""

combineerrors :: Either a String -> Either a String -> String
combineerrors (Left _) (Right err) = err
combineerrors (Right err) (Left _) = err
combineerrors (Right err1) (Right err2) = err1
combineerrors _ _ = ""

----------------------------------------------------------------------------
--synthesize rtype
----------------------------------------------------------------------------


--DESCR -> final_infer* functions return the error in me.env_in if there was one
--         if there is no error in me.env, then based on the type pushed down
--         one rtype for each subexpression will come up or 
--         a type error or ambiguous error
final_infer_mph :: RelAlgExpr -> Isa -> RelAlgType -> AltList -> InfType
final_infer_mph me _ _ (Left []) = error $"fatal: the AltList cannot be Left []: " ++ show me
final_infer_mph _ _ _ (Right err) = Right err
final_infer_mph me isas (tx,ty) (Left alts) = final_t
   where
   alts' = [(x,y)|(x,y)<-alts, isarelated x tx isas && isarelated y ty isas]
   final_t = if null alts' 
             then error $ "fatal: it should not be possible that the type pushed on a morphism does not match the declaration. Be sure that the expression this morphism is in has a type"++show (me,alts,tx,ty)
                  --there were alts and with the type pushed down there aren't
                  --this should not be possible with mphats on morphism only (not mphats on expressions)
             else if length alts'==1 
                  then Left(if tx==Universe then (fst$head alts') else tx
                           ,if ty==Universe then (snd$head alts') else ty) --TODO check on declaration property violations
                  else Right ("Ambiguous morphism: " ++ show me ++ show alts' ++show(tx,ty))

final_infer_conv :: InfType -> InfType
final_infer_conv (Left (x,y)) = Left (y,x)
final_infer_conv err = err

--the @lhs.pushed_type is processed in the rtype of the most right conjunct/disjunct
--RelAlgExpr should contain the Union or ISect expression
final_infer_ababab :: Isa -> InfRuleType -> InfType -> InfType -> AltList -> InfType
final_infer_ababab _ _ _ _ (Left []) = error "fatal: the AltList cannot be Left []."
final_infer_ababab _ _ _ _ (Right err) = Right err --error in alts of me (head+tail)
final_infer_ababab _ _ _ (Right err) _ = Right err --error in the head
final_infer_ababab _ _ (Right err) _ _ = Right err --error in the tail
final_infer_ababab isas irule (Left (tx,ty)) (Left (hx,hy)) (Left alts) = case final_t of
     Left inft -> if length (matches_alts inft)==1
                  then final_t
                  else error "fatal: or there should be an error, or only one matching alternative"
     Right _ -> final_t
     where 
     final_t | isarelated hx tx isas && isarelated hy ty isas = Left$infer_ababab isas irule (tx,ty) (hx,hy) 
             | otherwise = Right "AB->AB->AB type error"
     matches_alts :: RelAlgType -> [RelAlgType]
     matches_alts (infx,infy) = [(infx,infy)|(ax,ay)<-alts,isarelated ax infx isas, isarelated ay infy isas]


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

--main :: IO ()
--main = print$show$[case result1 test of Left t -> (result1 test,result2 test); x -> (x,[]); |test<-tests]
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


--result :: [Int]
--result = front_Syn_Tree (wrap_Tree test Inh_Tree)
result0 test = env_in_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 
result1 test = rtype_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 
result2 test = env_mph_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr testdecls testisa NoListOf (Universe,Universe)  ) ) 


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
                  alts_ababab _lhsIenv_isa _hdIenv_in _tlIenv_in
              _lhsOrtype =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIrtype _hdIenv_in
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          _tlIrtype _hdIrtype _env
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
                  error "fatal: undefined rtype on Nil of ISect-/UnionList"
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
                  alts_abbcac _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  final_infer_abbcac _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  if null(righterror _t) then lefttype _t else (fst _lhsItype_down,EmptyObject)
              _rsubOtype_down =
                  if null(righterror _t) then righttype _t else (EmptyObject,snd _lhsItype_down)
              _sub_err =
                  combineerrors _lsubIrtype _rsubIrtype
              _lhsOrtype =
                  if null(righterror _t)
                  then if null _sub_err
                       then Left (fst(lefttype _t),snd(righttype _t))
                       else Right _sub_err
                  else Right (righterror _t)
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
                  final_infer_mph _me _lhsIenv_isa _lhsItype_down _env
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
                  alts_abbcac _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  final_infer_abbcac _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  if null(righterror _t) then lefttype _t else (fst _lhsItype_down,EmptyObject)
              _rsubOtype_down =
                  if null(righterror _t) then righttype _t else (EmptyObject,snd _lhsItype_down)
              _sub_err =
                  combineerrors _lsubIrtype _rsubIrtype
              _lhsOrtype =
                  if null(righterror _t)
                  then if null _sub_err
                       then Left (fst(lefttype _t),snd(righttype _t))
                       else Right _sub_err
                  else Right (righterror _t)
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
                  alts_ababab _lhsIenv_isa _hdIenv_in _tlIenv_in
              _lhsOrtype =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIrtype _hdIenv_in
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          _tlIrtype _hdIrtype _env
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
                  error "fatal: undefined rtype on Nil of ISect-/UnionList"
              _lhsOenv_mph =
                  []
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOme,_lhsOrtype)))