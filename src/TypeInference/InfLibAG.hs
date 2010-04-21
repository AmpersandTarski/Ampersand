

-- UUAGC 0.9.14 (src/TypeInference/InfLibAG)


module TypeInference.InfLibAG 
  (infer
  ,RelAlgExpr(..)
  ,RelAlgMorph(..)
  ,RelDecl(..) 
  ,RelAlgObj(..)
  ,RelAlgType
  ,Isa
  ,TError(..)
  ,InfTree(..)
  ,DeclRuleType(..)
  ,InfRuleType(..)
  ) where

import TypeInference.InfLibAGFuncs

infer:: [RelDecl] -> Isa -> RelAlgExpr -> Either (RelAlgType,[(RelAlgExpr,RelAlgType,RelDecl)],InfTree) TError
infer reldecls isas root_expr =
  case rtype of
    Left (x,y) -> if x==Universe 
                  then Right$TErrorU "The source of the expression is the universal set" 
                  else if  y==Universe
                       then Right$TErrorU "The target of the expression is the universal set" 
                       else Left ((x,y), env_mph, inftree)
    Right err -> Right err
  where
  inftree = inftree_Syn_RelAlgExpr (agtree (head alltypes))
  env_mph = [(m,t,d)|(m,Left t,d)<-env_mph_Syn_RelAlgExpr (agtree (head alltypes))]
  env_in = env_in_Syn_RelAlgExpr (agtree (Universe,Universe))
  rtype = if length alltypes==1 
          then rtype_Syn_RelAlgExpr (agtree (head alltypes)) --finalize by pushing the type down again
          else if null alltypes
               --there is a type error in env_in
               then (\(Right env_in_err) -> Right env_in_err) env_in
               else Right$TErrorAmb "Ambiguous type" alltypes
  alltypes = case env_in of
     Left xs -> if null xs 
                then fatal 214 "the AltList cannot be Left []."
                else map fst xs 
     _ -> []
  agtree push = wrap_RelAlgExpr (sem_RelAlgExpr$normalise$root_expr)$Inh_RelAlgExpr reldecls isas NoListOf push

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
                    ( AltList,([(RelAlgExpr,InfType,RelDecl)]),InfTree,ISectList,InfType)
data Inh_ISectList  = Inh_ISectList {env_decls_Inh_ISectList :: [RelDecl],env_isa_Inh_ISectList :: Isa,listof_Inh_ISectList :: ListOf,type_down_Inh_ISectList :: RelAlgType}
data Syn_ISectList  = Syn_ISectList {env_in_Syn_ISectList :: AltList,env_mph_Syn_ISectList :: [(RelAlgExpr,InfType,RelDecl)],inftree_Syn_ISectList :: InfTree,me_Syn_ISectList :: ISectList,rtype_Syn_ISectList :: InfType}
wrap_ISectList :: T_ISectList  ->
                  Inh_ISectList  ->
                  Syn_ISectList 
wrap_ISectList sem (Inh_ISectList _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_ISectList _lhsOenv_in _lhsOenv_mph _lhsOinftree _lhsOme _lhsOrtype ))
sem_ISectList_Cons :: T_RelAlgExpr  ->
                      T_ISectList  ->
                      T_ISectList 
sem_ISectList_Cons hd_ tl_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _hdOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOinftree :: InfTree
              _lhsOme :: ISectList
              _hdOenv_decls :: ([RelDecl])
              _hdOenv_isa :: Isa
              _hdOlistof :: ListOf
              _tlOenv_decls :: ([RelDecl])
              _tlOenv_isa :: Isa
              _tlOlistof :: ListOf
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: AltList
              _hdIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _hdIinftree :: InfTree
              _hdIme :: RelAlgExpr
              _hdIrtype :: InfType
              _tlIenv_in :: AltList
              _tlIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _tlIinftree :: InfTree
              _tlIme :: ISectList
              _tlIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_ababab _lhsIlistof (_hdIme,_tlIme) _lhsIenv_isa _hdIenv_in _tlIenv_in
              _tp =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIenv_in (Left _lhsItype_down)
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          (Left _lhsItype_down) _hdIenv_in _tlIrtype
              _hdOtype_down =
                  if not(is_type_error _tp) then thetype _tp else fatal 55 "There should be a ababab error"
              _lhsOrtype =
                  _tp
              _lhsOenv_mph =
                  _hdIenv_mph ++ _tlIenv_mph
              _hdax =
                  headofaxiomlist (inferencerule_ababab _lhsIlistof _hdIme _tlIme) _tp _hdIinftree
              _lhsOinftree =
                  if null _tlIme
                  then _hdax
                  else axiomlist _tp _hdax _tlIinftree
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
              _tlOenv_decls =
                  _lhsIenv_decls
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOlistof =
                  _lhsIlistof
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIenv_mph,_hdIinftree,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_decls _hdOenv_isa _hdOlistof _hdOtype_down )
              ( _tlIenv_in,_tlIenv_mph,_tlIinftree,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_decls _tlOenv_isa _tlOlistof _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
sem_ISectList_Nil :: T_ISectList 
sem_ISectList_Nil  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOinftree :: InfTree
              _lhsOme :: ISectList
              _lhsOenv_in =
                  Left []
              _lhsOrtype =
                  fatal 45 "undefined rtype on Nil of ISect-/UnionList"
              _lhsOenv_mph =
                  []
              _lhsOinftree =
                  fatal 47 "undefined inftree on Nil of ISect-/UnionList"
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
-- RelAlgExpr --------------------------------------------------
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
                     ( AltList,([(RelAlgExpr,InfType,RelDecl)]),InfTree,RelAlgExpr,InfType)
data Inh_RelAlgExpr  = Inh_RelAlgExpr {env_decls_Inh_RelAlgExpr :: [RelDecl],env_isa_Inh_RelAlgExpr :: Isa,listof_Inh_RelAlgExpr :: ListOf,type_down_Inh_RelAlgExpr :: RelAlgType}
data Syn_RelAlgExpr  = Syn_RelAlgExpr {env_in_Syn_RelAlgExpr :: AltList,env_mph_Syn_RelAlgExpr :: [(RelAlgExpr,InfType,RelDecl)],inftree_Syn_RelAlgExpr :: InfTree,me_Syn_RelAlgExpr :: RelAlgExpr,rtype_Syn_RelAlgExpr :: InfType}
wrap_RelAlgExpr :: T_RelAlgExpr  ->
                   Inh_RelAlgExpr  ->
                   Syn_RelAlgExpr 
wrap_RelAlgExpr sem (Inh_RelAlgExpr _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_RelAlgExpr _lhsOenv_in _lhsOenv_mph _lhsOinftree _lhsOme _lhsOrtype ))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIinftree :: InfTree
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIinftree :: InfTree
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  push_type_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              __tup1 =
                  _ltp
              (_,_cb) =
                  __tup1
              _lsubOtype_down =
                  _ltp
              _ltp =
                  if not(is_b_error _t) then lefttype _t else fatal 65 "There should be an ambiguous b error"
              _rsubOtype_down =
                  if not(is_b_error _t) then righttype _t else fatal 66 "There should be an ambiguous b error"
              _lhsOrtype =
                  _tp
              _tp =
                  if not(is_b_error _t)
                  then final_infer_abbcac _lhsIenv_isa _lsubIrtype _rsubIrtype
                  else Right(b_error _t)
              _lhsOenv_mph =
                  _lsubIenv_mph ++ _rsubIenv_mph
              _lhsOinftree =
                  InfExprs (inferencerule_abbcac _me) (inferred _tp,_cb) [_lsubIinftree,_rsubIinftree]
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
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIinftree,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIinftree,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _subOenv_decls :: ([RelDecl])
              _subOenv_isa :: Isa
              _subOlistof :: ListOf
              _subIenv_in :: AltList
              _subIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _subIinftree :: InfTree
              _subIme :: RelAlgExpr
              _subIrtype :: InfType
              _lhsOenv_in =
                  alts_compl _me _subIenv_in
              _subOtype_down =
                  push_type_abab _lhsItype_down
              _tp =
                  case _me of
                     Compl (Morph{}) -> _subIrtype
                     _ -> error "complements on mphs only -> normalize"
              _lhsOrtype =
                  _tp
              _lhsOenv_mph =
                  _subIenv_mph
              _lhsOinftree =
                  complement_rule _tp _subIinftree
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
              ( _subIenv_in,_subIenv_mph,_subIinftree,_subIme,_subIrtype) =
                  (sub_ _subOenv_decls _subOenv_isa _subOlistof _subOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _subOenv_decls :: ([RelDecl])
              _subOenv_isa :: Isa
              _subOlistof :: ListOf
              _subIenv_in :: AltList
              _subIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _subIinftree :: InfTree
              _subIme :: RelAlgExpr
              _subIrtype :: InfType
              _lhsOenv_in =
                  alts_abba _subIenv_in
              _subOtype_down =
                  push_type_abba _lhsItype_down
              _lhsOrtype =
                  _tp
              _tp =
                  final_infer_conv _subIrtype
              _lhsOenv_mph =
                  _subIenv_mph
              _lhsOinftree =
                  InfExprs Conv_nc (inferred _tp,EmptyObject) [_subIinftree]
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
              ( _subIenv_in,_subIenv_mph,_subIinftree,_subIme,_subIrtype) =
                  (sub_ _subOenv_decls _subOenv_isa _subOlistof _subOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
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
              _lsubIinftree :: InfTree
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIinftree :: InfTree
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  error "no equivalence rule symbols -> normalize"
              _lhsOrtype =
                  error "no equivalence rule symbols -> normalize"
              _lhsOenv_mph =
                  error "no equivalence rule symbols -> normalize"
              _lhsOinftree =
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
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIinftree,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIinftree,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _sublstOenv_decls :: ([RelDecl])
              _sublstOenv_isa :: Isa
              _sublstIenv_in :: AltList
              _sublstIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _sublstIinftree :: InfTree
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
              _lhsOinftree =
                  _sublstIinftree
              _me =
                  ISect _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_decls =
                  _lhsIenv_decls
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIenv_mph,_sublstIinftree,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_decls _sublstOenv_isa _sublstOlistof _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
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
              _lsubIinftree :: InfTree
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIinftree :: InfTree
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  error "no implication rule symbols -> normalize"
              _lhsOrtype =
                  error "no implication rule symbols -> normalize"
              _lhsOenv_mph =
                  error "no implication rule symbols -> normalize"
              _lhsOinftree =
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
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIinftree,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIinftree,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _lhsOenv_in =
                  _env
              _env =
                  alts_mph _lhsIenv_decls _lhsIenv_isa _me
              __tup2 =
                  final_infer_mph _lhsIenv_decls _me _lhsIenv_isa _lhsItype_down _env
              (_t,_) =
                  __tup2
              (_,_tree) =
                  __tup2
              _lhsOrtype =
                  _t
              _lhsOenv_mph =
                  [( _me, _t, thedecl _lhsIenv_decls _lhsIenv_isa _me _t)]
              _lhsOinftree =
                  _tree
              _me =
                  Morph rel_ usertype_ locid_
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _lsubOenv_decls :: ([RelDecl])
              _lsubOenv_isa :: Isa
              _lsubOlistof :: ListOf
              _rsubOenv_decls :: ([RelDecl])
              _rsubOenv_isa :: Isa
              _rsubOlistof :: ListOf
              _lsubIenv_in :: AltList
              _lsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lsubIinftree :: InfTree
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: InfType
              _rsubIenv_in :: AltList
              _rsubIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _rsubIinftree :: InfTree
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa _lsubIenv_in _rsubIenv_in
              _t =
                  push_type_abbcac (_lsubIme,_rsubIme) _lhsIenv_isa (inferencerule_abbcac _me) _lhsItype_down _lsubIenv_in _rsubIenv_in
              __tup3 =
                  _ltp
              (_,_cb) =
                  __tup3
              _lsubOtype_down =
                  _ltp
              _ltp =
                  if not(is_b_error _t) then lefttype _t else fatal 65 "There should be an ambiguous b error"
              _rsubOtype_down =
                  if not(is_b_error _t) then righttype _t else fatal 66 "There should be an ambiguous b error"
              _lhsOrtype =
                  _tp
              _tp =
                  if not(is_b_error _t)
                  then final_infer_abbcac _lhsIenv_isa _lsubIrtype _rsubIrtype
                  else Right(b_error _t)
              _lhsOenv_mph =
                  _lsubIenv_mph ++ _rsubIenv_mph
              _lhsOinftree =
                  InfExprs (inferencerule_abbcac _me) (inferred _tp,_cb) [_lsubIinftree,_rsubIinftree]
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
              ( _lsubIenv_in,_lsubIenv_mph,_lsubIinftree,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_decls _lsubOenv_isa _lsubOlistof _lsubOtype_down )
              ( _rsubIenv_in,_rsubIenv_mph,_rsubIinftree,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_decls _rsubOenv_isa _rsubOlistof _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
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
              _lhsOinftree :: InfTree
              _lhsOme :: RelAlgExpr
              _sublstOenv_decls :: ([RelDecl])
              _sublstOenv_isa :: Isa
              _sublstIenv_in :: AltList
              _sublstIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _sublstIinftree :: InfTree
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
              _lhsOinftree =
                  _sublstIinftree
              _me =
                  Union _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_decls =
                  _lhsIenv_decls
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIenv_mph,_sublstIinftree,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_decls _sublstOenv_isa _sublstOlistof _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
-- UnionList ---------------------------------------------------
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
                    ( AltList,([(RelAlgExpr,InfType,RelDecl)]),InfTree,UnionList,InfType)
data Inh_UnionList  = Inh_UnionList {env_decls_Inh_UnionList :: [RelDecl],env_isa_Inh_UnionList :: Isa,listof_Inh_UnionList :: ListOf,type_down_Inh_UnionList :: RelAlgType}
data Syn_UnionList  = Syn_UnionList {env_in_Syn_UnionList :: AltList,env_mph_Syn_UnionList :: [(RelAlgExpr,InfType,RelDecl)],inftree_Syn_UnionList :: InfTree,me_Syn_UnionList :: UnionList,rtype_Syn_UnionList :: InfType}
wrap_UnionList :: T_UnionList  ->
                  Inh_UnionList  ->
                  Syn_UnionList 
wrap_UnionList sem (Inh_UnionList _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_decls _lhsIenv_isa _lhsIlistof _lhsItype_down )
     in  (Syn_UnionList _lhsOenv_in _lhsOenv_mph _lhsOinftree _lhsOme _lhsOrtype ))
sem_UnionList_Cons :: T_RelAlgExpr  ->
                      T_UnionList  ->
                      T_UnionList 
sem_UnionList_Cons hd_ tl_  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _hdOtype_down :: RelAlgType
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOinftree :: InfTree
              _lhsOme :: UnionList
              _hdOenv_decls :: ([RelDecl])
              _hdOenv_isa :: Isa
              _hdOlistof :: ListOf
              _tlOenv_decls :: ([RelDecl])
              _tlOenv_isa :: Isa
              _tlOlistof :: ListOf
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: AltList
              _hdIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _hdIinftree :: InfTree
              _hdIme :: RelAlgExpr
              _hdIrtype :: InfType
              _tlIenv_in :: AltList
              _tlIenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _tlIinftree :: InfTree
              _tlIme :: UnionList
              _tlIrtype :: InfType
              _lhsOenv_in =
                  _env
              _env =
                  alts_ababab _lhsIlistof (_hdIme,_tlIme) _lhsIenv_isa _hdIenv_in _tlIenv_in
              _tp =
                  if null _tlIme
                  then final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme [])
                                          (Left _lhsItype_down) _hdIenv_in (Left _lhsItype_down)
                  else final_infer_ababab _lhsIenv_isa (inferencerule_ababab _lhsIlistof _hdIme _tlIme)
                                          (Left _lhsItype_down) _hdIenv_in _tlIrtype
              _hdOtype_down =
                  if not(is_type_error _tp) then thetype _tp else fatal 55 "There should be a ababab error"
              _lhsOrtype =
                  _tp
              _lhsOenv_mph =
                  _hdIenv_mph ++ _tlIenv_mph
              _hdax =
                  headofaxiomlist (inferencerule_ababab _lhsIlistof _hdIme _tlIme) _tp _hdIinftree
              _lhsOinftree =
                  if null _tlIme
                  then _hdax
                  else axiomlist _tp _hdax _tlIinftree
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
              _tlOenv_decls =
                  _lhsIenv_decls
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOlistof =
                  _lhsIlistof
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIenv_mph,_hdIinftree,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_decls _hdOenv_isa _hdOlistof _hdOtype_down )
              ( _tlIenv_in,_tlIenv_mph,_tlIinftree,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_decls _tlOenv_isa _tlOlistof _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))
sem_UnionList_Nil :: T_UnionList 
sem_UnionList_Nil  =
    (\ _lhsIenv_decls
       _lhsIenv_isa
       _lhsIlistof
       _lhsItype_down ->
         (let _lhsOenv_in :: AltList
              _lhsOrtype :: InfType
              _lhsOenv_mph :: ([(RelAlgExpr,InfType,RelDecl)])
              _lhsOinftree :: InfTree
              _lhsOme :: UnionList
              _lhsOenv_in =
                  Left []
              _lhsOrtype =
                  fatal 45 "undefined rtype on Nil of ISect-/UnionList"
              _lhsOenv_mph =
                  []
              _lhsOinftree =
                  fatal 47 "undefined inftree on Nil of ISect-/UnionList"
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOenv_mph,_lhsOinftree,_lhsOme,_lhsOrtype)))