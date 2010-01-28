

-- UUAGC 0.9.10 (src/TypeInference/InfLibAG.ag)


module TypeInference.InfLibAG where


rd []        = []
rd (x:xs)    = x: rd [e|e<-xs, e/=x]


isarelated x y = x==y
--DESCR -> alts_* takes list(s) of types and concludes possible types for this expression
alts_isect {-isas-} xs = rd [(x,y)|((x,y),(x',y'))<-checks xs
                                  ,(x,y)/=(Universe,Universe)
                                  ,(x',y')/=(Universe,Universe)
                                  ,x `isarelated` x'
                                  ,y `isarelated` y'] --TODO remove doubles
checks [] = []
checks (x:xs) = [(x,x')|x'<-xs] ++ checks xs
alts_union xs = rd [x|x<-xs,x/=(Universe,Universe)] --TODO remove doubles
alts_comp xs ys = rd [(x,z)|(x,y)<-xs,(y',z)<-ys, y `isarelated` y']
alts_radd xs ys = rd [(x,z)|(x,y)<-xs,(y',z)<-ys, y `isarelated` y']
alts_conv xs = rd [(y,x)|(x,y)<-xs]
infer_isect {-isas-} t xs = alts_isect (t:xs)
infer_union {-isas-} t xs = alts_union (t:xs)
infer_comp (x',z') xs ys = if length inf_t==1 then head inf_t else (EmptyObject,EmptyObject)
   where inf_t = [(x,z)|(x,y)<-xs,(y',z)<-ys, x `isarelated` x', z `isarelated` z' ,y `isarelated` y']
infer_radd (x',z') xs ys = if length inf_t==1 then head inf_t else (EmptyObject,EmptyObject)
   where inf_t = [(x,z)|(x,y)<-xs,(y',z)<-ys, x `isarelated` x', z `isarelated` z' ,y `isarelated` y']
infer_conv (x,y) = (y,x)
infer_homo x = x
amberrors xs = ["Ambiguous ISect"++show x|x<-xs,isamb xs]
isamb xs = length xs>1
isnotype xs = null xs

--inferAG::ICtx -> NormExpr -> TypedExpr
--inferAG cx exprIn = exprout
--  where
--  exprOut = 
--  errorsOut = errors_Syn_

main :: IO ()
main = print (if null result2 then(show result1) else(show result2))

--testTree :: Tree
--testTree = Node (Tip 1) (Node (Tip 2) (Tip 3))
r0 = Morph (DRel "r" []) (Universe,Universe) 1
r = Morph (DRel "r" [RelDecl "r" (Object "A",Object "B")]) (Universe,Universe) 1
s = Morph (DRel "s" [RelDecl "s" (Object "C",Object "D")]) (Universe,Universe) 1
(/\) x y = ISect [x,y]
compl x = Compl x

--test :: T_Tree
--test = sem_Tree testTree
test::T_RelAlgExpr
test= sem_RelAlgExpr (compl r /\ s)

--result :: [Int]
--result = front_Syn_Tree (wrap_Tree test Inh_Tree)
result1 = rtype_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr [{-Isa-}] (Universe,Universe) ) ) 
result2 = errors_Syn_RelAlgExpr (wrap_RelAlgExpr test (Inh_RelAlgExpr [{-Isa-}] (Universe,Universe) ) ) 


--the heterogeneous relation algebra with a possibility to explicitly declare types on morphisms in an expression
--a is a data structure for storing language statement meta data like file position of a declaration

data RelAlgMorph = 
   DRel {rname::String, alts::[RelDecl]}
  |IdRel
  |VRel
  deriving (Show)
data RelAlgObj = Universe | EmptyObject | Object String deriving (Show,Eq)
type RelAlgType = (RelAlgObj, RelAlgObj)
data Isa = Isa (RelAlgObj, RelAlgObj)
data RelDecl = RelDecl {dname::String, dtype::RelAlgType} | IDecl | VDecl deriving (Show)
-- ISectList ---------------------------------------------------
type ISectList  = [(RelAlgExpr)]
-- cata
sem_ISectList :: ISectList  ->
                 T_ISectList 
sem_ISectList list  =
    (Prelude.foldr sem_ISectList_Cons sem_ISectList_Nil (Prelude.map sem_RelAlgExpr list) )
-- semantic domain
type T_ISectList  = ([Isa]) ->
                    RelAlgType ->
                    ( ([RelAlgType]),([String]),ISectList,RelAlgType)
data Inh_ISectList  = Inh_ISectList {env_isa_Inh_ISectList :: [Isa],type_down_Inh_ISectList :: RelAlgType}
data Syn_ISectList  = Syn_ISectList {env_in_Syn_ISectList :: [RelAlgType],errors_Syn_ISectList :: [String],me_Syn_ISectList :: ISectList,rtype_Syn_ISectList :: RelAlgType}
wrap_ISectList :: T_ISectList  ->
                  Inh_ISectList  ->
                  Syn_ISectList 
wrap_ISectList sem (Inh_ISectList _lhsIenv_isa _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_isa _lhsItype_down )
     in  (Syn_ISectList _lhsOenv_in _lhsOerrors _lhsOme _lhsOrtype ))
sem_ISectList_Cons :: T_RelAlgExpr  ->
                      T_ISectList  ->
                      T_ISectList 
sem_ISectList_Cons hd_ tl_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOenv_in :: ([RelAlgType])
              _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOme :: ISectList
              _hdOenv_isa :: ([Isa])
              _hdOtype_down :: RelAlgType
              _tlOenv_isa :: ([Isa])
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: ([RelAlgType])
              _hdIerrors :: ([String])
              _hdIme :: RelAlgExpr
              _hdIrtype :: RelAlgType
              _tlIenv_in :: ([RelAlgType])
              _tlIerrors :: ([String])
              _tlIme :: ISectList
              _tlIrtype :: RelAlgType
              _lhsOenv_in =
                  _hdIenv_in ++ _tlIenv_in
              _lhsOrtype =
                  head _t
              _t =
                  infer_isect _hdIrtype [_tlIrtype]
              _lhsOerrors =
                  if null(_hdIerrors ++ _tlIerrors)
                  then if null _t
                         then ["ISect error (fatal?)"]
                         else amberrors _t
                  else _hdIerrors ++ _tlIerrors
              _me =
                  (:) _hdIme _tlIme
              _lhsOme =
                  _me
              _hdOenv_isa =
                  _lhsIenv_isa
              _hdOtype_down =
                  _lhsItype_down
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIerrors,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_isa _hdOtype_down )
              ( _tlIenv_in,_tlIerrors,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_isa _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_ISectList_Nil :: T_ISectList 
sem_ISectList_Nil  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: ISectList
              _lhsOrtype =
                  (Universe,Universe)
              _lhsOerrors =
                  []
              _lhsOenv_in =
                  []
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
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
type T_RelAlgExpr  = ([Isa]) ->
                     RelAlgType ->
                     ( ([RelAlgType]),([String]),RelAlgExpr,RelAlgType)
data Inh_RelAlgExpr  = Inh_RelAlgExpr {env_isa_Inh_RelAlgExpr :: [Isa],type_down_Inh_RelAlgExpr :: RelAlgType}
data Syn_RelAlgExpr  = Syn_RelAlgExpr {env_in_Syn_RelAlgExpr :: [RelAlgType],errors_Syn_RelAlgExpr :: [String],me_Syn_RelAlgExpr :: RelAlgExpr,rtype_Syn_RelAlgExpr :: RelAlgType}
wrap_RelAlgExpr :: T_RelAlgExpr  ->
                   Inh_RelAlgExpr  ->
                   Syn_RelAlgExpr 
wrap_RelAlgExpr sem (Inh_RelAlgExpr _lhsIenv_isa _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_isa _lhsItype_down )
     in  (Syn_RelAlgExpr _lhsOenv_in _lhsOerrors _lhsOme _lhsOrtype ))
sem_RelAlgExpr_Comp :: T_RelAlgExpr  ->
                       T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_Comp lsub_ rsub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lsubOtype_down :: RelAlgType
              _rsubOtype_down :: RelAlgType
              _lhsOme :: RelAlgExpr
              _lsubOenv_isa :: ([Isa])
              _rsubOenv_isa :: ([Isa])
              _lsubIenv_in :: ([RelAlgType])
              _lsubIerrors :: ([String])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: RelAlgType
              _rsubIenv_in :: ([RelAlgType])
              _rsubIerrors :: ([String])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: RelAlgType
              _lhsOrtype =
                  infer_comp (Universe,Universe) [_lsubIrtype] [_rsubIrtype]
              _lhsOerrors =
                  _lsubIerrors ++ _rsubIerrors
              _lhsOenv_in =
                  alts_comp _lsubIenv_in _rsubIenv_in
              _t =
                  infer_comp _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  _t
              _rsubOtype_down =
                  _t
              _me =
                  Comp _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_isa =
                  _lhsIenv_isa
              _rsubOenv_isa =
                  _lhsIenv_isa
              ( _lsubIenv_in,_lsubIerrors,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_isa _lsubOtype_down )
              ( _rsubIenv_in,_rsubIerrors,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_isa _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Compl :: T_RelAlgExpr  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Compl sub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: RelAlgExpr
              _subOenv_isa :: ([Isa])
              _subOtype_down :: RelAlgType
              _subIenv_in :: ([RelAlgType])
              _subIerrors :: ([String])
              _subIme :: RelAlgExpr
              _subIrtype :: RelAlgType
              _lhsOrtype =
                  case _me of
                     Compl (Morph{}) -> _subIrtype
                     _ -> error "complements on mphs only -> normalize"
              _lhsOerrors =
                  case _me of
                    Compl (Morph{}) -> _subIerrors
                    _ -> error "complements on mphs only -> normalize"
              _lhsOenv_in =
                  case _me of
                      Compl (Morph{}) -> _subIenv_in
                      _ -> error "complements on mphs only -> normalize"
              _me =
                  Compl _subIme
              _lhsOme =
                  _me
              _subOenv_isa =
                  _lhsIenv_isa
              _subOtype_down =
                  _lhsItype_down
              ( _subIenv_in,_subIerrors,_subIme,_subIrtype) =
                  (sub_ _subOenv_isa _subOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Conv :: T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_Conv sub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _subOtype_down :: RelAlgType
              _lhsOme :: RelAlgExpr
              _subOenv_isa :: ([Isa])
              _subIenv_in :: ([RelAlgType])
              _subIerrors :: ([String])
              _subIme :: RelAlgExpr
              _subIrtype :: RelAlgType
              _lhsOrtype =
                  infer_conv _subIrtype
              _lhsOerrors =
                  _subIerrors
              _lhsOenv_in =
                  alts_conv _subIenv_in
              _subOtype_down =
                  infer_conv _lhsItype_down
              _me =
                  Conv _subIme
              _lhsOme =
                  _me
              _subOenv_isa =
                  _lhsIenv_isa
              ( _subIenv_in,_subIerrors,_subIme,_subIrtype) =
                  (sub_ _subOenv_isa _subOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Equiv :: T_RelAlgExpr  ->
                        T_RelAlgExpr  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Equiv lsub_ rsub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: RelAlgExpr
              _lsubOenv_isa :: ([Isa])
              _lsubOtype_down :: RelAlgType
              _rsubOenv_isa :: ([Isa])
              _rsubOtype_down :: RelAlgType
              _lsubIenv_in :: ([RelAlgType])
              _lsubIerrors :: ([String])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: RelAlgType
              _rsubIenv_in :: ([RelAlgType])
              _rsubIerrors :: ([String])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: RelAlgType
              _lhsOrtype =
                  error "no equivalence rule symbols -> normalize"
              _lhsOerrors =
                  error "no equivalence rule symbols -> normalize"
              _lhsOenv_in =
                  error "no equivalence rule symbols -> normalize"
              _me =
                  Equiv _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOtype_down =
                  _lhsItype_down
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOtype_down =
                  _lhsItype_down
              ( _lsubIenv_in,_lsubIerrors,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_isa _lsubOtype_down )
              ( _rsubIenv_in,_rsubIerrors,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_isa _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_ISect :: T_ISectList  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_ISect sublst_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _sublstOtype_down :: RelAlgType
              _lhsOme :: RelAlgExpr
              _sublstOenv_isa :: ([Isa])
              _sublstIenv_in :: ([RelAlgType])
              _sublstIerrors :: ([String])
              _sublstIme :: ISectList
              _sublstIrtype :: RelAlgType
              _lhsOrtype =
                  _sublstIrtype
              _lhsOerrors =
                  if null _sublstIerrors
                  then if null _t
                       then ["ISect error"]
                       else amberrors _t
                  else _sublstIerrors
              _lhsOenv_in =
                  _env
              _env =
                  alts_isect _sublstIenv_in
              _t =
                  infer_isect _lhsItype_down _env
              _sublstOtype_down =
                  if null _t then (Universe,Universe) else head _t
              _me =
                  ISect _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIerrors,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_isa _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Implic :: T_RelAlgExpr  ->
                         T_RelAlgExpr  ->
                         T_RelAlgExpr 
sem_RelAlgExpr_Implic lsub_ rsub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: RelAlgExpr
              _lsubOenv_isa :: ([Isa])
              _lsubOtype_down :: RelAlgType
              _rsubOenv_isa :: ([Isa])
              _rsubOtype_down :: RelAlgType
              _lsubIenv_in :: ([RelAlgType])
              _lsubIerrors :: ([String])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: RelAlgType
              _rsubIenv_in :: ([RelAlgType])
              _rsubIerrors :: ([String])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: RelAlgType
              _lhsOrtype =
                  error "no implication rule symbols -> normalize"
              _lhsOerrors =
                  error "no implication rule symbols -> normalize"
              _lhsOenv_in =
                  error "no implication rule symbols -> normalize"
              _me =
                  Implic _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_isa =
                  _lhsIenv_isa
              _lsubOtype_down =
                  _lhsItype_down
              _rsubOenv_isa =
                  _lhsIenv_isa
              _rsubOtype_down =
                  _lhsItype_down
              ( _lsubIenv_in,_lsubIerrors,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_isa _lsubOtype_down )
              ( _rsubIenv_in,_rsubIerrors,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_isa _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Morph :: RelAlgMorph ->
                        RelAlgType ->
                        Int ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Morph rel_ usertype_ locid_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: RelAlgExpr
              _lhsOrtype =
                  _t
              _t =
                  infer_homo _lhsItype_down
              _lhsOerrors =
                  if _env==[]
                  then ["Undeclared"]
                  else []
              _lhsOenv_in =
                  _env
              _env =
                  case _me of
                         Morph (DRel _ alts') ut _ -> if ut==(Universe,Universe)
                                                      then map dtype alts'
                                                      else [dt|dt<-map dtype alts', dt `isarelated` ut]
                         Morph _ ut _ -> [ut]
              _me =
                  Morph rel_ usertype_ locid_
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_RAdd :: T_RelAlgExpr  ->
                       T_RelAlgExpr  ->
                       T_RelAlgExpr 
sem_RelAlgExpr_RAdd lsub_ rsub_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lsubOtype_down :: RelAlgType
              _rsubOtype_down :: RelAlgType
              _lhsOme :: RelAlgExpr
              _lsubOenv_isa :: ([Isa])
              _rsubOenv_isa :: ([Isa])
              _lsubIenv_in :: ([RelAlgType])
              _lsubIerrors :: ([String])
              _lsubIme :: RelAlgExpr
              _lsubIrtype :: RelAlgType
              _rsubIenv_in :: ([RelAlgType])
              _rsubIerrors :: ([String])
              _rsubIme :: RelAlgExpr
              _rsubIrtype :: RelAlgType
              _lhsOrtype =
                  infer_radd (Universe,Universe) [_lsubIrtype] [_rsubIrtype]
              _lhsOerrors =
                  _lsubIerrors ++ _rsubIerrors
              _lhsOenv_in =
                  alts_radd _lsubIenv_in _rsubIenv_in
              _t =
                  infer_radd _lhsItype_down _lsubIenv_in _rsubIenv_in
              _lsubOtype_down =
                  _t
              _rsubOtype_down =
                  _t
              _me =
                  RAdd _lsubIme _rsubIme
              _lhsOme =
                  _me
              _lsubOenv_isa =
                  _lhsIenv_isa
              _rsubOenv_isa =
                  _lhsIenv_isa
              ( _lsubIenv_in,_lsubIerrors,_lsubIme,_lsubIrtype) =
                  (lsub_ _lsubOenv_isa _lsubOtype_down )
              ( _rsubIenv_in,_rsubIerrors,_rsubIme,_rsubIrtype) =
                  (rsub_ _rsubOenv_isa _rsubOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_RelAlgExpr_Union :: T_UnionList  ->
                        T_RelAlgExpr 
sem_RelAlgExpr_Union sublst_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _sublstOtype_down :: RelAlgType
              _lhsOme :: RelAlgExpr
              _sublstOenv_isa :: ([Isa])
              _sublstIenv_in :: ([RelAlgType])
              _sublstIerrors :: ([String])
              _sublstIme :: UnionList
              _sublstIrtype :: RelAlgType
              _lhsOrtype =
                  _sublstIrtype
              _lhsOerrors =
                  if null _sublstIerrors
                  then if null _t
                       then ["Union error"]
                       else amberrors _t
                  else _sublstIerrors
              _lhsOenv_in =
                  _env
              _env =
                  alts_union _sublstIenv_in
              _t =
                  infer_union _lhsItype_down _env
              _sublstOtype_down =
                  if null _t then (EmptyObject,EmptyObject) else head _t
              _me =
                  Union _sublstIme
              _lhsOme =
                  _me
              _sublstOenv_isa =
                  _lhsIenv_isa
              ( _sublstIenv_in,_sublstIerrors,_sublstIme,_sublstIrtype) =
                  (sublst_ _sublstOenv_isa _sublstOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
-- UnionList ---------------------------------------------------
type UnionList  = [(RelAlgExpr)]
-- cata
sem_UnionList :: UnionList  ->
                 T_UnionList 
sem_UnionList list  =
    (Prelude.foldr sem_UnionList_Cons sem_UnionList_Nil (Prelude.map sem_RelAlgExpr list) )
-- semantic domain
type T_UnionList  = ([Isa]) ->
                    RelAlgType ->
                    ( ([RelAlgType]),([String]),UnionList,RelAlgType)
data Inh_UnionList  = Inh_UnionList {env_isa_Inh_UnionList :: [Isa],type_down_Inh_UnionList :: RelAlgType}
data Syn_UnionList  = Syn_UnionList {env_in_Syn_UnionList :: [RelAlgType],errors_Syn_UnionList :: [String],me_Syn_UnionList :: UnionList,rtype_Syn_UnionList :: RelAlgType}
wrap_UnionList :: T_UnionList  ->
                  Inh_UnionList  ->
                  Syn_UnionList 
wrap_UnionList sem (Inh_UnionList _lhsIenv_isa _lhsItype_down )  =
    (let ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype) =
             (sem _lhsIenv_isa _lhsItype_down )
     in  (Syn_UnionList _lhsOenv_in _lhsOerrors _lhsOme _lhsOrtype ))
sem_UnionList_Cons :: T_RelAlgExpr  ->
                      T_UnionList  ->
                      T_UnionList 
sem_UnionList_Cons hd_ tl_  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOenv_in :: ([RelAlgType])
              _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOme :: UnionList
              _hdOenv_isa :: ([Isa])
              _hdOtype_down :: RelAlgType
              _tlOenv_isa :: ([Isa])
              _tlOtype_down :: RelAlgType
              _hdIenv_in :: ([RelAlgType])
              _hdIerrors :: ([String])
              _hdIme :: RelAlgExpr
              _hdIrtype :: RelAlgType
              _tlIenv_in :: ([RelAlgType])
              _tlIerrors :: ([String])
              _tlIme :: UnionList
              _tlIrtype :: RelAlgType
              _lhsOenv_in =
                  _hdIenv_in ++ _tlIenv_in
              _lhsOrtype =
                  head _t
              _t =
                  infer_union _hdIrtype [_tlIrtype]
              _lhsOerrors =
                  if null(_hdIerrors ++ _tlIerrors)
                  then if null _t
                         then ["Union error (fatal?)"]
                         else amberrors _t
                  else _hdIerrors ++ _tlIerrors
              _me =
                  (:) _hdIme _tlIme
              _lhsOme =
                  _me
              _hdOenv_isa =
                  _lhsIenv_isa
              _hdOtype_down =
                  _lhsItype_down
              _tlOenv_isa =
                  _lhsIenv_isa
              _tlOtype_down =
                  _lhsItype_down
              ( _hdIenv_in,_hdIerrors,_hdIme,_hdIrtype) =
                  (hd_ _hdOenv_isa _hdOtype_down )
              ( _tlIenv_in,_tlIerrors,_tlIme,_tlIrtype) =
                  (tl_ _tlOenv_isa _tlOtype_down )
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))
sem_UnionList_Nil :: T_UnionList 
sem_UnionList_Nil  =
    (\ _lhsIenv_isa
       _lhsItype_down ->
         (let _lhsOrtype :: RelAlgType
              _lhsOerrors :: ([String])
              _lhsOenv_in :: ([RelAlgType])
              _lhsOme :: UnionList
              _lhsOrtype =
                  (Universe,Universe)
              _lhsOerrors =
                  []
              _lhsOenv_in =
                  []
              _me =
                  []
              _lhsOme =
                  _me
          in  ( _lhsOenv_in,_lhsOerrors,_lhsOme,_lhsOrtype)))