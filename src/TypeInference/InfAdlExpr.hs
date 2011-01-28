{-# OPTIONS_GHC -Wall #-}
{-
TODO -> detect composition over universe
           RULE testUinverse2 MAINTAINS I[Order];V;V;V;I[Order]
        usage of a Concept in relats, which is not in any declaration (and thus (B,Universe) not in isas)
           RULE testUinverse3 MAINTAINS I;I[B]
 -
 - InfAdlExpr errors: 
   --See TODO at printterror. "[1] Type mismatch in rule"

   InfLibAG errors:
   "[0] Universal set in type"
   "[2] Ambiguous type"
   "[3] Relation undefined"
   "[4] Incompatible comparison" 
   "[5] Incompatible composition"
   "[6] Ambiguous composition"
   "[7] Homogeneous property on heterogeneous relation"
   "[8] Type is not homogeneous" 
   
   Isa errors:
   error ("!Fatal (module TypeInference.InfLibAdlExpr 22): "++
          show ["Concept "++show c1++" cannot be the specific of both "++show c2++" and "++show c3
                       ++ " if "++show c2++" and "++show c3 ++ 
                       " are not comparable. Specify the order with a GEN .. ISA .."
                       |(c1,c2,c3)<-checkrels])
 -}
module TypeInference.InfAdlExpr(infertype_and_populate) where
import TypeInference.InfLibAG
import Ampersand
import ShowADL
import Text.Pandoc
import Rendering.InfTree2Pandoc (texOnly_pandoctree,texOnly_writeexpr,texOnly_writerule)

fatal :: Int -> String -> a
fatal regel msg = error ("!Fatal (module TypeInference.InfLibAdlExpr "++show regel++"): "++msg )

----------------------------------------------------------------------------
--Ampersand conversie
----------------------------------------------------------------------------
--DESCR -> a function to add population to the relations in the expression, isa relations as a set of (Concept,Concept), declarations from the script, the expression => OR the type of the expression and the expression with typed and populated relations each of them bound to one declaration OR an error as String
infertype_and_populate
   :: (Relation Concept -> Relation Concept) ->
      [(Concept,Concept)] ->
      [Declaration Concept] ->
      (Concept,Concept) ->
      Expression (Relation Concept) ->
      Either ((Concept,Concept), Expression (Relation Concept) ,InfTree) (String,[Block])
infertype_and_populate populate isas ds (pushx,pushy) ex_in =
  case inf_expr of
    Left _ -> Left ((toCpt expr_src,toCpt expr_trg), enrich_expr uniqex,inftree)
    Right err -> Right (printterror ds uniqex err,errortrees (infertype_and_populate populate isas ds) ds uniqex err)
  where
  inferfromscript = infer (map fromDcl ds) (fromCptCpts isas) (fromCpt pushx, fromCpt pushy)
  Left ((expr_src,expr_trg),env_rel,inftree) = inf_expr
  (uniqex,_) = uniquerels 0 ex_in --give each morphism an identifier within the scope of this expression
  inf_expr = inferfromscript (fromExpr uniqex)
  enrich_expr (F exs) = F$map enrich_expr exs
  enrich_expr (Fdx exs) = Fdx$map enrich_expr exs
  enrich_expr (Fix exs) = Fix$map enrich_expr exs
  enrich_expr (Fux exs) = Fux$map enrich_expr exs
  enrich_expr (Cpx ex) = Cpx$enrich_expr ex
  enrich_expr (Tc ex) = Tc$enrich_expr ex
  enrich_expr (K0x ex) = K0x$enrich_expr ex
  enrich_expr (K1x ex) = K1x$enrich_expr ex
  enrich_expr (Tm mp i) = Tm (populate typedmp) i --populate all the (Populated a) in the typed and bound morphism
   where
   --use the identifier to get the type of the morphism and the declaration from the morphism binding
   --lookup the original declaration from the script by name, source and target
   --set the type of the morphism with the inferred type and bind the corresponding declaration to it
   ts = [(toCpt x,toCpt y,d') | (Morph _ _ i', (x,y),d')<-env_rel, i==i']
   (ec1,ec2,d) = if null ts then (NOthing,NOthing,d) else head ts
   typedmp = case mp of
      Rel{} -> if inline mp 
               then mp {relsrc=ec1,reltrg=ec2,reldcl=toDcl}
               else mp {relsrc=ec2,reltrg=ec1,reldcl=toDcl}
      I{} -> mp {relgen=ec1, relspc=ec1}
      V{} -> mp {reltyp=(ec1,ec2)}
      Mp1{} -> mp {rel1typ=ec1}
   toDcl = if null ds' then fatal 69 "could not find original declaration."
           else head ds'
      where ds' = [d'|d'<-ds, name d'==dname d, dtype d==(fromCpt(source d'),fromCpt(target d'))]

--DESCR -> if you need an identifier for relations within the scope of an expression 
uniquerels :: Int -> Expression (Relation c) -> (Expression (Relation c), Int)
uniquerels i (Tm mp _) = (Tm mp (i+1),i+1)
uniquerels i (F []) = (F [],i)
uniquerels i (F (ex:rexs)) = (F (lft:rghts),ri)
   where
   (lft,li) = uniquerels i ex
   (F rghts,ri) = (uniquerels li (F rexs))
uniquerels i (Fdx []) = (Fdx [],i)
uniquerels i (Fdx (ex:rexs)) = (Fdx (lft:rghts),ri)
   where
   (lft,li) = uniquerels i ex
   (Fdx rghts,ri) = (uniquerels li (Fdx rexs))
uniquerels i (Fix []) = (Fix [],i)
uniquerels i (Fix (ex:rexs)) = (Fix (lft:rghts),ri)
   where
   (lft,li) = uniquerels i ex
   (Fix rghts,ri) = (uniquerels li (Fix rexs))
uniquerels i (Fux []) = (Fux [],i)
uniquerels i (Fux (ex:rexs)) = (Fux (lft:rghts),ri)
   where
   (lft,li) = uniquerels i ex
   (Fux rghts,ri) = (uniquerels li (Fux rexs))
uniquerels i (Cpx ex) = (Cpx sb, si)
   where (sb,si) = uniquerels i ex
uniquerels i (Tc ex) = (Tc sb, si)
   where (sb,si) = uniquerels i ex
uniquerels i (K0x ex) = (K0x sb, si)
   where (sb,si) = uniquerels i ex
uniquerels i (K1x ex) = (K1x sb, si)
   where (sb,si) = uniquerels i ex


fromCptCpts :: [(Concept,Concept)] -> Isa
fromCptCpts xs = (fromCpt S,fromCpt S):[(fromCpt c1,fromCpt c2) |(c1,c2)<-xs]
fromCpt :: Concept -> RelAlgObj
fromCpt Anything = Universe
fromCpt NOthing = EmptyObject
fromCpt S = Object "#S#"
fromCpt c1 = Object (name c1)
toCpt :: RelAlgObj -> Concept
toCpt Universe = Anything
toCpt EmptyObject = NOthing 
toCpt (Object "#S#") = S 
toCpt (Object c1) = cptnew c1 
fromDcl :: Declaration Concept -> RelDecl
fromDcl d@(Sgn{}) = RelDecl {dname=name d
                            ,dtype=(fromCpt$source d,fromCpt$target d)
                            ,ishomo=foldr (||) False [True|p<-decprps d, elem p [Asy,Sym,Rfx,Trn]]
                            }
fromDcl _ = fatal 88 "only relation variables, not identities etc."
   
fromMphats :: [Concept] -> RelAlgType
fromMphats [] = (Universe,Universe)
fromMphats [c1] = (fromCpt c1,fromCpt c1)
fromMphats [c1,c2] = (fromCpt c1,fromCpt c2)
fromMphats _ = fatal 94 "too many relats"
 
--REMARK -> there will never be a Flip, because it is parsed flippedwise. The Flip is still implemented for other parse trees than the current Ampersand parse tree.
fromExpr :: Expression (Relation Concept) -> RelAlgExpr
fromExpr (Tm mp@(Rel{relyin=False}) i) = 
   Conv (Morph morph ((\(x,y)->(y,x))$fromMphats (relats mp)) i)
   where morph = DRel{rname=name mp}
fromExpr (Tm mp i) = 
   Morph morph (fromMphats (relats mp)) i
   where 
   morph = case mp of
     Rel{} -> DRel{rname=name mp}
     I{} -> IdRel
     V{} -> VRel
     Mp1{} -> IdRel
fromExpr (F []) = fatal 109 $ "Expression has no sub expressions F []." 
fromExpr (F (ex:[])) = fromExpr ex
fromExpr (F (ex:rexs)) = Comp (fromExpr ex) (fromExpr (F rexs))
fromExpr (Fdx []) = fatal 113 $ "Expression has no sub expressions F []." 
fromExpr (Fdx (ex:[])) = fromExpr ex
fromExpr (Fdx (ex:rexs)) = RAdd (fromExpr ex) (fromExpr (Fdx rexs))
fromExpr (Fix exs) = ISect$map fromExpr exs
fromExpr (Fux exs) = Union$map fromExpr exs
fromExpr (Cpx ex) = Compl (fromExpr ex)
fromExpr (Tc ex) = fromExpr ex
fromExpr (K0x ex) = fromExpr ex
fromExpr (K1x ex) = fromExpr ex

--TODO -> if I want "[1] Type mismatch in rule" to be recognized, then I'll have to analyse "[4] Incompatible comparison" errors. If the antecedent and consequent do have a type, then it is a type 1 error. But I do not want to make a union data type RuleOrExpression -> I want the rule operators to be expression operators so I can evaluate the expression.
--TODO -> I could print more in case of --verbose
printterror:: [Declaration Concept] -> Expression (Relation Concept) -> TError -> String
--TErrorU ETitle InfTree 
-- the source or target of the type of the root expression is the universe
printterror _ _ (TErrorU str _) 
              = "[0] "++str++"\n"
--the composition is over the universe
printterror _ root (TErrorUC str x y) 
              = "[10] "++str++"\nLeft operand: "++operandstr root (therels x)++"\nRight operand: "++operandstr root (therels y)
--TErrorAmb ETitle [RelAlgType] 
-- the type of the root expression is ambiguous
printterror _ _ (TErrorAmb str ts) 
              = "[2] "++str++"\nPossible types: "++ showtypes "\n\t"  ts++"\n"
--TError0 ETitle RelAlgObj
--the object is not defined in isas and not used in env_decls 
printterror _ _ (TError0 str c) 
              = "[9] "++str++": "++ show c++"\n"
--TError1 ETitle RelAlgExpr 
--the relation expression is not defined in the env_decls
printterror _ _ (TError1 str (Morph x (Universe,Universe) _)) 
              = "[3] "++str++": "++show x++"\n"
printterror _ _ (TError1 str (Morph x usrtype _)) 
              = "[3] "++str++": "++show x++"["++showtype usrtype++"]\n"
printterror _ _ (TError1 _ _) = fatal 140 "TError1 expects a relation expression."
--TError2 ETitle (RelAlgExpr,[RelAlgType]) ([RelAlgExpr],[RelAlgType]) 
--(ababab) there is no type in the first list matching a type in the second
printterror _ root (TError2 str (x,txs) (xs,txss)) 
              = "[4] "++str++"\nPossible types of left operand "++operandstr root (therels x)++":"++showtypes "\n\t" txs
                           ++"\nPossible types of right operand "++operandstr root (concat (map therels xs))
                                                                 ++":"++showtypes "\n\t" txss++"\n"
--TError3 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) 
--(abbcac) there is no b in the first list matching a b in the second
printterror _ root (TError3 str (x,txs) (y,tys)) 
              = "[5] "++str++"\nPossible types of left operand "++operandstr root (therels x)++":"++showtypes "\n\t" txs
                           ++"\nPossible types of right operand "++operandstr root (therels y)++":"++showtypes "\n\t" tys++"\n"
--TError4 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) [RelAlgObj] 
--(abbcac) there is more than one b in the first list matching a b in the second
printterror _ root (TError4 str (x,txs) (y,tys) tbs) 
              = "[6] "++str++"\nCompositions are possible over: "++show tbs
                           ++"\nPossible types of left operand "++operandstr root (therels x)++":"++showtypes "\n\t" txs
                           ++"\nPossible types of right operand "++operandstr root (therels y)++":"++showtypes "\n\t" tys++"\n"
--TError5 ETitle RelDecl 
--The declaration has an heteogeneous type and an homogeneous property
printterror ds _ (TError5 str d) 
              = "[7] "++str++" "++showADL toDcl++"\n"
              where toDcl = if null ds' then error "!Fatal (module TypeInference.InfLibAdlExpr 214): could not find original declaration."
                            else head ds'
                            where ds' = [d'|d'<-ds, name d'==dname d, dtype d==(fromCpt(source d'),fromCpt(target d'))]
--TError6 ETitle RelAlgType RelAlgExpr RelDecl 
--The declaration bound to the relation expression has an homogeneous property, but the type inferred is heterogeneous
printterror ds _ (TError6 str t (Morph m _ _) d) 
              = "[8] "++str++": " ++ showtype t
                      ++ (case m of
                            DRel{} -> "\nThe relation " ++show m++" has homogeneous properties on its declaration " 
                                      ++ showADL toDcl
                            IdRel{} -> "\nThe identity relation is an homogeneous relation"
                            _ -> fatal 174 "This cannot be a homogeneous relation."
                      )++"\n"
              where toDcl = if null ds' then error "!Fatal (module TypeInference.InfLibAdlExpr 227): could not find original declaration."
                            else head ds'
                            where ds' = [d'|d'<-ds, name d'==dname d, dtype d==(fromCpt(source d'),fromCpt(target d'))]
printterror _ _ (TError6 _ _ _ _) = fatal 223 "TError6 expects a relation expression."

showtype :: (RelAlgObj,RelAlgObj) -> String
showtype (x,y) = show x++"*"++show y
showtypes :: String -> [(RelAlgObj,RelAlgObj)] -> String
showtypes delim xs = concat (map ((++)delim ) (map showtype xs))

therels :: RelAlgExpr -> [Int]
therels  (ISect  sublst) = concat (map therels sublst)
therels  (Union  sublst) = concat (map therels sublst)
therels  (Comp   lsub rsub) = therels lsub ++ therels rsub
therels  (RAdd   lsub rsub) = therels lsub ++ therels rsub
therels  (Compl  sub) = therels sub 
therels  (Conv   sub) = therels sub
therels  (Implic lsub rsub) = therels lsub ++ therels rsub
therels  (Equiv  lsub rsub) = therels lsub ++ therels rsub
therels  (Morph  _ _ i) = [i]

operandstr:: Expression (Relation Concept) -> [Int] -> String
operandstr root rs = showADL(operand root rs)
operand:: Expression (Relation Concept) -> [Int] -> Expression (Relation Concept)
operand root rs = snd (operand' root)
  where
  niks = Tm (V [] (NOthing,NOthing)) (-1)
  operand' (Tm m i) = if elem i rs then (True,Tm m i) else (False,niks)
  operand' (Tc x) = if fst(operand' x) then (True,Tc (snd(operand' x))) else (False,niks)
  operand' (F  xs) = let xs'=[snd(operand' x) | x<-xs, fst(operand' x)]
                       in (not(null xs'),if length xs'==1 then head xs' else (F xs'))
  operand' (Fdx xs) = let xs'=[snd(operand' x) | x<-xs, fst(operand' x)]
                       in (not(null xs'),if length xs'==1 then head xs' else (Fdx xs'))
  operand' (Fix xs) = let xs'=[snd(operand' x) | x<-xs, fst(operand' x)]
                       in (not(null xs'),if length xs'==1 then head xs' else (Fix xs'))
  operand' (Fux xs) = let xs'=[snd(operand' x) | x<-xs, fst(operand' x)]
                       in (not(null xs'),if length xs'==1 then head xs' else (Fux xs'))
  operand' (K0x x) = if fst(operand' x) then (True,K0x (snd(operand' x))) else (False,niks)
  operand' (K1x x) = if fst(operand' x) then (True,K1x (snd(operand' x))) else (False,niks)
  operand' (Cpx x) = if fst(operand' x) then (True,Cpx (snd(operand' x))) else (False,niks)

--DESCR -> the inference trees of subexpressions, just before the error occurs
--         or all the inference trees in case of ambiguity
--         the error should contain subexpressions which can be inferred with inferfromscript
--         the root expression, the declarations are provided
--         operand root (therels x) is the original string of x::RelAlgExpr
--errortrees:: (RelAlgExpr -> Either (RelAlgType,[(RelAlgExpr,RelAlgType,RelDecl)],InfTree) TError) -> [Declaration Concept] -> Expression -> TError -> [Block]
errortrees::((Concept,Concept) -> Expression (Relation Concept) -> Either ((Concept,Concept), Expression (Relation Concept),InfTree) (String,[Block])) 
            -> [Declaration Concept] -> Expression (Relation Concept) -> TError -> [Block]
--TErrorU ETitle InfTree 
-- the source or target of the type of the root expression is the universe
errortrees _ ds root err@(TErrorU _ inftree) 
  = [errsct root (printterror ds root err)]
    ++texOnly_pandoctree (Just (inftree,root)) Nothing
--the composition is over the universe
errortrees f ds root err@(TErrorUC _ x y) 
  = [errsct root (printterror ds root err),errpar1]
    ++(inftreesub.f (Anything,Anything)) (operand root (therels x))
    ++(inftreesub.f (Anything,Anything)) (operand root (therels y))
--TErrorAmb ETitle [RelAlgType] 
-- the type of the root expression is ambiguous
errortrees f ds root err@(TErrorAmb _ ts) 
  = [errsct root (printterror ds root err),errpar1]
    ++ concat [(inftreesub.f (toCpt s,toCpt t)) root|(s,t)<-ts]
--TError0 ETitle RelAlgObj
--the object is not defined in isas and not used in env_decls 
errortrees _ ds root err@(TError0{}) 
  = [errsct root (printterror ds root err)]
--TError1 ETitle RelAlgExpr 
--the relation expression is not defined in the env_decls
errortrees _ ds root err@(TError1{}) 
  = [errsct root (printterror ds root err)]
--TError2 ETitle (RelAlgExpr,[RelAlgType]) ([RelAlgExpr],[RelAlgType]) 
--(ababab) there is no type in the first list matching a type in the second
errortrees f ds root err@(TError2 _ (x,txs) (xs,txss)) 
  = [errsct root (printterror ds root err),errpar1]
    ++concat([(inftreesub.f (toCpt s,toCpt t)) (operand root (therels x))|(s,t)<-txs]
           ++[(inftreesub.f (toCpt s,toCpt t)) (operand root (concat[therels e|e<-xs]))|(s,t)<-txss])
--TError3 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) 
--(abbcac) there is no b in the first list matching a b in the second 
errortrees f ds root err@(TError3 _ (x,txs) (y,tys))
  = concat$[[errsct root (printterror ds root err),errpar1]]
         ++[(inftreesub.f (toCpt s,toCpt t)) (operand root (therels x))|(s,t)<-txs]
         ++[(inftreesub.f (toCpt s,toCpt t)) (operand root (therels y))|(s,t)<-tys]
--TError4 ETitle (RelAlgExpr,[RelAlgType]) (RelAlgExpr,[RelAlgType]) [RelAlgObj] 
--(abbcac) there is more than one b in the first list matching a b in the second
errortrees f ds root err@(TError4 _ (x,txs) (y,tys) _) 
  = [errsct root (printterror ds root err),errpar1]
    ++concat([(inftreesub.f (toCpt s,toCpt t)) (operand root (therels x))|(s,t)<-txs]
           ++[(inftreesub.f (toCpt s,toCpt t)) (operand root (therels y))|(s,t)<-tys])
--TError5 ETitle RelDecl 
--The declaration has an heteogeneous type and an homogeneous property
errortrees _ ds root err@(TError5{})
  = [errsct root (printterror ds root err)]
--TError6 ETitle RelAlgType RelAlgExpr RelDecl 
--The declaration bound to the relation expression has an homogeneous property, but the type inferred is heterogeneous
errortrees _ ds root err@(TError6 _ _ (Morph _ _ _) _) 
  = [errsct root (printterror ds root err)]
errortrees _ ds root err = [Plain [Str ("No proof for error: " ++ (printterror ds root err))]]

--TODO -> make it possible to push a type on it, then it is expected to have no type errors.
inftreesub :: (Either ((Concept,Concept), Expression (Relation Concept),InfTree) (String,[Block])) -> [Block]
inftreesub (Left (tp,x,inftree)) = texOnly_pandoctree (Just (inftree,x)) (Just tp)
inftreesub (Right (_,inftree)) = inftree
--inftreesub (Right err) = fatal 239 ("Subexpressions are expected to have no type errors:\n"++fst err)

errsct :: Expression (Relation Concept) -> String -> Block
errsct x err = 
   Plain [TeX ("\\section{Type error in $"++ texOnly_writerule x++ "$ }\n")
         ,TeX [c|c<-"By definition: $"++texOnly_writerule x++" \\Leftrightarrow "++texOnly_writeexpr x++"$ \\newline \n",texOnly_writerule x/=texOnly_writeexpr x] 
         ,Str err
         ]
errpar1 :: Block
errpar1 = Para [Str "Printed next is the list of proofs for all the possible types claimed in the error message."]
