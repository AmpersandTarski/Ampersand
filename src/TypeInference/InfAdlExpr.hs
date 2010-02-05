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
module TypeInference.InfAdlExpr where
import TypeInference.InfLibAG
import Adl

----------------------------------------------------------------------------
--ADL conversie
----------------------------------------------------------------------------
--TODO -> if I want "[1] Type mismatch in rule" to be recognized, then I'll have to analyse "[4] Incompatible comparison" errors. If the antecedent and consequent do have a type, then it is a type 1 error.
infertype_and_populate :: (Morphism -> Morphism) -> [(Concept,Concept)] -> Declarations -> Expression -> Either ((Concept,Concept), Expression) String
infertype_and_populate populate isas ds ex_in =
  case inf_expr of
    Left _ -> Left ((toCpt expr_src,toCpt expr_trg), enrich_expr uniqex)
    Right err -> Right (show err)
  where
  Left ((expr_src,expr_trg),env_mph) = inf_expr
  (uniqex,_) = uniquemphs 0 ex_in --give each morphism an identifier within the scope of this expression
  inf_expr = infer (map fromDcl ds) (fromCptCpts isas) (fromExpr uniqex)
  enrich_expr (F exs) = F$map enrich_expr exs
  enrich_expr (Fd exs) = Fd$map enrich_expr exs
  enrich_expr (Fi exs) = Fi$map enrich_expr exs
  enrich_expr (Fu exs) = Fu$map enrich_expr exs
  enrich_expr (Cp ex) = Cp$enrich_expr ex
  enrich_expr (Tc ex) = Tc$enrich_expr ex
  enrich_expr (K0 ex) = K0$enrich_expr ex
  enrich_expr (K1 ex) = K1$enrich_expr ex
  enrich_expr (Tm mp i) = Tm (populate typedmp) i --populate all the (Populated a) in the typed and bound morphism
   where
   --use the identifier to get the type of the morphism and the declaration from the morphism binding
   --lookup the original declaration from the script by name, source and target
   --set the type of the morphism with the inferred type and bind the corresponding declaration to it
   ts = [(toCpt x,toCpt y,d') | (Morph _ _ i', (x,y),d')<-env_mph, i==i']
   (ec1,ec2,d) = if null ts then (NOthing,NOthing,d) else head ts
   typedmp = case mp of
      Mph{} -> if inline mp 
               then mp {mphtyp=(ec1,ec2),mphdcl=toDcl}
               else mp {mphtyp=(ec2,ec1),mphdcl=toDcl}
      I{} -> mp {mphgen=ec1, mphspc=ec1}
      V{} -> mp {mphtyp=(ec1,ec2)}
      Mp1{} -> mp {mph1typ=ec1}
   toDcl = if null ds' then error "fatal: could not find original declaration."
           else head ds'
      where ds' = [d'|d'<-ds, name d'==dname d, dtype d==(fromCpt(source d'),fromCpt(target d'))]

fromCptCpts :: [(Concept,Concept)] -> Isa
fromCptCpts xs = [(fromCpt c1,fromCpt c2) |(c1,c2)<-xs]
fromCpt :: Concept -> RelAlgObj
fromCpt Anything = Universe
fromCpt NOthing = EmptyObject
fromCpt c1 = Object (name c1)
toCpt :: RelAlgObj -> Concept
toCpt Universe = Anything
toCpt EmptyObject = NOthing 
toCpt (Object c1) = cptnew c1 
fromDcl :: Declaration -> RelDecl
fromDcl d@(Sgn{}) = RelDecl {dname=name d
                            ,dtype=(fromCpt$source d,fromCpt$target d)
                            ,ishomo=foldr (||) False [True|p<-decprps d, elem p [Asy,Sym,Rfx,Trn]]
                            }
fromDcl _ = error "only relvars"
   
fromMphats :: Concepts -> RelAlgType
fromMphats [] = (Universe,Universe)
fromMphats [c1] = (fromCpt c1,fromCpt c1)
fromMphats [c1,c2] = (fromCpt c1,fromCpt c2)
fromMphats _ = error "too many mphats"
 
--REMARK -> there will never be a Flip, because it is parsed flippedwise. The Flip is still implemented for other parse trees than the current ADL parse tree.
fromExpr :: Expression -> RelAlgExpr
fromExpr (Tm mp@(Mph{mphyin=False}) i) = 
   Conv (Morph morph ((\(x,y)->(y,x))$fromMphats (mphats mp)) i)
   where morph = DRel{rname=name mp}
fromExpr (Tm mp i) = 
   Morph morph (fromMphats (mphats mp)) i
   where 
   morph = case mp of
     Mph{} -> DRel{rname=name mp}
     I{} -> IdRel
     V{} -> VRel
     Mp1{} -> IdRel
fromExpr (F []) = error $ "Error in AdlExprAG.hs module TypeInference.AdlExprAG function fromExpr: " ++
                          "Expression has no sub expressions"++show (F [])++"." 
fromExpr (F (ex:[])) = fromExpr ex
fromExpr (F (ex:rexs)) = Comp (fromExpr ex) (fromExpr (F rexs))
fromExpr (Fd []) = error $"Error in AdlExprAG.hs module TypeInference.AdlExprAG function fromExpr: " ++
                          "Expression has no sub expressions"++show (Fd [])++"." 
fromExpr (Fd (ex:[])) = fromExpr ex
fromExpr (Fd (ex:rexs)) = RAdd (fromExpr ex) (fromExpr (Fd rexs))
fromExpr (Fi exs) = ISect$map fromExpr exs
fromExpr (Fu exs) = Union$map fromExpr exs
fromExpr (Cp ex) = Compl (fromExpr ex)
fromExpr (Tc ex) = fromExpr ex
fromExpr (K0 ex) = fromExpr ex
fromExpr (K1 ex) = fromExpr ex

