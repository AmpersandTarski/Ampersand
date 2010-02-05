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
import TypeInference.Isa
import Adl

----------------------------------------------------------------------------
--ADL conversie
----------------------------------------------------------------------------
--TODO -> if I want "[1] Type mismatch in rule" to be recognized, then I'll have to analyse "[4] Incompatible comparison" errors. If the antecedent and consequent do have a type, then it is a type 1 error.
infertype_and_populate :: (Morphism -> Morphism) -> Concepts -> Gens -> Declarations -> Expression -> Either ((Concept,Concept), Expression) String
infertype_and_populate populate cs gs ds ex_in = --error$show$ (map fromDcl ds, fromCptCpts$isaRels cs gs) 
  case rtype of
    Left (x,y) -> if x==Universe || y==Universe then error ("y") else Left ((toCpt x,toCpt y), enrich_expr uniqex)
    Right err -> Right err
  where
  (uniqex,_) = uniquemphs 0 ex_in
  env_in = env_in_Syn_RelAlgExpr (inftree (Universe,Universe))
  env_mph = [(m,t,d)|(m,Left t,d)<-env_mph_Syn_RelAlgExpr (inftree (head alltypes))] --finalize by pushing the type down again
  rtype = if length alltypes==1 
          then rtype_Syn_RelAlgExpr (inftree (head alltypes)) --finalize by pushing the type down again
          else if null alltypes
               then Right env_in_err
               else Right ("[2] Ambiguous type: " ++ show alltypes)
  alltypes = case env_in of
     Left xs -> [t| t<-xs]
     _ -> []
  env_in_err = case env_in of
     Right x -> x
     _ -> ""
  inftree push = wrap_RelAlgExpr (sem_RelAlgExpr$normalise$fromExpr uniqex)$Inh_RelAlgExpr (map fromDcl ds) (fromCptCpts$isaRels cs gs) NoListOf push
  enrich_expr (F exs) = F$map enrich_expr exs
  enrich_expr (Fd exs) = Fd$map enrich_expr exs
  enrich_expr (Fi exs) = Fi$map enrich_expr exs
  enrich_expr (Fu exs) = Fu$map enrich_expr exs
  enrich_expr (Cp ex) = Cp$enrich_expr ex
  enrich_expr (Tc ex) = Tc$enrich_expr ex
  enrich_expr (K0 ex) = K0$enrich_expr ex
  enrich_expr (K1 ex) = K1$enrich_expr ex
  enrich_expr (Tm mp i) = Tm (populate typedmp) i
   where
   ts = [(toCpt x,toCpt y,d') | (Morph _ _ i', (x,y),d')<-env_mph, i==i']
   (ec1,ec2,d) = if null ts then (NOthing,NOthing,d) else head ts
   typedmp = case mp of
      Mph{} -> mp {mphtyp=(ec1,ec2),mphdcl=toDcl}
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

