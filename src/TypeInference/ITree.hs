{-# OPTIONS_GHC -Wall #-}
module TypeInference.ITree where
import Adl.Concept
import Adl.MorphismAndDeclaration
import Adl.Prop
import CommonClasses
import TypeInference.AdlExpr
import Data.List

type Gamma = [Statement]
type Statements = [Statement]
data Statement = IsaStat  Concept Concept | --DESCR -> stating that the left concept IS-a right concepte
                 InfErr InfErrType   | --DESCR -> stating that the type of an expression cannot be inferred, because the inference attempt ended because of this error
                 EmptyStmt | --DESCR -> stating that there is no statement
                 BoundTo AdlExpr |
                 DeclExpr {declex::AdlExpr, homo::Bool} 
                 
data DisjType = DisjSrc | DisjTrg | DisjHomo deriving (Eq, Show)
data InfErrType = UndeclRel AdlExpr | 
                  IErr DisjType Concept Concept AdlExpr

instance Show InfErrType where
   showsPrec _ (UndeclRel (Relation{rel=mp})) = showString $ 
        "Undeclared relation " ++ name mp ++ "."
   showsPrec _ (UndeclRel expr) =  error $ "Error in ITree.hs module TypeInference.ITree instance Show InfErrType: " ++
                                           "The expression of UndeclRel must be a relation." 
   showsPrec _ (IErr _ c1 c2 _) = showString $ 
        "Disjunct concepts " ++ show c1 ++ " and " ++ show c2 ++ "."

instance Show Statement where
   showsPrec _ (IsaStat c1 c2) = showString $ show c1 ++ "is-a" ++ show c2
   showsPrec _ (InfErr x) = showString $ show x
   showsPrec _ (EmptyStmt) = showString $ "No Statement"
   showsPrec _ (BoundTo expr) = showString $ printexpr expr
   showsPrec _ de@(DeclExpr{}) = showString $ printexpr (declex de) ++ (if homo de then " {HOMO}" else "")
 
printexpr ex@(Relation mp _ _)= show mp ++ "[" ++ (show $ evalstmt (BoundTo ex)) ++"]"
printexpr (Implicate expr1 expr2 _)= printexpr expr1 ++ "|-" ++ printexpr expr2
printexpr (Equality expr1 expr2 _)= printexpr expr1 ++ "=" ++ printexpr expr2
printexpr (Union exprs _)= "UNION: " ++ (foldr (++) [] [", " ++ printexpr ex | ex<-exprs])
printexpr (Intersect exprs _)= "DISJ: " ++  (foldr (++) [] [", " ++ printexpr ex | ex<-exprs])
printexpr (Semicolon expr1 expr2 _)= printexpr expr1 ++ ";" ++ printexpr expr2
printexpr (Dagger expr1 expr2 _)= printexpr expr1 ++ "!" ++ printexpr expr2
printexpr (Complement expr _)= "-" ++ printexpr expr
printexpr (Flip expr _)= printexpr expr ++ "~" 

prlst xs = foldr (++) [] $ [x ++ "\n"|x<-xs]

instance Eq InfErrType where
   (UndeclRel expr)==(UndeclRel expr') = expr==expr'
   (IErr dtp c1 c2 expr)==(IErr dtp' c1' c2' expr') = expr==expr' && c1==c1' && c2==c2' && dtp==dtp'
   _ == _ = False

--DESCR -> constructs an IsaStat statement from an isa definition (spc,gen)
fromIsa :: (Concept,Concept) -> Statement
fromIsa (c1,c2) = IsaStat c1 c2

iserrstmt :: Statement -> Bool
iserrstmt (InfErr {}) = True
iserrstmt _ = False

instance Eq Statement where
  (IsaStat c1 c2)==(IsaStat c1' c2') = c1==c1' && c2==c2'
  (InfErr tp )==(InfErr tp' ) = tp==tp'
  EmptyStmt==EmptyStmt = True
  (DeclExpr expr h)==(DeclExpr expr' h') = expr==expr' && h==h'
  (BoundTo expr)==(BoundTo expr') = expr==expr'
  _==_ = False
------------------------------------------------------------
--DESCR -> Or I have a type, proofed by the fact that all alternatives resulting in a type, result in the same type.
--         Or I could not infer a type, proofed by the fact that all alternatives result in error(s).
--         Or I have an ambiguous type, proofed by the fact that some alternatives result in different types.
data Proof = Proven Gamma [ITree] | NoProof TypeErrorsType   
data TypeErrorsType = NoType Gamma [(Statements,ITree)] | AmbiguousType Gamma [ITree] deriving (Show)

instance Show Proof where
   showsPrec _ (Proven g ts) = showString $ show g ++ "\n" ++ show ts
   showsPrec _ (NoProof tp) = showString $ case tp of
      NoType g tts ->  show $ analyseerror g tts
      AmbiguousType g ts ->  show $ analyseamb g ts

instance Association Proof where
  source (Proven _ [] ) = Anything
  source (Proven gm inftrees ) = source $ evalstmt $ evaltree gm (head inftrees)
  source (NoProof _ ) = NOthing
  target (Proven _ [] ) = Anything
  target (Proven gm inftrees ) = target $ evalstmt $ evaltree gm (head inftrees)
  target (NoProof _ ) = NOthing

data ErrorCode = ErrCode Int 
instance Show ErrorCode where
   showsPrec _ (ErrCode 1) = showString $ "[1] Type mismatch in rule"
   showsPrec _ (ErrCode 2) = showString $ "[2] Ambiguous type"
   showsPrec _ (ErrCode 3) = showString $ "[3] Relation undefined"
   showsPrec _ (ErrCode 4) = showString $ "[4] Incompatible comparison" --union and disjunction
   showsPrec _ (ErrCode 5) = showString $ "[5] Incompatible composition"
   showsPrec _ (ErrCode 6) = showString $ "[6] Ambiguous composition"
   showsPrec _ (ErrCode 7) = showString $ "[7] Homogeneous property on heterogeneous relation"
   showsPrec _ (ErrCode 8) = showString $ "[8] Type is not homogeneous" 
   --showsPrec _ (ErrCode 9) = showString $ "[9] Type is not homogeneous" --merged with 8
   showsPrec _ (ErrCode i) = showString $ "[" ++ show i ++ "] Undefined error"
data TypeError = TypeError {errcode::ErrorCode, errmsg::String}
instance Show TypeError where
   showsPrec _ te =  showString $ show (errcode te) ++ "\n"++ show (errmsg te)

analyseerror :: Gamma -> [(Statements,ITree)] -> TypeError
analyseerror g tts = case mberr of
   Just (_,err@(UndeclRel _)) -> TypeError (ErrCode 3) (show err)
   Just (ss,(IErr dtp c1 c2 expr)) -> analysedisjunction g ss dtp c1 c2 expr
   Nothing                    -> TypeError (ErrCode 0) "No type error found in the proof. Contact the system administrator."
   where
   errors = [((ss,basetree),x)|(ss,basetree)<-tts, InfErr x<-ss]
   mberr = if null errors then Nothing else Just $ head errors
analyseerror _ _ = TypeError (ErrCode 0) "No inference trees in proof. Contact the system administrator." 

analysedisjunction :: Gamma -> (Statements,ITree) -> DisjType -> Concept -> Concept -> AdlExpr -> TypeError
analysedisjunction g (ss,basetree) dtp' c1 c2 expr = check7
   where
   BoundTo infexpr =  evaltree g basetree
   disjrel = case expr of
      Relation{} -> expr
      Complement{sub=dr@(Relation{})} -> dr
      Complement{sub=Flip{sub=dr@(Relation{})}} -> dr
      Flip{sub=Complement{sub=dr@(Relation{})}} -> dr
      Flip{sub=dr@(Relation{})} -> dr
      _ -> error $ "Error in ITree.hs module TypeInference.ITree function analysedisjunction: " ++
                   "The expression of IErr must be a relation or a complement or flip or a combination."
   Relation{rel=mp,mphid=i} = disjrel
   dtp = case mp of 
       Mph{mphyin=False} -> case dtp' of
                                 DisjSrc -> DisjTrg
                                 DisjTrg -> DisjSrc
                                 _ -> dtp'
       _ -> dtp'  
   printdtp = case dtp of
       DisjSrc -> "source"
       DisjTrg -> "target"
       DisjHomo -> "source or target"
   basedonrelations :: AdlExpr -> [AdlExpr]
   basedonrelations exprX= case exprX of
      Dagger{left=x, right=y} -> case dtp of
         DisjSrc -> basedonrelations x 
         DisjTrg -> basedonrelations y
         _ -> []
      Semicolon{left=x, right=y} -> case dtp of
         DisjSrc -> basedonrelations x 
         DisjTrg -> basedonrelations y
         _ -> []
      Union{lst=xs} -> [r|x<-xs,r<-basedonrelations x]
      Intersect{lst=xs} -> [r|x<-xs,r<-basedonrelations x] 
      Implicate{left=x, right=y} -> basedonrelations x ++ basedonrelations y
      Equality{left=x, right=y} -> basedonrelations x ++ basedonrelations y
      Complement{sub=x} -> basedonrelations x
      Flip{sub=x} -> basedonrelations x
      Relation{} -> [exprX]
   --DESCR -> If the disjunction is on the source or target of the antecedent or consequent 
   --         of an Implicate or Equality, then code 1
   intstr = show i ++ case i of 
         1 -> "st"
         2 -> "nd"
         _ -> "th"
   --USE -> first check7 and 8 before check1
   check1 = let
            err = case infexpr of
               Implicate{} -> [() | elem disjrel (basedonrelations infexpr)]
               Equality{} -> [() | elem disjrel (basedonrelations infexpr)]
               _ -> []
            msg = "The  " ++ printdtp ++ " of the antecedent does not match the " ++ printdtp ++ " of the consequent. "
                  ++ (show $ IErr dtp c1 c2 expr)
            in
            if (not.null) err
            then TypeError (ErrCode 1) msg
            else check4
   --USE -> first check1 before check4
   check4 = let
            err = case infexpr of
               Implicate{} -> [() | elem disjrel (basedonrelations (left infexpr))]
                           ++ [() | elem disjrel (basedonrelations (right infexpr))]
               Equality{} ->  [() | elem disjrel (basedonrelations (left infexpr))]
                           ++ [() | elem disjrel (basedonrelations (right infexpr))]
               _ -> [() | elem disjrel (basedonrelations infexpr)]
            msg = "The "++printdtp++" of the "++intstr++" relation "++name mp
                  ++" does not match the "++printdtp++"s of expressions it is compared to. "
                  ++ (show $ IErr dtp c1 c2 expr)
            in
            if (not.null) err
            then TypeError (ErrCode 4) msg
            else check5
   --USE -> first check4 before check5
   check5 = let msg = "The "++printdtp++" of the "++intstr++" relation "++name mp
                   ++ " does not match the expression it is composed with. "
                   ++ (show $ IErr dtp c1 c2 expr)
            in TypeError (ErrCode 5) msg 
   check7 = let
            err = case infexpr of
                Equality
                   {left=r1,right=r2} -> if null (meandmyflip r1 r2) then [] 
                                         else [(rel r,"symmetric")| r@(Relation{})<-meandmyflip r1 r2, hetero r]  
                Implicate
                   {left=Intersect{lst=[r1,r2]}
                   ,right=Relation{rel=I{}}} -> if null (meandmyflip r1 r2) then [] 
                                                     else [(rel r,"asymmetric")| r@(Relation{})<-meandmyflip r1 r2, hetero r]
                Implicate
                   {left=Semicolon{left=Relation{rel=r2@(Mph{})},right=Relation{rel=r3@(Mph{})}}
                   ,right=rex@(Relation{rel=r1@(Mph{})})} -> [(r1,"transitive") | r1==r2, r2==r3, hetero rex]
                Implicate
                   {left=Relation{rel=(I{})}
                   ,right=rex@(Relation{rel=r@(Mph{})})} -> [(r,"reflexive") | hetero rex]
                _ -> []
            meandmyflip r r' = case r of
               r@(Relation{rel=mp1@(Mph{})}) -> case r' of
                   Flip{sub=Relation{rel=mp2@(Mph{})}} -> [r|mp1==flp mp2]
                   _ -> []
               Flip{sub=Relation{rel=mp1@(Mph{})}} -> case r' of
                   r@(Relation{rel=mp2@(Mph{})}) -> [r|flp mp1==mp2]
                   _ -> []
               _ -> []
            hetero (Relation{tt=TT ct1 ct2 _}) = ct1 /= ct2
            hetero _ = error $  "Error in ITree.hs module TypeInference.ITree function analysedisjunction.check7.hetero: " ++
                                "The expression must be a Relation."  
            msg = "Heterogeneous relation " ++ (name.fst.head) err ++ " cannot be " ++ (snd.head) err++"."
            in
            if (not.null) err
            then TypeError (ErrCode 7) msg
            else check8
   check8 = let
            err = case dtp of
                DisjHomo -> [()] 
                _ -> []
            msg = "The source and target of the "++intstr++", homogeneous relation "++name mp++" do not match. "
                  ++ (show $ IErr dtp c1 c2 expr)
            in
            if (not.null) err
            then TypeError (ErrCode 8) msg
            else check1
--   return0 = TypeError (ErrCode 0) $ (show $ IErr dtp c1 c2 expr) -- ++ printexpr infexpr ++ show dtp ++ printexpr expr

--DESCR -> if ambiguity is a consequent of an ambiguous composition, then code 6 else code 2
analyseamb :: Gamma -> [ITree] -> TypeError
analyseamb g ts = 
   if not (sametype inftypes)
   then TypeError (ErrCode 2) $ "The type can be " ++ prinftypes ++ "."
   else if null (diffcomps exprrels)
        then TypeError (ErrCode 0) 
             "There appears to be an ambiguity, but details cannot be found. Contact the system administrator."
        else TypeError (ErrCode 6) $ printdiffcomps
   where
   prinftypes = (savetail.savetail.savetail.savetail) [c|itp<-inftypes,c<-" or "++show itp]
   infstmts = [evaltree g t|t<-ts]
   inftypes = map evalstmt infstmts
   sametype [] = True
   sametype (x:[]) = True
   sametype (x:y:xs) = x==y && sametype (y:xs)
   exprrels = [relations expr | (BoundTo expr)<-infstmts]
   printdiffcomps = (savetail.savetail) [c|str<-diffcomps exprrels, c<-". "++str]
   savetail [] = []
   savetail (x:xs) = xs
   diffcomps [] = []
   diffcomps (x:[]) = []
   diffcomps (x:y:xs) = ["The type of "++intstr (mphid r1) ++" relation (" 
                         ++ (name $ rel r1) ++ ") can be " ++ signstr r1
                         ++ " or " ++ signstr r2
                         |(r1,r2)<-zip x y, (tt r1)/=(tt r2),r1==r2 ] ++ diffcomps (y:xs)
      where 
      signstr r = show $ evalstmt (BoundTo r)
      intstr i = show i ++ case i of 
         1 -> "st"
         2 -> "nd"
         _ -> "th"

-------------------------------------------------------------------------------------------

--DESCR -> For type inference we defined rules to be able to construct an inference tree, to infer a type or type error, for all expressions.
--         Stmt is a basic statement
--         BindRule binds a TypeStat statement resulting in a BndStat statement
--         SpecRule specifies the domain or range of the type in a TypeStat statement given a certain IsaStat statement
--         DisjRule determines the BndStat statement of a disjunction expression based on the BndStat of its left and right expression
--         RelCompRule determines the BndStat statement of a relative composition expression based on the BndStat of its left and right expression
data ITree = Stmt {baseax::Statement}
           | DisjRule {axs1::[ITree], axs2::[ITree]}
           | UnionRule {axs1::[ITree], axs2::[ITree]}
           | ImplyRule {ax::ITree}
           | EqualRule {ax::ITree}
           | RelcompRule {comptype::CompType, ax1::ITree, ax2::ITree}
           | AddcompRule {comptype::CompType, ax1::ITree, ax2::ITree}
           | BindRule {bindtype::BindType, ax::ITree}
           | DComplRule {ax::ITree}
           | FlipRule {invtype::InvType, ax::ITree}
           | SpecRule {spectype::SpecType, ax1::ITree, ax2::ITree} 
           | DeMorganRule {dmtype::DMType, ax::ITree}
           deriving (Show)

data BindType = Bind | BindCompl deriving (Show)
data SpecType = SpecDomain | SpecRange deriving (Show) 
data CompType = Comp | CompInv1 | CompInv2 | CompDInv deriving (Show) 
data InvType = NoInv | Inv deriving (Show) 
data DMType = FAddcomp | FRelcomp | FUnion | FDisj deriving (Show) 
 
--DESCR -> returns all the Stmt statements in a tree
stmts :: ITree -> [Statement]
stmts (Stmt stmt) = [stmt]
stmts (DisjRule trs1 trs2) = foldr (++) [] $ [stmts tr1|tr1<-trs1] ++ [stmts tr2|tr2<-trs2]
stmts (UnionRule trs1 trs2) = foldr (++) [] $ [stmts tr1|tr1<-trs1] ++ [stmts tr2|tr2<-trs2]
stmts (ImplyRule tr) = stmts tr
stmts (EqualRule tr) = stmts tr
stmts (RelcompRule _ tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (AddcompRule _ tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (BindRule _ tr) = stmts tr
stmts (DComplRule tr) = stmts tr
stmts (FlipRule _ tr) = stmts tr
stmts (SpecRule _ tr1 tr2) = (stmts tr1) ++ (stmts tr2)
stmts (DeMorganRule _ tr) = stmts tr

--DESCR -> folds the tree to a single statement by evaluating all rules
--TODO -> check on axioms, now I just draw the conclusion, which is save if there are no bugs. In case of bugs there can be ugly pattern mismatch errors. So check and throw nice errors.
evaltree :: Gamma -> ITree -> Statement
evaltree _ (Stmt stmt) = stmt
evaltree gamma (DisjRule noinvs invs) = BoundTo $ Intersect sortedexs $ fromSign infT --this is the conclusion
   where
   --infer what T will be for the expression and all subexpressions
   infT@(infsrc,inftgt) = 
        if null disjinvsax --no complements
        then if null disjax then  (NOthing,NOthing) --nothing
             else (foldevalspec gamma gsrcsdisjax,foldevalspec gamma gtgtsdisjax) --only normal, take mostspec of declared normal
        else if null disjax
             then (foldevalgen gamma gsrcsdisjinvsax,foldevalgen gamma gtgtsdisjinvsax) --only complements, lub
             else (foldevalspec gamma gsrcsdisjax,foldevalspec gamma gtgtsdisjax) --some normal, take mostspec of declared normal
   disjax = map (evaltree gamma) noinvs
   --REMARK -> Take ToGen and not just val of the ConceptTerm, because val can be more specific if the disjunction contains a more specific complement subexpression. If there is no complement subexpression the foldevalspec will equal val.
   gsrcsdisjax = [toGen $ exprsrc x |(BoundTo x)<-disjax]
   gtgtsdisjax = [toGen $ exprtgt x |(BoundTo x)<-disjax]
   disjinvsax =  map (evaltree gamma) invs
   gsrcsdisjinvsax = [toGen $ exprsrc x |(BoundTo x)<-disjinvsax]
   gtgtsdisjinvsax = [toGen $ exprtgt x |(BoundTo x)<-disjinvsax]
   --reassign the inferred T to all subexpressions and sort the subexpressions to their original order
   disjcombax1 = [reassignexpr (Just infsrc,Just inftgt) x | (BoundTo x)<-disjax]
   disjcombax2 = [reassignexpr (Just infsrc,Just inftgt) x | (BoundTo x)<-disjinvsax]
   sortedexs = sort $ disjcombax1 ++ disjcombax2
evaltree gamma (UnionRule noinvs invs) = BoundTo $ Union sortedexs $ fromSign infT --this is the conclusion
   where
   --infer what T will be for the expression and all subexpressions
   infT@(infsrc,inftgt) = if null allaxs 
          then (NOthing,NOthing)
          else (foldevalgen gamma gsrcsallaxs,foldevalgen gamma gtgtsallaxs)
   disjax = map (evaltree gamma) noinvs
   disjinvsax =  map (evaltree gamma) invs
   allaxs = disjax ++ disjinvsax
   gsrcsallaxs = [toGen $ exprsrc x |(BoundTo x)<-allaxs]
   gtgtsallaxs = [toGen $ exprtgt x |(BoundTo x)<-allaxs]
   --reassign the inferred T to all subexpressions and sort the subexpressions to their original order
   disjcombaxs = [reassignexpr (Just infsrc,Just inftgt) x|(BoundTo x)<-allaxs]
   sortedexs = sort disjcombaxs
evaltree gamma (RelcompRule ct tr1 tr2) = BoundTo $ Semicolon 
                                        (reassignexpr (Nothing,Just c2) ex)
                                        (reassignexpr (Just c2,Nothing) rex)
                                        $ fromSign (c1,c3) --this is the conclusion
   where
   --REMARK -> only mph expressions and complement mph expressions will have a CTAS
   BoundTo ex = evaltree gamma tr1
   BoundTo rex = evaltree gamma tr2
   c1 = val $ exprsrc ex
   c2 = case ct of
      Comp -> val (exprtgt ex) --I already inferred the glb, (exprtgt ex)==(exprsrc rex)
      CompInv1 -> toGen (exprsrc rex) --the declared concept from the source of the right, or just the source of the right expression, because left is a complement
      CompInv2 -> toGen (exprtgt ex) --the declared concept from the target of the left morphism, or just the target of the left expression, because right is a complement
      CompDInv -> evalgen gamma (toGen $ exprsrc rex) (toGen $ exprtgt ex) --the most general of the two declared
   c3 = val $ exprtgt rex
evaltree gamma (AddcompRule ct tr1 tr2) = BoundTo $ Dagger 
                                        (reassignexpr (Nothing,Just c2) ex)
                                        (reassignexpr (Just c2,Nothing) rex)
                                        $ fromSign (c1,c3) --this is the conclusion
   where
   --REMARK -> only mph expressions and complement mph expressions will have a CTAS
   BoundTo ex = evaltree gamma tr1
   BoundTo rex = evaltree gamma tr2
   c1 = val $ exprsrc ex
   c2 = case ct of
      Comp -> evalgen gamma (toGen $ exprsrc rex) (toGen $ exprtgt ex) --the most general of the two declared
      CompInv1 -> toGen (exprsrc rex) --the declared concept from the source of the right, or just the source of the right expression, because left is a complement
      CompInv2 -> toGen (exprtgt ex) --the declared concept from the target of the left morphism, or just the target of the left expression, because right is a complement
      CompDInv -> val (exprtgt ex) --I already inferred the glb, (exprtgt ex)==(exprsrc rex)
   c3 = val $ exprtgt rex
evaltree gamma (ImplyRule tr) =  BoundTo $ Implicate x y (tt ex) 
   where
   BoundTo ex = evaltree gamma tr
   Union{lst=(invx:y:[])} = ex
   Complement{sub=x} = invx
evaltree gamma (EqualRule tr) = BoundTo $ Equality x y (tt implex1) 
   where
   DisjRule (impltr1:_:[]) _ = tr
   BoundTo (implex1@Implicate{left=x,right=y}) = evaltree gamma impltr1
   --REMARK (for TODO of implementation of axiom check) -> Because evalTree Union sorts on mphid, I cannot use evaltree on second Implicate
   --ImplyRule utr = impltr2
   --BoundTo ex = evaltree gamma utr
   --Union{lst=(y:invx:[])} = ex
   --Complement{sub=x} = invx
   --implex2 = Implicate x y (tt ex)
evaltree gamma (FlipRule it tr) = BoundTo boundexpr 
   where
   BoundTo sb = evaltree gamma tr
   infT = (tt sb){cts=exprtgt sb, ctt=exprsrc sb}
   boundexpr = case it of
     NoInv -> Flip {sub=sb, tt=infT}
     Inv -> let Complement{sub=sb'} = sb 
            in Complement{sub=Flip {sub=sb', tt=infT},tt=infT}
evaltree gamma (DComplRule tr) = BoundTo $ Complement (Complement sb (tt sb)) (tt sb)
   where
   BoundTo sb = evaltree gamma tr 
evaltree gamma (BindRule bt tr) = BoundTo boundexpr
   where
   DeclExpr sb _  = evaltree gamma tr
   boundexpr = case bt of
     Bind -> sb
     BindCompl -> Complement{sub=sb,tt=inversetype (tt sb)}
evaltree gamma (SpecRule st tr1 tr2) = DeclExpr (sb{tt=boundtype}) hm
   where
   IsaStat c3 _ = evaltree gamma tr1
   DeclExpr sb hm = evaltree gamma tr2
   boundtype = case st of
     SpecDomain -> (tt sb){cts=specval c3 (cts $ tt sb)}
     SpecRange ->  (tt sb){ctt=specval c3 (ctt $ tt sb)}
   specval :: Concept -> ConceptTerm -> ConceptTerm
   specval cs (CT{val=cg}) = CTAS{val=cs,as=cg}
   specval cs ct@(CTAS{}) = ct{val=cs}
evaltree gamma (DeMorganRule dmt tr) = BoundTo boundexpr  
   where
   BoundTo ex = evaltree gamma tr
   boundexpr = case dmt of
      FAddcomp -> let 
                  Semicolon{left=invx,right=invy} = ex
                  Complement{sub=x} = invx
                  Complement{sub=y} = invy
                  in Complement{sub=Dagger{left=x,right=y,tt=tt ex}
                               ,tt=inversetype (tt ex)}
      FRelcomp -> let 
                  Dagger{left=invx,right=invy} = ex
                  Complement{sub=x} = invx
                  Complement{sub=y} = invy
                  in Complement{sub=Semicolon{left=x,right=y,tt=tt ex}
                               ,tt=inversetype (tt ex)}
      FUnion -> let 
                Intersect{lst=invxs} = ex
                in Complement{sub=Union{lst=[x|(Complement{sub=x})<-invxs],tt=tt ex}
                             ,tt=inversetype (tt ex)}
      FDisj -> let 
               Union{lst=invxs} = ex
               in Complement{sub=Intersect{lst=[x|(Complement{sub=x})<-invxs],tt=tt ex}
                            ,tt=inversetype (tt ex)} 
evalstmt :: Statement -> Sign
evalstmt (BoundTo expr) = (val $ exprsrc expr,val $ exprtgt expr)
evalstmt _ = (NOthing,NOthing)

evalgen :: Gamma -> Concept -> Concept -> Concept
--results in I[Anything] being set to something concrete
evalgen _ c1 Anything = c1 
evalgen _ Anything c2 = c2
evalgen gamma c1 c2 = if elem (fromIsa (c1, c2)) gamma then c2
                      else if elem (fromIsa (c2, c1)) gamma then c1
                      else NOthing

evalspec :: Gamma -> Concept -> Concept -> Concept
evalspec gamma c1 c2 = if elem (fromIsa (c1, c2)) gamma then c1
                       else if elem (fromIsa (c2, c1)) gamma then c2
                       else NOthing

foldevalgen :: Gamma -> Concepts -> Concept
foldevalgen _ [] = Anything
foldevalgen gamma cs = foldr (evalgen gamma) NOthing cs

foldevalspec :: Gamma -> Concepts -> Concept
foldevalspec _ [] = NOthing
foldevalspec gamma cs = foldr (evalspec gamma) Anything cs

toGen :: ConceptTerm -> Concept
toGen (CTAS{as=x}) = x
toGen (CT{val=x}) = x

