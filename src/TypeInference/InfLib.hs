{-# OPTIONS_GHC #-}
module TypeInference.InfLib (typed, typedAs, ishomo, objects
                            ,adlinfertest) where
import Adl
import TypeInference.Input (isaRels)
--class a => Inferable t where
--  exprTypeIn:: t -> RelAlgExpr a
--class a => Inferred t where
--  exprTypeOut:: TypedExpr a -> t

--instance Inferable RelAlgExpr a where
  --exprTypeIn x = x
--instance Inferred TypedExpr a where
  --exprTypeOut x = x

typedAs :: (t1 -> RelAlgExpr a)->(TypedExpr a -> t2)-> ICtx a -> t1 ->  t2
typedAs exprTypeIn exprTypeOut cx expr = exprTypeOut$infer cx ((normalise.checknumbering)(exprTypeIn expr))

--TODO
checknumbering::RelAlgExpr a ->RelAlgExpr a 
checknumbering expr = expr

typed :: ICtx a -> RelAlgExpr a ->  TypedExpr a
typed = typedAs exprTypeIn exprTypeOut
   where
   exprTypeIn x = x 
   exprTypeOut x = x

--the heterogeneous relation algebra with a possibility to explicitly declare types on morphisms in an expression
--a is a data structure for storing language statement meta data like file position of a declaration
data RelAlgExpr a = 
   ISect {sublst::[RelAlgExpr a], rtype::RelAlgType}
  |Union {sublst::[RelAlgExpr a], rtype::RelAlgType}
  |Comp  {subs::(RelAlgExpr a,RelAlgExpr a), rtype::RelAlgType}
  |RAdd  {subs::(RelAlgExpr a,RelAlgExpr a), rtype::RelAlgType}
  |Compl {sub::RelAlgExpr a, rtype::RelAlgType}
  |Conv  {sub::RelAlgExpr a, rtype::RelAlgType}
  |Morph {rel::RelAlgMorph a, rtype::RelAlgType, usertype::RelAlgType, locid::Int}
  |Implic{subs::(RelAlgExpr a,RelAlgExpr a), rtype::RelAlgType}
  |Equiv {subs::(RelAlgExpr a,RelAlgExpr a), rtype::RelAlgType}
  deriving (Show)
data RelAlgMorph a = 
   DRel {rname::String, decl::[RelDecl a]}
  |IdRel
  |VRel
  deriving (Show)
data RelAlgObj = Universe | EmptyObject | Object String deriving (Show,Eq)
type RelAlgType = (RelAlgObj, RelAlgObj)
type NormExpr a = RelAlgExpr a
data TypedExpr a = Typed InvolvedConcepts (RelAlgExpr a)  | UndefinedExpr (InfError a) deriving (Show)
data InfError a = TypeError (ExprError a) [(RelAlgObj, RelAlgObj)]
                 |DeclNotFound [RelAlgMorph a]
                 |DeclAmbiguous [RelDecl a] 
                 |DeclNotHomo (RelAlgObj, RelAlgObj) deriving (Show)
data ExprError a = CompError (TypedExpr a,TypedExpr a)| ISectError [TypedExpr a] deriving (Show)
iscompl :: RelAlgExpr a -> Bool
iscompl (Compl{}) = True
iscompl (Conv{sub=(Compl{})}) = True
iscompl _ = False

--allegory objects
data OBJr a = OBJr {isas::[Isa a]} deriving (Show)
data MORr a = MORr {reldecls::[RelDecl a]} deriving (Show)
data ICtx a = ICtx {objr::(OBJr a), morr::(MORr a)} deriving (Show)
objects::ICtx a -> [RelAlgObj]
objects cx = 
  setlist$[fst$isarel x|x<-isas$objr cx]
        ++[snd$isarel x|x<-isas$objr cx]
        ++[fst$dtype d|d<-reldecls$morr cx]
        ++[snd$dtype d|d<-reldecls$morr cx]

setlist::(Eq a)=>[a]->[a]
setlist [] = []
setlist (x:xs) | elem x xs = setlist xs
               | otherwise = x:(setlist xs)

--Declarations by programmer
data Isa a = 
   IsaDecl {isarel::(RelAlgObj,RelAlgObj), ideclinfo::a}
  |IsaInf {isarel::(RelAlgObj,RelAlgObj)} 
  deriving (Show)
data RelDecl a = RelDecl {dname::String, dtype::RelAlgType, props::[DeclProp], ddeclinfo::a} deriving (Show)
data DeclProp = RFX | ASY | SYM | TRN deriving (Show)
homoprops = [RFX,ASY,SYM,TRN]
ishomo :: RelAlgExpr a -> Bool
ishomo expr@(ISect{}) = foldr (||) False [ishomo x|x<-sublst expr]
ishomo expr@(Union{}) = foldr (||) False [ishomo x|x<-sublst expr]
ishomo (Comp{subs=(x,y)}) = ishomo x && ishomo y
ishomo (RAdd{subs=(x,y)}) = ishomo x && ishomo y
ishomo expr@(Compl{}) = ishomo (sub expr)
ishomo expr@(Conv{}) = ishomo (sub expr)
ishomo (Implic{subs=(x,y)}) = ishomo x || ishomo y
ishomo (Equiv{subs=(x,y)}) = ishomo x || ishomo y
ishomo expr@(Morph {}) = ishomomph (rel expr)
ishomomph::RelAlgMorph a -> Bool
ishomomph m@(DRel{}) = ishomodecl (decl m)
ishomomph IdRel = True
ishomomph VRel = False
ishomodecl::[RelDecl a] -> Bool
ishomodecl [] = error "ishomodecl: No decl."
ishomodecl (d:[]) = not$null$props d --TODO

-------------------
normalise :: RelAlgExpr a -> NormExpr a
--rules
normalise (Implic{subs=(ant,cons)}) = normalise$ compl ant \/ cons
normalise (Equiv {subs=(ant,cons)}) = normalise(ant |- cons) /\ normalise(cons |- ant)
normalise (Compl {sub=(Implic{subs=(ant,cons)})}) = normalise$compl(compl ant \/ cons)
normalise (Compl {sub=(Equiv{subs=(ant,cons)})}) = normalise$compl((ant |- cons) /\ (cons |- ant))
--demorgan
normalise (Compl {sub=x@(ISect{sublst=xs})}) = x{sublst=foldISect$map (normalise.compl) xs}
normalise (Compl {sub=x@(Union{sublst=xs})}) = x{sublst=foldUnion$map (normalise.compl) xs}
normalise (Compl {sub=(Comp{subs=(x,y)})}) = normalise(compl x) ! normalise(compl y)
normalise (Compl {sub=(RAdd{subs=(x,y)})}) = normalise(compl x) *.* normalise(compl y)
--compl/conv
normalise (Compl {sub=(Conv{sub=x})}) = conv$normalise (compl x)
--double complement
normalise (Compl {sub=(Compl{sub=x})}) = normalise x
--recursion
normalise x@(ISect{sublst=xs}) = x{sublst=foldISect$map normalise xs}
normalise x@(Union{sublst=xs}) = x{sublst=foldUnion$map normalise xs}
normalise (Comp{subs=(x,y)}) = normalise x *.* normalise y
normalise (RAdd{subs=(x,y)}) = normalise x ! normalise y
normalise x@(Conv {sub=(Morph{})}) = x
normalise (Conv {sub=x}) = conv$normalise x
normalise x@(Compl {sub=(Morph{})}) = x
normalise x@(Morph{}) = x

(\/),(/\),(|-),(*.*),(!)::RelAlgExpr a -> RelAlgExpr a -> RelAlgExpr a
compl,conv::RelAlgExpr a -> RelAlgExpr a
(\/) x y = Union [x,y] (Universe,Universe)
(/\) x y = ISect [x,y] (Universe,Universe)
(|-) x y = Implic (x,y) (Universe,Universe)
-- (|-|) x y = Equiv (x,y) (Universe,Universe)
(*.*) x y = Comp (x,y) (Universe,Universe)
(!) x y = RAdd (x,y) (Universe,Universe)
compl x = Compl x (Universe,Universe)
conv x = Conv x (Universe,Universe)

foldISect::[RelAlgExpr a] -> [RelAlgExpr a]
foldISect [] = []
foldISect ((ISect{sublst=sxs}):xs) = sxs ++ foldISect xs
foldISect ((Conv{sub=(ISect{sublst=sxs})}):xs) = sxs ++ foldISect xs
foldISect (x:xs) = (x:foldISect xs)
foldUnion::[RelAlgExpr a] -> [RelAlgExpr a]
foldUnion [] = []
foldUnion ((Union{sublst=sxs}):xs) = sxs ++ foldUnion xs
foldUnion ((Conv{sub=(Union{sublst=sxs})}):xs) = sxs ++ foldUnion xs
foldUnion (x:xs) = (x:foldUnion xs)

--------------------------------------------------------
ramorphs::RelAlgExpr a -> [RelAlgExpr a]
ramorphs x = case x of
   ISect{} -> concat [ramorphs y|y<-sublst x]
   Union{} -> concat [ramorphs y|y<-sublst x]
   Comp{} ->  ramorphs(fst(subs x)) ++ ramorphs(snd(subs x))
   RAdd{} ->  ramorphs(fst(subs x)) ++ ramorphs(snd(subs x))
   Implic{} ->  ramorphs(fst(subs x)) ++ ramorphs(snd(subs x))
   Equiv{} ->  ramorphs(fst(subs x)) ++ ramorphs(snd(subs x))
   Compl{} ->  ramorphs(sub x)
   Conv{} ->  ramorphs(sub x)
   Morph{rel=DRel{}} -> [x]
   Morph{} -> []

infer2::ICtx a -> NormExpr a -> TypedExpr a
infer2 cx infx = 
  if null (nodecls$attachdecls infx) 
  then error "ok"
  else notdeclarederrors(nodecls$attachdecls infx)
  where
  --ISA
  (<<=) :: RelAlgObj -> RelAlgObj -> Bool
  (<<=) x y = elem (x,y) (map isarel$isas$objr cx)
  isarelated x y = (x <<= y) || (y <<= x)
  nodecls x = [y|y@(Morph{})<-ramorphs x, null(decl$rel y)]
  attachdecls x@(Morph{rel=m@(DRel{rname=mn})
                      ,usertype=(uc1,uc2)  }) 
    = x{rel=m{decl=alternatives}}
     where
     alternatives = [d | d@(RelDecl{dname=dn,dtype=(c1,c2)})<-(reldecls.morr) cx
                       , dn==mn
                       , if (uc1,uc2)==(Universe,Universe) then True else isarelated c1 uc1 && isarelated c2 uc2]
  attachdecls x@(Morph{}) = x
  attachdecls x@(Union{}) = x{sublst=[attachdecls x'|x'<-sublst x]}
  attachdecls x@(ISect{}) = x{sublst=[attachdecls x'|x'<-sublst x]}
  attachdecls x@(Comp{}) = x{subs=((attachdecls.fst)(subs x),(attachdecls.snd)(subs x))}
  attachdecls x@(RAdd{}) = x{subs=((attachdecls.fst)(subs x),(attachdecls.snd)(subs x))}
  attachdecls x@(Compl{}) = x{sub=attachdecls(sub x)}
  attachdecls x@(Conv{}) = x{sub=attachdecls(sub x)}
  attachdecls _ = error "not a normalized expression" 
  notdeclarederrors xs = UndefinedExpr (DeclNotFound [rel y|y@(Morph{})<-xs])
  inferdeep x

  onealternative xs = null [y|x<-xs,y@(Morph{})<-ramorphs x, length(decl$rel y)/=1]
  --start with infering morphisms and built up the expression again
  tryinfer xs | onealternative xs = onealtinfer x --just check, no ambiguity
              | otherwise = multaltinfer x --go deeper
  

--------------------------------------------------------
type InvolvedConcepts = ([RelAlgObj],[RelAlgObj])
--Expression types of iexp are reinferred and ingnored
--Only Morphisms can have explicit, user-defined types
--Format: 1) check whether expression is defined (over involved concepts <- linear movements only)
--           a) get the axioms (i.e. infer subexpressions) (errors in subexpression -> abort and push error up)
--           b) check the operator conditions from allegory theory (not defined -> compose error)
--        2) solve type dilemma with equalOn (over types of subexpressions and expression pattern)
--        3) maptype, respecting homogenity, the sources and targets on the original expression with inferred subexprs
--        -> involved concepts on source and target come from below (infer the subexpression/morphism -> declaration)
--        -> maptype adjusts the type of an isolated (inferred) subexpression (from above) taking into account homogenity
infer :: ICtx a -> NormExpr a -> TypedExpr a
infer cx iexp = case iexp of
   Comp{subs=(x,y)} -> tcomp (infer cx x) (infer cx y) (CComp$comptype (x,y))
   RAdd{subs=(x,y)} -> tcomp (infer cx x) (infer cx y) (CRAdd$comptype (x,y))
   ISect{sublst=xs} -> tintersect [infer cx x|x<-xs] (CISect)
   Union{sublst=xs} -> tintersect [infer cx x|x<-xs] (CUnion)
   Morph{} -> drelidv
   Compl{sub=x} -> tcompl (infer cx x)
   Conv{sub=x} -> tconv (infer cx x)
   _ -> error "expr?"
   where
   --ISA
   (<<=) :: RelAlgObj -> RelAlgObj -> Bool
   (<<=) x y = elem (x,y) (map isarel$isas$objr cx)
   isarelated x y = (x <<= y) || (y <<= x)
   --COMPOSITION
   tcomp tx@(Typed (cx,cx') x) ty@(Typed (cy,cy') y) patt =
      if definedcomp 
      then Typed cs$iexp{subs=(maptype (Just$fst x',Just$snd x') x
                              ,maptype (Just$fst y',Just$snd y') y)
                        ,rtype=(fst x',snd y')}
      else UndefinedExpr$TypeError (CompError (tx,ty)) [(cptx,cpty)
                                                     | cptx<-if ishomo x then setlist(cx'++cy) else cx'
                                                      ,cpty<-if ishomo y then setlist(cx'++cy) else cy
                                                      ,not$isarelated cptx cpty]
      where definedcomp =  (foldr (&&) True [isarelated cptx cpty
                                            | cptx<-if ishomo x then setlist(cx'++cy) else cx'
                                             ,cpty<-if ishomo y then setlist(cx'++cy) else cy])
            compOver = equalOn (patt (snd(rtype x),fst(rtype y)))
            x' = if ishomo x then (compOver,compOver) else (fst$rtype x,compOver)
            y' = if ishomo y then (compOver,compOver) else (compOver,snd$rtype y)
            cs = if ishomo x && ishomo y
                 then (setlist$cx++cy',setlist$cx++cy') 
                 else (if ishomo x then cx++cy else cx, if ishomo y then cx'++cy' else cy')
   tcomp x@(UndefinedExpr{}) _ _ = x
   tcomp _ y@(UndefinedExpr{}) _= y
   --INTERSECTION
   tintersect xs patt =
      if null undef
      then if definedisect
           then Typed cs$iexp{sublst=[maptype (Just isrc, Just itgt) x|Typed _ x<-xs]
                          ,rtype=(isrc,itgt)}
           else UndefinedExpr$TypeError (ISectError xs)
                     ([(x,y)| x<-c,y<-c,not$isarelated x y] ++ [(x,y)| x<-c',y<-c',not$isarelated x y])
      else head undef
      where
      undef = [x|x@(UndefinedExpr{})<-xs]
      definedisect = (foldr (&&) True [isarelated x y| x<-c,y<-c]) 
                   &&(foldr (&&) True [isarelated x y| x<-c',y<-c']) 
      cssrc = setlist [csrc|Typed (css,_) _<-xs, csrc<-css]
      cstgt = setlist [ctgt|Typed (_,css) _<-xs, ctgt<-css]
      cs@(c,c') = if homoexpr
           then (setlist$cssrc++cstgt,setlist$cssrc++cstgt) 
           else (cssrc, cstgt)
      homoexpr = foldr (||) False [ishomo x|Typed _ x<-xs]
      isrc = if homoexpr then equalOn (patt (srcs+<>+tgts)) else equalOn (patt srcs)
      itgt = if homoexpr then equalOn (patt (srcs+<>+tgts)) else equalOn (patt tgts)
      (+<>+) (x,y) (x',y') = (setlist$x++y,setlist$x'++y')
      srcs = ([fst$rtype x|Typed _ x<-xs, not$iscompl x],[fst$rtype x|Typed _ x<-xs, iscompl x])
      tgts = ([snd$rtype x|Typed _ x<-xs, not$iscompl x],[snd$rtype x|Typed _ x<-xs, iscompl x])
   --CONVERSION
   tconv (Typed (c,c') x) = Typed (c',c)$iexp{sub=x,rtype=(snd$rtype x,fst$rtype x)}
   tconv x = x
   --COMPLEMENT
   tcompl (Typed cs x@(Morph{})) = Typed cs$iexp{sub=x,rtype=(fst$rtype x,snd$rtype x)}
   tcompl (Typed _ _) = error "Complements can only be on morphisms -> normalise."
   tcompl x = x   
   --MORPHISMS
   drelidv = case rel iexp of
      DRel{} -> drel
      IdRel{} -> did
      VRel{} -> dv
      where
      did = Typed cs$iexp{rtype=usertype iexp}
            where cs = ([],[]) --todo -> mphats
      dv = Typed cs$iexp{rtype=usertype iexp}
           where cs = ([],[]) --todo -> mphats
      ds = [mor|mor<-reldecls$morr cx, dname mor==(rname$rel iexp)
                ,isarelated (fst$dtype mor) (fst$usertype iexp)
                ,isarelated (snd$dtype mor) (snd$usertype iexp)]
      drel = if length ds==1 
             then if ishomodecl[d'] && (fst$dtype d')/=(snd$dtype d') 
                  then UndefinedExpr$DeclNotHomo (fst$dtype d',snd$dtype d')
                  else Typed ([fst$dtype d'],[snd$dtype d'])$iexp{rtype=dtype d',rel=(rel iexp){decl=[d']}}
             else if null ds 
                  then UndefinedExpr$DeclNotFound [rel iexp]
                  else UndefinedExpr$DeclAmbiguous ds
             where d' = head ds
   --EQUALON
   equalOn :: ExprCtx -> RelAlgObj
   equalOn exprctx = case exprctx of
      CComp tp (x,y) -> case tp of
         CPlain -> big ispec$remuniverse [x,y]
         CInv1 -> y `ifuniverse` x
         CInv2 -> x `ifuniverse` y
         CInvs -> big igen$remuniverse [x,y]
      CRAdd tp (x,y) -> case tp of
         CPlain -> big igen$remuniverse [x,y]
         CInv1 -> x `ifuniverse` y
         CInv2 -> y `ifuniverse` x
         CInvs -> big ispec$remuniverse [x,y]
      CUnion (xs,cxs) -> big igen$remuniverse$setlist$xs++cxs
      CISect (xs,cxs) -> if null cxs
                         then big ispec$remuniverse xs --Plain
                         else if null xs
                              then big igen$remuniverse xs --Invs
                              else big ispec$remuniverse [equalOn$CISect ([],cxs)
                                                         ,equalOn$CISect (xs,[])]
   remuniverse :: [RelAlgObj] -> [RelAlgObj]
   remuniverse xs = [x|x<-xs,x/=Universe]
   ifuniverse x y = if x==Universe then y else x
   ispec :: RelAlgObj -> RelAlgObj -> RelAlgObj
   ispec x y | x `isarelated` y = x
             | y `isarelated` x = y
             | otherwise = error "ispec: x and y not IS-a related,"
   igen :: RelAlgObj -> RelAlgObj -> RelAlgObj
   igen x y  | x `isarelated` y = y
             | y `isarelated` x = x
             | otherwise = error "igen: x and y not IS-a related,"
   big :: (RelAlgObj -> RelAlgObj -> RelAlgObj) -> [RelAlgObj] -> RelAlgObj
   big _ [] = Universe
   big _ (x:[]) = x
   big genorspec xs = foldr genorspec (head xs) xs
   

--DESCR -> maptype maps a type down to inferred subexpressions/morphisms
--TODO  -> maybe it should be availabe for function infer only
maptype :: (Maybe RelAlgObj,Maybe RelAlgObj) -> NormExpr a -> NormExpr a
maptype (Nothing, Nothing) expr = expr
maptype tp@(src,tgt) expr = case expr of
   Comp{subs=(x,y)} -> maptypecomp x y
   RAdd{subs=(x,y)} -> maptypecomp x y
   ISect{sublst=xs} -> maptypeisect xs
   Union{sublst=xs} -> maptypeisect xs
   Morph{} -> expr' tp 
   Compl{sub=x} -> (expr' tp){sub=maptype tp x}
   Conv{sub=x} -> (expr' tp){sub=maptype (tgt,src) x}
   Implic{subs=(x,y)} -> (expr' tp){subs=(maptype tp x,maptype tp y)}
   Equiv {subs=(x,y)} -> (expr' tp){subs=(maptype tp x,maptype tp y)}
   where
   expr' (Nothing,Nothing) = expr
   expr' (Just src',Nothing)
     | ishomo expr = error "Cannot only map the source to a homogeneous relation." 
     | otherwise = expr{rtype=(src',snd$rtype expr)}
   expr' (Nothing,Just tgt')
     | ishomo expr = error "Cannot only map the target to a homogeneous relation." 
     | otherwise = expr{rtype=(fst$rtype expr,tgt')}
   expr' (Just src',Just tgt')
     | ishomo expr && src'/=tgt' = error "Cannot map different source and target to a homogeneous relation." 
     | otherwise = expr{rtype=(src',tgt')}
   --In case of homo, maptype source and target!
   maptypecomp x y = (expr' tp){subs=(if ishomo x then maptype (src,src) x else maptype (src,Nothing) x
                                     ,if ishomo y then maptype (tgt,tgt) y else maptype (Nothing,tgt) y)}
   maptypeisect xs = (expr' tp){sublst=[maptype (src,tgt) x|x<-xs]}

data CompType = CPlain | CInv1 | CInv2 | CInvs deriving (Show)
comptype :: (NormExpr a, NormExpr a) -> CompType
comptype (Compl{},Compl{}) = CInvs
comptype (Compl{},_) = CInv1
comptype (_,Compl{}) = CInv2
comptype _ = CPlain
data ISectType =  IPlain | IInvs | IComb deriving (Show)
isecttype :: [NormExpr a] -> ISectType
isecttype xs = if null [x|x<-xs, iscompl x]
               then IPlain
               else if null [x|x<-xs, not$iscompl x]
                    then IInvs
                    else IComb
data ExprCtx = 
   CComp CompType (RelAlgObj,RelAlgObj)
  |CRAdd CompType (RelAlgObj,RelAlgObj)
  |CISect ([RelAlgObj],[RelAlgObj])
  |CUnion ([RelAlgObj],[RelAlgObj])
   deriving (Show)

--------------------


printexpr :: RelAlgExpr a -> String
printexpr expr =  case expr of
   Comp{subs=(x,y)} -> printexpr x++";"++printexpr y ++ "{" ++ printobj(fst$rtype expr)++"*"++ printobj(snd$rtype expr) ++"}"
   RAdd{subs=(x,y)} -> printexpr x++"!"++printexpr y ++ "{" ++ printobj(fst$rtype expr)++"*"++ printobj(snd$rtype expr) ++"}"
   ISect{sublst=xs} -> "ISect"++show [printexpr x|x<-xs] ++"{"++printobj(fst$rtype expr)++"*"++printobj(snd$rtype expr)++ "}"
   Union{sublst=xs} -> "Union"++show [printexpr x|x<-xs] ++"{"++printobj(fst$rtype expr)++"*"++printobj(snd$rtype expr)++ "}"
   Morph{} -> printmph(rel expr) ++ "[" ++ printobj(fst$rtype expr)++"*"++ printobj(snd$rtype expr) ++ "]" 
   Compl{sub=x} -> "-("++printexpr x++")"
   Conv{sub=x} -> "("++printexpr x++")~"
   Implic{subs=(x,y)} -> printexpr x ++ " |- " ++ printexpr y
   Equiv {subs=(x,y)} -> printexpr x ++ " = " ++ printexpr y
printtyped::(Show a)=> TypedExpr a -> String
printtyped (Typed cs expr) = "involved concepts"++show cs++"\n"++printexpr expr
printtyped texpr = show texpr

printobj :: RelAlgObj -> String
printobj (Object x) = x
printobj Universe = "ALL"
printobj EmptyObject = "0"

printmph :: RelAlgMorph a -> String
printmph x@(DRel{}) = rname x
printmph IdRel = "I"
printmph VRel = "V"

-------------------------------------
adlinfertest :: Declarations -> Concepts ->  Gens -> Expression ->  (Expression,String)
adlinfertest ds cs gs = typedAs exprTypeIn exprTypeOut ictx
   where
   exprTypeIn::Expression -> RelAlgExpr a
   exprTypeIn x = fst (uniqueMphsE 0 x) 
   exprTypeOut::(Show a)=>TypedExpr a -> (Expression,String)
   exprTypeOut x = error$printtyped x
   ictx = ICtx{objr=OBJr{isas=fromCptCpts (isaRels cs  gs)}
              ,morr=MORr{reldecls=[fromDcl d|d@(Sgn{})<-ds]}
              }

fromCptCpts :: [(Concept,Concept)] -> [Isa a]
fromCptCpts xs = [IsaInf{isarel=(fromCpt c1,fromCpt c2)} |(c1,c2)<-xs]
fromCpt :: Concept -> RelAlgObj
fromCpt Anything = Universe
fromCpt NOthing = EmptyObject
fromCpt c1 = Object (name c1)
fromDcl :: Declaration -> RelDecl String
fromDcl d@(Sgn{}) = RelDecl {dname=name d
                            ,dtype=(fromCpt$source d,fromCpt$target d)
                            ,props=[] --TODO
                            ,ddeclinfo=""  --TODO
                            }
fromDcl _ = error "only relvars"
   
fromMphats :: Concepts -> RelAlgType
fromMphats [] = (Universe,Universe)
fromMphats [c1] = (fromCpt c1,fromCpt c1)
fromMphats [c1,c2] = (fromCpt c1,fromCpt c2)
fromMphats _ = error "too many mphats"
 
--REMARK -> there will never be a Flip, because it is parsed flippedwise. The Flip is still implemented for other parse trees than the current ADL parse tree.
uniqueMphsE :: Int -> Expression -> (RelAlgExpr a,Int)
uniqueMphsE i (Tm mp@(Mph{mphyin=False})) = 
   (Conv{rtype=(Universe,Universe)
        ,sub=Morph{rel=morph
                  ,usertype=(\(x,y)->(y,x))$fromMphats (mphats mp)
                  ,locid=(i+1)
                  ,rtype=(Universe,Universe)}
        },i+1)
   --(Flip (Relation mp (i+1) (fromSign $ sign mp)) unknowntype,i+1)
   where morph = DRel{rname=name mp, decl=[]}
uniqueMphsE i (Tm mp@(Mph{mphyin=True})) = 
   (Morph{rel=morph
         ,usertype=fromMphats (mphats mp)
         ,locid=(i+1)
         ,rtype=(Universe,Universe)
         },i+1)
   --(Relation mp (i+1) (fromSign $ sign mp),i+1)
   where morph = DRel{rname=name mp, decl=[]}
--REMARK -> implementation of sign of I{} is not the implementation needed.
uniqueMphsE i (Tm mp@(I{})) = 
   (Morph{rel=morph
         ,usertype=fromMphats (mphats mp)
         ,locid=(i+1)
         ,rtype=(Universe,Universe)
         },i+1)
   --(Relation mp (i+1) (fromSign $ (mphspc mp, mphspc mp)),i+1)
   where morph = IdRel
uniqueMphsE i (Tm mp@(V{})) = 
   (Morph{rel=morph
         ,usertype=fromMphats (mphats mp)
         ,locid=(i+1)
         ,rtype=(Universe,Universe)
         },i+1)
   --(Relation mp (i+1) (fromSign $ sign mp),i+1)
   where morph = VRel
uniqueMphsE i (Tm mp@(Mp1{})) = 
   (Morph{rel=morph
         ,usertype=fromMphats (mphats mp)
         ,locid=(i+1)
         ,rtype=(Universe,Universe)
         },i+1)
   --(Relation mp (i+1) (fromSign $ (mph1typ mp,mph1typ mp)),i+1)
   where morph = IdRel
uniqueMphsE _ (F []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (F [])++"." 
uniqueMphsE i (F (ex:rexs)) = 
   (Comp{subs=(fst lft,fst rght)
        ,rtype=(Universe,Universe)
        },snd rght)
   --(Semicolon (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (F rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE _ (Fd []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fd [])++"." 
uniqueMphsE i (Fd (ex:rexs)) = 
   (RAdd{subs=(fst lft,fst rght)
        ,rtype=(Universe,Universe)
        },snd rght)
   --(Dagger (fst lft) (fst rght) unknowntype, snd rght)
   where
   lft = uniqueMphsE i ex
   rght = case rexs of
     rex:[] -> uniqueMphsE (snd lft) rex
     _:_    -> uniqueMphsE (snd lft) (Fd rexs)
     []     -> uniqueMphsE i ex
uniqueMphsE _ (Fi []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fi [])++"." 
uniqueMphsE i (Fi (ex:rexs)) = case rexs of
     rex:[] -> let (mphrex,irex) = uniqueMphsE ilft rex
               in (ISect{sublst=[mphlft,mphrex]
                        ,rtype=(Universe,Universe)
                        }, irex)
               --(Intersect [mphlft,mphrex] unknowntype, irex)
     _:_    -> (ISect{sublst=(mphlft:mphrexs)
                     ,rtype=(Universe,Universe)
                     }, irexs)
               --(Intersect (mphlft:mphrexs) unknowntype, irexs)
     []     -> (ISect{sublst=[mphlft]
                     ,rtype=(Universe,Universe)
                     }, ilft)
               --(Intersect [mphlft] unknowntype, ilft)
   where
   (mphlft,ilft) = uniqueMphsE i ex
   (ISect{sublst=mphrexs}, irexs) =uniqueMphsE ilft (Fi rexs)
   --(Intersect mphrexs _, irexs) =uniqueMphsE ilft (Fi rexs)
uniqueMphsE _ (Fu []) = error $ "Error in AdlExpr.hs module TypeInference.AdlExpr function uniqueMphsE: " ++
                               "Expression has no sub expressions"++show (Fu [])++"." 
uniqueMphsE i (Fu (ex:rexs)) = case rexs of
     rex:[] -> let (mphrex,irex) = uniqueMphsE ilft rex
               in (Union{sublst=[mphlft,mphrex]
                        ,rtype=(Universe,Universe)
                        }, irex)
               --(Union [mphlft,mphrex] unknowntype, irex)
     _:_    -> (Union{sublst=(mphlft:mphrexs)
                     ,rtype=(Universe,Universe)
                     }, irexs)
               --(Union (mphlft:mphrexs) unknowntype, irexs)
     []     -> (Union{sublst=[mphlft]
                     ,rtype=(Universe,Universe)
                     }, ilft)
               --(Union [mphlft] unknowntype, ilft)
   where
   (mphlft,ilft) = uniqueMphsE i ex
   (Union{sublst=mphrexs}, irexs) =uniqueMphsE ilft (Fu rexs)
   --(Union mphrexs _, irexs) =uniqueMphsE ilft (Fu rexs)
uniqueMphsE i (Cp ex) = 
   (Compl{sub=fst sb
         ,rtype=(Universe,Universe)
         }, snd sb)
   --(Complement (fst sb) unknowntype, snd sb)
   where
   sb = uniqueMphsE i ex
uniqueMphsE i (Tc ex) = uniqueMphsE i ex
uniqueMphsE i (K0 ex) = uniqueMphsE i ex
uniqueMphsE i (K1 ex) = uniqueMphsE i ex


