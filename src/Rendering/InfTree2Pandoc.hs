module Rendering.InfTree2Pandoc where
import Text.Pandoc 
import TypeInference.InfLibAG (InfTree(..),InfRuleType(..),DeclRuleType(..))
import Data.Fspec   hiding (services)
import Adl
import Auxiliaries (sort')

--a document with proofs for the fspec
proofdoc :: Fspc -> Pandoc
proofdoc fSpec = Pandoc (Meta [] [] []) b
   where
   b = concat$
       [pandoctree(rrtyp_proof r) Nothing|r<-rules fSpec]
     ++[pandoctree(objctx_proof x) Nothing|od<-obs,x<-obs' od]
   obs = serviceS fSpec ++ concat [kdats k|k<-vkeys fSpec]
   obs' od = concat [(oda:obs' oda)|oda<-objats od]

--writes an inference tree as a piece of latex
pandoctree :: Maybe (InfTree,Expression) -> Maybe (Concept,Concept) -> [Block]
pandoctree Nothing _ = [Plain [Str "No inference tree has been calculated."]]
pandoctree (Just (tr,x)) jt = orig++[Plain$[TeX ("Normalized expression: $"++term++"$\n"), TeX "\n\\begin{prooftree}\n"]++il ++[TeX "\\end{prooftree}\n"] ++ refs]
   where 
   (il,term,refs,_,_) = pandoctree' tr
   env :: Expression -> [(Int,(Declaration,[Concept]))]
   env (Tm mp i) = [(i, (head(decls mp),mphats mp))]
   env (F xs) = concat(map env xs)
   env (Fd xs) = concat(map env xs)
   env (Fi xs) = concat(map env xs)
   env (Fu xs) = concat(map env xs)
   env (Cp ex) = env ex
   env (Tc ex) = env ex
   env (K0 ex) = env ex
   env (K1 ex) = env ex 
   writedecl d usrtype = case d of
           Sgn{} -> (if null usrtype then name d else "("++name d ++show usrtype++")")
                    ++ "::" ++ show(source d) ++"*"++ show(target d) ++" at "++ show (decfpos d)
           _ -> if null usrtype then name d else name d ++show usrtype
   (s,t) = case jt of {Just tp -> tp; _ -> (Anything,Anything)}
   orig = [Plain$
             [TeX ("\\paragraph{Expression: $"++writeexpr x++"$}\n"++[c|c<-"["++show s++"*"++show t++"]",s/=Anything,t/=Anything])
             ,TeX "\\begin{enumerate}\n"]
           ++[TeX ("\\item["++show i++" =]"++writedecl d usrtype++"\n")|(i,(d,usrtype))<-sort' fst (env x)]
           ++[TeX "\\end{enumerate}\n"]
          ]
   writeexpr expr = showExpr (" \\cup ", " \\cap ", " \\dagger ", ";", "*", "+", "-", "(", ")") expr
      where
      showExpr (union',inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr' = showchar (insParentheses expr')
         where
         showchar (Tm mph _)  = name mph++if inline mph then "" else "^{\\smile}"
         showchar (Fu []) = "-V"
         showchar (Fu fs) = chain' union' fs
         showchar (Fi []) = "V"
         showchar (Fi fs) = chain' inter fs
         showchar (Fd []) = "-I"
         showchar (Fd ts) = chain' rAdd ts
         showchar (F [])  = "I"
         showchar (F ts)  = chain' rMul ts
         showchar (K0 e') = showchar e'++clos0
         showchar (K1 e') = showchar e'++clos1
         showchar (Cp e') = "\\overline{"++showchar e'++"}"
         showchar (Tc f)  = lpar++showchar f++rpar
         chain' x' xs = head (map showchar xs) ++ concat [x' ++ f|f<-tail (map showchar xs)]

--writes a subtree as a separate tree i.e. the proof of a premise of the parent tree
pandoctree_ref :: (String,InfTree) -> [Inline]
pandoctree_ref (lbl,tr) = [TeX ("Premise: $"++lbl++"$\n"), TeX "\\begin{prooftree}\n"]++il ++[TeX "\\end{prooftree}\n"]
   where (il,_,_,_,_) = pandoctree' tr

--maxwidth MUST be at least 3
maxwidth::Int 
maxwidth=3
--returns (latextree::[Inline]
--        ,term in the conclusion of latextree::String
--        ,all the referenced trees (with own \begin and \end{prooftree}), which will be printed as such
--        ,the width of the latextree (make from the premises a reference when width will exceed maxwidth)
pandoctree' :: InfTree -> ([Inline],String,[Inline],Int,String)
pandoctree' (InfRel dt (c1,c2) _ i)
 |elem dt [D_id,D_v,D_id_c,D_v_c] = 
    ([TeX ("\\AxiomC{$ $}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}\n")
     ,TeX ("\\UnaryInfC{$"++r'++"::"++tp++" \\in \\Gamma$}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dtrel++")}\n")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++term++"["++tp++"]$}\n")
     ],term,[],1,tp)
 |otherwise =
    ([TeX ("\\AxiomC{$"++r'++"::"++tp++" \\in \\Gamma$}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}\n")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++term ++"["++tp++"]$}\n")
     ],term,[],1,tp)
   where 
   r' =  show i --TODO: dit zou een placeholder voor een format string kunnen worden. i=unieke identificatie van mph in expressie
   tp = ltxstr(if elem dt [D_rel_c_h,D_rel_h,D_id,D_id_c] then show c1 else show c1++"*"++ show c2)
   term = if elem dt [D_rel_c_h,D_rel_c,D_id_c,D_v_c] then "\\overline{"++r'++"}" else r'
   dtrel = case dt of D_id -> D_rel_h;  D_id_c -> D_rel_c_h; D_v -> D_rel;  D_v_c -> D_rel_c; _->dt;
pandoctree' (InfExprs rt ((c1,c2),cb) axs)
   |elem rt [Conv_nc, Conv_c] && length axs==1 = 
       let
       (il,ax,axrefs,width,_) =  pandoctree' (head axs)
       term = if rt==Conv_c then "\\overline{"++(br 0 (head axs) ax)++"^\\smile}" else (br 0 (head axs) ax)++"^\\smile"
       in
       ( il++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\UnaryInfC{$\\Gamma \\models " ++ term  ++ "["++tp++"]$}\n")
        ],term,axrefs,width,tp)
   |elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs, RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs,ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs==2 = 
       let
       (il1,ax1,ax1refs,width1,axtp1) =  pandoctree' (head axs)
       (il2,ax2,ax2refs,width2,axtp2) =  pandoctree' (head (tail axs))
       term | elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs] = (br 1 (head axs) ax1) ++ ";_{"++ltxstr(show cb)++ "}" ++ (br 1 (head (tail axs)) ax2)
            | elem rt [RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] = (br 2 (head axs) ax1) ++ "\\dagger_{"++ltxstr(show cb)++ "}" ++ (br 2 (head (tail axs)) ax2)
            | elem rt [ISect_cs, ISect_ncs, ISect_mix] = (br 3 (head axs) ax1) ++ "\\cap" ++ (br 3 (head (tail axs)) ax2)
            | otherwise = ax1 ++ "\\cup" ++ ax2 
       split = (width1+width2)>maxwidth
       split1 = split && width1==maxwidth --if the left tree has room for one branche (the reference of the right branche), then do not reference the left
       split2 = split && (not split1 || width2==maxwidth) --if the left didn't have room, then maybe the right does.
       ref1 = if split1 then pandoctree_ref (ax1++"["++axtp1++"]",head axs) else []
       ref2 = if split2 then pandoctree_ref (ax2++"["++axtp2++"]",head (tail axs)) else []
       width' = (if split1 then 1 else width1) + (if split2 then 1 else width2)
       il1' = if split1 then [TeX ("\\AxiomC{$\\Gamma \\models" ++ ax1++"["++tp1++"]$}\n")] else il1
       il2' = if split2 then [TeX ("\\AxiomC{$\\Gamma \\models" ++ ax2++"["++tp2++"]$}\n")] else il2
       tp1 = if elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs, RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] then ltxstr(show c1++"*"++ show cb) else tp
       tp2 = if elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs, RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] then ltxstr(show cb++"*"++ show c2) else tp
       in
       (il1'++il2'++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\BinaryInfC{$\\Gamma \\models " ++ term ++ "["++tp++"]$}\n")
        ],term,ref1++ref2++ax1refs++ax2refs,width',tp)
   |elem rt [ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs==3 = 
       let
       (il1,ax1,ax1refs,width1,axtp1) =  pandoctree' (head axs)
       (il2,ax2,ax2refs,width2,axtp2) =  pandoctree' (head (tail axs))
       (il3,ax3,ax3refs,width3,axtp3) =  pandoctree' (head (tail(tail axs)))
       term | elem rt [ISect_cs, ISect_ncs, ISect_mix] = (br 3 (head axs) ax1) ++ "\\cap" ++ (br 3 (head (tail axs)) ax2)++ "\\cap" ++ (br 3 (head (tail(tail axs))) ax3)
            | otherwise = ax1 ++ "\\cup" ++ax2 ++ "\\cup" ++ax3
       split = (width1+width2+width3)>maxwidth
       split1 = split && width1==maxwidth --if the left tree has room for one branche (the reference of the right branche), then do not reference the left
       split2 = split && (not split1 || width2==maxwidth) --if the left didn't have room, then maybe the middle does.
       split3 = split && ((not split1 && not split2) || width3==maxwidth) --if the left and middle didn't have room, then maybe the right does.
       ref1 = if split1 then pandoctree_ref (ax1++"["++axtp1++"]",head axs) else []
       ref2 = if split2 then pandoctree_ref (ax2++"["++axtp2++"]",head (tail axs)) else []
       ref3 = if split3 then pandoctree_ref (ax3++"["++axtp3++"]",head (tail (tail axs))) else []
       width' = (if split1 then 1 else width1) + (if split2 then 1 else width2) + (if split3 then 1 else width3)
       il1' = if split1 then [TeX ("\\AxiomC{$\\Gamma \\models" ++ ax1++"["++tp++"]$}\n")] else il1
       il2' = if split2 then [TeX ("\\AxiomC{$\\Gamma \\models" ++ ax2++"["++tp++"]$}\n")] else il2
       il3' = if split3 then [TeX ("\\AxiomC{$\\Gamma \\models" ++ ax3++"["++tp++"]$}\n")] else il3
       in
       (il1'++il2'++il3'++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\TrinaryInfC{$\\Gamma \\models " ++ term ++ "["++tp++"]$}\n")
        ],term,ref1++ref2++ref3++ax1refs++ax2refs++ax3refs,width',tp)
   |elem rt [ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs>3 = --LaTeX prooftree has max 3 axioms, so always split up this tree
       let
       iln = map pandoctree_ref (zip axn' axs) --use the term as a reference label
       axrefs = [ref |(_,_,ref,_,_)<-map pandoctree' axs]
       axn = [ax |(_,ax,_,_,_)<-map pandoctree' axs]
       axn' = [ax++ "["++axtp++"]"|(_,ax,_,_,axtp)<-map pandoctree' axs]
       op = if rt==Union_mix then "\\bigcup" else "\\bigcap"
       term = op++" \\{" ++ head axn ++ concat [", "++ ax |ax<-tail axn] ++ "\\}"
       axlist = " \\{" ++ head axn' ++ concat [", "++ ax|ax<-tail axn'] ++ "\\}"
       in
       ([TeX ("\\AxiomC{$\\Gamma \\models" ++ axlist ++"$}\n")
        ,TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\UnaryInfC{$\\Gamma \\models " ++ term  ++ "["++tp++"]$}\n")
        ] 
       ,term,concat (iln++axrefs),1,tp)
--   |elem rt [Union_mix]
  --    = (concat (map (fst.pandoctree') axs),"") --TODO
   |otherwise=error "TODO 90"
  where 
  tp = ltxstr(show c1++"*"++ show c2)

--inflib translates ONE to a concept with name #S#
ltxstr :: String -> String
ltxstr s = [c|c<-s,c/='#']

--functions to put brackets in the right places
br :: Integer -> InfTree -> String -> String
br i ax ax' = if i<(prio ax) then "("++ax'++")" else ax' 
prio::InfTree->Integer
prio (InfExprs rt _ _)
  | elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs] = 1
  | elem rt [RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] = 2
  | elem rt [ISect_cs, ISect_ncs, ISect_mix] = 3
  | elem rt [Union_mix] = 4
  | otherwise = 0
prio _ = 0
