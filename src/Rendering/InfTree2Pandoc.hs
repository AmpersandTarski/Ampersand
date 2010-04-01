module Rendering.InfTree2Pandoc where
import Text.Pandoc 
import Rendering.PandocAux
import TypeInference.InfLibAG (InfTree(..),InfRuleType(..),DeclRuleType(..),RelDecl(..))

{-
data InfTree = InfExprs InfRuleType [InfTree] | InfRel DeclRuleType RelDecl
               deriving (Show,Eq)
data DeclRuleType = D_rel|D_rel_h|D_rel_c|D_rel_c_h|D_id|D_v|D_id_c|D_v_c
                    deriving (Show,Eq)
data InfRuleType = ISect_cs | ISect_ncs | ISect_mix
                  |Union_mix
                  |Comp_ncs | Comp_c1 | Comp_c2 | Comp_cs
                  |RAdd_ncs | RAdd_c1 | RAdd_c2 | RAdd_cs 
                  |Conv_nc | Conv_c
                   deriving (Show,Eq)
-}

pandoctree :: Maybe InfTree -> Block
pandoctree Nothing = Plain [Str "No inference tree has been calculated."]
pandoctree (Just tr) = Plain$[TeX ("Expression: $"++term++"$\n"), TeX "\n\\begin{prooftree}\n"]++il ++[TeX "\\end{prooftree}\n"] ++ refs
   where (il,term,refs) = pandoctree' tr


pandoctree_ref :: (String,InfTree) -> [Inline]
pandoctree_ref (lbl,tr) = [TeX ("Premise: $"++lbl++"$\n"), TeX "\\begin{prooftree}\n"]++il ++[TeX "\\end{prooftree}\n"]
   where (il,_,_) = pandoctree' tr

pandoctree' :: InfTree -> ([Inline],String,[Inline])
pandoctree' (InfRel dt (c1,c2) r i)
 |elem dt [D_id,D_v,D_id_c,D_v_c] = 
    ([TeX ("\\AxiomC{$ $}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}\n")
     ,TeX ("\\UnaryInfC{$"++r'++"::"++tp++" \\in \\Gamma$}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dtrel++")}\n")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++term++"["++tp++"]$}\n")
     ],term,[])
 |otherwise =
    ([TeX ("\\AxiomC{$"++r'++"::"++tp++" \\in \\Gamma$}\n")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}\n")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++term ++"["++tp++"]$}\n")
     ],term,[])
   where 
   r' =  show i --TODO: dit zou een placeholder voor een format string kunnen worden. i=unieke identificatie van mph in expressie
   tp = if elem dt [D_rel_c_h,D_rel_h,D_id,D_id_c] then show c1 else show c1++"*"++ show c2
   term = if elem dt [D_rel_c_h,D_rel_c,D_id_c,D_v_c] then "\\overline{"++r'++"}" else r'
   dtrel = case dt of D_id -> D_rel_h;  D_id_c -> D_rel_c_h; D_v -> D_rel;  D_v_c -> D_rel_c; _->dt;
pandoctree' (InfExprs rt ((c1,c2),cb) axs)
   |elem rt [Conv_nc, Conv_c] && length axs==1 = 
       let
       (il,ax,axrefs) =  pandoctree' (head axs)
       term = if rt==Conv_c then "\\overline{"++(br 0 (head axs) ax)++"^\\smile}" else (br 0 (head axs) ax)++"^\\smile"
       in
       ( il++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\UnaryInfC{$\\Gamma \\models " ++ term  ++ "["++tp++"]$}\n")
        ],term,axrefs)
   |elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs, RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs,ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs==2 = 
       let
       (il1,ax1,ax1refs) =  pandoctree' (head axs)
       (il2,ax2,ax2refs) =  pandoctree' (head (tail axs))
       term | elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs] = (br 1 (head axs) ax1) ++ ";_{"++show cb++ "}" ++ (br 1 (head (tail axs)) ax2)
            | elem rt [RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] = (br 2 (head axs) ax1) ++ "\\dagger_{"++show cb++ "}" ++ (br 2 (head (tail axs)) ax2)
            | elem rt [ISect_cs, ISect_ncs, ISect_mix] = (br 3 (head axs) ax1) ++ "\\cap" ++ (br 3 (head (tail axs)) ax2)
            | otherwise = ax1 ++ "\\cup" ++ ax2 
       in
       (il1++il2++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\BinaryInfC{$\\Gamma \\models " ++ term ++ "["++tp++"]$}\n")
        ],term,ax1refs++ax2refs)
   |elem rt [ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs==3 = 
       let
       (il1,ax1,ax1refs) =  pandoctree' (head axs)
       (il2,ax2,ax2refs) =  pandoctree' (head (tail axs))
       (il3,ax3,ax3refs) =  pandoctree' (head (tail(tail axs)))
       term | elem rt [ISect_cs, ISect_ncs, ISect_mix] = (br 3 (head axs) ax1) ++ "\\cap" ++ (br 3 (head (tail axs)) ax2)++ "\\cap" ++ (br 3 (head (tail(tail axs))) ax3)
            | otherwise = ax1 ++ "\\cup" ++ax2 ++ "\\cup" ++ax3
       in
       (il1++il2++il3++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\TrinaryInfC{$\\Gamma \\models " ++ term ++ "["++tp++"]$}\n")
        ],term,ax1refs++ax2refs++ax3refs)
   |elem rt [ISect_cs, ISect_ncs, ISect_mix, Union_mix] && length axs>3 =
       let
       iln = map pandoctree_ref (zip axn' axs) --use the term as a reference label
       axrefs = [ref |(_,_,ref)<-map pandoctree' axs]
       axn = [ax |(_,ax,_)<-map pandoctree' axs]
       axn' = [ax++ "["++tp++"]"|(_,ax,_)<-map pandoctree' axs]
       op = if rt==Union_mix then "\\bigcup" else "\\bigcap"
       term = op++" \\{" ++ head axn ++ concat [", "++ ax |ax<-tail axn] ++ "\\}"
       axlist = " \\{" ++ head axn' ++ concat [", "++ ax|ax<-tail axn'] ++ "\\}"
       in
       ([TeX ("\\AxiomC{$\\Gamma \\models" ++ axlist ++"$}\n")
        ,TeX ("\\RightLabel{\\scriptsize("++show rt++")}\n")
        ,TeX ("\\UnaryInfC{$\\Gamma \\models " ++ term  ++ "["++tp++"]$}\n")
        ] 
       ,term,concat (iln++axrefs))
--   |elem rt [Union_mix]
  --    = (concat (map (fst.pandoctree') axs),"") --TODO
   |otherwise=error "TODO 90"
  where 
  tp = show c1++"*"++ show c2

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



testdoc :: [Block] -> String
testdoc b = writeLaTeX defaultWriterOptions (Pandoc (Meta [] [] []) b)
