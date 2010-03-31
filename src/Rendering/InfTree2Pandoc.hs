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

--TODO -> misschien moet ik iets toevoegen als een maximale breedte, als breedte overschreden dan in aparte boom met label en verwijziging naar label
--   refax1   refax2
--   --------------- rule
--        concl
--
--(refax1)
--         tree
--        ------rule
--         ax1
--(refax2)
--         tree
--        ------rule
--         ax2
pandoctree :: Maybe InfTree -> Block
pandoctree Nothing = Plain [Str "No inference tree has been calculated."]
pandoctree (Just tr) = Plain$[TeX "\\begin{prooftree}"]++fst(pandoctree' tr) ++[TeX "\\end{prooftree}"]


pandoctree' :: InfTree -> ([Inline],String)
pandoctree' (InfRel dt r)
 |elem dt [D_id,D_v,D_id_c,D_v_c] = 
    let 
    r' =  show r
    tp = if elem dt [D_id,D_id_c] then "Anything" else "Anything*Anything"
    dtrel = case dt of D_id -> D_rel_h;  D_id_c -> D_rel_c_h; D_v -> D_rel;  D_v_c -> D_rel_c; _->dt;
    concl = (if elem dt [D_id_c,D_v_c] then "\\overline{"++r'++"}" else r') ++"["++tp++"]"
    in
    ([TeX ("\\AxiomC{$ $}")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}")
     ,TeX ("\\UnaryInfC{$"++r'++"::"++tp++" \\in \\Gamma$}")
     ,TeX ("\\RightLabel{\\scriptsize("++show dtrel++")}")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++concl++"$}")
     ],concl)
 |otherwise = 
    let
    r' =  show r
    (c1,c2) = dtype r
    tp = if elem dt [D_rel_c_h,D_rel_h] then show c1 else show c1++"*"++ show c2
    concl = (if elem dt [D_rel_c_h,D_rel_c] then "\\overline{"++r'++"}" else r') ++"["++tp++"]"
    in
    ([TeX ("\\AxiomC{$"++r'++"::"++tp++" \\in \\Gamma$}")
     ,TeX ("\\RightLabel{\\scriptsize("++show dt++")}")
     ,TeX ("\\UnaryInfC{$\\Gamma \\models "++concl++"$}")
     ],concl)
pandoctree' (InfExprs rt axs) 
   |elem rt [Conv_nc, Conv_c] && length axs==1 = 
       let
       (il,ax) =  pandoctree' (head axs)
       (c1,c2) = ("todo","todo")
       tp = show c1++"*"++ show c2
       concl = (if rt==Conv_c then "\\overline{"++ax++"^\\smile}" else ax++"^\\smile")  ++ "["++tp++"]"
       in
       ( il++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}")
        ,TeX ("\\UnaryInfC{$\\Gamma \\models " ++ concl ++"$}")
        ],concl)
   |elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs, RAdd_ncs, RAdd_c1, RAdd_c2, RAdd_cs] && length axs==2 = 
       let
       (il1,ax1) =  pandoctree' (head axs)
       (il2,ax2) =  pandoctree' (head (tail axs))
       op = if elem rt [Comp_ncs, Comp_c1, Comp_c2, Comp_cs] then ";" else "\\dagger"
       (c1,c2) = ("todo","todo")
       tp = show c1++"*"++ show c2
       concl = ax1 ++ op ++ ax2 ++ "["++tp++"]"
       in
       (il1++il2++
        [TeX ("\\RightLabel{\\scriptsize("++show rt++")}")
        ,TeX ("\\BinaryInfC{$\\Gamma \\models " ++ concl ++"$}")
        ],"")
   |elem rt [ISect_cs, ISect_ncs, ISect_mix, Union_mix]
      = (concat (map (fst.pandoctree') axs),"") --TODO
   |elem rt [Union_mix]
      = (concat (map (fst.pandoctree') axs),"") --TODO
   |otherwise=error "TODO 90"

testdoc :: [Block] -> String
testdoc b = writeLaTeX defaultWriterOptions (Pandoc (Meta [] [] []) b)
