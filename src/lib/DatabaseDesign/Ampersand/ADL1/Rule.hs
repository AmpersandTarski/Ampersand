{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Rule    (
                consequent, antecedent, rulefromProp, ruleviolations, hasantecedent)
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Core.ParseTree        ( Prop(..))
   import DatabaseDesign.Ampersand.Classes.Populated              ( fullContents)
   import DatabaseDesign.Ampersand.Misc

   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.Rule"


   hasantecedent :: Rule -> Bool
   hasantecedent r
    = case rrexp r of
        EEqu{} -> True
        EImp{} -> True
        _      -> False
   antecedent :: Rule -> Expression
   antecedent r
    = case rrexp r of
        EEqu (le,_) -> le
        EImp (le,_) -> le
        _           -> fatal 134 $ "erroneous reference to antecedent of rule "++show r

   consequent :: Rule -> Expression
   consequent r
    = case rrexp r of
        EEqu (_,re) -> re
        EImp (_,re) -> re
        x           -> x

   ruleviolations :: [A_Gen] -> [Population] -> Rule -> Pairs
   ruleviolations gens pt r = case rrexp r of
        EEqu{} -> (cra >- crc) ++ (crc >- cra)
        EImp{} -> cra >- crc
        _      -> fullContents gens pt (EDcV (sign (consequent r))) >- crc  --everything not in con
        where cra = fullContents gens pt (antecedent r)
              crc = fullContents gens pt (consequent r)

-- rulefromProp specifies a rule that defines property prp of declaration d.
-- The table of all relations is provided, in order to generate shorter names if possible.
   rulefromProp :: Prop -> Declaration -> Rule
   rulefromProp prp d@Sgn{}
      = Ru { rrnm  = show prp++" "++name d++"::"++s++"*"++t
           , rrexp = rExpr
           , rrfps = origin d
           , rrmean = AMeaning $ explain True prp
           , rrmsg = explain False prp
           , rrviol = Nothing
           , rrtyp = sign rExpr
           , rrdcl = Just (prp,d)         -- For traceability: The original property and declaration.
           , r_env = decpat d             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
           , r_usr = Multiplicity
           , r_sgl = False
           , srrel = d{decnm=show prp++name d}
           }
          where
           s = name (source d)
           t = name (target d)
           r:: Expression
           r = EDcD d
           rExpr = case prp of
                        Uni-> flp r .:. r .|-. EDcI (target r)
                        Tot-> EDcI (source r)  .|-. r .:. flp r
                        Inj-> r .:. flp r .|-. EDcI (source r)
                        Sur-> EDcI (target r)  .|-. flp r .:. r
                        Sym-> r .==. flp r
                        Asy-> flp r ./\. r .|-. EDcI (source r)
                        Trn-> r .:. r .|-. r
                        Rfx-> EDcI (source r) .|-. r
                        Irf-> EDcI (source r) ./\. (EDcV (sign r) .-. r)
           explain isPositive prop = [ A_Markup English ReST (string2Blocks ReST (
                                 case prop of
                                   Sym-> state isPositive English (name d++"["++s++"]") "symmetric"
                                   Asy-> state isPositive English (name d++"["++s++"]") "antisymmetric"
                                   Trn-> state isPositive English (name d++"["++s++"]") "transitive"
                                   Rfx-> state isPositive English (name d++"["++s++"]") "reflexive"
                                   Irf-> state isPositive English (name d++"["++s++"]") "irreflexive"
                                   Uni-> state isPositive English (name d++"["++s++"*"++t++"]") "univalent"
                                   Sur-> state isPositive English (name d++"["++s++"*"++t++"]") "surjective"
                                   Inj-> state isPositive English (name d++"["++s++"*"++t++"]") "injective"
                                   Tot-> state isPositive English (name d++"["++s++"*"++t++"]") "total"
                                   ))
                          ,   A_Markup Dutch ReST (string2Blocks ReST (
                                 case prop of
                                   Sym-> state isPositive Dutch (name d++"["++s++"]") "symmetrisch."
                                   Asy-> state isPositive Dutch (name d++"["++s++"]") "antisymmetrisch."
                                   Trn-> state isPositive Dutch (name d++"["++s++"]") "transitief."
                                   Rfx-> state isPositive Dutch (name d++"["++s++"]") "reflexief."
                                   Irf-> state isPositive Dutch (name d++"["++s++"]") "irreflexief."
                                   Uni-> state isPositive Dutch (name d++"["++s++"*"++t++"]") "univalent"
                                   Sur-> state isPositive Dutch (name d++"["++s++"*"++t++"]") "surjectief"
                                   Inj-> state isPositive Dutch (name d++"["++s++"*"++t++"]") "injectief"
                                   Tot-> state isPositive Dutch (name d++"["++s++"*"++t++"]") "totaal"
                                   ))
                         ]

           state True  _       left right = left ++ " is " ++ right
           state False English left right = left ++ " is not " ++ right
           state False Dutch   left right = left ++ " is niet " ++ right

   rulefromProp _ _ = fatal 252 "Properties can only be set on user-defined relations."
