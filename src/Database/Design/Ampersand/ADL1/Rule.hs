module Database.Design.Ampersand.ADL1.Rule 
  (consequent, antecedent, rulefromProp, hasantecedent) where

import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.Rule"

hasantecedent :: Rule -> Bool
hasantecedent r
 = case rrexp r of
     EEqu{} -> True
     EInc{} -> True
     _      -> False
antecedent :: Rule -> Expression
antecedent r
 = case rrexp r of
     EEqu (le,_) -> le
     EInc (le,_) -> le
     _           -> fatal 134 $ "erroneous reference to antecedent of rule "++show r

consequent :: Rule -> Expression
consequent r
 = case rrexp r of
     EEqu (_,re) -> re
     EInc (_,re) -> re
     x           -> x

-- rulefromProp specifies a rule that defines property prp of declaration d.
-- The table of all relations is provided, in order to generate shorter names if possible.
rulefromProp :: Prop -> Declaration -> Maybe Rule
rulefromProp Aut _ = Nothing
rulefromProp prp d@Sgn{} =
  Just $ 
     Ru { rrnm  = show prp++" "++name d++"::"++s++"*"++t
        , rrexp = rExpr
        , rrfps = origin d
        , rrmean = AMeaning $ explain True prp
        , rrmsg = explain False prp
        , rrviol = Nothing
        , rrtyp = sign rExpr
        , rrdcl = Just (prp,d)         -- For traceability: The original property and declaration.
        , r_env = decpat d             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
        , r_usr = Multiplicity
        , isSignal = fatal 63 "It is determined later (when all MAINTAIN statements are available), what this value is." 
        }
       where
        s = name (source d)
        t = name (target d)
        r:: Expression
        r = EDcD d
        rExpr = if not (isEndo r) && prp `elem` [Sym, Asy, Trn, Rfx, Irf]
                then fatal 70 ("Illegal property of an endo relation "++show (name d)) else
                case prp of
                     Uni-> flp r .:. r .|-. EDcI (target r)
                     Tot-> EDcI (source r)  .|-. r .:. flp r
                     Inj-> r .:. flp r .|-. EDcI (source r)
                     Sur-> EDcI (target r)  .|-. flp r .:. r
                     Sym-> r .==. flp r
                     Asy-> flp r ./\. r .|-. EDcI (source r)
                     Trn-> r .:. r .|-. r
                     Rfx-> EDcI (source r) .|-. r
                     Irf-> r .|-. ECpl (EDcI (source r))
                     Aut -> fatal 78 "Aut should have been handled by pattern match on top-level declaration rulefromProp"
                     Prop -> fatal 78 "Prop should have been converted by the parser"
        explain isPositive prop = [ A_Markup English (string2Blocks ReST (
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
                                Aut -> fatal 90 "Aut should have been handled by pattern match on top-level declaration rulefromProp"
                                Prop -> fatal 90 "Prop should have been converted by the parser"
                                ))
                       ,   A_Markup Dutch (string2Blocks ReST (
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
                                Aut -> fatal 103 "Aut should have been handled by pattern match on top-level declaration rulefromProp"
                                Prop -> fatal 103 "Prop should have been converted by pattern the parser"
                                ))
                      ]

        state True  _       left right = left ++ " is " ++ right
        state False English left right = left ++ " is not " ++ right
        state False Dutch   left right = left ++ " is niet " ++ right

rulefromProp _ _ = fatal 252 "Properties can only be set on user-defined relations."
