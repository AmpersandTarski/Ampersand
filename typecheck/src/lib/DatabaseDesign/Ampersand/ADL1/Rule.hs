{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Rule    ( 
                consequent, antecedent, rulefromProp, isaRule, ruleviolations, violationsexpr, hasantecedent)     
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos             
   import DatabaseDesign.Ampersand.Basics                         ( fatalMsg,Identified(..), (>-))
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration    ( makeRelation)
   import DatabaseDesign.Ampersand.ADL1.Prop                      ( Prop(..))
   import DatabaseDesign.Ampersand.Classes.Populated              (contents)
   import DatabaseDesign.Ampersand.Misc
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.Rule"


   isaRule :: Rule -> Bool    -- tells whether this rule was declared as an ISA rule
   isaRule Ru{rrfps=FileLoc(FilePos(_,_,str))} = str == "ISA"
   isaRule _ = False

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
        _         -> fatal 134 $ "erroneous reference to antecedent of rule "++show r
                   
   consequent :: Rule -> Expression
   consequent r
    = case rrexp r of
        EEqu (_,re) -> re
        EImp (_,re) -> re
        x         -> x
                   
   --WHY -> why isn't this implemented as contents (violationsexpr r)?
   --ANSWER -> to avoid performance issues, probably only in most cases (ticket #319)
   ruleviolations :: Rule -> Pairs
   ruleviolations r = case rrexp r of
        EEqu {} -> (cra >- crc) ++ (crc >- cra)
        EImp {} -> cra >- crc
        _     -> contents (V (rrtyp r)) >- crc  --everything not in con
        where cra = contents (antecedent r) ; crc = contents (consequent r)
   violationsexpr :: Rule -> Expression
   violationsexpr r = EDif (ERel (V (rrtyp r)), rrexp r)
 
-- rulefromProp specifies a rule that defines property prp of declaration d.
-- The table of all declarations is provided, in order to generate shorter names if possible. 
   rulefromProp :: Prop -> Declaration -> Rule
   rulefromProp prp d@(Sgn{})
      = Ru { rrnm  = show prp++" "++name d++"::"++s++"*"++t
           , rrexp = case prp of
                        Uni-> EImp (ECps [EFlp r,r] ,       i$sign$ECps [EFlp r,r] )
                        Tot-> EImp (i$sign$ECps [r,EFlp r], ECps [r,EFlp r]        )
                        Inj-> EImp (ECps [r,EFlp r],        i$sign$ECps [r,EFlp r] )
                        Sur-> EImp (i$sign$ECps [EFlp r,r], ECps [EFlp r,r]        )
                        Sym-> EEqu (r,                   EFlp r               )
                        Asy-> EImp (EIsc [EFlp r,r],        i$sign$EIsc [EFlp r,r] )
                        Trn-> EImp (ECps [r,r],            r                   )
                        Rfx-> EImp (i$sign r ,           r                   )
                        Irf-> EIsc [i$sign r, EDif (ERel (V (sign r)), r) ]
           , rrfps = origin d
           , rrmean = AMeaning $ explain True prp 
           , rrmsg = explain False prp
           , rrviol = Nothing
           , rrtyp = case prp of
                        Uni-> sign$ECps [EFlp r,r]
                        Tot-> sign$ECps [r,EFlp r]
                        Inj-> sign$ECps [r,EFlp r]
                        Sur-> sign$ECps [EFlp r,r]
                        Sym-> h$sign r
                        Asy-> h$sign r
                        Trn-> h$sign r
                        Rfx-> h$sign r
                        Irf-> h$sign r
           , rrdcl = Just (prp,d)         -- For traceability: The original property and declaration.
           , r_env = decpat d             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
           , r_usr = False                
           , r_sgl = False
           , srrel = d{decnm=show prp++name d}
           }
          where
           s = name (source d)
           t = name (target d)
           i sgn   | isEndo sgn = ERel (I (source sgn)) 
                   | otherwise = fatal 239 "Bad multiplicity rule, the source and target of an identity must be identical."
           h sgn   | isEndo sgn = sgn
                   | otherwise = fatal 241 "Bad rule, the source and target of the relation must be identical."
           r:: Expression
           r = ERel (makeRelation d) 
           
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
           
   rulefromProp _ _ = fatal 252 "Properties can only be set on user-defined declarations."
