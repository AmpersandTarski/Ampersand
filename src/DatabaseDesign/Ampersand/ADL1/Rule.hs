{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Rule    ( 
                consequent, antecedent, rulefromProp, isaRule, ruleviolations, hasantecedent)     
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos             
   import DatabaseDesign.Ampersand.Basics                         ( fatalMsg,Identified(..), (>-))
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration    ( Declaration(..), makeRelation)
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
                   
   ruleviolations :: Rule -> Pairs
   ruleviolations r
    = case rrexp r of
        EEqu {} -> (cra >- crc) ++ (crc >- cra)
        EImp {} -> cra >- crc
        _     -> contents (V (rrtyp r)) >- crc  --everything not in con
      where cra = contents (antecedent r) ; crc = contents (consequent r)
 
-- rulefromProp specifies a rule that defines property prp of declaration d.
-- The table of all declarations is provided, in order to generate shorter names if possible. 
   rulefromProp :: [Declaration] -> Prop -> Declaration -> Rule
   rulefromProp ds prp d@(Sgn{})
      = Ru { rrnm  = case prp of
                        Uni-> "uni " ++name d++typ2
                        Tot-> "tot " ++name d++typ2
                        Inj-> "inj " ++name d++typ2
                        Sur-> "sur " ++name d++typ2
                        Sym-> "sym " ++name d++typ1
                        Asy-> "asy " ++name d++typ1
                        Trn-> "trn " ++name d++typ1
                        Rfx-> "rfx " ++name d++typ1
                        Irf-> "irf " ++name d++typ1
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
           , rrxpl = Means (Just English) (string2Blocks ReST (
                      case prp of
                        Sym-> name d++"["++s++"] is symmetric"    
                        Asy-> name d++"["++s++"] is antisymmetric"
                        Trn-> name d++"["++s++"] is transitive"
                        Rfx-> name d++"["++s++"] is reflexive"
                        Irf-> name d++"["++s++"] is irreflexive"
                        Uni-> name d++"["++s++"*"++t++"] is univalent"
                        Sur-> name d++"["++s++"*"++t++"] is surjective"
                        Inj-> name d++"["++s++"*"++t++"] is injective"
                        Tot-> name d++"["++s++"*"++t++"] is total"
                        )) :
                     [Means (Just Dutch) (string2Blocks ReST (
                      case prp of
                        Sym-> name d++"["++s++"] is symmetrisch."    
                        Asy-> name d++"["++s++"] is antisymmetrisch."
                        Trn-> name d++"["++s++"] is transitief."
                        Rfx-> name d++"["++s++"] is reflexief."
                        Irf-> name d++"["++s++"] is irreflexief."
                        Uni-> name d++"["++s++"*"++t++"] is univalent"
                        Sur-> name d++"["++s++"*"++t++"] is surjectief"
                        Inj-> name d++"["++s++"*"++t++"] is injectief"
                        Tot-> name d++"["++s++"*"++t++"] is totaal"
                        ))] 
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
           typ2 = if length [name d' |d'<-ds, d'==d]>1
                  then "["++s++ if s==t then "]" else "*"++t++"]"
                  else ""
           typ1 = if length [name d' |d'<-ds, d'==d]>1
                  then "["++s++"]"
                  else ""
           r:: Expression
           r = ERel (makeRelation d) 
   rulefromProp _ _ _ = fatal 252 "Properties can only be set on user-defined declarations."
