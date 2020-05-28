{-# LANGUAGE OverloadedStrings #-}
module Ampersand.ADL1.Rule 
  ( consequent, antecedent, hasantecedent
  , isPropertyRule, rulefromProp
  , propFullName
  )
where

import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Basics

hasantecedent :: Rule -> Bool
hasantecedent r
 = case formalExpression r of
     EEqu{} -> True
     EInc{} -> True
     _      -> False
antecedent :: Rule -> Expression
antecedent r
 = case formalExpression r of
     EEqu (le,_) -> le
     EInc (le,_) -> le
     _           -> fatal ("erroneous reference to antecedent of rule "<>tshow r)

consequent :: Rule -> Expression
consequent r
 = case formalExpression r of
     EEqu (_,re) -> re
     EInc (_,re) -> re
     x           -> x

isPropertyRule :: Rule -> Bool
isPropertyRule = isJust . rrdcl 
-- rulefromProp specifies a rule that defines property prp of relation d.
rulefromProp :: Prop -> Relation -> Rule
rulefromProp prp d =
     Ru { rrnm  = tshow prp<>" "<>showDcl
        , formalExpression = rExpr
        , rrfps = PropertyRule nm (origin d)
        , rrmean = meanings prp
        , rrmsg =  violMsg prp
        , rrviol = Nothing
        , rrdcl = Just (prp,d)         -- For traceability: The original property and relation.
        , rrpat = decpat d      
        , r_usr = Multiplicity
        , isSignal = fatal "It is determined later (when all MAINTAIN statements are available), what this value is." 
        }
       where
        nm = tshow prp<>" "<>showDcl
        showDcl = showRel d
        r:: Expression
        r = EDcD d
        rExpr = if not (isEndo r) && prp `elem` [Sym, Asy, Trn, Rfx, Irf]
                then fatal ("Illegal property of an endo relation "<>tshow (name d)) else
                case prp of
                     Uni-> r .:. ECpl (EDcI (target r)) .:. flp r .|-. ECpl (EDcI (source r))
                     Tot-> EDcI (source r)  .|-. r .:. flp r
                     Inj-> flp r .:. ECpl (EDcI (source r)) .:. r .|-. ECpl (EDcI (target r))
                     Sur-> EDcI (target r)  .|-. flp r .:. r
                     Sym-> r .==. flp r
                     Asy-> flp r ./\. r .|-. EDcI (source r)
                     Trn-> r .:. r .|-. r
                     Rfx-> EDcI (source r) .|-. r
                     Irf-> r .|-. ECpl (EDcI (source r))
                     Prop -> fatal "Prop should have been converted by the parser"
        meanings prop = map (Meaning . markup) [English,Dutch]
          where 
            markup lang = Markup lang (string2Blocks ReST $ f lang)
            f lang = showDcl<>" is "<>propFullName False lang prop
         
        violMsg prop = [ msg lang | lang <-[English,Dutch]]
          where
            s= name (source d)
            t= name (target d)
            msg lang = Markup lang (string2Blocks ReST $ f lang)
            f lang =
              case lang of
                English ->
                  case prop of
                    Sym-> explByFullName lang
                    Asy-> explByFullName lang
                    Trn-> explByFullName lang
                    Rfx-> explByFullName lang
                    Irf-> explByFullName lang
                    Uni-> "Each " <>s<>" may only have one "<>t<>"" <>" in the relation "<>name d
                    Inj-> "Each " <>t<>" may only have one "<>s<>"" <>" in the relation "<>name d
                    Tot ->"Every "<>s<>" must have a "      <>t<>"" <>" in the relation "<>name d
                    Sur ->"Every "<>t<>" must have a "      <>s<>"" <>" in the relation "<>name d
                    Prop -> fatal "Prop should have been converted by the parser"
                Dutch ->
                  case prop of
                    Sym-> explByFullName lang
                    Asy-> explByFullName lang
                    Trn-> explByFullName lang
                    Rfx-> explByFullName lang
                    Irf-> explByFullName lang
                    Uni-> "Elke "<>s<>" mag slechts één "<>t<>   " hebben" <>" in de relatie "<>name d
                    Inj-> "Elke "<>t<>" mag slechts één "<>s<>   " hebben" <>" in de relatie "<>name d
                    Tot-> "Elke "<>s<>" dient één "      <>t<>" te hebben" <>" in de relatie "<>name d
                    Sur-> "Elke "<>t<>" dient een "      <>s<>" te hebben" <>" in de relatie "<>name d
                    Prop -> fatal "Prop should have been converted by pattern the parser"
            explByFullName lang = showDcl<>" is "<>propFullName False lang prop

propFullName :: Bool -> Lang -> Prop -> Text
propFullName isAdjective lang prop =
  case lang of 
    English ->
        case prop of
          Sym-> "symmetric"
          Asy-> "antisymmetric"
          Trn-> "transitive"
          Rfx-> "reflexive"
          Irf-> "irreflexive"
          Uni-> "univalent"
          Sur-> "surjective"
          Inj-> "injective"
          Tot-> "total"
          Prop -> fatal "Prop should have been converted by the parser"
    Dutch -> (if isAdjective then snd else fst) $
        case prop of
          Sym-> ("symmetrisch"    ,"symmetrische")
          Asy-> ("antisymmetrisch","antisymmetrische")
          Trn-> ("transitief"     ,"transitieve")
          Rfx-> ("reflexief"      ,"reflexieve")
          Irf-> ("irreflexief"    ,"irreflexieve")
          Uni-> ("univalent"      ,"univalente")
          Sur-> ("surjectief"     ,"surjectieve")
          Inj-> ("injectief"      ,"injectieve")
          Tot-> ("totaal"         ,"totale")
          Prop -> fatal "Prop should have been converted by the parser"
