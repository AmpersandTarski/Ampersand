module Ampersand.ADL1.Rule
  ( consequent,
    antecedent,
    hasantecedent,
    isPropertyRule,
    rulefromProp,
    propFullName,
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Data.Text1 ((.<>), (<>.))

hasantecedent :: Rule -> Bool
hasantecedent r =
  case formalExpression r of
    EEqu {} -> True
    EInc {} -> True
    _ -> False

antecedent :: Rule -> Expression
antecedent r =
  case formalExpression r of
    EEqu (le, _) -> le
    EInc (le, _) -> le
    _ -> fatal ("erroneous reference to antecedent of rule " <> tshow r)

consequent :: Rule -> Expression
consequent r =
  case formalExpression r of
    EEqu (_, re) -> re
    EInc (_, re) -> re
    x -> x

isPropertyRule :: Rule -> Bool
isPropertyRule r = case rrkind r of
  Propty {} -> True
  _ -> False

-- rulefromProp specifies a rule that defines property prp of relation d.
rulefromProp :: AProp -> Relation -> Rule
rulefromProp prp rel =
  Ru
    { rrnm =
        toName
          (nameSpaceOf (name rel))
          (tshow prp <> "_" .<> showDcl),
      formalExpression = rExpr,
      rrfps = PropertyRule relIdentifier (origin rel),
      rrmean = meanings prp,
      rrmsg = violMsg prp,
      rrviol = Nothing,
      rrpat = decpat rel,
      rrkind = Propty prp rel
    }
  where
    relIdentifier = tshow prp <> " " .<> showDcl
    showDcl = showRel rel
    r :: Expression
    r = EDcD rel
    rExpr =
      if not (isEndo r) && prp `elem` [Sym, Asy, Trn, Rfx, Irf]
        then fatal ("Illegal property of an endo relation " <> tshow (name rel))
        else case prp of
          Uni -> r .:. ECpl (EDcI (target r)) .:. flp r .|-. ECpl (EDcI (source r))
          Tot -> EDcI (source r) .|-. r .:. flp r
          Inj -> flp r .:. ECpl (EDcI (source r)) .:. r .|-. ECpl (EDcI (target r))
          Sur -> EDcI (target r) .|-. flp r .:. r
          Sym -> r .==. flp r
          Asy -> flp r ./\. r .|-. EDcI (source r)
          Trn -> r .:. r .|-. r
          Rfx -> EDcI (source r) .|-. r
          Irf -> r .|-. ECpl (EDcI (source r))
    meanings prop = map (Meaning . markup) [English, Dutch]
      where
        markup lang =
          Markup
            { amLang = lang,
              amPandoc =
                string2Blocks ReST $
                  text1ToText showDcl <> " is " <> propFullName False lang prop
            }

    violMsg prop = [msg lang | lang <- [English, Dutch]]
      where
        s = tName (source rel)
        t = tName (target rel)
        msg lang =
          Markup
            { amLang = lang,
              amPandoc =
                string2Blocks ReST . text1ToText $
                  case lang of
                    English ->
                      case prop of
                        Sym -> explByFullName lang
                        Asy -> explByFullName lang
                        Trn -> explByFullName lang
                        Rfx -> explByFullName lang
                        Irf -> explByFullName lang
                        Uni -> ("Each " .<> s <>. " may only have one ") <> (t <>. " in the relation ") <> tName rel
                        Inj -> ("Each " .<> t <>. " may only have one ") <> (s <>. " in the relation ") <> tName rel
                        Tot -> ("Every " .<> s <>. " must have a ") <> (t <>. " in the relation ") <> tName rel
                        Sur -> ("Every " .<> t <>. " must have a ") <> (s <>. " in the relation ") <> tName rel
                    Dutch ->
                      case prop of
                        Sym -> explByFullName lang
                        Asy -> explByFullName lang
                        Trn -> explByFullName lang
                        Rfx -> explByFullName lang
                        Irf -> explByFullName lang
                        Uni -> ("Elke " .<> s <>. " mag slechts één ") <> (t <>. " hebben in de relatie ") <> tName rel
                        Inj -> ("Elke " .<> t <>. " mag slechts één ") <> (s <>. " hebben in de relatie ") <> tName rel
                        Tot -> ("Elke " .<> s <>. " dient één ") <> (t <>. " te hebben in de relatie ") <> tName rel
                        Sur -> ("Elke " .<> t <>. " dient een ") <> (s <>. " te hebben in de relatie ") <> tName rel
            }
        explByFullName lang = showDcl <>. " is " <> propFullName False lang prop

propFullName :: Bool -> Lang -> AProp -> Text
propFullName isAdjective lang prop =
  case lang of
    English ->
      case prop of
        Sym -> "symmetric"
        Asy -> "antisymmetric"
        Trn -> "transitive"
        Rfx -> "reflexive"
        Irf -> "irreflexive"
        Uni -> "univalent"
        Sur -> "surjective"
        Inj -> "injective"
        Tot -> "total"
    Dutch -> (if isAdjective then snd else fst) $
      case prop of
        Sym -> ("symmetrisch", "symmetrische")
        Asy -> ("antisymmetrisch", "antisymmetrische")
        Trn -> ("transitief", "transitieve")
        Rfx -> ("reflexief", "reflexieve")
        Irf -> ("irreflexief", "irreflexieve")
        Uni -> ("univalent", "univalente")
        Sur -> ("surjectief", "surjectieve")
        Inj -> ("injectief", "injectieve")
        Tot -> ("totaal", "totale")
