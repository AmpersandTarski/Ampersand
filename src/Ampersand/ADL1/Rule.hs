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
  Rule
    { rrnm =
        withNameSpace
          (nameSpaceOf rel)
          $ case try2Name RuleName (tshow prp <> (tshow . abs . hash . tshow $ rel)) of
            Left err -> fatal $ "Not a proper name: " <> err
            Right (nm, _) -> nm,
      rrlbl = Just . Label $ tshow prp <> " rule for relation " <> tshow rel,
      formalExpression = rExpr,
      rrfps = PropertyRule relIdentifier (origin rel),
      rrmean = meanings prp,
      rrmsg = violMsg prp,
      rrviol = Nothing,
      rrpat = decpat rel,
      rrkind = Propty prp rel
    }
  where
    relIdentifier :: Text1
    relIdentifier = toText1Unsafe $ tshow prp <> "_" <> tshow rel
    showDcl :: Text
    showDcl = tshow rel
    r :: Expression
    r = EDcD rel
    rExpr =
      if not (isEndo r) && prp `elem` [Sym, Asy, Trn, Rfx, Irf]
        then fatal ("Illegal property of an endo relation " <> fullName rel)
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
                string2Blocks ReST
                  $ showDcl
                  <> " is "
                  <> propFullName False lang prop
            }

    violMsg prop = [msg lang | lang <- [English, Dutch]]
      where
        s = fullName1 (source rel)
        t = fullName1 (target rel)
        msg lang =
          Markup
            { amLang = lang,
              amPandoc =
                string2Blocks ReST
                  . text1ToText
                  $ case lang of
                    English ->
                      case prop of
                        Sym -> explByFullName lang
                        Asy -> explByFullName lang
                        Trn -> explByFullName lang
                        Rfx -> explByFullName lang
                        Irf -> explByFullName lang
                        Uni -> toText1Unsafe $ "Each " <> text1ToText s <> " may only have one " <> text1ToText t <> " in the relation " <> text1ToText (fullName1 rel)
                        Inj -> toText1Unsafe $ "Each " <> text1ToText t <> " may only have one " <> text1ToText s <> " in the relation " <> text1ToText (fullName1 rel)
                        Tot -> toText1Unsafe $ "Every " <> text1ToText s <> " must have a " <> text1ToText t <> " in the relation " <> text1ToText (fullName1 rel)
                        Sur -> toText1Unsafe $ "Every " <> text1ToText t <> " must have a " <> text1ToText s <> " in the relation " <> text1ToText (fullName1 rel)
                    Dutch ->
                      case prop of
                        Sym -> explByFullName lang
                        Asy -> explByFullName lang
                        Trn -> explByFullName lang
                        Rfx -> explByFullName lang
                        Irf -> explByFullName lang
                        Uni -> toText1Unsafe $ "Elke " <> text1ToText s <> " mag slechts één " <> text1ToText t <> " hebben in de relatie " <> text1ToText (fullName1 rel)
                        Inj -> toText1Unsafe $ "Elke " <> text1ToText t <> " mag slechts één " <> text1ToText s <> " hebben in de relatie " <> text1ToText (fullName1 rel)
                        Tot -> toText1Unsafe $ "Elke " <> text1ToText s <> " dient één " <> text1ToText t <> " hebben in de relatie " <> text1ToText (fullName1 rel)
                        Sur -> toText1Unsafe $ "Elke " <> text1ToText t <> " dient één " <> text1ToText s <> " hebben in de relatie " <> text1ToText (fullName1 rel)
            }
        explByFullName lang = toText1Unsafe $ showDcl <> (" is " <> propFullName False lang prop)

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
    Dutch -> (if isAdjective then snd else fst)
      $ case prop of
        Sym -> ("symmetrisch", "symmetrische")
        Asy -> ("antisymmetrisch", "antisymmetrische")
        Trn -> ("transitief", "transitieve")
        Rfx -> ("reflexief", "reflexieve")
        Irf -> ("irreflexief", "irreflexieve")
        Uni -> ("univalent", "univalente")
        Sur -> ("surjectief", "surjectieve")
        Inj -> ("injectief", "injectieve")
        Tot -> ("totaal", "totale")
