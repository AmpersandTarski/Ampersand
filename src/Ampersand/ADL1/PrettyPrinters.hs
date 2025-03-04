{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ampersand.ADL1.PrettyPrinters (Pretty (..), prettyPrint) where

import Ampersand.Basics hiding (view, (<$>))
import Ampersand.Core.ParseTree
import Data.List (nub)
import RIO.Char (toUpper)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial (replace)
import Text.PrettyPrint.Leijen

prettyPrint :: (Pretty a) => a -> Text
prettyPrint x = T.pack $ displayS (renderPretty rfrac col_width doc) ""
  where
    col_width = 120
    rfrac = 0.4
    doc = pretty x

(<~>) :: (Pretty b) => Doc -> b -> Doc
(<~>) a b = a <+> pretty b

(<+\>) :: Doc -> Doc -> Doc
(<+\>) a b = a <$$> b

(<~\>) :: (Pretty b) => Doc -> b -> Doc
(<~\>) a b = a <+\> pretty b

perline :: (Pretty a) => [a] -> Doc
perline = vsep . map pretty

quote :: Text -> Doc
quote = text . show

quotePurpose :: Text -> Doc
quotePurpose p = text "{+" </> escapeExpl p </> text "+}"
  where
    escapeExpl = text . T.unpack . escapeCommentStart . escapeLineComment . escapeExplEnd
    escapeCommentStart = escape "{-"
    escapeLineComment = escape "--"
    escapeExplEnd = escape "+}"
    escape x = replace' x (T.intersperse ' ' x)
    replace' :: Text -> Text -> Text -> Text
    replace' needle replacement haystack
      | T.null needle = fatal "Empty needle."
      | otherwise -- replace is now safe to use, because we have a non-empty needle
        =
          Partial.replace needle replacement haystack

prettyhsep :: (Pretty a) => [a] -> Doc
prettyhsep = hsep . map pretty

commas :: [Doc] -> Doc
commas = encloseSep empty empty comma

listOf :: (Pretty a) => [a] -> Doc
listOf = commas . map pretty

listOfBy :: (a -> Doc) -> [a] -> Doc
listOfBy fun = commas . map fun

listOf1 :: (Pretty a) => NE.NonEmpty a -> Doc
listOf1 = listOf . NE.toList

separate :: (Pretty a) => Text -> [a] -> Doc
separate d xs = encloseSep empty empty ((text . T.unpack) d) $ map pretty xs

instance Pretty Name where
  pretty = text . T.unpack . fullName

instance Pretty Label where
  pretty (Label x) = text "LABEL" <~> (text . T.unpack . tshow) x

instance Pretty P_Context where
  pretty (PCtx nm lbl _ lang markup pats rs ds cs ks rrules reprs vs gs ifcs ps pops metas enfs) =
    text "CONTEXT"
      <~> nm
      <~> lbl
      <~> lang
      <~> markup
      <+\> perline metas
      <+\> perline ps
      <+\> perline pats
      <+\> perline rs
      <+\> perlineRelations ds
      <+\> perline cs
      <+\> perline ks
      <+\> perline rrules
      <+\> perline reprs
      <+\> perline vs
      <+\> perline gs
      <+\> perline ifcs
      <+\> perline pops
      <+\> perline enfs
      <+\> text "ENDCONTEXT"

perlineRelations :: [P_Relation] -> Doc
perlineRelations ds =
  if length ds > 4 * length (nub (map src ds))
    then vsep [vsep [pretty (d :: P_Relation) | d <- ds, src d == s] | s <- nub (map src ds)] -- if there are many wide tables (as is the case with data-analysis), then sort per entity.
    else vsep (map pretty ds)
  where
    src = pSrc . dec_sign

instance Pretty MetaData where
  pretty (MetaData _ nm val) =
    text "META" <+> quote (text1ToText nm) <+> quote val

instance Pretty P_RoleRule where
  pretty (Maintain _ roles rules) =
    text "ROLE" <+> listOf1 roles <+> text "MAINTAINS" <+> commas (NE.toList . fmap pretty $ rules)

instance Pretty Role where
  pretty (Role _ nm _ _) = pretty nm -- FIXME:This doesn't take into account the fact that a role could be a service.

instance Pretty P_Pattern where
  pretty (P_Pat _ nm lbl rls gns dcs rruls reprs cds ids vds xps pop _ enfs) =
    text "PATTERN"
      <~> nm
      <~> lbl
      <+\> perline rls
      <+\> perline gns
      <+\> perline dcs
      <+\> perline rruls
      <+\> perline reprs
      <+\> perline cds
      <+\> perline ids
      <+\> perline vds
      <+\> perline xps
      <+\> perline pop
      <+\> perline enfs
      <+\> text "ENDPATTERN"

instance Pretty P_Relation where
  pretty (P_Relation nm sign lbl prps dflts pragma mean _) =
    text "RELATION"
      <+> (text . T.unpack . localNameOf) nm <~> sign <~> lbl
      <+> props
      <+> if null dflts
        then empty
        else
          text "DEFAULT"
            <+\> (hsep . map pretty) dflts
            <+\> pretty pragma
            <+\> prettyhsep mean
    where
      props
        | null prps = empty
        | otherwise = pretty $ Set.toList prps

instance Pretty Pragma where
  pretty (Pragma _ l m r) = text "PRAGMA" <+> hsep (map quote [l, m, r])

instance (Pretty a) => Pretty (Term a) where
  pretty p = case p of
    Prim a -> pretty a
    -- level 0 (rule)
    PEqu _ t1 t2 -> two t1 t2 "="
    PInc _ t1 t2 -> two t1 t2 " |- "
    -- level 1
    PIsc _ t1 t2 -> two t1 t2 "/\\"
    PUni _ t1 t2 -> two t1 t2 "\\/"
    -- level 2
    PDif _ t1 t2 -> two t1 t2 "-"
    -- level 3
    PLrs _ t1 t2 -> two t1 t2 "/"
    PRrs _ t1 t2 -> two t1 t2 "\\"
    PDia _ t1 t2 -> two t1 t2 "<>"
    -- level 4
    PCps _ t1 t2 -> two t1 t2 ";"
    PRad _ t1 t2 -> two t1 t2 "!"
    PPrd _ t1 t2 -> two t1 t2 "#"
    -- level 5
    PKl0 _ t -> post t "*"
    PKl1 _ t -> post t "+"
    PFlp _ t -> post t "~"
    PCpl _ t -> pre t " -" -- a double dash can happen when combined with PDif, therefore the extra space
    -- level 6
    PBrk _ t -> parens $ pretty t
    where
      post t op = pretty t <> text op
      pre t op = text op <> pretty t
      two t1 t2 op = pretty t1 <> text op <> pretty t2

instance Pretty TermPrim where
  pretty p = case p of
    PI _ -> text "I"
    Pid _ cpt -> text "I[" <> pretty cpt <> text "]"
    PBin _ oper -> pretty oper
    PBind _ oper cpt -> pretty oper <> text "[" <> pretty cpt <> text "]"
    Patm _ val mCpt ->
      pretty val <> case mCpt of
        Nothing -> empty
        Just concept -> text "[" <> pretty concept <> text "]"
    PVee _ -> text "V"
    Pfull _ s1 s2 -> text "V" <~> P_Sign s1 s2
    PNamedR rel -> pretty rel

instance Pretty PBinOp where
  pretty p = case p of
    LessThan -> text "<"
    LessThanOrEqual -> text "<="
    GreaterThan -> text ">"
    GreaterThanOrEqual -> text ">="

instance Pretty P_NamedRel where
  pretty (PNamedRel _ str mpSign) = (text . T.unpack . localNameOf) str <~> mpSign

instance Pretty (PairView TermPrim) where
  pretty (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)

instance Pretty (PairView (Term TermPrim)) where
  pretty (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)

instance Pretty (PairViewSegment TermPrim) where
  pretty (PairViewText _ str) = text "TXT" <+> quote str
  pretty (PairViewExp _ srcTgt term) = pretty srcTgt <~> term

instance Pretty (PairViewSegment (Term TermPrim)) where
  pretty (PairViewText _ str) = text "TXT" <+> quote str
  pretty (PairViewExp _ srcTgt term) = pretty srcTgt <~> term

instance Pretty SrcOrTgt where
  pretty = text . map toUpper . show

instance Pretty (P_Rule TermPrim) where
  pretty (P_Rule _ nm lbl expr mean msg viol) =
    text "RULE"
      <+> pretty nm
      <+> pretty lbl
      <+> text ":"
        <~> expr
        <+\> perline mean
        <+\> perline msg
        <~\> viol

instance Pretty (P_Enforce TermPrim) where
  pretty (P_Enforce _ rel op expr) =
    text "ENFORCE"
      <+> pretty rel
      <+> pretty op
        <~> expr

instance Pretty EnforceOperator where
  pretty op = case op of
    IsSuperSet _ -> text ">:"
    IsSubSet _ -> text ":<"
    IsSameSet _ -> text ":="

instance Pretty PConceptDef where
  pretty (PConceptDef _ nm lbl def mean _) -- from, the last argument, is not used in the parser
    =
    text "CONCEPT" <~> nm <~> lbl
      <+> pretty def <+\> perline mean

instance Pretty PCDDef where
  pretty (PCDDefNew mean) = pretty mean
  pretty (PCDDefLegacy def ref) = quote def <+> maybeText ("[" <> ref <> "]")
    where
      maybeText txt =
        if T.null txt
          then empty
          else quote txt

instance Pretty P_Population where
  pretty p = case p of
    P_RelPopu _ _ _ nrel cs -> text "POPULATION" <+> pretty nrel <+> text "CONTAINS" <+> contents cs
    P_CptPopu _ cpt ps -> text "POPULATION" <+> pretty cpt <+> text "CONTAINS" <+> pretty ps
    where
      contents = list . map pretty

instance Pretty Representation where
  pretty (Repr _ cs tt) = text "REPRESENT" <+> listOf1 cs <~> text "TYPE" <+> pretty tt

instance Pretty TType where
  pretty = text . show

instance Pretty P_Interface where
  pretty (P_Ifc isAPI nm lbl roles obj _ _) =
    text (if isAPI then "API " else "INTERFACE ")
      <~> nm
      <~> lbl
      <+> iroles
      <+> interfaceExpression
      <+> crud (obj_crud obj)
      <+> prettyObject InterfaceKind obj
    where
      iroles =
        if null roles
          then empty
          else text "FOR" <+> listOf roles
      interfaceExpression = text ":" <~> pretty (obj_ctx obj)
      crud Nothing = empty
      crud (Just cruds) = pretty cruds

prettyObject :: ObjectKind -> P_BoxBodyElement -> Doc
prettyObject objectKind obj =
  maybeQuoteLabel
    (obj_PlainName obj)
    <+> ( case obj of
            (P_BoxItemTerm _ _ _ ctx mCrud mView msub) -> case objectKind of
              InterfaceKind -> view mView <$> pretty msub
              SubInterfaceKind _ -> pretty ctx <+> crud mCrud <+> view mView <$> pretty msub
              IdentSegmentKind -> pretty ctx <+> crud mCrud <+> view mView <$> pretty msub
              ViewSegmentKind -> pretty ctx <+> view mView
            (P_BxTxt _ _ str) ->
              text "TXT" <+> quote str
        )
  where
    crud Nothing = empty
    crud (Just cruds) = pretty cruds
    view :: Maybe Name -> Doc
    view Nothing = empty
    view (Just v) = (text . T.unpack) ("<" <> localNameOf v <> ">")
    maybeQuoteLabel :: Maybe Text1 -> Doc
    maybeQuoteLabel lbl =
      case lbl of
        Nothing -> mempty
        Just t -> case T.words (text1ToText t) of
          [] -> mempty
          [word] -> quote word <> text ":" -- quote just to be safe. Otherwise we have to deal with exceptions.
          _ -> quote (text1ToText t) <> text ":"

instance Pretty P_Cruds where
  pretty (P_Cruds _ str) = (text . T.unpack . text1ToText) str

instance Pretty (P_SubIfc TermPrim) where
  pretty p = case p of
    P_Box _ c bs -> boxSpec c <+> text "[" <> listOfBy (prettyObject (SubInterfaceKind 2)) bs <> text "]"
    P_InterfaceRef _ isLink nm -> text ((if isLink then "LINKTO " else "") ++ "INTERFACE") <~> nm
    where
      boxSpec :: HTMLtemplateCall -> Doc
      boxSpec x = text "BOX " <+> encloseSep (text " <") (text "> ") (text " ") items
        where
          items = (text . T.unpack . text1ToText . btType $ x) : (map prettyKey . btKeys $ x)
          prettyKey :: TemplateKeyValue -> Doc
          prettyKey kv =
            (text . T.unpack . text1ToText . tkkey $ kv)
              <+> ( case tkval kv of
                      Nothing -> mempty
                      Just t -> text " = " <+> (text . show $ t)
                  )

instance Pretty (P_IdentDf TermPrim) where
  pretty (P_Id _ nm lbl cpt ats) =
    text "IDENT" <~> nm <~> lbl <+> text ":" <~> cpt <+> parens (listOf1 ats)

instance Pretty (P_IdentSegmnt TermPrim) where
  pretty (P_IdentExp obj) = prettyObject IdentSegmentKind obj

instance Pretty P_ViewDef where
  pretty (P_Vd _ nm lbl cpt isDefault html ats) =
    -- improved syntax. Legacy syntax must not be used here anymore.
    text "VIEW" <~> nm <~> lbl
      <+> text ":"
        <~> cpt
      <+> (if isDefault then text "DEFAULT" else empty)
      <+> braces (listOf ats)
        <~> html
      <+> text "ENDVIEW"

instance Pretty ViewHtmlTemplate where
  pretty (ViewHtmlTemplateFile file) = text "HTML" <+> text "TEMPLATE" <+> quote (T.pack file)

instance Pretty (P_ViewSegment TermPrim) where
  pretty (P_ViewSegment mlab _ pl) =
    ( case mlab of
        Nothing -> empty
        Just str -> (text . T.unpack . tshow . text1ToText $ str) <+> text ":"
    )
      <~> pretty pl

instance Pretty (P_ViewSegmtPayLoad TermPrim) where
  pretty (P_ViewExp expr) = pretty expr
  pretty (P_ViewText txt) = text "TXT" <+> quote txt

instance Pretty PPurpose where
  pretty (PPurpose _ obj markup refIds) =
    text "PURPOSE" <~> obj <~> lang
      <+> refs refIds
        <+\> quotePurpose (mString markup)
    where
      lang = mFormat markup
      refs rs =
        if null rs
          then empty
          else text "REF" <+> quote (T.intercalate "; " rs)

instance Pretty PRef2Obj where
  pretty p = case p of
    PRef2ConceptDef nm -> text "CONCEPT" <~> nm
    PRef2Relation namedRel -> text "RELATION" <~> namedRel
    PRef2Rule nm -> text "RULE" <~> nm
    PRef2IdentityDef nm -> text "IDENT" <~> nm
    PRef2ViewDef nm -> text "VIEW" <~> nm
    PRef2Pattern nm -> text "PATTERN" <~> nm
    PRef2Interface nm -> text "INTERFACE" <~> nm
    PRef2Context nm -> text "CONTEXT" <~> nm

instance Pretty PMeaning where
  pretty (PMeaning markup) = text "MEANING" <~> markup

instance Pretty PMessage where
  pretty (PMessage markup) = text "MESSAGE" <~> markup

instance Pretty P_Concept where
  pretty (PCpt nm) = pretty nm
  pretty P_ONE = text "ONE"

instance Pretty P_Sign where
  pretty (P_Sign src tgt) = brackets (pretty src <> text "*" <> pretty tgt)

instance Pretty PClassify where
  pretty p =
    case p of
      PClassify _ spc gen ->
        text "CLASSIFY"
          <+> pretty spc
          <+> ( case (NE.length gen, NE.filter (spc /=) gen) of
                  (2, [x]) -> text "ISA" <~> x
                  _ -> text "IS" <+> separate "/\\" (NE.toList gen)
              )

instance Pretty Lang where
  pretty x = text "IN" <+> (text . map toUpper . show $ x)

instance Pretty P_Markup where
  pretty (P_Markup lang format str) =
    pretty lang <~> format <+\> quotePurpose str

instance Pretty PandocFormat where
  pretty = text . map toUpper . show

instance Pretty PProp where
  pretty p = case p of
    P_Uni -> text "UNI"
    P_Inj -> text "INJ"
    P_Sur -> text "SUR"
    P_Tot -> text "TOT"
    P_Sym -> text "SYM"
    P_Asy -> text "ASY"
    P_Trn -> text "TRN"
    P_Rfx -> text "RFX"
    P_Irf -> text "IRF"
    P_Prop -> text "SYM, ASY"
    P_Map -> text "UNI, TOT"
    P_Bij -> text "INJ, SUR"

-- _ -> text . map toUpper . show $ p

instance Pretty PRelationDefault where
  pretty x = case x of
    PDefAtom sOrT pav -> pretty sOrT <+> text "VALUE " <+> (cat . punctuate (text ", ") . toList $ fmap pretty pav)
    PDefEvalPHP sOrT txt -> pretty sOrT <+> text "EVALPHP " <+> text (show txt)

instance Pretty PAtomPair where
  pretty (PPair _ l r) =
    text "("
      <+> pretty l
        <~> text ","
      <+> pretty r
        <~> text ")"

instance Pretty PAtomValue where
  pretty pav =
    case pav of
      PSingleton _o _ mav -> case mav of
        Nothing -> fatal ("The singleton " <> tshow pav <> " has no type, so it cannot be accuratly prettyprinted in a population statement.")
        Just val -> text "{" <+> pretty val <+> text "}"
      ScriptString _o s -> text . show $ s
      XlsxString _o s -> text . show $ s
      ScriptInt _o i -> text . show $ i
      ScriptFloat _o d -> text . show $ d
      XlsxDouble _o d -> fatal ("Prettyprinting a value " <> tshow d <> " from a .xlsx-file, which has to be shown in a term, however the technicaltype is not known")
      ComnBool _o b -> text . map toUpper . show $ b
      ScriptDate _o x -> text . show $ x
      ScriptDateTime _o x -> text . show $ x
