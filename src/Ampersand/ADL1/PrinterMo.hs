{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ampersand.ADL1.PrinterMo (PrettyMo (..), prettyMo, prettyMoText) where

import qualified Ampersand.ADL1.PrettyPrinters as PP
import Ampersand.Basics hiding (view, (<$>))
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.Lexer (keywords)
import Data.List (nub)
import RIO.Char (isLetter, toUpper)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial (replace)
import Text.PrettyPrint.Leijen as Leijen

class PrettyMo a where
  prettyMo :: a -> Doc

prettyMoText :: PrettyMo a => a -> Text
prettyMoText x = T.pack $ displayS (renderPretty 0.4 100 (prettyMo x)) ""

-- (<~>) :: (PrettyMo a, PrettyMo b) => a -> b -> Doc
-- (<~>) a b = prettyMo a <+> prettyMo b

-- instance PrettyMo Text where
--     prettyMo txt = text $ T.unpack txt  -- Zet Text om naar Doc

-- instance PrettyMo a => PrettyMo (Maybe a) where
--   prettyMo Nothing  = text "Nothing"  -- Or however you want to represent Nothing
--   prettyMo (Just x) = prettyMo x      -- Use the existing PrettyMo instance for `a`

--
(<+\!>) :: Doc -> Doc -> Doc
(<+\!>) a b = a <$$> pretty b

(<~!>) :: Pretty b => Doc -> b -> Doc
(<~!>) a b = a <+> pretty b

(<~\!>) :: Pretty b => Doc -> b -> Doc
(<~\!>) a b = a <+> pretty b

(<+\>) :: Doc -> Doc -> Doc
(<+\>) a b = a <$$> b

(<~>) :: PrettyMo b => Doc -> b -> Doc
(<~>) a b = a <+> prettyMo b

(<~\>) :: PrettyMo b => Doc -> b -> Doc
(<~\>) a b = a <+\> prettyMo b

perline :: PrettyMo a => [a] -> Doc
perline = vsep . map prettyMo

quote :: Text -> Doc
quote = text . show

quotePurpose :: Text -> Doc
quotePurpose p = text "{+ " <> escapeExpl p <> text " +}"
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

quoteMeaning :: T.Text -> Doc
quoteMeaning m = text "\"" <> escapeExpl m <> text "\""
  where
    escapeExpl = text . T.unpack . escapeCommentStart . escapeLineComment . escapeExplEnd
    escapeCommentStart = escape "{-"
    escapeLineComment = escape "--"
    escapeExplEnd = escape "\""
    escape x = replace' x (T.intersperse ' ' x)
    replace' :: T.Text -> T.Text -> T.Text -> T.Text
    replace' needle replacement haystack
      | T.null needle = error "Empty needle."
      | otherwise =
        Partial.replace needle replacement haystack

isId :: Text -> Bool
isId xs =
  case T.uncons xs of
    Nothing -> False
    Just (h, _) -> T.all isIdChar xs && isFirstIdChar h && xs `notElem` map T.pack keywords
      where
        isFirstIdChar x = x == '_' || isLetter x
        isIdChar x = isFirstIdChar x || elem x ['0' .. '9']

isUpperId :: Text -> Bool
isUpperId xs =
  case T.uncons xs of
    Nothing -> False
    Just (h, _) -> isId xs && h `elem` ['A' .. 'Z']

maybeQuote :: Text -> Doc
maybeQuote a = if isId a then (text . T.unpack) a else quote a

-- adds quotes unless it's an upper identifier
quoteConcept :: Text -> Doc
quoteConcept a = if isUpperId a then (text . T.unpack) a else quote a

prettyhsep :: PrettyMo a => [a] -> Doc
prettyhsep = hsep . map prettyMo

commas :: [Doc] -> Doc
commas = encloseSep empty empty comma

listOf :: PrettyMo a => [a] -> Doc
listOf = commas . map prettyMo

listOf1 :: PrettyMo a => NE.NonEmpty a -> Doc
listOf1 = listOf . NE.toList

separate :: PrettyMo a => Text -> [a] -> Doc
separate d xs = encloseSep empty empty ((text . T.unpack) d) $ map prettyMo xs

instance PrettyMo P_Context where
  prettyMo (PCtx nm _ lang markup pats rs ds cs ks rrules reprs vs gs ifcs ps pops metas enfs) =
    text "CONTEXT"
      <+> quoteConcept nm
      <~!> lang
      <~!> markup
      <+\> perline metas
      <+\> perline pats
      -- <+\> perline rs
      <+\> perlineRelations ds
      <+\> perline cs
      -- <+\> perline ks
      <+\> perline rrules
      <+\> perline reprs
      <+\> perline vs
      <+\> perline gs
      <+\> perline ifcs
      <+\> perline pops
      <+\> perline enfs
      <+\> perline ps
      <+\> text "ENDCONTEXT"

perlineRelations :: [P_Relation] -> Doc
perlineRelations ds =
  if length ds > 4 * length (nub (map src ds))
    then vsep [vsep [prettyMo (d :: P_Relation) | d <- ds, src d == s] | s <- nub (map src ds)] -- if there are many wide tables (as is the case with data-analysis), then sort per entity.
    else vsep (map prettyMo ds)
  where
    src = pSrc . dec_sign

instance PrettyMo MetaData where
  prettyMo (MetaData _ nm val) =
    text "META" <+> quote nm <+> quote val

instance PrettyMo P_RoleRule where
  prettyMo (Maintain _ roles rules) =
    text "ROLE" <+> listOf1 roles <+> text "MAINTAINS" <+> commas (NE.toList . fmap maybeQuote $ rules)

instance PrettyMo Role where
  prettyMo (Role nm) = maybeQuote nm
  prettyMo (Service nm) = maybeQuote nm

instance PrettyMo P_Pattern where
  prettyMo (P_Pat _ nm rls gns dcs rruls reprs cds ids vds xps pop _ enfs) =
    text "PATTERN"
      <+> quoteConcept nm
      -- <+\> perline rls
      <+\> perline gns
      <+\> perline dcs
      <+\> perline rruls
      <+\> perline reprs
      <+\> perline cds
      -- <+\> perline ids
      <+\> perline vds
      <+\> perline xps
      <+\> perline pop
      <+\> perline enfs
      <+\> text "ENDPATTERN"

instance PrettyMo P_Relation where
  prettyMo (P_Relation nm sign prps dflts pragma mean _) =
    text "RELATION"
      <+> (text . T.unpack) nm
      <~> sign
      <~!> props
      <+> if null dflts
        then empty
        else
          text "DEFAULT"
            <+\> (hsep . map prettyMo) dflts
            <+\> pretty pragma
            <+\> prettyhsep mean
    where
      props
        | null prps = empty
        | otherwise = pretty $ Set.toList prps

instance PrettyMo Pragma where
  prettyMo (Pragma _ l m r) = text "PRAGMA" <+> hsep (map quote [l, m, r])

instance PrettyMo a => PrettyMo (Term a) where
  prettyMo p = case p of
    Prim a -> prettyMo a
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
    PBrk _ t -> parens $ prettyMo t
    where
      post t op = prettyMo t <> text op
      pre t op = text op <> prettyMo t
      two t1 t2 op = prettyMo t1 <> text op <> prettyMo t2

instance PrettyMo TermPrim where
  prettyMo p = case p of
    PI _ -> text "I"
    Pid _ concept -> text "I[" <> prettyMo concept <> text "]"
    Patm _ val mCpt ->
      prettyMo val <> case mCpt of
        Nothing -> empty
        Just concept -> text "[" <> prettyMo concept <> text "]"
    PVee _ -> text "V"
    Pfull _ s1 s2 -> text "V" <~> P_Sign s1 s2
    PNamedR rel -> prettyMo rel

instance PrettyMo P_NamedRel where
  prettyMo (PNamedRel _ str mpSign) = (text . T.unpack) str <~!> mpSign

instance PrettyMo (PairView TermPrim) where
  prettyMo (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)

instance PrettyMo (PairView (Term TermPrim)) where
  prettyMo (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)

instance PrettyMo (PairViewSegment TermPrim) where
  prettyMo (PairViewText _ str) = text "TXT" <+> quote str
  prettyMo (PairViewExp _ srcTgt term) = prettyMo srcTgt <~> term

instance PrettyMo (PairViewSegment (Term TermPrim)) where
  prettyMo (PairViewText _ str) = text "TXT" <+> quote str
  prettyMo (PairViewExp _ srcTgt term) = prettyMo srcTgt <~> term

instance PrettyMo SrcOrTgt where
  prettyMo = text . map toUpper . show

-- instance PrettyMo (P_Rule TermPrim) where
--   prettyMo (P_Rule _ nm expr mean msg viol) =
--     text "RULE" <+> rName
--       <~> expr
--       <+\> perline mean
--       <+\> perline msg
--       <~\> viol
--     where
--       rName =
--         if T.null nm
--           then empty
--           else maybeQuote nm <> text ":"

instance PrettyMo (P_Enforce TermPrim) where
  prettyMo (P_Enforce _ rel op expr) =
    text "ENFORCE" <+> prettyMo rel <+> prettyMo op
      <~> expr

instance PrettyMo EnforceOperator where
  prettyMo op = case op of
    IsSuperSet _ -> text ">:"
    IsSubSet _ -> text ":<"
    IsSameSet _ -> text ":="

instance PrettyMo PConceptDef where
  prettyMo (PConceptDef _ cpt def _mean _) -- from, the last argument, is not used in the parser
    =
    text "CONCEPT" <+> quoteConcept cpt
      <+> prettyMo def
      <+> prettyhsep _mean

instance PrettyMo PCDDef where -- aangepast
  prettyMo (PCDDefNew def) =
    let prettyNoMeaning (PMeaning markup) = prettyMo markup -- Local function to adjust printing
     in prettyNoMeaning def -- Use the local function for pretty printing
  prettyMo (PCDDefLegacy def ref) = quote def <+> maybeText ("[" <> ref <> "]")
    where
      maybeText txt =
        if T.null txt
          then empty
          else quote txt

instance PrettyMo P_Population where
  prettyMo p = case p of
    P_RelPopu _ _ _ nrel cs -> text "POPULATION" <+> prettyMo nrel <+> text "CONTAINS" <+> contents cs
    P_CptPopu _ cpt ps -> text "POPULATION" <+> prettyMo cpt <+> text "CONTAINS" <+> pretty ps
    where
      contents = list . map prettyMo

instance PrettyMo Representation where
  prettyMo (Repr _ cs tt) = text "REPRESENT" <+> listOf1 cs <~!> text "TYPE" <+> prettyMo tt

instance PrettyMo TType where
  prettyMo = text . show

instance PrettyMo P_Interface where
  prettyMo (P_Ifc isAPI nm roles obj _ _) =
    text (if isAPI then "API " else "INTERFACE ") <+> maybeQuote nm
      <+> iroles
      <+> ( case obj of
              P_BxExpr {} ->
                nest 2 (text ":" <+> prettyMo (obj_ctx obj) <$> pretty (obj_msub obj))
              P_BxTxt {} -> fatal "TXT must not be used directly in a PP_Ifc."
          )
    where
      iroles =
        if null roles
          then empty
          else text "FOR" <+> listOf roles

-- instance PrettyMo a => PrettyMo (P_BoxItem a) where
--   prettyMo obj =
--     maybeQuote (name obj) <+> text ":"
--       <+> case obj of
--         (P_BxExpr _ _ ctx mCrud mView msub) ->
--           nest 2 (prettyMo ctx <+> crud mCrud <+> view mView <$> prettyMo msub)
--         (P_BxTxt _ _ str) ->
--           text "TXT" <+> quote str
--     where
--       crud Nothing = empty
--       crud (Just cruds) = prettyMo cruds
--       view Nothing = empty
--       view (Just v) = (text . T.unpack) ("<" <> v <> ">")

instance PrettyMo P_Cruds where
  prettyMo (P_Cruds _ str) = (text . T.unpack) str

-- instance PrettyMo a => PrettyMo (P_SubIfc a) where
--   prettyMo p = case p of
--     P_Box _ c bs -> boxSpec c <+> text "[" <> listOf bs <> text "]"
--     P_InterfaceRef _ isLink str -> text ((if isLink then "LINKTO " else "") ++ "INTERFACE") <+> maybeQuote str
--     where
--       boxSpec :: BoxHeader -> Doc
--       boxSpec x = text "BOX " <+> encloseSep (text " <") (text "> ") (text " ") items
--         where
--           items = (text . T.unpack . btType $ x) : (map prettyKey . btKeys $ x)
--           prettyKey :: TemplateKeyValue -> Doc
--           prettyKey kv =
--             (text . T.unpack . name $ kv)
--               <+> ( case tkval kv of
--                       Nothing -> mempty
--                       Just t -> text " = " <+> (text . show $ t)
--                   )

-- instance PrettyMo (P_IdentDf TermPrim) where
--   prettyMo (P_Id _ lbl cpt ats) =
--     text "IDENT" <+> maybeQuote lbl <+> text ":" <~> cpt <+> parens (listOf1 ats)

-- instance PrettyMo (P_IdentSegmnt TermPrim) where
--   prettyMo (P_IdentExp obj) =
--     case obj of
--       (P_BxExpr nm _ ctx _ mView _) ->
--         ( if T.null nm
--             then prettyMo ctx -- no label
--             else maybeQuote nm <> text ":" <~> ctx
--         )
--           <+> (view . fmap T.unpack) mView
--       (P_BxTxt nm _ str) ->
--         maybeQuote nm
--           <~> text "TXT" <+> quote str
--     where
--       view Nothing = empty
--       view (Just v) = prettyMo v

instance PrettyMo P_ViewDef where
  prettyMo (P_Vd _ lbl cpt True Nothing ats) =
    -- legacy syntax
    text "VIEW" <+> maybeQuote lbl <+> text ":"
      <~> cpt <+> parens (listOf ats)
  prettyMo (P_Vd _ lbl cpt isDefault html ats) =
    -- new syntax
    text "VIEW" <+> maybeQuote lbl <+> text ":"
      <~> cpt
      <+> (if isDefault then text "DEFAULT" else empty)
      <+> braces (listOf ats)
      <~!> html
      <+> text "ENDVIEW"

instance PrettyMo ViewHtmlTemplate where
  prettyMo (ViewHtmlTemplateFile file) = text "HTML" <+> text "TEMPLATE" <+> quote (T.pack file)

instance PrettyMo (P_ViewSegment TermPrim) where
  prettyMo (P_ViewSegment mlab _ pl) =
    ( case mlab of
        Nothing -> empty
        Just str -> maybeQuote str <+> text ":"
    )
      <~!> prettyMo pl

instance PrettyMo (P_ViewSegmtPayLoad TermPrim) where
  prettyMo (P_ViewExp expr) = prettyMo expr
  prettyMo (P_ViewText txt) = text "TXT" <+> quote txt

instance PrettyMo PPurpose where
  prettyMo (PRef2 _ obj markup refIds) =
    text "PURPOSE" <~> obj <~!> lang <+> refs refIds
      <+\> quotePurpose (mString markup)
    where
      lang = mFormat markup
      refs rs =
        if null rs
          then empty
          else text "REF" <+> quote (T.intercalate "; " rs)

instance PrettyMo PRef2Obj where
  prettyMo p = case p of
    PRef2ConceptDef str -> text "CONCEPT" <+> quoteConcept str
    PRef2Relation namedRel -> text "RELATION" <~> namedRel
    PRef2Rule str -> text "RULE" <+> maybeQuote str
    PRef2IdentityDef str -> text "IDENT" <+> maybeQuote str
    PRef2ViewDef str -> text "VIEW" <+> maybeQuote str
    PRef2Pattern str -> text "PATTERN" <+> maybeQuote str
    PRef2Interface str -> text "INTERFACE" <+> maybeQuote str
    PRef2Context str -> text "CONTEXT" <+> maybeQuote str

instance PrettyMo PMeaning where
  prettyMo (PMeaning markup) = text "MEANING" <~> markup

instance PrettyMo PMessage where
  prettyMo (PMessage markup) = text "MESSAGE" <~> markup

instance PrettyMo P_Concept where
  prettyMo (PCpt nm) = quoteConcept nm
  prettyMo P_ONE = text "ONE"

instance PrettyMo P_Sign where
  prettyMo (P_Sign src tgt) = brackets (prettyMo src <> text "*" <> prettyMo tgt)

instance PrettyMo PClassify where
  prettyMo p =
    case p of
      PClassify _ spc gen ->
        text "CLASSIFY" <+> prettyMo spc
          <+> ( case (NE.length gen, NE.filter (spc /=) gen) of
                  (2, [x]) -> text "ISA" <~> x
                  _ -> text "IS" <+> separate "/\\" (NE.toList gen)
              )

instance PrettyMo Lang where
  prettyMo x = text "IN" <+> (text . map toUpper . show $ x)

instance PrettyMo P_Markup where
  prettyMo (P_Markup lang format str) =
    pretty lang <~!> format <+\> quoteMeaning str -- Customize as needed
    -- Conversion function if needed

instance PrettyMo PandocFormat where
  prettyMo = text . map toUpper . show

instance PrettyMo PProp where
  prettyMo p = case p of
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

--_ -> text . map toUpper . show $ p

instance PrettyMo PRelationDefault where
  prettyMo x = case x of
    PDefAtom sOrT pav -> prettyMo sOrT <+> text "VALUE " <+> (cat . punctuate (text ", ") . toList $ fmap prettyMo pav)
    PDefEvalPHP sOrT txt -> prettyMo sOrT <+> text "EVALPHP " <+> text (show txt)

instance PrettyMo PAtomPair where
  prettyMo (PPair _ l r) =
    text "(" <+> prettyMo l
      <~!> text "," <+> prettyMo r
      <~!> text ")"

instance PrettyMo PAtomValue where
  prettyMo pav =
    case pav of
      PSingleton _o _ mav -> case mav of
        Nothing -> fatal ("The singleton " <> tshow pav <> " has no type, so it cannot be accuratly prettyprinted in a population statement.")
        Just val -> text "{" <+> prettyMo val <+> text "}"
      ScriptString _o s -> text . show $ s
      XlsxString _o s -> text . show $ s
      ScriptInt _o i -> text . show $ i
      ScriptFloat _o d -> text . show $ d
      XlsxDouble _o d -> fatal ("Prettyprinting a value " <> tshow d <> " from a .xlsx-file, which has to be shown in a term, however the technicaltype is not known")
      ComnBool _o b -> text . map toUpper . show $ b
      ScriptDate _o x -> text . show $ x
      ScriptDateTime _o x -> text . show $ x
