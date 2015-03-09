{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Design.Ampersand.ADL1.PrettyPrinters
where

import Text.PrettyPrint.Leijen
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Lexer(keywords)
import Database.Design.Ampersand.ADL1.Pair (Paire(..))
import Data.List (intercalate)
import Data.List.Utils (replace)
import Debug.Trace

pretty_print :: Pretty a => a -> String
pretty_print x = displayS (renderPretty rfrac col_width doc) ""
        where col_width = 120
              rfrac = 0.4
              doc = pretty x

(<~>) :: Pretty b => Doc -> b -> Doc
(<~>) a b = a <+> pretty b

(<+\>) :: Doc -> Doc -> Doc
(<+\>) a b = a <$$> b

(<~\>) :: Pretty b => Doc -> b -> Doc
(<~\>) a b = a <+\> pretty b

perline :: Pretty a => [a] -> Doc
perline = vsep . map pretty

perlinePrefix :: Pretty a => String -> [a] -> Doc
perlinePrefix pref xs = vsep $ map addPrefix xs
           where addPrefix x = text pref <+> pretty x

quoteWith :: String -> String -> String -> Doc
quoteWith l r x = enclose (text l) (text r) (text x)

quote :: String -> Doc
quote = dquotes.text.escapeAll
        where escapeAll = escapeQuote.escapeBreaklines.escapeSlash
              escapeQuote = escape "\""
              escapeBreaklines = replace "\n" "\\n"
              escapeSlash = escape "\\"
              escape x = replace x ("\\" ++ x)

quoteAll :: [String] -> [Doc]
quoteAll = map quote

quotePurpose :: String -> Doc
quotePurpose x = text "{+" </> text x </> text "-}"

isId :: String -> Bool
isId a = length a > 0 && all isIdChar a && isFirstIdChar(head a) && a `notElem` keywords
       where isFirstIdChar x = elem x $ "_"++['a'..'z']++['A'..'Z']
             isIdChar x = isFirstIdChar x || elem x ['0'..'9']

isUpperId :: String -> Bool
isUpperId xs = isId xs && (head xs) `elem` ['A'..'Z']

maybeQuote :: String -> Doc
maybeQuote a = if isId a then text a else quote a

-- adds quotes unless it's an upper identifier
quoteConcept :: String -> Doc
quoteConcept a = if isUpperId a then text a else quote a

prettyhsep :: Pretty a => [a] -> Doc
prettyhsep = hsep . map pretty

commas :: [Doc] -> Doc
commas = encloseSep empty empty comma

listOf :: Pretty a => [a] -> Doc
listOf = commas . map pretty

prettyPair :: Paire -> Doc
prettyPair (Paire src trg) = quote src <+> text "*" <+> quote trg

listOfLists :: [[String]] -> Doc
listOfLists xs = commas $ map (hsep.quoteAll) xs

separate :: Pretty a => String -> [a] -> Doc
separate d xs = encloseSep empty empty (text d) $ map pretty xs

--TODO: This replace shouldn't be necessary, I don't know why quotes are getting into the Prel
-- Example to test: AmpersandData\FormalAmpersand\AST.adl
takeQuote :: String -> String
takeQuote str = replace "\"" "" str

labelArgs :: [[String]] -> Doc
labelArgs args = if null args || all null args
                 then empty
                 else braces $ listOfLists args

instance Pretty P_Context where
    pretty p = text "CONTEXT" <+> quoteConcept(ctx_nm p) <~> ctx_lang p
               <~> ctx_markup p
               <+\> perline (ctx_metas p)
               <+\> themes
               <+\> perline (ctx_ps p)
               <+\> perline (ctx_PPrcs p)
               <+\> perline (ctx_pats p)
               <+\> perline (ctx_rs p)
               <+\> perline (ctx_ds p)
               <+\> perline (ctx_cs p)
               <+\> perline (ctx_ks p)
               <+\> perline (ctx_vs p)
               <+\> perline (ctx_gs p)
               <+\> perline (ctx_ifcs p)
               <+\> perline (ctx_pops p)
               <+\> perlinePrefix "SQLPLUG" (ctx_sql p)
               <+\> perlinePrefix "PHPPLUG" (ctx_php p)
               <+\> text "ENDCONTEXT"
             where themes = if null $ ctx_thms p then empty
                            else text "THEMES" <+> commas (map quoteConcept $ ctx_thms p)

instance Pretty Meta where
    pretty p = text "META" <~> mtObj p <+> quote (mtName p) <+> quote (mtVal p)

instance Pretty MetaObj where
    pretty ContextMeta = empty -- for the context meta we don't need a keyword

instance Pretty P_Process where
    pretty p = text "PROCESS" <+> quoteConcept (procNm p) <+\>
               perline (procRules p) <+\>
               perline (procGens p) <+\>
               perline (procDcls p) <+\>
               perline (procRRuls p) <+\>
               perline (procRRels p) <+\>
               perline (procCds p) <+\>
               perline (procIds p) <+\>
               perline (procVds p) <+\>
               perline (procXps p) <+\>
               perline (procPop p) <+\>
               text "ENDPROCESS"

instance Pretty P_RoleRelation where
    pretty (P_RR roles rels _) =
        text "ROLE" <+> listOf roles <+> text "EDITS" <+> listOf rels

instance Pretty RoleRule where
    pretty (Maintain roles rules _) =
        text "ROLE" <+> listOf roles <+> text "MAINTAINS" <+> commas (map maybeQuote rules)

instance Pretty Role where
    pretty (Role name) = maybeQuote name

instance Pretty P_Pattern where
    pretty (P_Pat nm _ _ rls gns dcs cds ids vds xps pop) =
          text "PATTERN"
          <+>  quoteConcept nm
          <+\> perline rls
          <+\> perline gns
          <+\> perline dcs
          <+\> perline cds
          <+\> perline ids
          <+\> perline vds
          <+\> perline xps
          <+\> perline pop
          <+>  text "ENDPATTERN"

instance Pretty P_Declaration where
    pretty (P_Sgn nm sign prps prL prM prR mean popu _ plug) =
        text "RELATION" <+> text nm <~> sign <+> props <+> byplug <+\> pragma <+\> prettyhsep mean <+\> content
        where props   = if prps == [Sym, Asy] then text "[PROP]"
                        else text "[" <> listOf prps <> text "]"
              byplug  = if plug then text "BYPLUG" else empty
              pragma  = if all null [prL, prM, prR] then empty
                        else text "PRAGMA" <+> quote prL <+> quote prM <+> quote prR
              content = if null popu then empty
                        else text "=\n[" <+> commas (map prettyPair popu) <+> text "]"

instance Pretty a => Pretty (Term a) where
   pretty p = case p of
       Prim a -> pretty a
       -- level 0 (rule)
       PEqu _ t1 t2 -> two t1 t2 "="
       PImp _ t1 t2 -> two t1 t2 " |- "
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
       PKl0 _ t -> pos t "*"
       PKl1 _ t -> pos t "+"
       PFlp _ t -> pos t "~"
       PCpl _ t -> pre t " -" -- a double dash can happen when combined with PDif, therefore the extra space
       -- level 6
       PBrk _ t -> parens $ pretty t
       
       where pos t op     = pretty t <> text op
             pre t op     = text op <> pretty t
             two t1 t2 op = pretty t1 <> text op <> pretty t2

instance Pretty TermPrim where
    pretty p = case p of
        PI _ -> text "I"
        Pid _ concept -> text "I[" <> pretty concept <> text "]"
        Patm _ str (Just concept) -> singleQuote str <> text "[" <> pretty concept <> text "]"
        Patm _ str Nothing -> singleQuote str
        PVee _ -> text "V"
        Pfull _ s1 s2 -> text "V" <~> (P_Sign s1 s2)
        Prel _ str -> text (takeQuote str)
        PTrel _ str sign -> text (takeQuote str) <~> sign
      where singleQuote = squotes . text

instance Pretty a => Pretty (PairView a) where
    pretty (PairView ss) = text "VIOLATION" <+> parens (listOf ss)

instance Pretty a => Pretty (PairViewSegment a) where
    pretty (PairViewText str) = text "TXT" <+> quote str
    pretty (PairViewExp srcTgt term) = pretty srcTgt <~> term

instance Pretty SrcOrTgt where
    pretty Src = text "SRC"
    pretty Tgt = text "TGT"

instance Pretty a => Pretty (P_Rule a) where
    pretty (P_Ru nm expr _ mean msg viol) =
                text "RULE" <+> name <~>
                expr <+\>
                perline mean <+\>
                perline msg <~\>
                viol
            where name = if null nm then empty
                         else maybeQuote nm <> text ":"

instance Pretty ConceptDef where
    pretty (Cd _ cpt plug def typ ref _) -- from, the last argument, is not used in the parser
        = text "CONCEPT" <+> quoteConcept cpt <+> (if plug then text "BYPLUG" else empty)
               <+> quote def <+> type_ <+> maybeText ref
        where type_ = if null typ then empty
                      else text "TYPE" <+> quote typ
              maybeText txt = if null txt then empty
                              else quote txt

instance Pretty P_Population where
    pretty p = case p of
                P_RelPopu nm    _ cs -> text "POPULATION" <+> maybeQuote (takeQuote nm)        <+> text "CONTAINS" <+> contents cs
                P_TRelPop nm tp _ cs -> text "POPULATION" <+> maybeQuote (takeQuote nm) <~> tp <+> text "CONTAINS" <+> contents cs
                P_CptPopu nm    _ ps -> text "POPULATION" <+> quoteConcept nm  <+> text "CONTAINS" <+> list (quoteAll ps)
               where contents = list . map prettyPair

instance Pretty P_Interface where
    pretty (P_Ifc name klass prms args roles obj _ _) =
        text "INTERFACE" <+> maybeQuote name <+> class_
               <+> params <+> labelArgs args <+> iroles
               <+> text ":" <~\> obj_ctx obj <~> obj_msub obj
                 where class_ = case klass of
                                     Nothing  -> empty
                                     Just str -> text "CLASS" <+> quoteConcept str
                       params = if null prms then empty
                                else parens $ listOf prms
                       iroles = if null roles then empty
                                else text "FOR" <+> listOf roles

instance Pretty a => Pretty (P_ObjDef a) where
    pretty (P_Obj nm _ ctx msub strs) =
        quote nm <+> labelArgs strs <+> text ":"
                 <~> ctx <~> msub

instance Pretty a => Pretty (P_SubIfc a) where
    pretty p = case p of
                P_Box _ c bs         -> box_type c <+> text "[" <> listOf bs <> text "]"
                P_InterfaceRef _ str -> text "INTERFACE" <+> maybeQuote str
            where box_type Nothing  = text "BOX"
                  box_type (Just x) = text x -- ROWS, COLS, TABS

instance Pretty P_IdentDef where
    pretty (P_Id _ lbl cpt ats) =
        text "IDENT" <+> maybeQuote lbl <+> text ":" <~> cpt <+> parens (listOf ats)

instance Pretty P_IdentSegment where
    pretty (P_IdentExp (P_Obj nm _ ctx _ strs)) =
              if null nm
              then pretty ctx -- no label
              else text nm <+> labelArgs strs <> text ":" <~> ctx

instance Pretty a => Pretty (P_ViewD a) where
    pretty (P_Vd _ lbl cpt ats) =
        text "VIEW" <+> maybeQuote lbl   <+> text ":"
                    <~> cpt <+> parens (listOf ats)

instance Pretty a => Pretty (P_ViewSegmt a) where
    pretty (P_ViewExp o)    = pretty $obj_ctx o
    pretty (P_ViewText txt) = text "TXT" <+> quote txt
    pretty (P_ViewHtml htm) = text "PRIMHTML" <+> quote htm
                        
instance Pretty PPurpose where
    pretty p = text "PURPOSE" <~> pexObj p <~> lang <+> refs (pexRefIDs p)
             <+\> quotePurpose (mString markup)
        where markup = pexMarkup p
              lang = mFormat markup
              refs rs = if null rs then empty
                        else text "REF" <+> quote (intercalate "; " rs)

instance Pretty PRef2Obj where
    pretty p = case p of
        PRef2ConceptDef str       -> text "CONCEPT"   <+> quoteConcept str
        PRef2Declaration termPrim -> text "RELATION"  <~> termPrim
        PRef2Rule str             -> text "RULE"      <+> maybeQuote str
        PRef2IdentityDef str      -> text "IDENT"     <+> maybeQuote str
        PRef2ViewDef str          -> text "VIEW"      <+> maybeQuote str
        PRef2Pattern str          -> text "PATTERN"   <+> maybeQuote str
        PRef2Process str          -> text "PROCESS"   <+> maybeQuote str
        PRef2Interface str        -> text "INTERFACE" <+> maybeQuote str
        PRef2Context str          -> text "CONTEXT"   <+> maybeQuote str
        PRef2Fspc str             -> text "PRef2Fspc" <+> maybeQuote str

instance Pretty PMeaning where
    pretty (PMeaning markup) = text "MEANING" <~> markup

instance Pretty PMessage where
    pretty (PMessage markup) = text "MESSAGE" <~> markup

instance Pretty P_Concept where
    pretty p = case p of
        PCpt _      -> quoteConcept$ p_cptnm p
        P_Singleton -> text "ONE"

instance Pretty P_Sign where
    pretty (P_Sign src tgt) = brackets (pretty src <> maybeTgt)
        where maybeTgt = if src `equal` tgt then empty
                         else text "*" <> pretty tgt
              equal (PCpt x) (PCpt y) = x == y
              equal P_Singleton P_Singleton = True
              equal _ _ = False

instance Pretty P_Gen where
    pretty p = case p of
            PGen spc gen _ -> text "CLASSIFY" <~> spc <+> text "ISA" <~> gen
            P_Cy spc rhs _ -> text "CLASSIFY" <~> spc <+> text "IS"  <+> separate "/\\" rhs

instance Pretty Lang where
    pretty Dutch   = text "IN DUTCH"
    pretty English = text "IN ENGLISH"

instance Pretty P_Markup where
    pretty (P_Markup lang format str) =
        pretty lang <~> format <+\> quotePurpose str

instance Pretty PandocFormat where
    pretty p = case p of
        ReST     -> text "REST"
        HTML     -> text "HTML"
        LaTeX    -> text "LATEX"
        Markdown -> text "MARKDOWN"

instance Pretty Label where
    pretty (Lbl nm _ strs) =
        text "LABEL?" <+> maybeQuote nm <+> listOfLists strs

instance Pretty Prop where
    pretty p = text $ case p of
                Uni -> "UNI"
                Inj -> "INJ"
                Sur -> "SUR"
                Tot -> "TOT"
                Sym -> "SYM"
                Asy -> "ASY"
                Trn -> "TRN"
                Rfx -> "RFX"
                Irf -> "IRF"
                Aut -> "AUT"
