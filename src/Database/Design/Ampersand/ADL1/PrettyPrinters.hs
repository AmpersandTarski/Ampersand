{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Design.Ampersand.ADL1.PrettyPrinters(prettyPrint)
where

import Text.PrettyPrint.Leijen
import Database.Design.Ampersand.Basics        (fatalMsg)
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Lexer(keywords)
import Data.List (intercalate,intersperse)
import Data.List.Utils (replace)
import Data.Char (toUpper)

fatal :: Int -> String -> a
fatal = fatalMsg "PrettyPrinters"

prettyPrint :: Pretty a => a -> String
prettyPrint x = displayS (renderPretty rfrac col_width doc) ""
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

--quoteWith :: String -> String -> String -> Doc
--quoteWith l r x = enclose (text l) (text r) (text x)

quote :: String -> Doc
quote = text.show
--singlequote :: String -> Doc
--singlequote = squotes.text.escapeAll
--escapeAll :: [Char] -> [Char]
--escapeAll = escapeQuote.escapeBreaklines.escapeSlash
--        where escapeQuote = escape "\""
--              escapeBreaklines = replace "\n" "\\n"
--              escapeSlash = escape "\\"
--              escape x = replace x ("\\" ++ x)

quoteAll :: [String] -> [Doc]
quoteAll = map quote

quotePurpose :: String -> Doc
quotePurpose p = text "{+" </> escapeExpl p </> text "-}"
        where escapeExpl = text.escapeCommentStart.escapeLineComment.escapeExplEnd
              escapeCommentStart = escape "{-"
              escapeLineComment = escape "--"
              escapeExplEnd = escape "-}"
              escape x = replace x (intersperse ' ' x)

isId :: String -> Bool
isId a = not (null a) && all isIdChar a && isFirstIdChar(head a) && a `notElem` keywords
       where isFirstIdChar x = elem x $ "_"++['a'..'z']++['A'..'Z']
             isIdChar x = isFirstIdChar x || elem x ['0'..'9']

isUpperId :: String -> Bool
isUpperId xs = isId xs && head xs `elem` ['A'..'Z']

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

listOfLists :: [[String]] -> Doc
listOfLists xs = commas $ map (hsep.quoteAll) xs

separate :: Pretty a => String -> [a] -> Doc
separate d xs = encloseSep empty empty (text d) $ map pretty xs

--TODO: This replace shouldn't be necessary, I don't know why quotes are getting into the Prel
-- Example to test: AmpersandData\FormalAmpersand\AST.adl
takeQuote :: String -> String
takeQuote = replace "\"" ""

labelArgs :: [[String]] -> Doc
labelArgs args = if null args || all null args
                 then empty
                 else braces $ listOfLists args

prettyLabel :: String -> [[String]] -> Doc
prettyLabel nm strs = maybeQuote nm <+> labelArgs strs

instance Pretty P_Context where
    pretty (PCtx nm _ lang markup thms pats rs ds cs ks rrules rrels reprs vs gs ifcs ps pops sql php metas) =
               text "CONTEXT"
               <+> quoteConcept nm
               <~> lang
               <~> markup
               <+\> perline metas
               <+\> themes
               <+\> perline ps
               <+\> perline pats
               <+\> perline rs
               <+\> perline ds
               <+\> perline cs
               <+\> perline ks
               <+\> perline rrules
               <+\> perline rrels
               <+\> perline reprs
               <+\> perline vs
               <+\> perline gs
               <+\> perline ifcs
               <+\> perline pops
               <+\> perlinePrefix "SQLPLUG" sql
               <+\> perlinePrefix "PHPPLUG" php
               <+\> text "ENDCONTEXT"
             where themes | null thms = empty
                          | otherwise = text "THEMES" <+> commas (map quoteConcept thms)

instance Pretty Meta where
    pretty (Meta _ obj name val) =
        text "META" <~> obj <+> quote name <+> quote val

instance Pretty MetaObj where
    pretty ContextMeta = empty -- for the context meta we don't need a keyword

instance Pretty P_RoleRelation where
    pretty (P_RR _ roles rels) =
        text "ROLE" <+> listOf roles <+> text "EDITS" <+> listOf rels

instance Pretty P_RoleRule where
    pretty (Maintain _ roles rules) =
        text "ROLE" <+> listOf roles <+> text "MAINTAINS" <+> commas (map maybeQuote rules)

instance Pretty Role where
    pretty (Role name) = maybeQuote name
    pretty (Service name) = maybeQuote name

instance Pretty P_Pattern where
    pretty (P_Pat _ nm rls gns dcs rruls rrels reprs cds ids vds xps pop _) =
          text keyword
          <+>  quoteConcept nm
          <+\> perline rls
          <+\> perline gns
          <+\> perline dcs
          <+\> perline rruls
          <+\> perline rrels
          <+\> perline reprs
          <+\> perline cds
          <+\> perline ids
          <+\> perline vds
          <+\> perline xps
          <+\> perline pop
          <+>  text ("END"++keyword)
        where keyword = if null rruls && null rrels then "PATTERN" else "PROCESS"

instance Pretty P_Declaration where
    pretty (P_Sgn nm sign prps pragma mean popu _ plug) =
        text "RELATION" <+> text nm <~> sign <+> props <+> byplug <+\> pragmas <+\> prettyhsep mean <+\> content
        where props   = if prps == [Sym, Asy] then text "[PROP]"
                        else text "[" <> listOf prps <> text "]"
              byplug  | plug        = text "BYPLUG"
                      | otherwise   = empty
              pragmas | null pragma = empty
                      | otherwise   = text "PRAGMA" <+> hsep (map quote pragma)
              content | null popu   = empty
                      | otherwise   = text "=\n[" <+> commas (map pretty popu) <+> text "]"

instance Pretty a => Pretty (Term a) where
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
        Patm _ val (Just concept) -> text (show val) <> text "[" <> pretty concept <> text "]"
        Patm _ val Nothing        -> text (show val) 
        PVee _ -> text "V"
        Pfull _ s1 s2 -> text "V" <~> P_Sign s1 s2
        PNamedR rel -> pretty rel

--instance Pretty PSingleton where
--    pretty = text . show
instance Pretty P_NamedRel where
    pretty (PNamedRel _ str mpSign) = text (takeQuote str) <~> mpSign

instance Pretty a => Pretty (PairView a) where
    pretty (PairView ss) = text "VIOLATION" <+> parens (listOf ss)

instance Pretty a => Pretty (PairViewSegment a) where
    pretty (PairViewText _ str) = text "TXT" <+> quote str
    pretty (PairViewExp _ srcTgt term) = pretty srcTgt <~> term

instance Pretty SrcOrTgt where
    pretty Src = text "SRC"
    pretty Tgt = text "TGT"

instance Pretty a => Pretty (P_Rule a) where
    pretty (P_Ru _ nm expr mean msg viol) =
                text "RULE" <+> name <~>
                expr <+\>
                perline mean <+\>
                perline msg <~\>
                viol
            where name = if null nm then empty
                         else maybeQuote nm <> text ":"

instance Pretty ConceptDef where
    pretty (Cd _ cpt plug def ref _) -- from, the last argument, is not used in the parser
        = text "CONCEPT" <+> quoteConcept cpt <+> (if plug then text "BYPLUG" else empty)
               <+> quote def <+> maybeText ref
        where maybeText txt = if null txt then empty
                              else quote txt

instance Pretty P_Population where
    pretty p = case p of
                P_RelPopu _ _ _ nrel  cs -> text "POPULATION" <+> pretty nrel      <+> text "CONTAINS" <+> contents cs
                P_CptPopu _ nm    ps -> text "POPULATION" <+> quoteConcept nm  <+> text "CONTAINS" <+> pretty ps
               where contents = list . map pretty

instance Pretty Representation where
    pretty (Repr _ cs tt) = text "REPRESENT" <+> listOf cs <~> text "TYPE" <+> pretty tt

instance Pretty TType where
    pretty = text . show
      
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
    pretty (P_Obj nm _ ctx mCrud mView msub strs) =
        prettyLabel nm strs <+> text ":"
                 <~> ctx <+> crud mCrud <+> view mView <~> msub
        where crud Nothing = empty
              crud (Just cruds) = pretty cruds
              view Nothing  = empty
              view (Just v) = text ("<" ++ v ++ ">")
instance Pretty P_Cruds where
    pretty (P_Cruds _ str) = text str
instance Pretty a => Pretty (P_SubIfc a) where
    pretty p = case p of
                P_Box _ c bs         -> box_type c <+> text "[" <> listOf bs <> text "]"
                P_InterfaceRef _ isLink str -> text ((if isLink then "LINKTO "else "")++"INTERFACE") <+> maybeQuote str
            where box_type Nothing  = text "BOX"
                  box_type (Just x) = text x -- ROWS, COLS, TABS

instance Pretty a => Pretty (P_IdentDf a) where
    pretty (P_Id _ lbl cpt ats) =
        text "IDENT" <+> maybeQuote lbl <+> text ":" <~> cpt <+> parens (listOf ats)

instance Pretty a => Pretty (P_IdentSegmnt a) where
    pretty (P_IdentExp (P_Obj nm _ ctx _ mView _ strs)) =
              if null nm
              then pretty ctx -- no label
              else prettyLabel nm strs <> text ":" <~> ctx <+> view mView
        where view Nothing  = empty
              view (Just v) = pretty v

instance Pretty a => Pretty (P_ViewD a) where
    pretty (P_Vd _ lbl cpt True Nothing ats) = -- legacy syntax
        text "VIEW" <+> maybeQuote lbl   <+> text ":"
                    <~> cpt <+> parens (listOf ats)
    pretty (P_Vd _ lbl cpt isDefault html ats) = -- new syntax
        text "VIEW" <+> maybeQuote lbl  <+> text ":"
                    <~> cpt <+> (if isDefault then text "DEFAULT" else empty)
                    <+> braces (listOf ats) <~> html <+> text "ENDVIEW"

instance Pretty ViewHtmlTemplate where
    pretty (ViewHtmlTemplateFile str) = text "HTML" <+> text "TEMPLATE" <+> quote str

instance Pretty a => Pretty (P_ViewSegmt a) where
    pretty (P_ViewExp _ (P_Obj nm _ ctx _ _ _ _))
                              = maybeQuote nm <+> text ":" <~> ctx
    pretty (P_ViewText _ txt) = text "TXT" <+> quote txt
    pretty (P_ViewHtml _ htm) = text "PRIMHTML" <+> quote htm
                        
instance Pretty PPurpose where
    pretty (PRef2 _ obj markup refIds) =
             text "PURPOSE" <~> obj <~> lang <+> refs refIds
             <+\> quotePurpose (mString markup)
        where lang = mFormat markup
              refs rs = if null rs then empty
                        else text "REF" <+> quote (intercalate "; " rs)

instance Pretty PRef2Obj where
    pretty p = case p of
        PRef2ConceptDef str       -> text "CONCEPT"   <+> quoteConcept str
        PRef2Declaration namedRel -> text "RELATION"  <~> namedRel
        PRef2Rule str             -> text "RULE"      <+> maybeQuote str
        PRef2IdentityDef str      -> text "IDENT"     <+> maybeQuote str
        PRef2ViewDef str          -> text "VIEW"      <+> maybeQuote str
        PRef2Pattern str          -> text "PATTERN"   <+> maybeQuote str
        PRef2Interface str        -> text "INTERFACE" <+> maybeQuote str
        PRef2Context str          -> text "CONTEXT"   <+> maybeQuote str

instance Pretty PMeaning where
    pretty (PMeaning markup) = text "MEANING" <~> markup

instance Pretty PMessage where
    pretty (PMessage markup) = text "MESSAGE" <~> markup

instance Pretty P_Concept where
    pretty (PCpt name) = quoteConcept name
    pretty P_Singleton = text "ONE"

instance Pretty P_Sign where
    pretty (P_Sign src tgt) = brackets (pretty src <> maybeTgt)
        where maybeTgt = if src `equal` tgt then empty
                         else text "*" <> pretty tgt
              equal (PCpt x) (PCpt y) = x == y
              equal P_Singleton P_Singleton = True
              equal _ _ = False

instance Pretty P_Gen where
    pretty p = case p of
            PGen _ spc gen -> text "CLASSIFY" <~> spc <+> text "ISA" <~> gen
            P_Cy _ spc rhs -> text "CLASSIFY" <~> spc <+> text "IS"  <+> separate "/\\" rhs

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
                Prop -> "PROP"

instance Pretty PAtomPair where
    pretty (PPair _ l r) = text "(" <+> pretty l 
                       <~> text "," <+> pretty r 
                       <~> text ")"

instance Pretty PAtomValue where
    pretty pav =  
      case pav of 
       PSingleton   _ _ mav -> case mav of
                                Nothing  -> fatal 405 $ "The singleton "++show pav++" has no type, so it cannot be accuratly prettyprinted in a population statement."
                                Just val -> pretty val
       ScriptString   _ s -> text . show $ s
       XlsxString     _ s -> text . show $ s
       ScriptInt      _ i -> text . show $ i
       ScriptFloat    _ d -> text . show $ d
       XlsxDouble     _ _ -> fatal 267 $ "We got a value from an .xlsx file, which has to be shown in an expression, however the technicaltype is not known"
       ComnBool       _ b -> text . map toUpper . show $ b
       ScriptDate     _ x -> text . show $ x
       ScriptDateTime _ x -> text . show $ x




