module Database.Design.Ampersand.ADL1.PrettyPrinters
where

import Database.Design.Ampersand.Core.ParseTree
import Data.Char
import Data.List (intercalate)
import Data.List.Utils (replace)

(<+>) :: String -> String -> String
(<+>) a b = a ++ space ++ b
          where space = if null a || isSpace(last a) then ""
                        else " "

(<~>) :: Pretty b => String -> b -> String
(<~>) a b = a <+> pretty b

(<+\>) :: String -> String -> String
(<+\>) a b = a ++ break_line ++ b
           where break_line = if null a || last a == '\n' then ""
                              else "\n"

(<~\>) :: Pretty b => String -> b -> String
(<~\>) a b = a <+\> (pretty b)

perline :: Pretty b => [b] -> String
perline bs = unlines (map pretty bs)

quoteWith :: String -> String -> String -> String
quoteWith q1 q2 str = q1 ++ str ++ q2

quote :: String -> String
quote str = quoteWith "\"" "\"" $ escape "\"" str
    where escape x = replace x ("\\" ++ x)

quoteAll :: [String] -> [String]
quoteAll = map quote

maybeQuote :: String -> String
maybeQuote a = if any isSpace a then quote a
               else a

prettyunwords :: Pretty a => [a] -> String
prettyunwords xs = unwords $ map pretty xs

listSep :: Pretty a => String -> [a] -> String
listSep sep xs = intercalate sep $ map pretty xs

commas :: [String] -> String
commas = intercalate ","

class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty (Maybe a) where
    pretty (Just x) = pretty x
    pretty Nothing = ""

instance Pretty P_Context where
    pretty p = "CONTEXT" <+> ctx_nm p <~> ctx_lang p
               <~> ctx_markup p
               <+\> perline (ctx_metas p)
               <+>  perline (ctx_ps p)
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
               <+\> perline (ctx_sql p)
               <+\> perline (ctx_php p)
               <+> unlines (quoteAll $ ctx_thms p)
               <+\> "ENDCONTEXT"

instance Pretty Meta where
    pretty p = "META" <~> mtObj p <+> quote (mtName p) <+> quote (mtVal p)

instance Pretty MetaObj where
    pretty _ = ""

instance Pretty P_Process where
    pretty p = "PROCESS" <+> procNm p <+\>
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
               "ENDPROCESS"

instance Pretty P_RoleRelation where
    pretty p = show p

instance Pretty RoleRule where
    pretty p = "ROLE" <+> id_list mRoles <+> "MAINTAINS" <+> id_list mRules
        where id_list prop = commas (map maybeQuote $ prop p)

instance Pretty P_Pattern where
    pretty p = show p

instance Pretty P_Declaration where
    -- pretty p = dec_nm p <+> "::" <~> dec_sign p <~> pFun <~> pConceptRef
    pretty p = "RELATION" <+> dec_nm p <~> dec_sign p <+> props <+> byplug <+\> pragma <+\> meanings <+\> content
        where props = "[" ++ (listSep "," $ dec_prps p) ++ "]"
              byplug = if (dec_plug p) then "BYPLUG" else ""
              pragma = if null (concat [dec_prL p, dec_prM p, dec_prR p]) then ""
                       else "PRAGMA" <+> quote (dec_prL p) <+> quote (dec_prM p) <+> quote (dec_prR p)
              meanings = prettyunwords (dec_Mean p)
              content = if null (dec_popu p) then ""
                        else "=" <+> intercalate "," (map prettyPair (dec_popu p))
              prettyPair :: (String,String) -> String
              prettyPair (a,b) = quote a <+> "*" <+> quote b

instance Pretty a => Pretty (Term a) where
   pretty p = case p of
       Prim a -> pretty a
       PEqu _ t1 t2 -> two t1 t2 "="
       PImp _ t1 t2 -> two t1 t2 " |- "
       PIsc _ t1 t2 -> two t1 t2 "/\\"
       PUni _ t1 t2 -> two t1 t2 "\\/"
       PDif _ t1 t2 -> two t1 t2 "-"
       PLrs _ t1 t2 -> two t1 t2 "/"
       PRrs _ t1 t2 -> two t1 t2 "\\"
       PDia _ t1 t2 -> two t1 t2 "<>"
       PCps _ t1 t2 -> two t1 t2 ";"
       PRad _ t1 t2 -> two t1 t2 "!"
       PPrd _ t1 t2 -> two t1 t2 "*"
       PKl0 _ t -> pos t "*"
       PKl1 _ t -> pos t "+"
       PFlp _ t -> pos t "~"
       PCpl _ t -> pre t "-"
       PBrk _ t -> "( " ++ pretty t ++ " )"
       where pos t op     = "" ++ pretty t ++ op ++ ""
             pre t op     = "" ++ op ++ pretty t ++ ""
             two t1 t2 op = "" ++ pretty t1 ++ op ++ pretty t2 ++ ""

instance Pretty TermPrim where
    pretty (PI _) = "I"
    pretty (Pid _ pConcept) = "I[" ++ pretty pConcept ++ "]"
    pretty (Patm _ str (Just pConcept)) = str ++ "[" ++ pretty pConcept ++ "]"
    pretty (Patm _ str Nothing) = str
    pretty (PVee _) = "V"
    pretty (Pfull _ s1 s2) = "V" <~> (P_Sign s1 s2)
    pretty (Prel _ str) = str
    pretty (PTrel _ str sign) = str <~> sign

instance Pretty a => Pretty (PairView a) where
    pretty _ = ""

instance Pretty a => Pretty (PairViewSegment a) where
    pretty _ = ""

instance Pretty a => Pretty (PairViewTerm a) where
    pretty _ = ""

instance Pretty a => Pretty (PairViewSegmentTerm a) where
    pretty _ = ""

instance Pretty SrcOrTgt where
    pretty p = case p of
                    Src -> "SRC"
                    Tgt -> "TGT"

instance Pretty a => Pretty (P_Rule a) where
    pretty p = "RULE" <+> name <~>
               rr_exp p <+\>
               perline (rr_mean p) <+\>
               perline (rr_msg p) <~\>
               rr_viol p
             where name = if null (rr_nm p) then ""
                          else (maybeQuote $ rr_nm p) ++ ":"

instance Pretty ConceptDef where
    pretty p = "CONCEPT" <+> maybeQuote (cdcpt p) <+> (if cdplug p then "BYPLUG" else "")
               <+> quote (cddef p) <+> type_ <+> quote (cdref p) -- cdfrom p
        where type_ = if null $ cdtyp p then ""
                      else "TYPE" <+> cdtyp p

instance Pretty P_Population where
    pretty p = show p

instance Pretty P_Interface where
    pretty p = "INTERFACE" <+> maybeQuote (ifc_Name p) <+> class_
               <+> params <+> args <+> roles -- ifc_Prp
               <+> ":" <~\> obj_ctx (ifc_Obj p) <~> obj_msub (ifc_Obj p)
                 where class_ = case ifc_Class p of
                                     Nothing  -> ""
                                     Just str -> "CLASS" <+> maybeQuote str
                       params = if null $ ifc_Params p then ""
                                else "(" <+> listSep "," (ifc_Params p) <+> ")"
                       args = if null $ ifc_Args p then ""
                              else "{" <+> (commas $ map (unwords.quoteAll) (ifc_Args p)) <+> "}"
                       roles = if null $ ifc_Roles p then ""
                               else "FOR" <+> (commas . quoteAll $ ifc_Roles p)

instance Pretty P_IClass where
    pretty p = show p

instance Pretty a => Pretty (P_ObjDef a) where
    pretty p = maybeQuote (obj_nm p) <+> args <+> ":"
            <~> obj_ctx p <~> obj_msub p
           where args = if null $ obj_strs p then ""
                        else "{" <+> (commas $ map (unwords.quoteAll) (obj_strs p)) <+> "}"

instance Pretty a => Pretty (P_SubIfc a) where
    pretty p = case p of
                P_Box _ bs -> type_ <+> "[" ++ listSep "," bs ++ "]"
                P_InterfaceRef _ str -> "INTERFACE" <+> maybeQuote str
            where type_ = "BOX" -- "ROWS" or "COLS" too?

instance Pretty P_IdentDef where
    pretty p = show p

instance Pretty P_IdentSegment where
    pretty p = show p

instance Pretty a => Pretty (P_ViewD a) where
    pretty _ = ""

instance Pretty a => Pretty (P_ViewSegmt a) where
    pretty _ = ""

instance Pretty PPurpose where
    pretty p = "PURPOSE" <~> pexObj p <~> lang <+> refs (pexRefIDs p)
             <+> quoteWith "{+" "-}" (mString markup)
        where markup = pexMarkup p
              lang = mFormat markup
              refs rs = unlines (map format rs)
              format r = "REF" <+> maybeQuote r

instance Pretty PRef2Obj where
    pretty p = case p of
        PRef2ConceptDef str       -> "CONCEPT"   <+> maybeQuote str
        PRef2Declaration termPrim -> "RELATION"  <~> termPrim
        PRef2Rule str             -> "RULE"      <+> maybeQuote str
        PRef2IdentityDef str      -> "IDENT"     <+> maybeQuote str
        PRef2ViewDef str          -> "VIEW"      <+> maybeQuote str
        PRef2Pattern str          -> "PATTERN"   <+> maybeQuote str
        PRef2Process str          -> "PROCESS"   <+> maybeQuote str
        PRef2Interface str        -> "INTERFACE" <+> maybeQuote str
        PRef2Context str          -> "CONTEXT"   <+> maybeQuote str
        PRef2Fspc str             -> "PRef2Fspc" <+> maybeQuote str

instance Pretty PMeaning where
    pretty (PMeaning markup) = "MEANING" <~> markup

instance Pretty PMessage where
    pretty (PMessage markup) = "MESSAGE" <~> markup

instance Pretty P_Concept where
    pretty p = case p of
        PCpt _      -> quote$ p_cptnm p
        P_Singleton -> "ONE"

instance Pretty P_Sign where
    pretty p = "[" ++ src ++ tgt ++ "]"
        where src = pretty $ pSrc p
              tgt = if pSrc p `equal` pTgt p then ""
                    else "*" ++ pretty (pTgt p)
              equal (PCpt x) (PCpt y) = x == y
              equal P_Singleton P_Singleton = True
              equal _ _ = False

instance Pretty P_Gen where
    pretty p = show p

instance Pretty Lang where
    pretty Dutch   = "IN DUTCH"
    pretty English = "IN ENGLISH"

instance Pretty P_Markup where
    pretty p = pretty (mLang p) <~> mFormat p <+\> quoteWith "{+\n" "-}\n" (mString p)

--instance Pretty ContextElement where
--    pretty p = case p of
--                    CMeta x    -> pretty meta
--                    CPat x     -> pretty pattern
--                    CPrc x     -> pretty x
--                    CRul x     -> pretty x
--                    CCfy x     -> pretty x
--                    CRel x     -> pretty x
--                    CCon f     -> pretty (f $ "CONTEXT " ++ nm)
--                    CGen x     -> pretty x
--                    CIndx x    -> pretty x
--                    CView x    -> pretty x
--                    Cifc x     -> pretty x
--                    CSqlPlug x -> pretty x
--                    CPhpPlug x -> pretty x
--                    CPrp x     -> pretty x
--                    CPop x     -> pretty x
--                    CThm xs    -> if null xs then "" else "THEMES" <+> commas xs
--                    CIncl x    -> "INCLUDE" <+> x

instance Pretty PandocFormat where
    pretty p = case p of
        ReST     -> "REST"
        HTML     -> "HTML"
        LaTeX    -> "LATEX"
        Markdown -> "MARKDOWN"

instance Pretty Label where
    pretty p = show p

instance Pretty Prop where
    pretty p = show p
