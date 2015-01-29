module Database.Design.Ampersand.ADL1.PrettyPrinters
where

import Database.Design.Ampersand.Core.ParseTree

(<+>) :: String -> String -> String
(<+>) a b = a ++ " " ++ b

(<~>) :: Pretty b => String -> b -> String
(<~>) a b = a <+> pretty b

(<+\>) :: String -> String -> String
(<+\>) a b = a ++ "\n" ++ b

(<~\>) :: Pretty b => String -> b -> String
(<~\>) a b = a <+\> (pretty b)

perline :: Pretty b => [b] -> String
perline bs = unlines (map pretty bs)

quoteWith :: Pretty a => String -> String -> (a -> String) -> a -> String
quoteWith q1 q2 f p = q1 ++ (f p) ++ q2

quote :: Pretty a => (a -> String) -> a -> String
quote = quoteWith "\"" "\""

class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty (Maybe a) where
    pretty (Just x) = pretty x
    pretty Nothing = ""

instance Pretty P_Context where
    pretty p = "CONTEXT" <+> ctx_nm p <~> ctx_lang p <+\>
               perline (ctx_metas p) <+>
               perline (ctx_ps p)
         --, ctx_markup :: Maybe PandocFormat  -- ^ The default markup format for free text in this context
         --, ctx_thms ::   [String]         -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         --, ctx_pats ::   [P_Pattern]      -- ^ The patterns defined in this context
         --, ctx_PPrcs ::  [P_Process]      -- ^ The processes as defined by the parser
         --, ctx_rs ::     [(P_Rule TermPrim)]         -- ^ All user defined rules in this context, but outside patterns and outside processes
         --, ctx_ds ::     [P_Declaration]  -- ^ The relations defined in this context, outside the scope of patterns
         --, ctx_cs ::     [ConceptDef]     -- ^ The concept definitions defined in this context, outside the scope of patterns
         --, ctx_ks ::     [P_IdentDef]     -- ^ The identity definitions defined in this context, outside the scope of patterns
         --, ctx_vs ::     [P_ViewDef]      -- ^ The view definitions defined in this context, outside the scope of patterns
         --, ctx_gs ::     [P_Gen]          -- ^ The gen definitions defined in this context, outside the scope of patterns
         --, ctx_ifcs ::   [P_Interface]    -- ^ The interfaces defined in this context
         --,        -- ^ The purposes defined in this context, outside the scope of patterns and processes
         --, ctx_pops ::   [P_Population]   -- ^ The populations defined in this context
         --, ctx_sql ::    [P_ObjectDef]    -- ^ user defined sqlplugs, taken from the Ampersand script
         --, ctx_php ::    [P_ObjectDef]    -- ^ user defined phpplugs, taken from the Ampersand script
         --, 

instance Pretty Meta where
    pretty p = "META" <~> mtObj p <+> quote mtName p <+> quote mtVal p

instance Pretty MetaObj where
    pretty _ = ""

instance Pretty P_Process where
    pretty p = show p

instance Pretty P_RoleRelation where
    pretty p = show p

instance Pretty RoleRule where
    pretty p = show p

instance Pretty P_Pattern where
    pretty p = show p

instance Pretty P_Declaration where
    pretty p = show p

instance Pretty (Term a) where
    pretty _ = ""

instance Pretty TermPrim where
    pretty (PI _) = "I"
    pretty (Pid _ pConcept) = "I[" ++ pretty pConcept ++ "]"
    pretty (Patm _ str (Just pConcept)) = str ++ "[" ++ pretty pConcept ++ "]"
    pretty (Patm _ str Nothing) = str
    pretty (PVee _) = "V"
    pretty (Pfull _ s1 s2) = "V" <~> (P_Sign s1 s2)
    pretty (Prel _ str) = str
    pretty (PTrel _ str sign) = str <~> sign

instance Pretty (PairView a) where
    pretty _ = ""

instance Pretty (PairViewSegment a) where
    pretty _ = ""

instance Pretty (PairViewTerm a) where
    pretty _ = ""

instance Pretty (PairViewSegmentTerm a) where
    pretty _ = ""

instance Pretty SrcOrTgt where
    pretty p = show p

instance Pretty (P_Rule a) where
    pretty _ = ""

instance Pretty ConceptDef where
    pretty p = show p

instance Pretty P_Population where
    pretty p = show p

instance Pretty P_Interface where
    pretty p = show p

instance Pretty P_IClass where
    pretty p = show p

instance Pretty (P_ObjDef a) where
    pretty _ = ""

instance Pretty (P_SubIfc a) where
    pretty _ = ""

instance Pretty P_IdentDef where
    pretty p = show p

instance Pretty P_IdentSegment where
    pretty p = show p

instance Pretty (P_ViewD a) where
    pretty _ = ""

instance Pretty (P_ViewSegmt a) where
    pretty _ = ""

instance Pretty PPurpose where
    pretty p = "PURPOSE" <~> pexObj p <~> pexMarkup p <+> unlines (pexRefIDs p)

instance Pretty PRef2Obj where
    pretty (PRef2ConceptDef str)       = "CONCEPT"   <+> str
    pretty (PRef2Declaration termPrim) = "RELATION"  <~> termPrim
    pretty (PRef2Rule str)             = "RULE"      <+> str
    pretty (PRef2IdentityDef str)      = "IDENT"     <+> str
    pretty (PRef2ViewDef str)          = "VIEW"      <+> str
    pretty (PRef2Pattern str)          = "PATTERN"   <+> str
    pretty (PRef2Process str)          = "PROCESS"   <+> str
    pretty (PRef2Interface str)        = "INTERFACE" <+> str
    pretty (PRef2Context str)          = "CONTEXT"   <+> str
    pretty (PRef2Fspc str)             = "PRef2Fspc" <+> str

instance Pretty PMeaning where
    pretty p = show p

instance Pretty PMessage where
    pretty p = show p

instance Pretty P_Concept where
    pretty p = show p

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
    pretty p = pretty (mLang p) <~> mFormat p <+\> quoteWith "{+\n" "-}\n" mString p

instance Pretty PandocFormat where
    pretty p = show p

instance Pretty Label where
    pretty p = show p

instance Pretty Prop where
    pretty p = show p
