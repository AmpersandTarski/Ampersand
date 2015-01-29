module Database.Design.Ampersand.ADL1.PrettyPrinters
where

import Database.Design.Ampersand.Core.ParseTree

class Pretty a where
    pretty :: a -> String

instance Pretty P_Context where
    pretty p = show p

instance Pretty Meta where
    pretty p = show p

instance Pretty MetaObj where
    pretty p = show p

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
    pretty p = show p

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
    pretty p = show p

instance Pretty PRef2Obj where
    pretty p = show p

instance Pretty PMeaning where
    pretty p = show p

instance Pretty PMessage where
    pretty p = show p

instance Pretty P_Concept where
    pretty p = show p

instance Pretty P_Sign where
    pretty p = show p

instance Pretty P_Gen where
    pretty p = show p

instance Pretty Lang where
    pretty p = show p

instance Pretty P_Markup where
    pretty p = show p

instance Pretty PandocFormat where
    pretty p = show p

instance Pretty Label where
    pretty p = show p

instance Pretty Prop where
    pretty p = show p
