module Ampersand.Graphic.ClassDiagram
  ( ClassDiag (..),
    Class (..),
    CdAttribute (..),
    Association (..),
    Aggregation (..),
    Generalization (..),
    Deleting (..),
    Method (..),
    Multiplicities (..),
    MinValue (..),
    MaxValue (..),
  )
where

import Ampersand.ADL1
  ( AClassify,
    AProp (..),
    A_Concept,
    Relation,
    TType (..),
  )
import Ampersand.Basics
import qualified RIO.Text as T

data ClassDiag = OOclassdiagram
  { cdName :: !Name,
    -- | list of classes with the optional name of a subgraph in which they belong
    classes :: ![(Class, Maybe Name)],
    assocs :: ![Association], --
    geners :: ![Generalization], --
    ooCpts :: ![A_Concept]
  }
  deriving (Show)

instance Named ClassDiag where
  name = cdName

data Class = OOClass
  { -- | name of the class
    clName :: !Name,
    -- | Main concept of the class. (link tables do not have a main concept)
    clcpt :: !(Maybe (A_Concept, TType)),
    -- | Attributes of the class
    clAtts :: ![CdAttribute]
  }
  deriving (Eq, Show)

instance Named Class where
  name = name . clName

data CdAttribute = OOAttr
  { -- | name of the attribute
    attNm :: !Name,
    -- | type of the attribute (Concept name or built-in type)
    attTyp :: !Name,
    -- | says whether the attribute is optional
    attOptional :: !Bool,
    attProps :: ![AProp]
  }
  deriving (Show, Eq)

instance Named CdAttribute where
  name = attNm

data MinValue = MinZero | MinOne deriving (Show, Eq)

data MaxValue = MaxOne | MaxMany deriving (Show, Eq)

data Multiplicities = Mult MinValue MaxValue deriving (Show)

data Association = OOAssoc
  { -- | source: the name of the source class
    assSrc :: !Name,
    -- | the name of the attribute in the source class
    assSrcPort :: !Name,
    -- | left hand side properties
    asslhm :: !Multiplicities,
    -- | left hand side role, if it exists
    asslhr :: !(Maybe Name),
    -- | target: the name of the target class
    assTgt :: !Name,
    -- | right hand side properties
    assrhm :: !Multiplicities,
    -- | right hand side role, if it exists
    assrhr :: !(Maybe Name),
    -- | the relation that caused this association , if any.
    assmdcl :: !(Maybe Relation)
  }
  deriving (Show)

data Aggregation = OOAggr
  { aggDel :: Deleting, --
    aggChild :: A_Concept, --
    aggParent :: A_Concept --
  }
  deriving (Show, Eq)

newtype Generalization = OOGener
  { genAgen :: AClassify --
  }
  deriving (Show)

data Deleting = Open | Close --
  deriving (Show, Eq)

data Method
  = OOMethodC
      !Name -- name of this method, which creates a new object (producing a handle)
      [CdAttribute] -- list of parameters: attribute names and types
  | OOMethodR
      !Name -- name of this method, which yields the attribute values of an object (using a handle).
      [CdAttribute] -- list of parameters: attribute names and types
  | OOMethodS
      !Name -- name of this method, which selects an object using key attributes (producing a handle).
      [CdAttribute] -- list of parameters: attribute names and types
  | OOMethodU
      !Name -- name of this method, which updates an object (using a handle).
      [CdAttribute] -- list of parameters: attribute names and types
  | OOMethodD !Name -- name of this method, which deletes an object (using nothing but a handle).
  | OOMethod
      !Name -- name of this method, which deletes an object (using nothing but a handle).
      [CdAttribute] -- list of parameters: attribute names and types
      !Name -- result: a type
  deriving (Eq)

instance Show Method where
  show (OOMethodC nm cs) = T.unpack $ fullName nm <> "(" <> T.intercalate "," [fullName n | OOAttr n _ _ _ <- cs] <> "):handle"
  show (OOMethodR nm as) = T.unpack $ fullName nm <> "(handle):[" <> T.intercalate "," [fullName n | OOAttr n _ _ _ <- as] <> "]"
  show (OOMethodS nm ks) = T.unpack $ fullName nm <> "(" <> T.intercalate "," [fullName n | OOAttr n _ _ _ <- ks] <> "):handle"
  show (OOMethodD nm) = T.unpack $ fullName nm <> "(handle)"
  show (OOMethodU nm cs) = T.unpack $ fullName nm <> "(handle," <> T.intercalate "," [fullName n | OOAttr n _ _ _ <- cs] <> ")"
  show (OOMethod nm cs r) = T.unpack $ fullName nm <> "(" <> T.intercalate "," [fullName n | OOAttr n _ _ _ <- cs] <> "): " <> fullName r

--
--   testCD
--    = OOclassdiagram
--      [ OOClass "Plan" [ooAttr "afkomst" "Actor"] []
--      , OOClass "Formulier" [ooAttr "plan" "Plan",ooAttr "van" "Actor",ooAttr "aan" "Actor",ooAttr "sessie" "Sessie"] []
--      , OOClass "Dossier" [ooAttr "eigenaar" "Actor"] []
--      , OOClass "Gegeven" [ooAttr "type" "Gegevenstype",ooAttr "in" "Dossier",ooAttr "veldnaam" "Veldnaam",ooAttr "waarde" "Waarde"] []
--      , OOClass "Veld" [ooAttr "type" "Veldtype",ooAttr "waarde" "Waarde"] []
--      , OOClass "Veldtype" [ooAttr "veldnaam" "Veldnaam",ooAttr "formuliertype" "Plan",ooAttr "gegevenstype" "Gegevenstype"] []
--      , OOClass "Sessie" [ooAttr "dossier" "Dossier",ooAttr "uitgevoerd" "Actor"] []
--      ]
--      [ OOAssoc "Plan" "0..n" "" "Plan" "0..n" "stap"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzage"
--      , OOAssoc "Formulier" "0..n" "" "Formulier" "0..n" "in"
--      , OOAssoc "Formulier" "0..n" "" "Plan" "0..n" "stap"
--      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "aan"
--      , OOAssoc "Gegeven" "0..n" "" "Formulier" "0..n" "op"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzage"
--      , OOAssoc "Actor" "0..n" "" "Actor" "0..n" "gedeeld"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzagerecht"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzagerecht"
--      , OOAssoc "Autorisatie" "0..n" "" "Gegeven" "0..n" "object"
--      , OOAssoc "Actie" "0..n" "" "Gegeven" "0..n" "object"
--      , OOAssoc "Autorisatie" "0..n" "" "Actie" "0..n" "op"
--      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "door"
--      , OOAssoc "Actie" "0..n" "" "Actor" "0..n" "door"
--      , OOAssoc "Veld" "0..n" "" "Gegeven" "0..n" "bindt"
--      , OOAssoc "Sessie" "0..1" "" "Actor" "0..1" "actief"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "openstaand"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "openstaand"
--      ]
--      [ OOAggr Close "Dossier" "Formulier"
--      , OOAggr Close "Formulier" "Veld"
--      ]
--      []
--      ("NoPat",[])
--      where ooAttr nm t = OOAttr nm t True
