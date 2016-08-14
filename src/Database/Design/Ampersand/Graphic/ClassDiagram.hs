module Ampersand.Graphic.ClassDiagram
         (ClassDiag(..), Class(..), CdAttribute(..), Association(..),
          Aggregation(..), Generalization(..), Deleting(..), Method(..),
          Multiplicities(..) , MinValue(..), MaxValue(..)
           ) where
import Data.List
import Ampersand.Basics
import Ampersand.FSpec --  hiding (Association,Box)

data ClassDiag = OOclassdiagram {cdName :: String
                                ,classes :: [Class]           --
                                ,assocs :: [Association]      --
                                ,aggrs ::  [Aggregation]      --
                                ,geners :: [Generalization]   --
                                ,ooCpts :: [A_Concept]}
                         deriving Show
instance Named ClassDiag where
   name = cdName

data Class          = OOClass  { clName :: String          -- ^ name of the class
                               , clcpt ::  Maybe (A_Concept) -- ^ Main concept of the class. (link tables do not have a main concept)
                               , clAtts :: [CdAttribute]   -- ^ Attributes of the class
                               , clMths :: [Method]        -- ^ Methods of the class
                               } deriving Show
instance Named Class where
   name = clName
data CdAttribute    = OOAttr   { attNm :: String            -- ^ name of the attribute
                               , attTyp :: String           -- ^ type of the attribute (Concept name or built-in type)
                               , attOptional :: Bool        -- ^ says whether the attribute is optional
                               } deriving Show
instance Named CdAttribute where
   name = attNm
data MinValue = MinZero | MinOne deriving (Show, Eq)

data MaxValue = MaxOne | MaxMany deriving (Show, Eq)

data Multiplicities = Mult MinValue MaxValue deriving Show

data Association    = OOAssoc  { assSrc ::     String           -- ^ source: the name of the source class
                               , assSrcPort :: String           -- ^ the name of the attribute in the source class
                               , asslhm ::     Multiplicities   -- ^ left hand side multiplicities
                               , asslhr ::     String           -- ^ left hand side role
                               , assTgt ::     String           -- ^ target: the name of the target class
                               , assrhm ::     Multiplicities   -- ^ right hand side multiplicities
                               , assrhr ::     String           -- ^ right hand side role
                               , assmdcl ::    Maybe Declaration -- ^ the declarations that caused this association , if any.
                               } deriving Show
data Aggregation    = OOAggr   { aggDel :: Deleting             --
                               , aggChild ::  A_Concept         --
                               , aggParent :: A_Concept         --
                               } deriving (Show, Eq)
data Generalization = OOGener  { genAgen :: A_Gen               --
                               } deriving (Show)

data Deleting       = Open | Close                      --
                                 deriving (Show, Eq)
data Method         = OOMethodC      String             -- name of this method, which creates a new object (producing a handle)
                                     [CdAttribute]      -- list of parameters: attribute names and types
                    | OOMethodR      String             -- name of this method, which yields the attribute values of an object (using a handle).
                                     [CdAttribute]      -- list of parameters: attribute names and types
                    | OOMethodS      String             -- name of this method, which selects an object using key attributes (producing a handle).
                                     [CdAttribute]      -- list of parameters: attribute names and types
                    | OOMethodU      String             -- name of this method, which updates an object (using a handle).
                                     [CdAttribute]      -- list of parameters: attribute names and types
                    | OOMethodD      String             -- name of this method, which deletes an object (using nothing but a handle).
                    | OOMethod       String             -- name of this method, which deletes an object (using nothing but a handle).
                                     [CdAttribute]      -- list of parameters: attribute names and types
                                     String             -- result: a type

instance Show Method where
  showsPrec _ (OOMethodC nm cs)  = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-cs]++"):handle")
  showsPrec _ (OOMethodR nm as)  = showString (nm++"(handle):["++intercalate "," [ n | OOAttr n _ _<-as]++"]")
  showsPrec _ (OOMethodS nm ks)  = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-ks]++"):handle")
  showsPrec _ (OOMethodD nm)     = showString (nm++"(handle)")
  showsPrec _ (OOMethodU nm cs)  = showString (nm++"(handle,"++intercalate "," [ n | OOAttr n _ _<-cs]++")")
  showsPrec _ (OOMethod nm cs r) = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-cs]++"): "++r)

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
