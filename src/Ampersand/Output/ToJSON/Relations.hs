{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Ampersand.Output.ToJSON.Relations 
  (Relations)
where
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Core.AbstractSyntaxTree 
import Ampersand.FSpec.FSpecAux
import Data.Maybe

data Relations = Relations [Relation]deriving (Generic, Show)
data Relation = Relation
  { relJSONname         :: String
  , relJSONsignature    :: String
  , relJSONsrcConceptId :: String
  , relJSONtgtConceptId :: String
  , relJSONuni          :: Bool
  , relJSONtot          :: Bool
  , relJSONinj          :: Bool
  , relJSONsur          :: Bool
  , relJSONprop         :: Bool
  , relJSONaffectedConjuncts :: [String]
  , relJSONmysqlTable   :: RelTableInfo
  } deriving (Generic, Show)
data RelTableInfo = RelTableInfo -- Contains info about where the relation is implemented in SQL
  { rtiJSONname    :: String
  , rtiJSONtableOf :: Maybe String -- specifies if relation is administrated in table of srcConcept (i.e. "src"), tgtConcept (i.e. "tgt") or its own n-n table (i.e. null).
  , rtiJSONsrcCol  :: TableCol
  , rtiJSONtgtCol  :: TableCol
  } deriving (Generic, Show)
data TableCol = TableCol
  { tcJSONname     :: String
  , tcJSONnull     :: Bool
  , tcJSONunique   :: Bool
  } deriving (Generic, Show)
instance ToJSON Relations where
  toJSON = amp2Jason
instance ToJSON Relation where
  toJSON = amp2Jason
instance ToJSON RelTableInfo where
  toJSON = amp2Jason
instance ToJSON TableCol where
  toJSON = amp2Jason
instance JSON MultiFSpecs Relations where
 fromAmpersand multi _ = Relations (map (fromAmpersand multi) (vrels (userFSpec multi)))
instance JSON Declaration Relation where
 fromAmpersand multi dcl = Relation 
         { relJSONname       = name dcl
         , relJSONsignature  = name dcl ++ (show . sign) dcl
         , relJSONsrcConceptId  = escapeIdentifier . name . source $ dcl 
         , relJSONtgtConceptId  = escapeIdentifier . name . target $ dcl
         , relJSONuni      = isUni dcl
         , relJSONtot      = isTot dcl
         , relJSONinj      = isInj dcl
         , relJSONsur      = isSur dcl
         , relJSONprop     = isProp dcl
         , relJSONaffectedConjuncts = map rc_id  $ fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)
         , relJSONmysqlTable = fromAmpersand multi dcl
         }
      where fSpec = userFSpec multi
         
instance JSON Declaration RelTableInfo where
 fromAmpersand multi dcl = RelTableInfo
  { rtiJSONname    = name plug
  , rtiJSONtableOf = srcOrtgt
  , rtiJSONsrcCol  = fromAmpersand multi srcAtt
  , rtiJSONtgtCol  = fromAmpersand multi trgAtt
  }
   where fSpec = userFSpec multi
         (plug,srcAtt,trgAtt) = getDeclarationTableInfo fSpec dcl
         (plugSrc,_)          = getConceptTableInfo fSpec (source dcl)
         (plugTrg,_)          = getConceptTableInfo fSpec (target dcl)
         srcOrtgt
           | plug == plugSrc = Just "src"
           | plug == plugTrg = Just "tgt"
           | otherwise       = Nothing 
instance JSON SqlAttribute TableCol where
 fromAmpersand _ att = TableCol
  { tcJSONname   = attName att
  , tcJSONnull   = attDBNull att
  , tcJSONunique = attUniq att
  }


