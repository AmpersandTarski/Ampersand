{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Database.Design.Ampersand.Output.ToJSON.Relations 
  (Relations)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand
import Data.Maybe

data Relations = Relations [Relation]deriving (Generic, Show)
data Relation = Relation
  { relJSONname        :: String
  , relJSONsignature   :: String
  , relJSONsrcConcept  :: String
  , relJSONtgtConcept  :: String
  , relJSONuni         :: Bool
  , relJSONtot         :: Bool
  , relJSONinj         :: Bool
  , relJSONsur         :: Bool
  , relJSONaffectedConjuncts :: [String]
  , relJSONmysqlTable  :: RelTableInfo
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
instance JSON FSpec Relations where
 fromAmpersand fSpec _ = Relations (map (fromAmpersand fSpec) (allDecls fSpec))
instance JSON Declaration Relation where
 fromAmpersand fSpec dcl = Relation 
         { relJSONname       = name dcl
         , relJSONsignature  = name dcl ++ (show . sign) dcl
         , relJSONsrcConcept  = name . source $ dcl 
         , relJSONtgtConcept  = name . target $ dcl
         , relJSONuni      = isUni dcl
         , relJSONtot      = isTot dcl
         , relJSONinj      = isInj dcl
         , relJSONsur      = isSur dcl
         , relJSONaffectedConjuncts = map rc_id  $ fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)
         , relJSONmysqlTable = fromAmpersand fSpec dcl
         }
instance JSON Declaration RelTableInfo where
 fromAmpersand fSpec dcl = RelTableInfo
  { rtiJSONname    = name plug
  , rtiJSONtableOf = srcOrtgt
  , rtiJSONsrcCol  = fromAmpersand fSpec srcAtt
  , rtiJSONtgtCol  = fromAmpersand fSpec trgAtt
  }
   where (plug,srcAtt,trgAtt) = getDeclarationTableInfo fSpec dcl
         (plugSrc,_)          = getConceptTableInfo fSpec (source dcl)
         (plugTrg,_)          = getConceptTableInfo fSpec (target dcl)
         srcOrtgt
           | plug == plugSrc = Just "src"
           | plug == plugTrg = Just "tgt"
           | otherwise       = Nothing 
instance JSON SqlAttribute TableCol where
 fromAmpersand _ att = TableCol
  { tcJSONname   = attName att
  , tcJSONnull   = attNull att
  , tcJSONunique = attUniq att
  }


