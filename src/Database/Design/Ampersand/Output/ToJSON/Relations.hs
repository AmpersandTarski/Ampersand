{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Database.Design.Ampersand.Output.ToJSON.Relations 
  (Relations)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
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
  } deriving (Generic, Show)
instance ToJSON Relations where
  toJSON = amp2Jason
instance ToJSON Relation where
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
         }
