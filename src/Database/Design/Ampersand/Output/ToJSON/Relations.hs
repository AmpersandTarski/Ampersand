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
  , relJSONsrcMin      :: Int
  , relJSONsrcMax      :: Maybe Int
  , relJSONtgtMin      :: Int
  , relJSONtgtMax      :: Maybe Int
  , relJSONaffectedConjuncts :: [String]
--  , relJSONaffectedInvConjunctIds  :: [Conjunct]
--  , relJSONaffectedSigConjunctIds  :: [Conjunct]
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
         , relJSONaffectedConjuncts = map rc_id  $ fromMaybe [] (lookup dcl $ allConjsPerDecl fSpec)
         , relJSONsrcMin      = if isSur dcl then 1 else 0 
         , relJSONsrcMax      = if isInj dcl then Just 1 else Nothing
         , relJSONtgtMin      = if isTot dcl then 1 else 0
         , relJSONtgtMax      = if isUni dcl then Just 1 else Nothing
--         , relJSONaffectedInvConjunctIds  = 
--         , relJSONaffectedSigConjunctIds  
         }
