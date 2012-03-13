{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Apps.RAPIdentifiers
        --abstract data type ConceptIdentifier with one getter and constructor functions for atom identifiers of RAP concepts
       (ConceptIdentifier, getid
                         , nonsid, fsid, cptid, patid, ruleid, genid, sgnid, decid, relid, expridid, atomidid, pairid, pairidid, imageid, fileid, usrid, gid
        --abstract data type IdentifierNamespace with constructor functions for RAP concept identifiers requiring a different namespace than the default
       ,IdentifierNamespace, rulens, decns)
        --the default namespace for RAPv1 (March 2012) is a CONTEXT, because each CONTEXT will be inserted in its own set of SQL tables.
        --this is done because one big RAP requires a database cleanup procedure to prevent performance issues on the production environment of March 2012
        --the entire RAP as the default namespace would require all concept identifiers to be qualified with a namespace like the identifier of a CONTEXT
where
import Data.HashTable (hashString)
import DatabaseDesign.Ampersand_Prototype.CoreImporter

data ConceptIdentifier = CID String
getid :: ConceptIdentifier -> String
getid (CID x) = x
data IdentifierNamespace = CNS String

{- concept identifier functions (concepts of RAP) -}
--identifiers for a RAP concept on the outside of the conceptual model do not have to be limited to the default namespace
--e.g. an atom x of type Blob, String or Varid may just be (CID x)
--use nonsid :: String -> ConceptIdentifier for those concepts
nonsid :: String -> ConceptIdentifier
nonsid x = CID x
fsid :: Fspc -> ConceptIdentifier
fsid fs = CID $ name fs
cptid :: A_Concept -> ConceptIdentifier
cptid c = CID $ name c
patid :: Pattern -> ConceptIdentifier
patid p = CID $ name p
ruleid :: Rule -> ConceptIdentifier
ruleid r = CID $ name r
genid :: A_Gen -> ConceptIdentifier
genid g = CID $ "(" ++ getid(cptid (source g)) ++ "," ++ getid(cptid (target g)) ++ ")"
sgnid :: Sign -> ConceptIdentifier
sgnid sgn = CID $ getid(cptid (source sgn)) ++ "*" ++ getid(cptid (target sgn))
decid :: Declaration -> ConceptIdentifier
decid d = CID $ name d ++ "::" ++ getid(cptid (source d)) ++ "*" ++ getid(cptid (target d))
relid :: String -> Sign -> ConceptIdentifier
relid nm sgn = CID $ nm ++ "[" ++ getid(cptid (source sgn)) ++ "*" ++ getid(cptid (target sgn)) ++ "]"
expridid :: (IdentifierNamespace, Expression) -> ConceptIdentifier
expridid (CNS ns,expr) = CID $ ns ++ "#" ++ show expr
atomidid :: String -> A_Concept -> ConceptIdentifier
atomidid x c  = CID $ show$hashString (x ++ "[" ++ getid(cptid c) ++ "]") --limit of data length in database is 256
pairid :: (String,String) -> Sign -> ConceptIdentifier
pairid (x,y) sgn = CID $ show$hashString (x  ++ "*" ++ y ++ "[" ++ getid(sgnid sgn) ++ "]") --limit of data length in database is 256
pairidid :: Association r => (String,String) -> (IdentifierNamespace, r) -> ConceptIdentifier
pairidid (x,y) (CNS ns,r) = CID $ ns ++ "#" ++ getid(pairid (x,y) (sign r))
imageid :: Picture -> ConceptIdentifier
imageid pic = CID$"Image_" ++ uniqueName pic
fileid :: (String,String) -> ConceptIdentifier
fileid (path,fn) = CID (path++fn)
usrid :: String -> ConceptIdentifier
usrid usr = CID usr
gid :: Int -> String -> ConceptIdentifier
gid op fn = CID (show op++"("++fn++")")

{- identifier namespace functions (namespaces for concept identifiers with a different namespace than CONTEXT) -}
rulens :: Rule -> IdentifierNamespace
rulens r = CNS $ "Rule_" ++ getid(ruleid r)
decns :: Declaration -> IdentifierNamespace
decns d  = CNS $ "Declaration_" ++ getid(decid d)
