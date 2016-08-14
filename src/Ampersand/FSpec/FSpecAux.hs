module Ampersand.FSpec.FSpecAux 
  (getDeclarationTableInfo,getConceptTableInfo)
where
import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.FSpec.FSpec

-- return table name and source and target column names for relation dcl
getDeclarationTableInfo :: FSpec -> Declaration -> (PlugSQL,SqlAttribute,SqlAttribute) 
getDeclarationTableInfo fSpec dcl 
     = case filter thisDcl . concatMap getRelInfos $ [p | InternalPlug p<-plugInfos fSpec ] of
                [(p,store)] -> (p,rsSrcAtt store,rsTrgAtt store)
                []          -> fatal 32 $ "Relation not found: "++name dcl
                _           -> fatal 33 $ "Relation found multiple times: "++name dcl
  where
    getRelInfos :: PlugSQL -> [(PlugSQL,RelStore)]
    getRelInfos p = zip (repeat p) (dLkpTbl p)  
    thisDcl :: (a,RelStore) -> Bool
    thisDcl (_,store) = rsDcl store == dcl
-- return table name and source and target column names for relation rel, or nothing if the relation is not found

getConceptTableInfo :: FSpec -> A_Concept -> (PlugSQL,SqlAttribute)
getConceptTableInfo fSpec cpt 
  = case lookupCpt fSpec cpt of
      []  -> fatal 55 $ "No plug found for concept '"++name cpt++"'."
      [x] -> x  --Any of the resulting plugs should do. 
      xs  -> fatal 58 $ "Only one result expected:"++show xs
