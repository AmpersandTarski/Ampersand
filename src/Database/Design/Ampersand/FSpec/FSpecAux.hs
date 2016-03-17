module Database.Design.Ampersand.FSpec.FSpecAux 
  (getDeclarationTableInfo,getConceptTableInfo)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Data.Maybe(catMaybes)

-- return table name and source and target column names for relation dcl
getDeclarationTableInfo :: FSpec -> Declaration -> (PlugSQL,SqlAttribute,SqlAttribute) 
getDeclarationTableInfo fSpec dcl 
     = case filter thisDcl . concatMap getRelInfos $ [p | InternalPlug p<-plugInfos fSpec ] of
                [(p,_,s,t)] -> (p,s,t)
                []          -> fatal 32 $ "Relation not found: "++name dcl
                _           -> fatal 33 $ "Relation found multiple times: "++name dcl
  where
    getRelInfos :: PlugSQL -> [(PlugSQL,Declaration,SqlAttribute,SqlAttribute) ]  
    getRelInfos p =
      case p of 
        TblSQL{} -> catMaybes . map relInfo . mLkpTbl $ p
        BinSQL{} -> let (src,trg) = columns p in
                    case mLkp p of
                      EDcD d        -> [(p,d,src,trg)]
                      EFlp (EDcD d) -> [(p,d,trg,src)]
                      expr   -> fatal 35 $ "Unexpected expression in BinSQL: "++show expr  
        ScalarSQL{} -> []
      where 
        relInfo :: (Expression,SqlAttribute,SqlAttribute) -> Maybe (PlugSQL,Declaration,SqlAttribute,SqlAttribute)
        relInfo (expr,src,trg) =
          case expr of
            EDcD d        -> Just (p,d,src,trg)
            EFlp (EDcD d) -> Just (p,d,trg,src)
            EEps _ _      -> Nothing
            _             -> fatal 40 $ "Unexpected expression: "++show expr
    thisDcl :: (a,Declaration,b,c) -> Bool
    thisDcl (_,d,_,_) = d == dcl
-- return table name and source and target column names for relation rel, or nothing if the relation is not found

getConceptTableInfo :: FSpec -> A_Concept -> (PlugSQL,SqlAttribute)
getConceptTableInfo fSpec cpt 
  = case lookupCpt fSpec cpt of
      []  -> fatal 55 $ "No plug found for concept '"++name cpt++"'."
      [x] -> x  --Any of the resulting plugs should do. 
      xs  -> fatal 58 $ "Only one result expected:"++show xs
