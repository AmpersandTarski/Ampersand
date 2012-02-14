module DatabaseDesign.Ampersand.ADL1.TypeCheck (inferType) where

import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Core.Poset
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Basics
import Data.List

-- TODO: specializations & nice errors

inferType :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
             ctxt -> P_Expression -> ([Sign], P_Expression) 
inferType context e = inferTypeWithCast context Nothing Nothing e

inferTypeWithCast :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                     ctxt -> Maybe A_Concept -> Maybe A_Concept -> P_Expression -> ([Sign], P_Expression) 
inferTypeWithCast context  mSrc mTgt e =
  let (possibleTypes, typedExp) = infer context assigned e
      assigned = case cast mSrc mTgt possibleTypes of
                                 [aType] -> aType
                                 []      -> typeError "not typeable"
                                 _       -> typeError "ambiguous"
   in  (possibleTypes, typedExp)
 where cast srcCast tgtCast types = [ sg | sg@(Sign s t) <- types, srcCast `eqOrNothing` s && tgtCast `eqOrNothing` t ]
       Nothing `eqOrNothing` _  = True
       Just t1 `eqOrNothing` t2 = t1 == t2

infer :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
         ctxt -> Sign -> P_Expression -> ([Sign], P_Expression) 
infer context ~assigned@(Sign assignedSrc assignedTgt) pExp =
  case pExp of 
    (Prel P_I)         -> ( [ Sign c c | c <- concs context ]
                          , if assignedSrc==assignedTgt then Prel P_I `withType` assigned else typeError "I")
    (Prel mp1@P_Mp1{}) -> ( [ Sign c c | c <- concs context ]
                          , if assignedSrc==assignedTgt then Prel mp1 `withType` assigned else typeError "Mp1")
    (Prel P_V)         -> ( [ Sign src tgt | src <- concs context, tgt <- concs context ], Prel P_V `withType` assigned)
    (Prel (P_Rel nm o)) -> let relTypes = getRelTypes context nm
                           in (relTypes, if assigned `elem` relTypes then Prel (P_Rel nm o) `withType` assigned else typeError "Rel")
    (PTyp e tp) -> let (possibles, typedExp) = infer context assigned e
                       (Sign aSrc aTgt) = pSign2aSign context tp 
                   in  ([Sign aSrc aTgt], if (Sign aSrc aTgt) `elem` possibles
                                          then if aSrc==assignedSrc && aTgt==assignedTgt 
                                               then typedExp
                                               else typeError "sig" -- assigned type from above doesn't fit
                                               else typeError "wrong type sig") -- type sig doesn't fit possible types
    (PFlp e)        -> let (psbls, typedExp) = infer context (Sign assignedTgt assignedSrc) e in (map (\(Sign s t)-> Sign t s) psbls, PFlp typedExp)
    (PBrk e)        -> mapA PBrk $ infer context assigned e 
    (PCpl e)        -> mapA PCpl $ infer context assigned e 
    (Pequ (e1, e2)) -> mapA (Pequ . listToPair) $ inferListEqual context assigned [e1, e2] 
    (Pimp (e1, e2)) -> mapA (Pimp . listToPair) $ inferListEqual context assigned [e1, e2] 
    (PDif (e1, e2)) -> mapA (PDif . listToPair) $ inferListEqual context assigned [e1, e2]
    (PUni es)       -> mapA PUni $ inferListEqual context assigned es                            
    (Pisc es)       -> mapA Pisc $ inferListEqual context assigned es                     
    (PCps es)       -> mapA PCps $ inferListChain context assigned es 
    (PRad es)       -> mapA PRad $ inferListChain context assigned es
    (PPrd es)       -> mapA PPrd $ inferListChain context assigned es

    (PLrs _)        -> typeError "Left residuals are not yet supported"
    (PRrs _)        -> typeError "Left residuals are not yet supported"
    (PKl0 _)        -> typeError "Kleene star is not yet supported"
    (PKl1 _)        -> typeError "Kleene plus is  not yet supported"

inferListEqual :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                  ctxt -> Sign -> [P_Expression] -> ([Sign], [P_Expression])
inferListEqual _       _        []     = typeError "Incorrect P structure"
inferListEqual context assigned [e]    = mapA (\x-> [x]) $ infer context assigned e
inferListEqual context assigned (e:es) = 
  let (possibles1, typedExp) = infer context assigned e
      (possibles2, typedExps) = inferListEqual context assigned es
      possibleTypes = [ Sign src tgt | Sign src tgt <- possibles1
                      , Sign src' tgt' <- possibles2
                      , src == src' && tgt == tgt' ]
      finalType = if assigned `elem` possibleTypes then typedExp:typedExps  else typeError "list not equally typed"
  in  (possibleTypes, finalType) -- finalType is typedExp or aExp

inferListChain :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                  ctxt -> Sign -> [P_Expression] -> ([Sign], [P_Expression])
inferListChain _       _                    []     = typeError "Incorrect P structure"
inferListChain context assigned             [e]    = mapA (\x-> [x]) $ infer context assigned e
inferListChain context ~(Sign assignedSrc assignedTgt) (e:es) = 
  let (possibles1, typedExp) = infer context (Sign assignedSrc middleType) e 
      (possibles2, typedExps) = inferListChain context (Sign middleType assignedTgt) es
      possibleTypes = [  Sign src tgt | Sign src med <- possibles1, Sign med' tgt <- possibles2, med == med' ]
      left  = [ med |  Sign src med <- possibles1, src == assignedSrc ]
      right = [ med |  Sign med tgt <- possibles2, tgt == assignedTgt ]
      middleType = case left `intersect` right of
                     [m] -> m
                     []  -> typeError "compose no match"
                     _   -> typeError $ "compose" ++ "ambiguity"
  in  (nub possibleTypes, typedExp : typedExps) -- maybe doubles are already an typeError?  

-- Utils

withType :: P_Expression -> Sign -> P_Expression
withType e t = PTyp e (pSign t)

mapA :: (a->a') -> ([Sign], a) -> ([Sign], a')
mapA f (types, a) = (types, f a)

getRelTypes :: Language context => context -> String -> [Sign] 
getRelTypes context relName = [Sign (source d) (target d) | d<-declarations context, name d==relName ]

listToPair :: [a] -> (a,a)
listToPair [e1,e2] = (e1,e2) -- explicit function, so we can fatal when something is wrong
listToPair _       = typeError "fatal, wrong list length"

-- Temporary stuff
typeError :: String -> a
typeError msg = error $ "New type checker: " ++ msg


-- temp backward mapping because we generate a typed P now
pSign :: Sign -> P_Sign
pSign (Sign s t) = P_Sign [pConcept s, pConcept t]
 where pConcept C{cptnm = c} = PCpt c 
       pConcept ONE          = P_Singleton
       


-- Duplicated from P2AConverters
pSign2aSign :: (Language l, ConceptStructure l, Identified l) => l -> P_Sign -> Sign
pSign2aSign contxt (P_Sign cs) = Sign (head ts) (last ts)
  where ts = map (pCpt2aCpt contxt) cs
        
pCpt2aCpt :: (Language l, ConceptStructure l, Identified l) => l -> P_Concept -> A_Concept
pCpt2aCpt contxt pc
    = case pc of
        PCpt{} -> c 
        P_Singleton -> ONE
      where 
      c = C {cptnm = p_cptnm pc
            ,cptgE = genE contxt
            ,cptos = nub$[srcPaire p | d<-declarations contxt,decusr d,p<-contents d, source d <= c]
                       ++[trgPaire p | d<-declarations contxt,decusr d,p<-contents d, target d <= c]
                       ++[v | r<-rules contxt,Mp1 v c'<-mors r,c'<=c]
            ,cpttp = head ([cdtyp cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]++[""])
            ,cptdf = [cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]
            }
