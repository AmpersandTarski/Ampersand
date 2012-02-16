module DatabaseDesign.Ampersand.ADL1.TypeCheck (inferType) where

import Prelude hiding (Ord(..))
import DatabaseDesign.Ampersand.Core.Poset.Instances
import DatabaseDesign.Ampersand.Core.Poset
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Basics hiding (sort)
import Data.List hiding (sort)
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Input.ADL1.CtxError

import Debug.Trace

-- TODO: specializations & nice errors

-- maybe don't use lists. It is nec. for enforcing src == tgt in I, but we can use EndoSign Concept for that
{-

Idea: upward, add supertypes if necessary, and going down, narrow when required.
Maybe we can even replace types by supertypes (does this create a problem when for example I[a*a] becomes I[aa*a]? 
-}
inferType :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
             ctxt -> ([Expression], [String]) -> P_Expression -> ([Expression], [String]) 
inferType context oldTypeCheckerResults@(ampExps, _) e =
  let (possibles, res) = inferTypeWithCast context Nothing Nothing e
      --(possibles, res) = (fst $ infer context (error "assigned accessed") e, "NO RES")
      msg = "\nType checking:"++showADL e++"\n" ++
            "Ampersand:\n"++ concat [ showADL e ++ " :: [" ++ show (source ampExp) ++ "*" ++ show (target ampExp) ++ "]\n" | ampExp <- ampExps] ++
            "Martijn, possibles: "++ show possibles ++ "\n" ++
            "greatestSign: "++ show (greatestSign possibles) ++ "\n" ++
            case res of Right typedExp -> "Typed:" ++ showADL typedExp
                        Left  err      -> "ERROR:" ++ err
                        
  in  trace msg oldTypeCheckerResults

inferTypeWithCast :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                     ctxt -> Maybe A_Concept -> Maybe A_Concept -> P_Expression -> ([Sign], Either String P_Expression) 
inferTypeWithCast context  mSrc mTgt e =
  let (possibleTypes, result) = infer context assigned e
      (assigned,mErr) = case cast mSrc mTgt possibleTypes of
                               [aType] -> (aType, Nothing)
                               []      -> (undefined, Just "not typeable")
                               _       -> (undefined, Just "ambiguous") -- todo use dummy type instead of undefined
   in  (possibleTypes, case mErr of Just err -> Left err
                                    Nothing -> result)
 where cast srcCast tgtCast types =
         greatestSign [ sg | sg@(Sign s t) <- types, srcCast `eqOrNothing` s && tgtCast `eqOrNothing` t ]
       Nothing `eqOrNothing` _  = True
       Just t1 `eqOrNothing` t2 = t2 <= t1

infer :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
         ctxt -> Sign -> P_Expression -> ([Sign], Either String P_Expression) 
infer context ~assigned@(Sign assignedSrc assignedTgt) pExp =
  case pExp of 
    (Prel P_I)         -> ( [ Sign c c | c <- concs context ]
                          , if assignedSrc==assignedTgt then Right $ Prel P_I `withType` assigned else typeError "I")
    (Prel mp1@P_Mp1{}) -> ( [ Sign c c | c <- concs context ]
                          , if assignedSrc==assignedTgt then Right $ Prel mp1 `withType` assigned else typeError "Mp1")
    (Prel P_V)         -> ( [ Sign src tgt | src <- concs context, tgt <- concs context ], Right $ Prel P_V `withType` assigned)
    (Prel (P_Rel nm o)) -> let relTypes = getRelTypes context nm
                           in (relTypes, if assigned `elem` relTypes then Right $ Prel (P_Rel nm o) `withType` assigned else typeError "Rel")
    (PTyp e tp) -> let (possibles, typedExp) = infer context assigned' e
                       (Sign src tgt) = pSign2aSign context tp 
                       possibles' = [Sign s t | Sign s t <- possibles, s<=src && t <= tgt]
                       assigned' = if src<=assignedSrc && tgt<=assignedTgt
                                   then Sign src tgt
                                   else if assignedSrc <= src && assignedTgt <= tgt
                                        then Sign assignedSrc assignedTgt
                                        else error "Something is wrong"
                   in  ( possibles'
                       , if not . null $ possibles'
                         then typedExp 
                         else typeError "wrong type sig") -- type sig doesn't fit possible types
    (PFlp e)        -> case infer context (Sign assignedTgt assignedSrc) e of 
                          (psbls, Left err) -> (psbls, Left err) 
                          (psbls, Right typedExp) ->(map (\(Sign s t)-> Sign t s) psbls, Right $ PFlp typedExp)
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

    (PLrs _)        -> error "Left residuals are not yet supported"
    (PRrs _)        -> error "Left residuals are not yet supported"
    (PKl0 _)        -> error "Kleene star is not yet supported"
    (PKl1 _)        -> error "Kleene plus is  not yet supported"

inferListEqual :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                  ctxt -> Sign -> [P_Expression] -> ([Sign], Either String [P_Expression])
inferListEqual _       _        []     = error "Fatal, incorrect P structure"
inferListEqual context assigned [e]    = mapA (\x-> [x]) $ infer context assigned e
inferListEqual context assigned (e:es) =
  let (possiblesHead, resHead) = infer context assigned e
      (possiblesTail, resTail) = inferListEqual context assigned es
      possiblesList = nub $
                      [ Sign src tgt | Sign src tgt <- possiblesHead
                                     , Sign src' tgt' <- possiblesTail
                                     , src >= src' && tgt >= tgt' ] ++
                      [ Sign src tgt | Sign src tgt <- possiblesTail
                                     , Sign src' tgt' <- possiblesHead
                                     , src >= src' && tgt >= tgt' ]
      resList = case (resHead, resTail) of
                  (Left err, _)       -> Left err
                  (Right _, Left err) -> Left err
                  (Right typedExp, Right typedExps) -> if assigned `elem` possiblesList 
                                                       then Right $ typedExp:typedExps
                                                       else Left  "list not equally typed"   
  in trace ("\ninferListEqual "++show possiblesHead ++" vd "++ show possiblesTail ++ "  "++show possiblesList) $
       (possiblesList, resList)

inferListChain :: (Language ctxt, ConceptStructure ctxt, Identified ctxt) =>
                  ctxt -> Sign -> [P_Expression] -> ([Sign], Either String [P_Expression])
inferListChain _       _                    []     = error "Fatal, incorrect P structure"
inferListChain context assigned             [e]    = mapA (\x-> [x]) $ infer context assigned e
inferListChain context ~assigned@(Sign assignedSrc assignedTgt) (e:es) = 
  let (possiblesHead, resHead) = infer context (Sign assignedSrc middleType) e 
      (possiblesTail, resTail) = inferListChain context (Sign middleType assignedTgt) es
      possiblesList = nub [  Sign src tgt | Sign src med <- possiblesHead, Sign med' tgt <- possiblesTail, med == med' ]
      left  = [ med |  Sign src med <- possiblesHead, src == assignedSrc ]
      right = [ med |  Sign med tgt <- possiblesTail, tgt == assignedTgt ]
      (middleType, mErr) = case left `intersect` right of
                             [m] -> (m, Nothing)
                             []  -> (undefined, Just "compose no match")
                             _   -> (undefined, Just $ "compose" ++ "ambiguity")
      resList = case (resHead, resTail, mErr) of
                  (Left err, _, _)             -> Left err
                  (Right _, Left err, _)       -> Left err
                  (Right _, Right _, Just err) -> Left err
                  (Right typedExp, Right typedExps, Nothing) -> 
                    if assigned `elem` possiblesList 
                    then Right $ typedExp:typedExps
                    else Left  "list not equally typed"   
                    
  in  (possiblesList, resList)  

-- Utils

greatestSign :: [Sign] -> [Sign]
greatestSign signs = [ sign | sign  <- signs, all (sign `isSuperType`) signs ]
 where (Sign s1 t1) `isSuperType` (Sign s2 t2) = s1 >= s2 && t1 >= t2 
{-
greatestSign (sign : signs) = greatestSign' sign signs
 where greatestSign' gr []              = [gr] 
       greatestSign' (Sign grs grt) (Sign s t : ss) | grs >= s && grt >= t = greatestSign' (Sign grs grt) ss
       greatestSign' (Sign grs grt) (Sign s t : ss) | s >= grs && t >= grt = greatestSign' (Sign s t) ss
       greatestSign' _              _               | otherwise            = []
-}
 
withType :: P_Expression -> Sign -> P_Expression
withType e t = PTyp e (pSign t)

mapA :: (a->a') -> ([Sign], Either String a) -> ([Sign], Either String a')
mapA f (types, result) = (types, case result of Right a  -> Right $ f a
                                                Left err -> Left err)

getRelTypes :: Language context => context -> String -> [Sign] 
getRelTypes context relName = [Sign (source d) (target d) | d<-declarations context, name d==relName ]

listToPair :: [a] -> (a,a)
listToPair [e1,e2] = (e1,e2) -- explicit function, so we can fatal when something is wrong
listToPair _       = error "fatal, wrong list length"

-- Temporary stuff
typeError :: String -> Either String a
typeError msg = Left $ "New type checker: " ++ msg


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
{-
BUGS:

ab :: A * B
ab /\ I
ampersand: !fatal error 705 (module P2A_Converter, Ampersand v2.2.0.444:445M)
  no solutions and no inferUniIsc for ab/\ I 


BUGS?
aabb[A*B] :: [A*B]      no type error?
No, narrows aabb

ab -  aabb   :: [A*B]   no type error?
ab /\ -aabb  :: [AA*BB] why not equal to previous? (and why no type error)

UNCLEAR:

example Stef different errors when e1 - e2 is rewritten to e1 /\ -e2?

RULE t3 : V[AA*B] /\ V[A*BB]
The pattern named "Type" contains errors in the rule at line line 36, file "Type.adl":
Incomparable types in expression  V [AA*B]/\ V [A*BB] between
   [AA*B] (in V[AA*B])
   [A*BB] (in V[A*BB])
why not AA*BB?



BUGS
type checker: infer PCpl is implemented just like PBrk, so it cannot affect typing (which is wrong)

RelBinGenSQL does not do anything for PTyp, so narrowing relations does not work


-}