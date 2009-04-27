{-# OPTIONS_GHC -Wall #-}
{-
USE    -> This module is intended for test purposes only.
DESCR  -> This module contains compare functions for data types in the Adl domain
EXTEND -> Other functions intended for tests on or with Adl data types can be added to this module
-}
module Test.AdlTestFunctions
where
   import Adl
   import Typology                    (Inheritance(Isa))
   import Classification              (Classification)
   import Data.Maybe

   compareTCOutput :: (Contexts,[String]) -> (Contexts,[String]) -> String
   compareTCOutput (_,_:[]) _ = "Left type checker returns errors."
   compareTCOutput _ (_,_:[]) = "Right type checker returns errors"
   compareTCOutput (ctxs1,_) (ctxs2,_) =
          (foldr (++) [] (catMaybes [compareContext cx1 cx2 | cx1<-ctxs1, cx2<-ctxs2, name cx1==name cx2]))

   compareContext :: Context -> Context -> Maybe String
   compareContext cx1@(Ctx{}) cx2@(Ctx{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in Context with name " ++ name cx1 ++ ":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareIsa (ctxisa cx1) (ctxisa cx2),
                              compareWorld (ctxwrld cx1) (ctxwrld cx2),
                              compareRules (ctxrs cx1) (ctxrs cx2),
                              compareDecls (ctxds cx1) (ctxds cx2),
                              compareCDefs (ctxcs cx1) (ctxcs cx2),
                              compareKDefs (ctxks cx1) (ctxks cx2),
                              compareODefs (ctxos cx1) (ctxos cx2)]
   compareContext _ _ = Just $ "Only comparing Contexts with constructor Ctx\n"

   --Isa [(a,a)] [a]
   compareIsa :: Inheritance Concept -> Inheritance Concept -> Maybe String
   compareIsa (Isa isas1 cs1) (Isa isas2 cs2)
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in ISA trees:\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes $ isasInLists ++ cptsInLists
          isasInLists =  [if elem isa2 isas1 then Nothing
                          else Just $ "IS-a Relation " ++ show isa2 ++ " from LIST2 not in LIST1\n"
                          | isa2<-isas2]
                         ++
                         [if elem isa1 isas2 then Nothing
                          else Just $ "IS-a Relation " ++ show isa1 ++ " from LIST1 not in LIST2\n"
                          | isa1<-isas1]
          cptsInLists =  [if elem c2 cs1 then Nothing
                          else Just $ "Independent Concept " ++ show c2 ++ " from LIST2 not in LIST1\n"
                          | c2<-cs2]
                         ++
                         [if elem c1 cs2 then Nothing
                          else Just $ "Independent Concept " ++ show c1 ++ " from LIST1 not in LIST2\n"
                          | c1<-cs1]

   compareWorld :: [Classification Context] -> [Classification Context] -> Maybe String
   compareWorld wrld1 wrld2
          | wrld1 == wrld2 = Nothing
          | otherwise      = Just $ "Context hierarchies " ++ show wrld1 ++ " does not equal " ++ show wrld2 ++ "\n"

   compareRules :: Rules -> Rules -> Maybe String
   compareRules rls1 rls2
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in Rules:\n" ++
                                  (foldr (++) nrerr detail)
          where
          detail :: [String]
          detail = catMaybes  compoutput
          compoutput :: [Maybe String]
          compoutput = [compareRule rl1 rl2 | rl1@(Ru{})<-rls1, rl2@(Ru{})<-rls2, rrfps rl1==rrfps rl2]
                       ++ [compareRule rl1 rl2 | rl1@(Sg{})<-rls1, rl2@(Sg{})<-rls2, srfps rl1==srfps rl2]
          nrerr = if length rls1 /= length rls2 then "Number of produced rules differ:\nRules from left:\n" ++ show rls1 ++ "Rules from right:\n" ++ show rls2 ++ "\n"
                  else if length compoutput /= length rls1 then "Not all rules of left can be correlated by file position to rules of right:\nRules from left:\n" ++ show rls1 ++ "Rules from right:\n" ++ show rls2 ++ "\n"
                  else []

   compareRule :: Rule -> Rule -> Maybe String
   compareRule rl1@(Ru {}) rl2@(Ru {})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in rule at file position "++ show (rrfps rl1) ++":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareExpression (rrant rl1) (rrant rl2),
                              compareExpression (rrcon rl1) (rrcon rl2),
                              compareSign (rrtyp rl1) (rrtyp rl2)]
   compareRule rl1@(Sg {}) rl2@(Sg {})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in rule at file position "++ show (srfps rl1) ++":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareRule (srsig rl1) (srsig rl2),
                              compareSign (srtyp rl1) (srtyp rl2),
                              compareDecl (srrel rl1) (srrel rl2)]
   compareRule _ _ = Just $ "Only comparing rules with constructor Ru and Sg.\n"

   compareDecls :: Declarations -> Declarations -> Maybe String
   compareDecls dcls1 dcls2
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in lists of Declarations:\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes declsInLists
          declsInLists = [if elem dcl2 dcls1 then Nothing
                          else Just $ "Declaration " ++ show dcl2 ++ "from LIST2 not in LIST1"
                          | dcl2<-dcls2] 
                         ++
                         [if elem dcl1 dcls2 then Nothing
                          else Just $ "Declaration " ++ show dcl1 ++ "from LIST1 not in LIST2"
                          | dcl1<-dcls1]

   compareCDefs :: ConceptDefs -> ConceptDefs -> Maybe String
   compareCDefs cds1 cds2
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in lists of ConceptDef:\n" ++
                                  (foldr (++) nrerr detail)
          where
          detail :: [String]
          detail = catMaybes  compoutput
          compoutput :: [Maybe String]
          compoutput = [compareCDef cd1 cd2 | cd1<-cds1, cd2<-cds2, cdpos cd1==cdpos cd2]
          printtrace = "LIST1:\n" ++ show cds1 ++ "LIST2:\n" ++ show cds2 ++ "\n"
          nrerr = if length cds1 /= length cds2 then "Number of produced ConceptDefs differ:\n" ++ printtrace
                  else if length compoutput /= length cds1 then "Not all ConceptDefs of LIST1 can be correlated by file position to ConceptDefs of LIST2:\n" ++ printtrace
                  else []
                  
   compareCDef :: ConceptDef -> ConceptDef -> Maybe String
   compareCDef cd1@(Cd{}) cd2@(Cd{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in Concept Def at file position "++ show (cdpos cd1) ++":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [if cdref cd1==cdref cd2 then Nothing else Just $ "ConceptDef Ref " ++ cdref cd1 ++ " does not equal " ++ cdref cd2 ++ "\n"]
   compareCDef _ _ = Just $ "Only comparing concept defs with constructor Cd\n"

   compareKDefs :: KeyDefs -> KeyDefs -> Maybe String
   compareKDefs kds1 kds2
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in lists of KeyDef:\n" ++
                                  (foldr (++) nrerr detail)
          where
          detail :: [String]
          detail = catMaybes  compoutput
          compoutput :: [Maybe String]
          compoutput = [compareKDef kd1 kd2 | kd1<-kds1, kd2<-kds2, kdpos kd1==kdpos kd2]
          printtrace = "LIST1:\n" ++ show kds1 ++ "LIST2:\n" ++ show kds2 ++ "\n"
          nrerr = if length kds1 /= length kds2 then "Number of produced KeyDefs differ:\n" ++ printtrace
                  else if length compoutput /= length kds1 then "Not all KeyDefs of LIST1 can be correlated by file position to KeyDefs of LIST2:\n" ++ printtrace
                  else []
                  
   compareKDef :: KeyDef -> KeyDef -> Maybe String
   compareKDef kd1@(Kd{}) kd2@(Kd{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in Key Def at file position "++ show (kdpos kd1) ++":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareExpression (kdctx kd1) (kdctx kd2),
                              compareODefs (kdats kd1) (kdats kd2)]
   compareKDef _ _ = Just $ "Only comparing Key defs with constructor Kd\n"

   compareODefs :: ObjectDefs -> ObjectDefs -> Maybe String
   compareODefs ods1 ods2
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in lists of ObjectDef:\n" ++
                                  (foldr (++) nrerr detail)
          where
          detail :: [String]
          detail = catMaybes  compoutput
          compoutput :: [Maybe String]
          compoutput = [compareODef od1 od2 | od1<-ods1, od2<-ods2, objpos od1==objpos od2]
          printtrace = "LIST1:\n" ++ show ods1 ++ "LIST2:\n" ++ show ods2 ++ "\n"
          nrerr = if length ods1 /= length ods2 then "Number of produced ObjectDefs differ:\n" ++ printtrace
                  else if length compoutput /= length ods1 then "Not all ObjectDefs of LIST1 can be correlated by file position to ObjectDefs of LIST2:\n" ++ printtrace
                  else []
                  
   compareODef :: ObjectDef -> ObjectDef -> Maybe String
   compareODef od1@(Obj{}) od2@(Obj{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Differences in Object Def at file position "++ show (objpos od1) ++ ":\n" ++ --printtrace ++
                                  (foldr (++) [] detail)
          where
          --printtrace = "OBJDEF1\n" ++ show od1 ++ "OBJDEF2\n" ++ show od2 ++ "\n"
          detail :: [String]
          detail = catMaybes [compareExpression (objctx od1) (objctx od2),
                              compareODefs (objats od1) (objats od2),
                              compareDirectives (objstrs od1) (objstrs od2)]
   compareODef _ _ = Just $ "Only comparing Object defs with constructor Obj\n"
          
   ----------------------------------------------------


   compareExpressions :: Expressions -> Expressions -> Maybe String
   compareExpressions es1 es2 
                   | length es1 /= length es2  = Just $ mainerror ++ "Number of expressions differ:\n" ++ printtrace
                   | otherwise                 = detail es1 es2
                   where
                   mainerror = "Difference in lists of Expressions\n"
                   printtrace = "LIST1:\n" ++ show es1 ++ "LIST2:\n" ++ show es2 ++ "\n"
                   detail :: Expressions -> Expressions -> Maybe String
                   detail [] [] = Nothing
                   detail (x:xs) (y:ys)
                              | detail2 == Nothing = detail xs ys
                              | otherwise          = Just $ mainerror ++ (fromJust detail2) ++ printtrace
                              where
                              detail2 :: Maybe String
                              detail2 = (compareExpression x y)

   compareExpression :: Expression -> Expression -> Maybe String
   compareExpression e1@(Tm{}) e2@(Tm{}) = compareMph (m e1) (m e2)
   compareExpression e1@(Tc{}) e2@(Tc{}) = compareExpression (e e1) (e e2)
   compareExpression e1@(K0{}) e2@(K0{}) = compareExpression (e e1) (e e2)
   compareExpression e1@(K1{}) e2@(K1{}) = compareExpression (e e1) (e e2)
   compareExpression e1@(Cp{}) e2@(Cp{}) = compareExpression (e e1) (e e2)
   compareExpression e1@(F{}) e2@(F{}) = compareExpressions (es e1) (es e2)
   compareExpression e1@(Fd{}) e2@(Fd{}) = compareExpressions (es e1) (es e2)
   compareExpression e1@(Fi{}) e2@(Fi{}) = compareExpressions (es e1) (es e2)
   compareExpression e1@(Fu{}) e2@(Fu{}) = compareExpressions (es e1) (es e2)
   compareExpression e1 e2 = Just $ "Expression " ++ show e1 ++ " does not equal " ++ show e2 ++ "\n"

   compareMph :: Morphism -> Morphism -> Maybe String
   compareMph m1@(Mph{}) m2@(Mph{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Morphism " ++ show m1 ++ " does not equal " ++ show m2 ++ ":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareMphAts (mphats m1) (mphats m2),
                              compareSign (mphtyp m1) (mphtyp m2),
                              compareYin (mphyin m1) (mphyin m2),
                              compareDecl (mphdcl m1) (mphdcl m2)]
   compareMph m1@(I{}) m2@(I{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Morphism " ++ show m1 ++ " does not equal " ++ show m2 ++ ":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareMphAts (mphats m1) (mphats m2),
                              compareConcept (mphgen m1) (mphgen m2),
                              compareConcept (mphspc m1) (mphspc m2),
                              compareYin (mphyin m1) (mphyin m2)]
   compareMph m1@(V{}) m2@(V{})
          | detail == [] = Nothing
          | otherwise    = Just $ "Morphism " ++ show m1 ++ " does not equal " ++ show m2 ++ ":\n" ++
                                  (foldr (++) [] detail)
          where
          detail :: [String]
          detail = catMaybes [compareMphAts (mphats m1) (mphats m2),
                              compareSign (mphtyp m1) (mphtyp m2)]
   compareMph m1 m2 = Just $ "Morphism " ++ show m1 ++ " does not equal " ++ show m2 ++ "\n"

   compareMphAts :: [Concept] -> [Concept] -> Maybe String
   compareMphAts a1 a2 | a1 == a2  = Nothing
                       | otherwise = Just $ "MphAts " ++ show a1 ++ " do not equal " ++ show a2 ++ "\n"

   compareSign :: (Concept,Concept) -> (Concept,Concept) -> Maybe String
   compareSign (a1,b1) (a2,b2) | a1==a2 && b1==b2 = Nothing
                               | otherwise        = Just $ "Sign " ++ show (a1,b1) ++ " does not equal " ++ show (a2,b2) ++ "\n"

   compareConcept :: Concept -> Concept -> Maybe String
   compareConcept a1 a2 | a1 == a2  = Nothing
                       | otherwise = Just $ "Concept " ++ show a1 ++ " does not equal " ++ show a2 ++ "\n"

   compareYin :: Bool -> Bool -> Maybe String
   compareYin a1 a2 | a1 == a2  = Nothing
                    | otherwise = Just $ "Yin " ++ show a1 ++ " does not equal " ++ show a2 ++ "\n"
                    
   compareDecl :: Declaration -> Declaration -> Maybe String
   compareDecl a1 a2 | a1 == a2  = Nothing
                     | otherwise = Just $ "Declaration " ++ show a1 ++ " does not equal " ++ show a2 ++ "\n"
    
   compareDirectives :: [[String]] -> [[String]] -> Maybe String
   compareDirectives a1 a2 | a1==a2    = Nothing
                           | otherwise = Just $ "Directives " ++ show a1 ++ " do not equal " ++ show a2 ++ "\n"





