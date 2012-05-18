{-# OPTIONS_GHC -Wall -XTypeSynonymInstances #-}
{- The SimpleTypeChecker.hs is a prototype ghci-script for the new type checker.
It is meant only for explaining how the type checker works.
Work needs to be done on commenting and cleaning up the code for this purpose.
This file is not meant to be included in the compile sequence for Ampersand.
-}
module SimpleTypeChecker where
  import qualified Data.Set as Set
  import qualified Data.Graph as Graph
  import qualified Data.Tree as Tree
  import Data.List

  data Relation   = Rel String deriving (Eq, Ord)
  data Expression = Pequ  (Expression, Expression) -- equivalence
                  | Pimp  (Expression, Expression) -- implication
                  | ExpRel Relation -- for simplicity, we expect expressions in flip-normal form
                  | ExpFlp Relation -- flip / relational inverse
                  | ExpId  String                -- identity element restricted to a type
                  | ExpIsc Expression Expression -- intersection
                  | ExpUni Expression Expression -- union
                  | ExpApp Expression Expression -- apply / compose / append
                  deriving (Eq, Ord)
  data Type       = TypExpr Expression deriving (Eq, Ord, Show)
                    
  class Flippable a where -- notational convenience
   flp :: a -> Expression
  instance Flippable Expression where
   flp (Pequ (a,b)) = flp a .= flp b
   flp (Pimp (a,b)) = flp a .< flp b
   flp (ExpRel a)   = ExpFlp a
   flp (ExpFlp a)   = ExpRel a
   flp (ExpIsc a b) = flp a /\ flp b
   flp (ExpUni a b) = flp a \/ flp b
   flp (ExpApp a b) = flp b .* flp a
   flp (ExpId a)    = ExpId a

  instance Show Relation where
   showsPrec _ (Rel str) = showString (str)

  instance Show Expression where
   showsPrec _ (Pequ (a,b)) = showString (show a++" .= "++ show b)
   showsPrec _ (Pimp (a,b)) = showString (show a++" .< "++ show b)
   showsPrec _ (ExpRel a)   = showString (show a)
   showsPrec _ (ExpFlp a)   = showString (show a++"~")
   showsPrec _ (ExpIsc a b) = showString ("("++show a++"/\\"++ show b++")")
   showsPrec _ (ExpUni a b) = showString ("("++show a++"\\/"++ show b++")")
   showsPrec _ (ExpApp a b) = showString ("("++show a++".*"++ show b++")")
   showsPrec _ (ExpId a)    = showString ("I["++a++"]")

  -- constructor functions..
  infixl 3 .=
  infixl 3 .<
  infixl 4 \/
  infixl 5 /\
  infixl 7 .*
  (.=) :: Expression -> Expression -> Expression
  (.=) a b = Pequ (a,b)
  (.<) :: Expression -> Expression -> Expression
  (.<) a b = Pimp (a,b)
  (/\) :: Expression -> Expression -> Expression
  (/\) a b = ExpIsc a b
  (\/) :: Expression -> Expression -> Expression
  (\/) a b = ExpUni a b
  (.*) :: Expression -> Expression -> Expression
  (.*) a b = ExpApp a b
  
  sampleInput :: [Expression]
  sampleInput
   = [ i "Rechter" .< i "Persoon" -- ouderwetse ISA regel:
     , i "Rechter" .* r "leidt zitting" .* i "Zitting" .= r "leidt zitting" -- typering van de relatie "leidt zitting"[Rechter*Zitting]
     , i "Zitting" .* r "kamer" .* i "Kamer" .= r "kamer" -- typering van de relatie "kamer"
     , r "leidt zitting" .* r "kamer" .= r "zit in kamer" -- regel over "zit in kamer" (de relatie krijgt een type door deze regel!)
     -- nu enkele "misschien" type-fouten:
     -- SICK -> nogmaals afleiden wat het type van een relatie is:
     , r "slaapt op" .< (i "Luilak" /\ i "Slaper" .* i "Locatiehebbende") .* flp (r "slaapt op")
     -- de relatie "gegeven" kan niet getypeerd worden:
     , r "kamer" .< r "gegeven"
     -- Persoon isa rechter
     , i "Persoon" .< i "Rechter"
     ] where
        r  a = ExpRel (Rel a)
        i  a = ExpId a

  subExpressions :: Expression -> [Expression]
  subExpressions (Pequ (a,b)) = [Pequ (a,b)] ++ subExpressions a ++ subExpressions b
  subExpressions (Pimp (a,b)) = [Pimp (a,b)] ++ subExpressions a ++ subExpressions b
  subExpressions (ExpRel a)   = [ExpRel a]
  subExpressions (ExpFlp a)   = [ExpFlp a]
  subExpressions (ExpIsc a b) = [ExpIsc a b] ++ subExpressions a ++ subExpressions b
  subExpressions (ExpUni a b) = [ExpUni a b] ++ subExpressions a ++ subExpressions b
  subExpressions (ExpApp a b) = [ExpApp a b] ++ subExpressions a ++ subExpressions b
  subExpressions (ExpId a)    = [ExpId a]

  -- create unordered / non-increasing pair
  unord :: Ord t => (t,t) -> (t,t)
  unord (a,b) | a > b     = (a,b)
              | otherwise = (b,a)
  
  typing :: [Expression] -> Set.Set (Type, Type) -- subtypes (.. is subset of ..)
  typing exprs = Set.fromList [ t | expr<-exprs, x<-subExpressions expr, t<-uType x]
   where
     uType   (Pequ (a,b)) = [(dom a,dom b), (dom b,dom a), (cod a,cod b), (cod b,cod a)]
     uType   (Pimp (a,b)) = [(dom a,dom b), (cod a,cod b)]
     uType   (ExpRel _)   = []
     uType   (ExpFlp _)   = []
     uType o@(ExpIsc a b) = [(dom o,dom a), (dom o,dom b), (cod o,cod a), (cod o,cod b)]
     uType o@(ExpUni a b) = [(dom a,dom o), (dom b,dom o), (cod a,cod o), (cod b,cod o)]
     uType o@(ExpApp a@(ExpId _) b) = [(dom o,dom b), (dom o,dom a), (cod o,cod b)]
     uType o@(ExpApp a b) = [(dom o,dom a), (cod o,cod b)]
     uType o@(ExpId _)    = [(dom o,dom o)]

     dom = TypExpr
     cod = TypExpr . flp

  printTypes :: IO ()
  printTypes = foldr1 (>>) $ map putStrLn lnes
    where
      (typeErrors,conceptTree,typedRels,accounting) = calcTypes sampleInput
      lnes = ["Type errors:"]++typeErrors
                    ++["","Concept tree:"]++conceptTree
                    ++["","Relations:"]++typedRels -- ++[accounting]
  
  calcTypes :: [Expression] -> ([String],[String],[String],String)
  calcTypes sentences = (typeErrors,conceptTree,typedRels,accounting)
   where
    accounting = -- accounting for the results: (for debugging purposes)
      "\nsubexpressions:\n  "++intercalate "\n  " ((map show . nub) [x | expr<-sentences, x<-subExpressions expr]) ++
      "\neq:\n  "++intercalate "\n  " (map show (Set.toList eq))++
      "\neqClasses:\n  "++intercalate "\n  " (map show eqClasses)++
      "\nst:\n  "++intercalate "\n  " (map show (Set.toList st)) ++
      "\nstClasses:\n  "++intercalate "\n  " (map show stClasses)++
      "\neqVerts:\n  "++intercalate "\n  " (map show eqVerts) ++
      "\nstVerts:\n  "++intercalate "\n  " (map show stVerts)
    st :: Set.Set (Type, Type)
    st = typing sentences
    eq :: Set.Set (Type, Type)
    eq = Set.map unord (Set.intersection (Set.map swap st) st) where swap (a,b) = (b,a)
    eqGraph  
     = makegraph eqVerts eq
    eqVerts
     = zip [0..] $ Set.toAscList $ Set.union (Set.map fst eq)
                                             (Set.map snd eq)
    eqClasses
     = forestToClasses eqVerts $ Graph.components eqGraph
    stVerts
     = zip [0..] $ Set.toAscList $ Set.map getEqClass $
         (Set.union (Set.union (Set.map fst st) (Set.map snd st))
                    (Set.fromList (map (\x -> snd (fst x)) eqClasses)))
    getEqClass vert = head ([a | ((_,a),as)<-eqClasses, Set.member vert as]++[vert])
    stGraph = makegraph stVerts (Set.map (\(x,y) -> (getEqClass x,getEqClass y)) st)
    stClasses
     = forestToClasses stVerts $ Graph.scc stGraph
    makegraph verts tuples
     = Graph.buildG (0,length verts - 1) 
         [(i',j) | (k,l)<-Set.toAscList tuples,(i',k')<-verts,k==k',(j,l')<-verts,l==l']
    forestToClasses verts forest
     = map (\x->(verts !! (foldr1 min x)
                , Set.fromList $ map (\y->(snd (verts !! y))) x))
        $ map Tree.flatten forest
    -- type errors come in three kinds:
    -- 1. Two named types are equal.
    --    This is usually unintended: user should give equal types equal names.
    -- 2. The type of a relation cannot be determined.
    --    This means that there is no named type in which it is contained.
    -- 3. The type of a term has no name??
    --    I don't know if these can be considered as type-errors
    --    perhaps if this type is too high up, or too low under
    typeErrors
     = [ "1. These concepts are trivially equal: " ++ intercalate ", " concs
       | (_,as)<-eqClasses, let concs = [c|TypExpr (ExpId c) <- Set.toList as], length concs>1]
        ++
       [ "1. These concepts are derived to be equal: " ++ intercalate ", " concs
       | (_,as)<-stClasses, let concs = [c|TypExpr (ExpId c) <- Set.toList as], length concs>1]
    conceptTree
     = [src ++ " ISA " ++ (
        foldr1 (+++)  trgs)
         | ((i',TypExpr (ExpId src)),_) <- stClasses
         , let trgs= [tn | (_,TypExpr (ExpId tn))<-(map ((!!) stVerts) $ Graph.reachable stGraph i')]
         ]
    typedRels
     = [relname++"[ "++srcnames++" * "++trgnames++" ]"
       | (Rel relname) <- Set.toList $ rels sentences
       , let srcnames = nametree (ExpRel (Rel relname))
       , let trgnames = nametree (ExpFlp (Rel relname))]
    nametree e
     = wrisects [s | Just s <- map getName $ Graph.dfs stGraph [i'|(i',tp)<-stVerts,tp==(getEqClass $ TypExpr e)]]
    getName :: Tree.Tree Int -> Maybe String
    getName a
     = prefer (snd (stVerts !! Tree.rootLabel a)) ([n | Just n <- map getName $ Tree.subForest a])
    prefer :: Type -> [String] -> Maybe String
    prefer (TypExpr (ExpId c)) _ = Just c
    prefer _ [] = Nothing
    prefer _ as = Just $ wrisects as
    wrisects [] = "??"
    wrisects as = foldr1 (\x y-> x ++ " /\\ " ++ y) as
    (+++) a b = a ++", "++ b

  (!!!) :: [a] -> Int -> a
  (!!!) a b | b >= length a = error "index too large!"
            | otherwise = (!!) a b
  
  rels :: [Expression] -> Set.Set Relation
  rels x = (Set.fromList . concat . map erels) x
   where 
     erels (Pequ (a,b)) = erels a ++ erels b
     erels (Pimp (a,b)) = erels a ++ erels b
     erels (ExpRel a)   = [a]
     erels (ExpFlp a)   = [a]
     erels (ExpId _)    = []
     erels (ExpIsc a b) = erels a ++ erels b
     erels (ExpUni a b) = erels a ++ erels b
     erels (ExpApp a b) = erels a ++ erels b
