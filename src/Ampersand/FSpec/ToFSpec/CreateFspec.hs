{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.ToFSpec.CreateFspec
  ( BuildRecipe
  , BuildStep(Grind,EncloseInConstraints)
  , StartContext(..)
  , MetaModel(..)
  , createFspec 
  , script
  , merge
  , andThen
  )

where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Core.ShowPStruct  -- Just for debugging purposes
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.MetaModels
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.Input
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
-- | create an FSpec, based on the provided command-line options.
--   Without the command-line switch "--meta-tables", 
--   Ampersand compiles its script (userP_Ctx) straightforwardly in first order relation algebra.
--   This is useful for simple scripts and the compilation process is easy to understand.
--
--   With "--meta-tables" switched on, Ampersand does more.
--   This switch is useful for higher order Ampersand,
--   in which the user can work with the rules, relations and concepts of the model inside the model.
--   Besides the user script, userP_Ctx, Ampersand creates its own metamodel, rapP_Ctx, which is generated from "AST.adl"
--   This metamodel is populated with the result of grinding userP_Ctx, being populationPctx.
--   Grinding means to analyse the script down to the binary relations that constitute the metamodel.
--   The combination of model and populated metamodel results in the Guarded FSpec,
--   which is the result of createFSpec.

createFspec :: (HasFSpecGenOpts env, HasRootFile env, HasLogFunc env) => 
               BuildRecipe -> RIO env (Guarded FSpec)
createFspec recipe = do 
    env <- ask
    metaModelsMap :: Map MetaModel GrindInfo <- do 
         let fun :: (HasLogFunc env, HasFSpecGenOpts env) => MetaModel -> RIO env (MetaModel , GrindInfo)
             fun m = (,) m <$> mkGrindInfo m
         Map.fromList <$> (sequence $ fun <$> (Set.toList $ metaModelsIn recipe))
    parsedUserScript :: Guarded P_Context <- do
         rootFile <- fromMaybe (fatal "No script was given!") <$> (view rootFileL)
         snd <$> parseFileTransitive rootFile -- the P_Context of the user's sourceFile
    let cooked :: Guarded P_Context
        cooked = cook env recipe metaModelsMap parsedUserScript
    return . join $ pCtx2Fspec env <$> cooked

class MetaModelContainer a where
  metaModelsIn :: a -> Set MetaModel
-- | A recipe to build an FSpec defines the way that FSpec should be constructed.
--   It consists of a initial P_Context and list of follow-up steps.
data BuildRecipe = BuildRecipe StartContext [BuildStep]
instance MetaModelContainer BuildRecipe where
  metaModelsIn (BuildRecipe x y) = metaModelsIn x `Set.union` metaModelsIn y
-- | The initial context to use in a recipe. It is either the user's script or
--   the script from a given MetaModel. 
data StartContext = UserScript | MetaScript MetaModel
instance MetaModelContainer StartContext where
  metaModelsIn UserScript = mempty
  metaModelsIn (MetaScript m) = Set.singleton m
-- | A buildstep describes a conversion to a given context.  
data BuildStep = 
    Grind MetaModel       -- ^ Grind the given P_Context using the given MetaModel. The resulting P_Context
                          --   contains all relations from the Metamodel. Those relations are populated using the 
                          --   original P_Context. 
  | MergeWith BuildRecipe -- ^ Merge the given P_Context with the P_Context that is the result of 
                          --   applying the BuildRecipe.
  | EncloseInConstraints  -- ^ Apply the encloseInConstraints function to the given P_Context.
instance MetaModelContainer BuildStep where
  metaModelsIn (Grind m) = Set.singleton m
  metaModelsIn (MergeWith x) = metaModelsIn x
  metaModelsIn EncloseInConstraints = mempty 
instance MetaModelContainer a => MetaModelContainer [a] where
  metaModelsIn = Set.unions . fmap metaModelsIn

-- | A simple recipe that builds from a script
script :: StartContext -> BuildRecipe
script x = BuildRecipe x []

-- | Merge two recipes together
merge :: BuildRecipe -> BuildRecipe -> BuildRecipe
merge a b = a `andThen` MergeWith b

-- | Add an additional step after the steps of a recipe
andThen :: BuildRecipe -> BuildStep -> BuildRecipe
andThen (BuildRecipe start steps) step = BuildRecipe start (steps<>[step])

-- | This functions does the work in the kitchen: use the recipe to return a
--   P_Context from which the FSpec can be built. 
--   Note that we do not want this function to run in the RIO monad, for we want it to
--   be pure. Information that would otherwise have to be read as a side effect is now 
--   given as parameter, like the original user's P_Context, and a map that can be used
--   to obtain GrindInfo for metamodels. 
cook :: (HasFSpecGenOpts env) => 
         env -- ^ The environment
      -> BuildRecipe -- ^ Instructions for the man in the kitchen
      -> Map MetaModel GrindInfo -- ^ A map containing all GrindInfo that could be required
      -> Guarded P_Context  -- ^ The original user's P_Context, Guarded because it might have errors 
      -> Guarded P_Context 
cook env (BuildRecipe start steps) grindInfoMap user = 
    join $ doSteps <$> case start of
                    UserScript -> user
                    MetaScript mm -> pure . pModel $ gInfo mm
  where 
  doSteps :: P_Context -> Guarded P_Context
  doSteps pCtx = foldM nextStep pCtx steps
    where
      nextStep :: P_Context -> BuildStep -> Guarded P_Context
      nextStep ctx step = 
        case step of 
          EncloseInConstraints -> pure $ encloseInConstraints ctx 
          Grind mm -> grind (gInfo mm) <$> (pCtx2Fspec env ctx)
          MergeWith recipe -> mergeContexts ctx <$> cook env recipe grindInfoMap user
  gInfo :: MetaModel -> GrindInfo
  gInfo mm = case Map.lookup mm grindInfoMap of
            Just x -> x
            Nothing -> fatal $ "metaModel `"<>tshow mm<>"`was not found!"


-- | To analyse spreadsheets means to enrich the context with the relations that are defined in the spreadsheet.
--   The function encloseInConstraints does not populate existing relations.
--   Instead it invents relations from a given population, which typically comes from a spreadsheet.
--   This is different from the normal behaviour, which checks whether the spreadsheets comply with the Ampersand-script.
--   This function is called only with option 'dataAnalysis' on.
encloseInConstraints :: P_Context -> P_Context
encloseInConstraints pCtx = enrichedContext
  where
  --The result of encloseInConstraints is a P_Context enriched with the relations in genericRelations
  --The population is reorganized in genericPopulations to accommodate the particular ISA-graph.
    enrichedContext :: P_Context
    enrichedContext
     = pCtx{ ctx_ds     = mergeRels (genericRelations<>declaredRelations)
           , ctx_pops   = genericPopulations
           }
    declaredRelations ::  [P_Relation]   -- relations declared in the user's script
    popRelations ::       [P_Relation]   -- relations that are "annotated" by the user in Excel-sheets.
                                         -- popRelations are derived from P_Populations only.
    declaredRelations = mergeRels (ctx_ds pCtx<>concatMap pt_dcs (ctx_pats pCtx))
    -- | To derive relations from populations, we derive the signature from the population's signature directly.
    --   Multiplicity properties are added to constrain the population without introducing violations.
    popRelations 
     = [ computeProps rel
       | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-ctx_pops pCtx<>[pop |pat<-ctx_pats pCtx, pop<-pt_pop pat]
       , Just src'<-[src], Just tgt'<-[tgt]
       , rel<-[ P_Sgn{ dec_nm     = name pop
                     , dec_sign   = P_Sign src' tgt'
                     , dec_prps   = mempty
                     , dec_pragma = mempty
                     , dec_Mean   = mempty
                     , pos        = origin pop
                     }]
       , signatur rel `notElem` map signatur declaredRelations
       ]
       where
          computeProps :: P_Relation -> P_Relation
          computeProps rel
           = rel{dec_prps = Set.fromList ([ Uni | isUni popR]<>[ Tot | isTot ]<>[ Inj | isInj popR ]<>[ Sur | isSur ])}
              where
               sgn  = dec_sign rel
               s = pSrc sgn; t = pTgt sgn
               popu :: P_Concept -> Set.Set PAtomValue
               popu c = (Set.fromList . concat .map p_popas) [ pop | pop@P_CptPopu{}<-pops, name c==name pop ]
               popR :: Set.Set PAtomPair
               popR = (Set.fromList . concat. map p_popps )
                      [ pop
                      | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-pops, Just src'<-[src], Just tgt'<-[tgt]
                      , name rel==name pop, src'== s, tgt'== t
                      ]
               domR = Set.fromList . map ppLeft  . Set.toList $ popR
               codR = Set.fromList . map ppRight . Set.toList $ popR
               equal f (a,b) = f a == f b
               isUni :: Set.Set PAtomPair -> Bool
               isUni x = null . Set.filter (not . equal ppRight) . Set.filter (equal ppLeft) $ cartesianProduct x x
               isTot = popu s `Set.isSubsetOf` domR
               isInj :: Set.Set PAtomPair -> Bool
               isInj x = null . Set.filter (not . equal ppLeft) . Set.filter (equal ppRight) $ cartesianProduct x x
               isSur = popu t `Set.isSubsetOf` codR
               cartesianProduct :: -- Should be implemented as Set.cartesianProduct, but isn't. See https://github.com/commercialhaskell/rio/issues/177
                                   (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
               cartesianProduct xs ys = Set.fromList $ liftA2 (,) (toList xs) (toList ys)
    genericRelations ::   [P_Relation]   -- generalization of popRelations due to CLASSIFY statements
    genericPopulations :: [P_Population] -- generalization of popRelations due to CLASSIFY statements
    -- | To derive relations from populations, we derive the signature from the population's signature directly.
    --   Multiplicity properties are added to constrain the population without introducing violations.
    (genericRelations, genericPopulations)
     = recur [] popRelations pops invGen
       where
        recur :: [P_Concept]->[P_Relation]->[P_Population]->[(P_Concept,Set.Set P_Concept)]->([P_Relation], [P_Population])
        recur     seen         unseenrels    unseenpops      ((g,specs):invGens)
         = if g `elem` seen then fatal ("Concept "<>name g<>" has caused a cycle error.") else
           recur (g:seen) (genericRels<>remainder) (genericPops<>remainPop) invGens
           where
            sameNameTargetRels :: [NE.NonEmpty P_Relation]
            sameNameTargetRels = eqCl (\r->(name r,targt r)) unseenrels
            genericRels ::    [P_Relation]
            remainingRels :: [[P_Relation]]
            (genericRels, remainingRels)
             = L.unzip
               [ ( headrel{ dec_sign = P_Sign g (targt (NE.head sRel))
                          , dec_prps = let test prop = prop `elem` foldr Set.intersection Set.empty (fmap dec_prps sRel)
                                       in Set.fromList ([Uni |test Uni]<>[Tot |test Tot]<>[Inj |test Inj]<>[Sur |test Sur])
                          }  -- the generic relation that summarizes sRel
            --   , [ rel| rel<-sRel, sourc rel `elem` specs ]                    -- the specific (and therefore obsolete) relations
                 , [ rel| rel<-NE.toList sRel, sourc rel `notElem` specs ]                 -- the remaining relations
                 )
               | sRel<-sameNameTargetRels
               , specs `Set.isSubsetOf` (Set.fromList . NE.toList $ (fmap sourc sRel))
               , headrel<-[NE.head sRel]
               ]
            remainder :: [P_Relation]
            remainder
             = concat (remainingRels<>fmap NE.toList
                       [ sRel | sRel<-sameNameTargetRels
                       , not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap sourc sRel))]
                      )
            sameNameTargetPops :: [NE.NonEmpty P_Population]
            sameNameTargetPops = eqCl (\r->(name r,tgtPop r)) unseenpops
            genericPops ::    [P_Population]
            remainingPops :: [[P_Population]]
            (genericPops, remainingPops)
             = L.unzip
               [ ( headPop{p_src=Just g}                   -- the generic relation that summarizes sRel
            --   , [ pop| pop<-sPop, srcPop pop `elem` specs ]    -- the specific (and therefore obsolete) populations
                 , [ pop| pop<-NE.toList sPop, srcPop pop `notElem` specs ] -- the remaining relations
                 )
               | sPop<-sameNameTargetPops
               , specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop)
               , headPop@P_RelPopu{}<-[NE.head sPop] -- Restrict to @P_RelPopu{} because field name p_src is being used
               ]
            remainPop :: [P_Population]
            remainPop
             = concat (remainingPops<>fmap NE.toList
                       [ sPop | sPop<-sameNameTargetPops
                       , not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop))]
                      )
        recur _ rels popus [] = (rels,popus)
        srcPop, tgtPop :: P_Population -> P_Concept -- get the source concept of a P_Population.
        srcPop pop@P_CptPopu{} = PCpt (name pop)
        srcPop pop@P_RelPopu{p_src = src} = case src of Just s -> s; _ -> fatal ("srcPop ("<>showP pop<>") is mistaken.")
        tgtPop pop@P_CptPopu{} = PCpt (name pop)
        tgtPop pop@P_RelPopu{p_tgt = tgt} = case tgt of Just t -> t; _ -> fatal ("tgtPop ("<>showP pop<>") is mistaken.")

    sourc, targt :: P_Relation -> P_Concept -- get the source concept of a P_Relation.
    sourc = pSrc . dec_sign
    targt = pTgt . dec_sign
    invGen :: [(P_Concept,Set.Set P_Concept)]  -- each pair contains a concept with all of its specializations
    invGen = [ (fst (NE.head cl), Set.fromList spcs)
             | cl<-eqCl fst [ (g,specific gen) | gen<-ctx_gs pCtx, g<-NE.toList (generics gen)]
             , g<-[fst (NE.head cl)], spcs<-[[snd c | c<-NE.toList cl, snd c/=g]], not (null spcs)
             ]
    signatur :: P_Relation -> (Text, P_Sign)
    signatur rel =(name rel, dec_sign rel)
    concepts = L.nub $
            [ PCpt (name pop) | pop@P_CptPopu{}<-ctx_pops pCtx] <>
            [ src' | P_RelPopu{p_src = src}<-ctx_pops pCtx, Just src'<-[src]] <>
            [ tgt' | P_RelPopu{p_tgt = tgt}<-ctx_pops pCtx, Just tgt'<-[tgt]] <>
            map sourc declaredRelations<> map targt declaredRelations<>
            concat [specific gen: NE.toList (generics gen)| gen<-ctx_gs pCtx]
    pops = computeConceptPopulations (ctx_pops pCtx<>[p |pat<-ctx_pats pCtx, p<-pt_pop pat])   -- All populations defined in this context, from POPULATION statements as well as from Relation declarations.
    computeConceptPopulations :: [P_Population] -> [P_Population]
    computeConceptPopulations pps -- I feel this computation should be done in P2A_Converters.hs, so every A_structure has compliant populations.
     = [ P_CptPopu{pos = OriginUnknown, p_cpt = c, p_popas = L.nub $
                       [ atom | cpt@P_CptPopu{}<-pps, PCpt (name cpt) == c, atom<-p_popas cpt]<>
                       [ ppLeft pair
                       | pop@P_RelPopu{p_src = src}<-pps, Just src'<-[src], src' == c
                       , pair<-p_popps pop]<>
                       [ ppRight pair
                       | pop@P_RelPopu{p_tgt = tgt}<-pps, Just tgt'<-[tgt], tgt' == c
                       , pair<-p_popps pop]}
       | c<-concepts
       ] <>
       [ rpop{p_popps=concat (fmap p_popps cl)}
       | cl<-eqCl (\pop->(name pop,p_src pop,p_tgt pop)) [ pop | pop@P_RelPopu{}<-pps], rpop<-[NE.head cl]
       ]

--    specializations :: P_Concept -> [P_Concept]
--    specializations cpt = nub $ cpt: [ specific gen | gen<-ctx_gs pCtx, cpt `elem` generics gen ]
--    generalizations :: P_Concept -> [P_Concept]
--    generalizations cpt = nub $ cpt: [ g | gen<-ctx_gs pCtx, g<-NE.toList (generics gen), cpt==specific gen ]

