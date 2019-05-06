{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.ToFSpec.CreateFspec
  ( createMulti
  , pCtx2Fspec
  )

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Core.A2P_Converters
import           Ampersand.Core.ShowPStruct  -- Just for debugging purposes
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.FSpec.Transformers 
import           Ampersand.Input
import           Ampersand.Misc
import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)
import qualified Data.Set as Set
import           System.FilePath

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
--   which is the result of createMulti.
createMulti :: Options  -- ^The options derived from the command line
            -> IO(Guarded MultiFSpecs)
createMulti opts@Options{..} =
  do fAmpP_Ctx :: Guarded P_Context <-
        if genMetaFile ||
           genRapPopulationOnly ||
           addSemanticMetamodel
        then parseMeta opts -- the P_Context of the formalAmpersand metamodel
        else return --Not very nice way to do this, but effective. Don't try to remove the return, otherwise the fatal could be evaluated... 
               $ fatal "With the given switches, the formal ampersand model is not supposed to play any part."
     userP_Ctx:: Guarded P_Context <- 
        case fileName of
          Just x -> snd <$> parseADL opts x -- the P_Context of the user's sourceFile
          Nothing -> exitWith . WrongArgumentsGiven $ ["Please supply the name of an ampersand file"]
    
     systemP_Ctx:: Guarded P_Context <- parseSystemContext opts

     let fAmpFSpec :: FSpec
         fAmpFSpec = case pCtx2Fspec opts fAmpP_Ctx of
                       Checked f _ -> f
                       Errors errs -> fatal . unlines $
                            "The FormalAmpersand ADL scripts are not type correct:"
                          : (intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)

         userP_CtxPlus :: Guarded P_Context
         userP_CtxPlus =
              if addSemanticMetamodel 
              then addSemanticModel <$> userP_Ctx
              else                      userP_Ctx
          where
            -- | When the semantic model of Formal Ampersand is added to the user's model, we add
            --   the relations as wel as the generalisations to it, so they are available to the user
            --   in an implicit way. We want other things, like Idents, Views and REPRESENTs available too.
            addSemanticModel :: P_Context -> P_Context
            addSemanticModel pCtx  
              = pCtx {ctx_ds = ctx_ds pCtx ++ map aRelation2pRelation (Set.toList . instances $ fAmpFSpec)
                     ,ctx_gs = ctx_gs pCtx ++ map aClassify2pClassify (Set.toList . instances $ fAmpFSpec)
                     ,ctx_vs = ctx_vs pCtx ++ map aViewDef2pViewDef (Set.toList . instances $ fAmpFSpec)
                     ,ctx_ks = ctx_ks pCtx ++ map aIdentityDef2pIdentityDef (Set.toList . instances $ fAmpFSpec)
                     ,ctx_reprs = ctx_reprs pCtx ++ (reprList . fcontextInfo $ fAmpFSpec)
                     }

         userGFSpec :: Guarded FSpec
         userGFSpec = 
            pCtx2Fspec opts $ 
              if useSystemContext
              then mergeContexts <$> userP_CtxPlus   -- the FSpec resuting from the user's souceFile
                                 <*> systemP_Ctx -- the system artifacts required for all ampersand prototypes
              else userP_Ctx
         
         result :: Guarded MultiFSpecs
         result = 
           if genRapPopulationOnly
           then case userGFSpec of 
                  Errors err -> Errors err  
                  Checked usrFSpec _
                           -> let grinded :: P_Context
                                  grinded = grind opts fAmpFSpec usrFSpec -- the user's sourcefile grinded, i.e. a P_Context containing population in terms of formalAmpersand.
                                  metaPopPCtx :: Guarded P_Context
                                  metaPopPCtx = mergeContexts grinded <$> fAmpP_Ctx
                                  metaPopFSpec :: Guarded FSpec
                                  metaPopFSpec = pCtx2Fspec opts metaPopPCtx
                              in MultiFSpecs <$> (pCtx2Fspec opts $ mergeContexts <$> userP_CtxPlus <*> pure grinded)
                                             <*> (Just <$> metaPopFSpec)
           else MultiFSpecs <$> userGFSpec <*> pure Nothing
     res <- if genMetaFile
            then writeMetaFile fAmpFSpec userGFSpec
            else return $ pure ()
     return (res >> result)
  where
    useSystemContext :: Bool
    useSystemContext = genPrototype
    writeMetaFile :: FSpec -> Guarded FSpec -> IO (Guarded ())
    writeMetaFile faSpec userSpec = 
       case makeMetaFile opts faSpec <$> userSpec of
        Checked (filePath,metaContents) ws -> 
                  do verboseLn $ "Generating meta file in path "++dirOutput
                     writeFile (dirOutput </> filePath) metaContents      
                     verboseLn $ "\"" ++ filePath ++ "\" written"
                     return $ Checked () ws
        Errors err -> return (Errors err)

pCtx2Fspec :: Options -> Guarded P_Context -> Guarded FSpec
pCtx2Fspec opts c = makeFSpec opts <$> join (pCtx2aCtx opts <$> encloseInConstraints opts c)

-- | To analyse spreadsheets means to enrich the context with the relations that are defined in the spreadsheet.
--   This is different from the normal behaviour, which checks whether the spreadsheets comply with the Ampersand-script.
--   The function analyse is therefore called only with option 'dataAnalysis' on.
encloseInConstraints :: Options -> Guarded P_Context -> Guarded P_Context
encloseInConstraints opts (Checked pCtx warnings) | dataAnalysis opts = Checked enrichedContext warnings
  where
    enrichedContext :: P_Context
    enrichedContext
     = pCtx{ ctx_ds     = mergeRels (genericRelations++declaredRelations)
           , ctx_pops   = genericPopulations
           }
    declaredRelations ::  [P_Relation]   -- relations declared in the script
    popRelations ::       [P_Relation]   -- relations that are "annotated" by the user in Excel-sheets.
                                         -- popRelations are derived from P_Populations only.
    genericRelations ::   [P_Relation]   -- generalization of popRelations due to CLASSIFY statements
    genericPopulations :: [P_Population] -- generalization of popRelations due to CLASSIFY statements
    declaredRelations = nub (ctx_ds pCtx++concatMap pt_dcs (ctx_pats pCtx))
    -- | To derive relations from populations, we derive the signature from the population's signature directly.
    --   Multiplicity properties are added to constrain the population without introducing violations.
    popRelations 
     = [ computeProps rel
       | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-ctx_pops pCtx++[pop |pat<-ctx_pats pCtx, pop<-pt_pop pat]
       , Just src'<-[src], Just tgt'<-[tgt]
       , rel<-[P_Sgn{dec_nm=name pop, dec_sign=P_Sign (PCpt src') (PCpt tgt'), dec_prps=Set.fromList [], dec_pragma=[], dec_Mean=[], pos=origin pop }]
       , signature rel `notElem` map signature declaredRelations
       ]
       where
          computeProps :: P_Relation -> P_Relation
          computeProps rel
           = rel{dec_prps = Set.fromList ([ Uni | isUni popR]++[ Tot | isTot ]++[ Inj | isInj popR ]++[ Sur | isSur ])}
              where
               sgn  = dec_sign rel
               s = pSrc sgn; t = pTgt sgn
               popu :: P_Concept -> Set.Set PAtomValue
               popu c = (Set.fromList . concat .map p_popas) [ pop | pop@P_CptPopu{}<-pops, name c==name pop ]
               popR :: Set.Set PAtomPair
               popR = (Set.fromList . concat. map p_popps )
                      [ pop
                      | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-pops, Just src'<-[src], Just tgt'<-[tgt]
                      , name rel==name pop, src'==name s, tgt'==name t
                      ]
               domR = Set.mapMonotonic ppLeft popR   --  The use of `mapMonotonic :: (a->b) -> Set a -> Set b` requires that ppLeft is strictly increasing.
               codR = Set.mapMonotonic ppRight popR  --  Alert: `mapMonotonic` does NOT check. It just produces erroneous results if it doesn't...
               equal f (a,b) = f a == f b
               isUni :: Set.Set PAtomPair -> Bool
               isUni x = null . Set.filter (not . equal ppRight) . Set.filter (equal ppLeft) $ Set.cartesianProduct x x
               isTot = popu s `Set.isSubsetOf` domR
               isInj :: Set.Set PAtomPair -> Bool
               isInj x = null . Set.filter (not . equal ppLeft) . Set.filter (equal ppRight) $ Set.cartesianProduct x x
               isSur = popu t `Set.isSubsetOf` codR
    (genericRelations, genericPopulations)
     = recur [] popRelations pops invGen
       where
        recur :: [P_Concept]->[P_Relation]->[P_Population]->[(P_Concept,Set.Set P_Concept)]->([P_Relation], [P_Population])
        recur     seen         unseenrels    unseenpops      ((g,specs):invGens)
         = if g `elem` seen then fatal ("Concept "++name g++" has caused a cycle error.") else
           recur (g:seen) (genericRels++remainder) (genericPops++remainPop) invGens
           where
            sameNameTargetRels :: [[P_Relation]]
            sameNameTargetRels = eqCl (\r->(name r,targt r)) unseenrels
            genericRels ::    [P_Relation]
            remainingRels :: [[P_Relation]]
            (genericRels, remainingRels)
             = unzip
               [ ( headrel{ dec_sign = P_Sign g (targt headrel)
                          , dec_prps = let test prop = prop `elem` foldr1 Set.intersection (map dec_prps sRel)
                                       in Set.fromList ([Uni |test Uni]++[Tot |test Tot]++[Inj |test Inj]++[Sur |test Sur])
                          }  -- the generic relation that summarizes sRel
            --   , [ rel| rel<-sRel, sourc rel `elem` specs ]                    -- the specific (and therefore obsolete) relations
                 , [ rel| rel<-sRel, sourc rel `notElem` specs ]                 -- the remaining relations
                 )
               | sRel<-sameNameTargetRels
               , specs `Set.isSubsetOf` Set.fromList (map sourc sRel)
               , headrel<-take 1 sRel ]
            remainder :: [P_Relation]
            remainder
             = concat (remainingRels++
                       [ sRel | sRel<-sameNameTargetRels
                       , not (specs `Set.isSubsetOf` Set.fromList (map sourc sRel))]
                      )
            sameNameTargetPops :: [[P_Population]]
            sameNameTargetPops = eqCl (\r->(name r,tgtPop r)) unseenpops
            genericPops ::    [P_Population]
            remainingPops :: [[P_Population]]
            (genericPops, remainingPops)
             = unzip
               [ ( headPop{p_src=Just (name g)}                   -- the generic relation that summarizes sRel
            --   , [ pop| pop<-sPop, srcPop pop `elem` specs ]    -- the specific (and therefore obsolete) populations
                 , [ pop| pop<-sPop, srcPop pop `notElem` specs ] -- the remaining relations
                 )
               | sPop<-sameNameTargetPops
               , specs `Set.isSubsetOf` Set.fromList (map srcPop sPop)
               , headPop<-take 1 sPop ]
            remainPop :: [P_Population]
            remainPop
             = concat (remainingPops++
                       [ sPop | sPop<-sameNameTargetPops
                       , not (specs `Set.isSubsetOf` Set.fromList (map srcPop sPop))]
                      )
        recur _ rels popus [] = (rels,popus)
        srcPop, tgtPop :: P_Population -> P_Concept -- get the source concept of a P_Population.
        srcPop pop@P_CptPopu{} = PCpt (name pop)
        srcPop pop@P_RelPopu{p_src = src} = case src of Just s -> PCpt s; _ -> fatal ("srcPop ("++showP pop++") is mistaken.")
        tgtPop pop@P_CptPopu{} = PCpt (name pop)
        tgtPop pop@P_RelPopu{p_tgt = tgt} = case tgt of Just t -> PCpt t; _ -> fatal ("tgtPop ("++showP pop++") is mistaken.")
    sourc, targt :: P_Relation -> P_Concept -- get the source concept of a P_Relation.
    sourc = pSrc . dec_sign
    targt = pTgt . dec_sign
    invGen :: [(P_Concept,Set.Set P_Concept)]  -- each pair contains a concept with all of its specializations
    invGen = [ (fst (head cl), Set.fromList spcs)
             | cl<-eqCl fst [ (g,specific gen) | gen<-ctx_gs pCtx, g<-NEL.toList (generics gen)]
             , g<-[fst (head cl)], spcs<-[[snd c | c<-cl, snd c/=g]], not (null spcs)
             ]
    signature :: P_Relation -> (String, P_Sign)
    signature rel =(name rel, dec_sign rel)
    concepts = nub $
            [ PCpt (name pop) | pop@P_CptPopu{}<-ctx_pops pCtx] ++
            [ PCpt src' | P_RelPopu{p_src = src}<-ctx_pops pCtx, Just src'<-[src]] ++
            [ PCpt tgt' | P_RelPopu{p_tgt = tgt}<-ctx_pops pCtx, Just tgt'<-[tgt]] ++
            map sourc declaredRelations++ map targt declaredRelations++
            concat [specific gen: NEL.toList (generics gen)| gen<-ctx_gs pCtx]
    pops = computeConceptPopulations (ctx_pops pCtx++[p |pat<-ctx_pats pCtx, p<-pt_pop pat])   -- All populations defined in this context, from POPULATION statements as well as from Relation declarations.
    computeConceptPopulations :: [P_Population] -> [P_Population]
    computeConceptPopulations pps -- I feel this computation should be done in P2A_Converters.hs, so every A_structure has compliant populations.
     = [ P_CptPopu{pos = OriginUnknown, p_cnme = name c, p_popas = nub $
                       [ atom | cpt@P_CptPopu{}<-pps, PCpt (name cpt) == c, atom<-p_popas cpt]++
                       [ ppLeft pair
                       | pop@P_RelPopu{p_src = src}<-pps, Just src'<-[src], PCpt src' == c
                       , pair<-p_popps pop]++
                       [ ppRight pair
                       | pop@P_RelPopu{p_tgt = tgt}<-pps, Just tgt'<-[tgt], PCpt tgt' == c
                       , pair<-p_popps pop]}
       | c<-concepts
       ] ++
       [ rpop{p_popps=concat (map p_popps cl)}
       | cl<-eqCl (\pop->(name pop,p_src pop,p_tgt pop)) [ pop | pop@P_RelPopu{}<-pps], rpop<-take 1 cl
       ]
encloseInConstraints _ gCtx = gCtx

--    specializations :: P_Concept -> [P_Concept]
--    specializations cpt = nub $ cpt: [ specific gen | gen<-ctx_gs pCtx, cpt `elem` generics gen ]
--    generalizations :: P_Concept -> [P_Concept]
--    generalizations cpt = nub $ cpt: [ g | gen<-ctx_gs pCtx, g<-NEL.toList (generics gen), cpt==specific gen ]