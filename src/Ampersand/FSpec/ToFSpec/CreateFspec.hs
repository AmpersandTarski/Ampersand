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
import           Ampersand.Core.A2P_Converters
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

encloseInConstraints :: Options -> Guarded P_Context -> Guarded P_Context
encloseInConstraints opts (Checked pCtx warnings) | dataAnalysis opts = Checked (analyse pCtx) warnings
encloseInConstraints _ gCtx = gCtx

analyse :: P_Context -> P_Context
analyse pCtx
 = PCtx{ ctx_nm     = ctx_nm     pCtx
       , ctx_pos    = ctx_pos    pCtx
       , ctx_lang   = ctx_lang   pCtx
       , ctx_markup = ctx_markup pCtx
       , ctx_pats   = ctx_pats   pCtx
       , ctx_rs     = ctx_rs     pCtx
       , ctx_ds     = [ rel{dec_prps = computeProps rel}
                      | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-ctx_pops pCtx, Just src'<-[src], Just tgt'<-[tgt]
                      , rel<-[P_Sgn{dec_nm=name pop, dec_sign=P_Sign (PCpt src') (PCpt tgt'), dec_prps=Set.fromList [], dec_pragma=[], dec_Mean=[], pos=origin pop }]
                      , signature rel `notElem` map signature (ctx_ds pCtx)] ++
                      ctx_ds pCtx
       , ctx_cs     = [ c
                      | pop@P_CptPopu{}<-ctx_pops pCtx
                      , c<-[Cd{pos=origin pop, cdcpt=p_cnme pop, cddef="", cdref="", cdfrom=""}]
                      , name c `notElem` map name (ctx_cs pCtx)] ++
                      ctx_cs pCtx
       , ctx_ks     = ctx_ks     pCtx
       , ctx_rrules = ctx_rrules pCtx
       , ctx_rrels  = ctx_rrels  pCtx
       , ctx_reprs  = ctx_reprs  pCtx
       , ctx_vs     = ctx_vs     pCtx
       , ctx_gs     = ctx_gs     pCtx
       , ctx_ifcs   = ctx_ifcs   pCtx
       , ctx_ps     = ctx_ps     pCtx
       , ctx_pops   = ctx_pops   pCtx
       , ctx_metas  = ctx_metas  pCtx
       }
    where
      signature :: P_Relation -> (String, P_Sign)
      signature rel =(name rel, dec_sign rel)
      pops = ctx_pops pCtx
      computeProps rel
       = Set.fromList ([ Uni | isUni ]++[ Tot | isTot ]++[ Inj | isInj ]++[ Sur | isSur ])
          where
           sgn  = dec_sign rel
           s = pSrc sgn; t = pTgt sgn
           popS = Set.fromList (concat [ p_popas pop | pop@P_CptPopu{}<-pops, name s==name pop ])
           popT = Set.fromList (concat [ p_popas pop | pop@P_CptPopu{}<-pops, name t==name pop ])
           popR = (Set.fromList . concat)
                  [ p_popps pop
                  | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-pops, Just src'<-[src], Just tgt'<-[tgt]
                  , name rel==name pop, src'==name s, tgt'==name t
                  ]
           domR = Set.mapMonotonic ppLeft popR   --  The use of `mapMonotonic :: (a->b) -> Set a -> Set b` requires that ppLeft is strictly increasing.
           codR = Set.mapMonotonic ppRight popR
           isUni = null [ () | pair0<-Set.toList popR, pair1<-Set.toList popR, ppLeft pair0==ppLeft pair1, ppRight pair0/=ppRight pair1]
           isUni'= null [(pair0,pair1) | (pair0,pair1) <- Set.toList $ Set.cartesianProduct popR popR
                                       , equalLefts (pair0,pair1)
                                       , not . equalRights $ (pair0,pair1)
                                       ]
         --  isUni'' = isNull . filter (not . equalRights) . filter equalLefts $ [(pair0,pair1) | pair0 <-Set.toList popR, pair1 <-Set.toList popR]
           
           isUni5 :: Set.Set PAtomPair -> Bool
           isUni5 x = null . Set.filter (not . equalRights) . Set.filter equalLefts $ Set.cartesianProduct x x
           equalLefts, equalRights :: (PAtomPair,PAtomPair) -> Bool
           equalLefts  (a,b) = ppLeft a == ppLeft b
           equalRights (a,b) = ppRight a == ppLeft b
           
           isTot = popS `Set.isSubsetOf` domR
           isInj = null [ () | pair0<-Set.toList popR, pair1<-Set.toList popR, ppRight pair0==ppRight pair1, ppLeft pair0/=ppLeft pair1]
           isSur = popT `Set.isSubsetOf` codR