{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.FSpec.ShowMeatGrinder
  ( makeMetaFile
  , grind 
  , addSemanticModel
  , GrindInfo(..)
  , MetaModel(..)
  )
where

import           Ampersand.Basics
import           Ampersand.Core.A2P_Converters
import           Ampersand.ADL1
import           Ampersand.Core.ShowPStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Transformers
import           Ampersand.Misc
import qualified RIO.List as L
import qualified RIO.Set as Set

data MetaModel = FormalAmpersand | FADocumented | SystemContext
       deriving (Eq, Ord, Enum, Bounded)
instance Named MetaModel where
  name FormalAmpersand = "Formal Ampersand"
  name FADocumented    = "Formal Ampersand (documented)"
  name SystemContext   = "System context"

data GrindInfo = GrindInfo
    { metaModel    :: MetaModel
    , pModel       :: P_Context
    , fModel       :: FSpec
    , transformers :: Options -> FSpec -> [Transformer]
    }

-- | The 'grind' function creates a P_Context that contains the population for every
--   relation in the metamodel. The population is defined by the given FSpec,
--   which usually is the FSpec of the user. 
grind :: Options -> GrindInfo -> FSpec -> P_Context
grind opts grindInfo userFspec =
  PCtx{ ctx_nm     = "Grinded_"++name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = map aRelation2pRelation . Set.toList . instances . fModel $ grindInfo
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = map aClassify2pClassify . Set.toList . instances . fModel $ grindInfo
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = mapMaybe populationFromPop . Set.toList $ metaPops2
      , ctx_metas  = []
      }
  where
    metaPops2 :: Set.Set Pop
    metaPops2 = Set.fromList 
              . concatMap (Set.toList . grindedPops opts grindInfo userFspec)
              . Set.toList . instances . fModel $ grindInfo
    populationFromPop :: Pop -> Maybe P_Population
    populationFromPop pop =
      case pop of 
        Comment{} -> Nothing
        Pop{}     -> Just $
             P_RelPopu { p_src  = Nothing
                       , p_tgt  = Nothing
                       , pos    = orig
                       , p_nmdr = PNamedRel 
                            { pos    = orig
                            , p_nrnm = name rel
                            , p_mbSign = Just . aSign2pSign . sign $ rel
                            }
                       , p_popps = map convertPair . Set.toList . popPairs $ pop
                       }
          where rel = popRelation pop
                orig = MeatGrinder
                convertPair :: (PopAtom,PopAtom) -> PAtomPair
                convertPair (a,b) = 
                    PPair { pos = orig
                          , ppLeft  = pAtom2AtomValue a
                          , ppRight = pAtom2AtomValue b
                          }
            
                pAtom2AtomValue :: PopAtom -> PAtomValue
                pAtom2AtomValue atm = 
                  case atm of 
                    DirtyId str         -> ScriptString orig str
                    PopAlphaNumeric str -> ScriptString orig str
                    PopInt i            -> ScriptInt orig i
           
-- | When the semantic model of a metamodel is added to the user's model, we add
--   the relations as wel as the generalisations to it, so they are available to the user
--   in an implicit way. We want other things, like Idents, Views and REPRESENTs available too.
addSemanticModel :: GrindInfo -> P_Context -> P_Context
addSemanticModel gInfo pCtx = 
   PCtx    
        { ctx_nm     = ctx_nm     pCtx
        , ctx_pos    = ctx_pos    pCtx
        , ctx_lang   = ctx_lang   pCtx
        , ctx_markup = ctx_markup pCtx
        , ctx_pats   = ctx_pats   pCtx `uni` ctx_pats   pCtxOfMetaModel
        , ctx_rs     = ctx_rs     pCtx `uni` ctx_rs     pCtxOfMetaModel
        , ctx_ds     = ctx_ds     pCtx `uni` ctx_ds     pCtxOfMetaModel
        , ctx_cs     = ctx_cs     pCtx `uni` ctx_cs     pCtxOfMetaModel
        , ctx_ks     = ctx_ks     pCtx `uni` ctx_ks     pCtxOfMetaModel
        , ctx_rrules = ctx_rrules pCtx `uni` ctx_rrules pCtxOfMetaModel
        , ctx_reprs  = ctx_reprs  pCtx `uni` ctx_reprs  pCtxOfMetaModel
        , ctx_vs     = ctx_vs     pCtx `uni` ctx_vs     pCtxOfMetaModel
        , ctx_gs     = ctx_gs     pCtx `uni` ctx_gs     pCtxOfMetaModel
        , ctx_ifcs   = ctx_ifcs   pCtx `uni` (if True -- DISABLED. See issue #979
                                              then [] 
                                              else ctx_ifcs   pCtxOfMetaModel)
        , ctx_ps     = ctx_ps     pCtx 
        , ctx_pops   = ctx_pops   pCtx `uni` ctx_pops   pCtxOfMetaModel
        , ctx_metas  = ctx_metas  pCtx
        }
           where
            pCtxOfMetaModel = pModel gInfo
            uni :: Eq a => [a] -> [a] -> [a]
            uni xs ys = L.nub (xs ++ ys)
 
data Pop = Pop { popPairs  :: Set.Set (PopAtom,PopAtom)
               , popRelation :: Relation
               }
         | Comment { comment :: [String]  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   } deriving (Eq,Ord)

showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> showP . aRelation2pRelation . popRelation $ pop
      Comment{} -> L.intercalate "\n" . map ("-- " ++) . comment $ pop
-- ^ Write the meta-information of an FSpec to a file. This is usefull for debugging.
--   The comments that are in Pop are preserved. 
makeMetaFile :: Options -> GrindInfo -> FSpec -> (FilePath,String)
makeMetaFile opts@Options{..} metaModel userFspec
  = ("MetaPopulationFile.adl", content )
  where
    content = unlines $
        ([ "{- Do not edit manually. This code has been generated!!!"
        , "    Generated with "++ampersandVersionStr
        , "    Generated at "++show genTime
        , " "
        , "The populations defined in this file are the populations from the user's"
        , "model named '"++name userFspec++"'."
        , ""
        , "-}"
        , "CONTEXT Grinded_"++name userFspec
        , "" ]
        ++ listOfConcepts
        ++ [""]
        ++ body
        ++
        [ ""
        , "ENDCONTEXT"
        ])
    body :: [String]
    body =
         L.intercalate [""]
       . L.sort
       . map (lines . showPop )
       . concatMap (Set.toList . popsOfRelation)
       . L.sortOn showRel
       . Set.toList . instances . fModel $ metaModel
    listOfConcepts :: [String]
    listOfConcepts = map ("-- "++) .
                     L.intercalate [""] . 
                     map showCpt . L.sortOn name . Set.toList . instances . fModel $ metaModel
       where
        showCpt :: A_Concept -> [String]
        showCpt cpt = [name cpt] ++ ( map ("  "++)
                                    . L.sort 
                                    . map show
                                    . Set.toList
                                    $ pAtomsOfConcept cpt
                                    )
        
    popsOfRelation :: Relation -> Set.Set Pop
    popsOfRelation = grindedPops opts metaModel userFspec
    pAtomsOfConcept :: A_Concept -> Set.Set PopAtom
    pAtomsOfConcept cpt = getPopsSet Src `Set.union` getPopsSet Tgt
      where getPopsSet :: SrcOrTgt -> Set.Set PopAtom
            getPopsSet x = Set.fromList . map (case x of
                                                 Src -> fst
                                                 Tgt -> snd
                                              )
                         . Set.toList . Set.unions.  map popPairs 
                         . Set.toList . Set.unions . map popsOfRelation 
                         . Set.toList . Set.filter (\rel-> case x of
                                                             Src -> source rel == cpt
                                                             Tgt -> target rel == cpt
                                                   ) 
                         . instances . fModel $ metaModel

grindedPops :: Options -> GrindInfo -> FSpec -> Relation -> Set.Set Pop
grindedPops opts@Options{..} grindInfo userFspec rel = 
  case filter (isForRel rel) ((transformers grindInfo) opts userFspec) of
    []  -> fatal . unlines $ 
              ["Every relation in "++name (metaModel grindInfo)++" must have a transformer in Transformers.hs"
              ,"   Violations:"
              ] ++ map ("      "++) viols
            where 
              viols = map showRelOrigin 
                    . Set.toList
                    . Set.filter hasNoTransformer 
                    . instances . fModel $ grindInfo
              hasNoTransformer :: Relation -> Bool
              hasNoTransformer d = null (filter (isForRel d) ((transformers grindInfo) opts userFspec))
              showRelOrigin :: Relation -> String
              showRelOrigin r = showRel r++" ( "++show (origin r)++" )."
    ts  -> Set.fromList . map transformer2Pop $ ts 
  where
    transformer2Pop :: Transformer -> Pop
    transformer2Pop (Transformer relName src tgt popPairs) 
      | not ( all (ttypeOf (source rel)) (map fst . Set.toList $ popPairs) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "++name (metaModel grindInfo)++"."
                 , "   The population of the relation `"++ relName ++"["++ src ++" * "++ tgt ++"]` "
                 , "   violates this rule for concept `"++ src ++"`. In "++name (metaModel grindInfo)++" "
                 , "   the TType of this concept is "++(show . cptTType (fModel grindInfo) $ source rel)++"."
                 ]
      | not ( all (ttypeOf (target rel)) (map snd . Set.toList $ popPairs) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "++name (metaModel grindInfo)++"."
                 , "   The population of the relation `"++ relName ++"["++ src ++" * "++ tgt ++"]` "
                 , "   violates this rule for concept `"++ tgt ++"`. In "++name (metaModel grindInfo)++" "
                 , "   the TType of this concept is "++(show . cptTType (fModel grindInfo) $ target rel)++"." 
                 ]
      | otherwise = Pop { popRelation = rel
                        , popPairs    = popPairs
                        }
      where ttypeOf :: A_Concept -> (PopAtom -> Bool)
            ttypeOf cpt =
              case (cptTType (fModel grindInfo)) cpt of
                Object          -> isDirtyId
                Alphanumeric    -> isTextual
                BigAlphanumeric -> isTextual
                HugeAlphanumeric -> isTextual
                tt              -> fatal $ "No test available yet. "++show tt++" encountered for the first time in "++name (metaModel grindInfo)++""
            isDirtyId pa = case pa of
                            DirtyId{}         -> True
                            _                 -> False
            isTextual pa = case pa of
                            PopAlphaNumeric{} -> True
                            _                 -> False
                                        
                            
                                
isForRel :: Relation -> Transformer -> Bool
isForRel rel (Transformer n s t _ ) =
    and [ name rel == n
        , name (source rel) == s
        , name (target rel) == t]
                        
