{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.FSpec.ShowMeatGrinder
  ( grind 
  , GrindInfo(..)
  , MetaModel(..)
  )
where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.A2P_Converters
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Transformers
import qualified RIO.Set as Set
import qualified RIO.Text as T

data MetaModel = FormalAmpersand | PrototypeContext
       deriving (Eq, Ord, Enum, Bounded, Show)
instance Named MetaModel where
  name FormalAmpersand = "Formal Ampersand"
  name PrototypeContext   = "System context"

data GrindInfo = GrindInfo
    { metaModel    :: MetaModel
    , pModel       :: P_Context
    , fModel       :: FSpec
    , transformers :: FSpec -> [Transformer]
    }

-- | The 'grind' function creates a P_Context that contains the population for every
--   relation in the metamodel for which the GrindInfo is given. 
--   The population is defined by the given FSpec, which usually is the FSpec of the user. 
grind :: GrindInfo -> FSpec -> P_Context
grind grindInfo userFspec =
  PCtx{ ctx_nm     = "Grinded_"<>name userFspec
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
      , ctx_pops   = populationFromPop <$> metaPops2
      , ctx_metas  = []
      }
  where
    metaPops2 :: [Pop]
    metaPops2 = concatMap (Set.toList . grindedPops grindInfo userFspec)
              . Set.toList . instances . fModel $ grindInfo
    populationFromPop :: Pop -> P_Population
    populationFromPop pop =
             P_RelPopu { p_src  = Just $ aCpt2pCpt (source rel)
                       , p_tgt  = Just $ aCpt2pCpt (target rel)
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
           
data Pop = Pop { popPairs  :: Set.Set (PopAtom,PopAtom)
               , popRelation :: Relation
               }

grindedPops :: GrindInfo -> FSpec -> Relation -> Set.Set Pop
grindedPops grindInfo userFspec rel = 
  case filter (isForRel rel) ((transformers grindInfo) userFspec) of
    []  -> fatal . T.unlines $ 
              ["Every relation in "<>name (metaModel grindInfo)<>" must have a transformer in Transformers.hs"
              ," However, the following relations have none:"
              ] <> map ("      "<>) viols
            where 
              viols = map showRelOrigin 
                    . Set.toList
                    . Set.filter hasNoTransformer 
                    . instances . fModel $ grindInfo
    [t] -> Set.singleton . transformer2Pop $ t
    ts  -> fatal . T.unlines $ 
              ["Every relation in "<>name (metaModel grindInfo)<>" must have a transformer in Transformers.hs"
              ," However there are "<>tshow (length ts)<>" transformers for relation: "
              ,"      "<>showRelOrigin rel
              ]
  where
    showRelOrigin :: Relation -> Text
    showRelOrigin r = showRel r<>" ( "<>tshow (origin r)<>" )."
    hasNoTransformer :: Relation -> Bool
    hasNoTransformer d = null (filter (isForRel d) ((transformers grindInfo) userFspec))
    transformer2Pop :: Transformer -> Pop
    transformer2Pop (Transformer relName src tgt popPairs) 
      | not ( all (ttypeOf (source rel)) (map fst . Set.toList $ popPairs) ) =
             fatal . T.unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "<>name (metaModel grindInfo)<>"."
                 , "   The population of the relation `" <> relName <>"["<> src <>" * "<> tgt <>"]` "
                 , "   violates this rule for concept `"<> src <>"`. In "<>name (metaModel grindInfo)<>" "
                 , "   the TType of this concept is "<>(tshow . cptTType (fModel grindInfo) $ source rel)<>"."
                 ]
      | not ( all (ttypeOf (target rel)) (map snd . Set.toList $ popPairs) ) =
             fatal . T.unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "<>name (metaModel grindInfo)<>"."
                 , "   The population of the relation `"<> relName <>"["<> src <>" * "<> tgt <>"]` "
                 , "   violates this rule for concept `"<> tgt <>"`. In "<>name (metaModel grindInfo)<>" "
                 , "   the TType of this concept is "<>(tshow . cptTType (fModel grindInfo) $ target rel)<>"." 
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
                tt              -> fatal $ "No test available yet. "<>tshow tt<>" encountered for the first time in "<>name (metaModel grindInfo)<>""
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
                        
