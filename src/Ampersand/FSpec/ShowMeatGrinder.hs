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
  )
where

import           Ampersand.Basics
import           Ampersand.Core.A2P_Converters
import           Ampersand.ADL1
import           Ampersand.Core.ShowPStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Transformers
import           Ampersand.Misc
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set

-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: Options -> FSpec -> FSpec -> P_Context
grind opts@Options{..} formalAmpersand userFspec =
  PCtx{ ctx_nm     = "Grinded_"++name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = mapMaybe relationFromPop . Set.toList $ metaPops2
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_rrels  = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = map aClassify2pClassify . Set.toList . instances $ formalAmpersand
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = mapMaybe populationFromPop . Set.toList $ metaPops2
      , ctx_metas  = []
      }
  where
    metaPops2 :: Set.Set Pop
    metaPops2 = Set.fromList 
              . concatMap (Set.toList . grindedPops opts formalAmpersand userFspec)
              . Set.toList . instances $ formalAmpersand
    relationFromPop :: Pop -> Maybe P_Relation
    relationFromPop pop =
      case pop of 
        Comment{} -> Nothing
        Pop{}     -> 
          Just (aRelation2pRelation (popRelation pop))
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
                orig = Origin "Population generated due to the meatgrinder"
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
           

      --    case string2AValue . unwords . words . show . popPairs $ pop of
      --       Checked x _ 
      --         -> case checkAtomValues (popRelation pop) x of
      --              Checked _ _ -> x
      --              Errors errs -> fatal . unlines $
      --                 [ "ERROR in tupels that are generated in the meatgrinder for relation"
      --                 , "  "++showRel (popRelation pop)
      --                 ] ++ (intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)

      --       Errors errs 
      --         -> fatal . unlines $
      --                 [ "ERROR in tupels that are generated in the meatgrinder for relation"
      --                 , "  "++showRel (popRelation pop)
      --                 ] ++ (intersperse (replicate 30 '=') . fmap show . NEL.toList $ errs)
      -- checkAtomValues :: Relation -> [PAtomPair] -> Guarded AAtomPairs
      -- checkAtomValues rel pps = Set.fromList <$> (sequence $ map fun pps)
      --       where
      --         fun pp = mkAtomPair 
      --           <$> pAtomValue2aAtomValue (source rel) (ppLeft  pp)
      --           <*> pAtomValue2aAtomValue (target rel) (ppRight pp)
            
      --         pAtomValue2aAtomValue :: A_Concept -> PAtomValue -> Guarded AAtomValue
      --         pAtomValue2aAtomValue cpt pav =
      --           case unsafePAtomVal2AtomValue typ (Just cpt) pav of
      --             Left msg -> Errors . pure $ mkIncompatibleAtomValueError pav msg
      --             Right av -> pure av
      --           where typ = cptTType formalAmpersand cpt
            
      -- string2AValue :: String -> Guarded [PAtomPair]
      -- string2AValue = runParser pContent "Somewhere in formalAmpersand files"
 
data Pop = Pop { popPairs  :: Set.Set (PopAtom,PopAtom)
               , popRelation :: Relation
               }
         | Comment { comment :: [String]  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   } deriving (Eq,Ord)

showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> showP . aRelation2pRelation . popRelation $ pop
      Comment{} -> intercalate "\n" . map ("-- " ++) . comment $ pop

-- ^ Write the meta-information of an FSpec to a file. This is usefull for debugging.
--   The comments that are in Pop are preserved. 
makeMetaFile :: Options -> FSpec -> FSpec -> (FilePath,String)
makeMetaFile opts@Options{..} formalAmpersand userFspec
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
         intercalate [""]
       . sort
       . map (lines . showPop )
       . concatMap (Set.toList . popsOfRelation)
       . sortOn showRel
       . Set.toList . instances $ formalAmpersand
    listOfConcepts :: [String]
    listOfConcepts = map ("-- "++) .
                     intercalate [""] . 
                     map showCpt . sortOn name . Set.toList . instances $ formalAmpersand
       where
        showCpt :: A_Concept -> [String]
        showCpt cpt = [name cpt] ++ ( map ("  "++)
                                    . sort 
                                    . map show
                                    . Set.toList
                                    $ pAtomsOfConcept cpt
                                    )
        
    popsOfRelation :: Relation -> Set.Set Pop
    popsOfRelation = grindedPops opts formalAmpersand userFspec
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
                         . instances $ formalAmpersand

grindedPops :: Options -> FSpec -> FSpec -> Relation -> Set.Set Pop
grindedPops opts@Options{..} formalAmpersand userFspec rel = 
  case filter (isForRel rel) (transformers opts userFspec) of
    []  -> fatal . unlines $ 
              ["Every relation in FormalAmpersand.adl must have a transformer in Transformers.hs"
              ,"   Violations:"
              ] ++ map ("      "++) viols
            where 
              viols = map showRelOrigin 
                    . Set.toList
                    . Set.filter hasNoTransformer 
                    . instances $ formalAmpersand
              hasNoTransformer :: Relation -> Bool
              hasNoTransformer d = null (filter (isForRel d) (transformers opts userFspec))
              showRelOrigin :: Relation -> String
              showRelOrigin r = showRel r++" ( "++show (origin r)++" )."
    ts  -> Set.fromList . map transformer2Pop $ ts 
  where
    transformer2Pop :: Transformer -> Pop
    transformer2Pop (Transformer n s t ps) 
      | not ( all (ttypeOf (source rel)) (map fst . Set.toList $ ps) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in formalampersand.adl."
                 , "   The population of the relation `"++n++"["++s++" * "++t++"]` "
                 , "   violates this rule for concept `"++s++"`. In formalAmpersand.adl "
                 , "   the TType of this concept is "++(show . cptTType formalAmpersand $ source rel)++"."
                 ]
      | not ( all (ttypeOf (target rel)) (map snd . Set.toList $ ps) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in formalampersand.adl."
                 , "   The population of the relation `"++n++"["++s++" * "++t++"]` "
                 , "   violates this rule for concept `"++t++"`. In formalAmpersand.adl "
                 , "   the TType of this concept is "++(show . cptTType formalAmpersand $ target rel)++"." 
                 ]
      | otherwise = Pop { popRelation = rel
                        , popPairs    = ps
                        }
      where ttypeOf :: A_Concept -> (PopAtom -> Bool)
            ttypeOf cpt =
              case (cptTType formalAmpersand) cpt of
                Object          -> isDirtyId
                Alphanumeric    -> isTextual
                BigAlphanumeric -> isTextual
                HugeAlphanumeric -> isTextual
                tt              -> fatal $ "No test available yet. "++show tt++" encountered for the first time in FormalAmpersand.adl"
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
                        
