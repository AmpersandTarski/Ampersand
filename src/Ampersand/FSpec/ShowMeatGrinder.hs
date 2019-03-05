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
  , MetaFSpec(..)
  )
where

import           Ampersand.Basics
import           Ampersand.Core.A2P_Converters
import           Ampersand.ADL1
import           Ampersand.Core.ShowPStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.Transformers
import           Ampersand.Input
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Misc
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)
import           Data.Maybe
import qualified Data.Set as Set

data MetaFSpec = MetaFSpec
    { metaModelFileName :: String
    , model             :: FSpec
    , transformers      :: Options -> FSpec -> [Transformer]
    }
-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: Options -> MetaFSpec -> FSpec -> P_Context
grind opts@Options{..} metaModel userFspec =
  PCtx{ ctx_nm     = "Grinded_"++name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = mapMaybe (extractFromPop metaModel) . Set.toList $ metaPops2
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_rrels  = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = map aClassify2pClassify . Set.toList . instances . model $ metaModel
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = []
      , ctx_metas  = []
      }
  where
    metaPops2 :: Set.Set Pop
    metaPops2 = Set.fromList 
              . concatMap (Set.toList . grindedPops opts metaModel userFspec)
              . Set.toList . instances . model $ metaModel

extractFromPop :: MetaFSpec -> Pop -> Maybe P_Relation
extractFromPop metaModel pop =
  case pop of 
    Comment{} -> Nothing
    Pop{}     -> 
      Just (aRelation2pRelation (popRelation pop)) {dec_popu = tuples}
      
     where
      tuples :: [PAtomPair]
      tuples =
         case string2AValue . unwords . words . show . Set.toList . popPairs $ pop of
            Checked x _ 
              -> case checkAtomValues (popRelation pop) x of
                   Checked _ _ -> x
                   Errors errs -> showFatal errs

            Errors errs 
              -> showFatal errs
         where
          showFatal errs =
            fatal . unlines $
              [ "ERROR in tupels that are generated in the meatgrinder for relation"
              , "  "++showRel (popRelation pop)
              ] ++
              [ "      - "++ show pr
              | pr <- Set.toList . popPairs $ pop
              ]++ (intersperse (replicate 30 '=') . fmap showErr . NEL.toList $ errs)
      checkAtomValues :: Relation -> [PAtomPair] -> Guarded AAtomPairs
      checkAtomValues rel pps = Set.fromList <$> (sequence $ map fun pps)
            where
              fun pp = mkAtomPair 
                <$> pAtomValue2aAtomValue (source rel) (ppLeft  pp)
                <*> pAtomValue2aAtomValue (target rel) (ppRight pp)
            
              pAtomValue2aAtomValue :: A_Concept -> PAtomValue -> Guarded AAtomValue
              pAtomValue2aAtomValue cpt pav =
                case unsafePAtomVal2AtomValue typ (Just cpt) pav of
                  Left msg -> Errors . pure $ mkIncompatibleAtomValueError pav msg
                  Right av -> pure av
                where typ = cptTType (model metaModel) cpt
            
      string2AValue :: String -> Guarded [PAtomPair]
      string2AValue = runParser pContent ("Somewhere in "++metaModelFileName metaModel++" files")
 
data Pop = Pop { popPairs  :: Set.Set (PopAtom,PopAtom)
               , popRelation :: Relation
               }
         | Comment { comment :: [String]  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   } deriving (Eq,Ord)

showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> showP ((aRelation2pRelation (popRelation pop)) {dec_popu = map foo . sortShow . Set.toList $ popPairs pop} )
      Comment{} -> intercalate "\n" . map ("-- " ++) . comment $ pop
    where sortShow :: [(PopAtom,PopAtom)] -> [(PopAtom,PopAtom)]
          sortShow = sortOn x
            where x :: (PopAtom,PopAtom) -> String
                  x (a,b) = show a++show b
          foo :: (PopAtom,PopAtom) -> PAtomPair
          foo (a,b) = PPair { pos = o
                            , ppLeft  = pAtom2AtomValue a
                            , ppRight = pAtom2AtomValue b
                            }
            where
              pAtom2AtomValue :: PopAtom -> PAtomValue
              pAtom2AtomValue atm = 
                case atm of 
                  DirtyId str         -> ScriptString o str
                  PopAlphaNumeric str -> ScriptString o str
                  PopInt i            -> ScriptInt o i
              o = Origin "meatgrinder"

-- ^ Write the meta-information of an FSpec to a file. This is usefull for debugging.
--   The comments that are in Pop are preserved. 
makeMetaFile :: Options -> MetaFSpec -> FSpec -> (FilePath,String)
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
         intercalate [""]
       . sort
       . map (lines . showPop )
       . concatMap (Set.toList . popsOfRelation)
       . sortOn showRel
       . Set.toList . instances . model $ metaModel
    listOfConcepts :: [String]
    listOfConcepts = map ("-- "++) .
                     intercalate [""] . 
                     map showCpt . sortOn name . Set.toList . instances . model $ metaModel
       where
        showCpt :: A_Concept -> [String]
        showCpt cpt = [name cpt] ++ ( map ("  "++)
                                    . sort 
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
                         . instances . model $ metaModel

grindedPops :: Options -> MetaFSpec -> FSpec -> Relation -> Set.Set Pop
grindedPops opts@Options{..} metaModel userFspec rel = 
  case filter (isForRel rel) ((transformers metaModel) opts userFspec) of
    []  -> fatal . unlines $ 
              ["Every relation in "++metaModelFileName metaModel++" must have a transformer in Transformers.hs"
              ,"   Violations:"
              ] ++ map ("      "++) viols
            where 
              viols = map showRelOrigin 
                    . Set.toList
                    . Set.filter hasNoTransformer 
                    . instances . model $ metaModel
              hasNoTransformer :: Relation -> Bool
              hasNoTransformer d = null (filter (isForRel d) ((transformers metaModel) opts userFspec))
              showRelOrigin :: Relation -> String
              showRelOrigin r = showRel r++" ( "++show (origin r)++" )."
    ts  -> Set.fromList . map transformer2Pop $ ts 
  where
    --metaModelFileName :: String
    --metaModelFileName = "formalAmpersand.adl"
    --transformers :: FSpec -> [Transformer]
    --transformers = transformersFormalAmpersand
    transformer2Pop :: Transformer -> Pop
    transformer2Pop (Transformer n s t ps) 
      | not ( all (ttypeOf (source rel)) (map fst . Set.toList $ ps) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "++metaModelFileName metaModel++"."
                 , "   The population of the relation `"++n++"["++s++" * "++t++"]` "
                 , "   violates this rule for concept `"++s++"`. In "++metaModelFileName metaModel++" "
                 , "   the TType of this concept is "++(show . cptTType (model metaModel) $ source rel)++"."
                 ]
      | not ( all (ttypeOf (target rel)) (map snd . Set.toList $ ps) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in "++metaModelFileName metaModel++"."
                 , "   The population of the relation `"++n++"["++s++" * "++t++"]` "
                 , "   violates this rule for concept `"++t++"`. In "++metaModelFileName metaModel++" "
                 , "   the TType of this concept is "++(show . cptTType (model metaModel) $ target rel)++"." 
                 ]
      | otherwise = Pop { popRelation = rel
                        , popPairs    = ps
                        }
      where ttypeOf :: A_Concept -> (PopAtom -> Bool)
            ttypeOf cpt =
              case (cptTType (model metaModel)) cpt of
                Object          -> isDirtyId
                Alphanumeric    -> isTextual
                BigAlphanumeric -> isTextual
                HugeAlphanumeric -> isTextual
                tt              -> fatal $ "No test available yet. "++show tt++" encountered for the first time in "++metaModelFileName metaModel++""
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
                        
