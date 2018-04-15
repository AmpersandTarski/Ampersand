{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Ampersand.Input
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Misc
import           Data.List
import qualified Data.List.NonEmpty as NEL (toList)
import           Data.Maybe
import qualified Data.Set as Set

-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: FSpec -> FSpec -> P_Context
grind formalAmpersand userFspec =
  PCtx{ ctx_nm     = "Grinded_"++name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = mapMaybe (extractFromPop formalAmpersand) metaPops2
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_rrels  = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = map aGen2pGen (instances formalAmpersand) 
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = []
      , ctx_sql    = []
      , ctx_php    = []
      , ctx_metas  = []
      }
  where
    metaPops2 :: [Pop]
    metaPops2 = concatMap (grindedPops formalAmpersand userFspec)
              . instances $ formalAmpersand


extractFromPop :: MetaFSpec -> Pop -> Maybe P_Relation
extractFromPop formalAmpersand pop =
  case pop of 
    Comment{} -> Nothing
    Pop{}     -> 
      Just (aRelation2pRelation (popRelation pop)) {dec_popu = tuples}
      
     where
      tuples :: [PAtomPair]
      tuples =
         case string2AValue . unwords . words . show . popPairs $ pop of
            Checked x -> case checkAtomValues (popRelation pop) x of
                          Checked _ -> x
                          Errors errs -> fatal . unlines $
                             [ "ERROR in tupels that are generated in the meatgrinder for relation"
                             , "  "++showRel (popRelation pop)
                             ] ++ (intersperse (replicate 30 '=') . fmap showErr . NEL.toList $ errs)
                             
            Errors errs -> fatal . unlines $
                             [ "ERROR in tupels that are generated in the meatgrinder for relation"
                             , "  "++showRel (popRelation pop)
                             ] ++ (intersperse (replicate 30 '=') . fmap showErr . NEL.toList $ errs)
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
                where typ = cptTType formalAmpersand cpt
            
      string2AValue :: String -> Guarded [PAtomPair]
      string2AValue = runParser pContent "Somewhere in formalAmpersand files"
 
data Pop = Pop { popPairs  :: [(PopAtom,PopAtom)]
               , popRelation :: Relation
               }
         | Comment { comment :: [String]  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   } deriving (Eq,Ord)

showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> showP ((aRelation2pRelation (popRelation pop)) {dec_popu = map foo . sortShow $ popPairs pop} )
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


type MetaFSpec = FSpec



-- ^ Write the meta-information of an FSpec to a file. This is usefull for debugging.
--   The comments that are in Pop are preserved. 
makeMetaFile :: FSpec -> FSpec -> (FilePath,String)
makeMetaFile formalAmpersand userFspec
  = ("MetaPopulationFile.adl", content )
  where
    content = unlines $
        ([ "{- Do not edit manually. This code has been generated!!!"
        , "    Generated with "++ampersandVersionStr
        , "    Generated at "++show (genTime (getOpts userFspec))
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
       . concatMap popsOfRelation
       . sortOn showRel
       . instances $ formalAmpersand
    listOfConcepts :: [String]
    listOfConcepts = map ("-- "++) .
                     intercalate [""] . 
                     map showCpt $ cpts
       where
        showCpt :: A_Concept -> [String]
        showCpt cpt = [name cpt] ++ ( map ("  "++)
                                    . sort 
                                    . map show
                                    $ pAtomsOfConcept cpt
                                    )
        cpts::[A_Concept] = sortOn name . instances $ formalAmpersand 
        
    popsOfRelation :: Relation -> [Pop]
    popsOfRelation = sort . grindedPops formalAmpersand userFspec
    pAtomsOfConcept :: A_Concept -> [PopAtom]
    pAtomsOfConcept cpt = 
      nub $
         (nub . map fst . concatMap popPairs . concatMap popsOfRelation . filter isForSource . instances $ formalAmpersand)
         ++
         (nub . map snd . concatMap popPairs . concatMap popsOfRelation . filter isForTarget . instances $ formalAmpersand)               
      where isForSource :: Relation -> Bool
            isForSource rel = source rel == cpt
            isForTarget :: Relation -> Bool
            isForTarget rel = target rel == cpt

grindedPops :: FSpec -> FSpec -> Relation -> [Pop]
grindedPops formalAmpersand userFspec rel = 
  case filter (isForRel rel) (transformers userFspec) of
    []  -> fatal . unlines $ 
              ["Every relation in FormalAmpersand.adl must have a transformer in Transformers.hs"
              ,"   Violations:"
              ] ++ map ("      "++) viols
            where 
              viols = map showRel 
                    . filter hasNoTransformer 
                    . instances $ formalAmpersand
              hasNoTransformer :: Relation -> Bool
              hasNoTransformer d = null (filter (isForRel d) (transformers userFspec))
    ts  -> map transformer2Pop $ ts 
  where
    transformer2Pop :: Transformer -> Pop
    transformer2Pop (Transformer n s t ps) 
      | not ( all (ttypeOf (source rel)) (map fst ps) ) =
             fatal . unlines $
                 [ "The TType of the population produced by the meatgrinder must"
                 , "   match the TType of the concept as specified in formalampersand.adl."
                 , "   The population of the relation `"++n++"["++s++" * "++t++"]` "
                 , "   violates this rule for concept `"++s++"`. In formalAmpersand.adl "
                 , "   the TType of this concept is "++(show . cptTType formalAmpersand $ source rel)++"."
                 ]
      | not ( all (ttypeOf (target rel)) (map snd ps) ) =
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
                        
