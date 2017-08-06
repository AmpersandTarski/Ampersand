{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE OverloadedLabels #-}
module Ampersand.FSpec.ShowMeatGrinder
  ( dumpGrindFile
  , grind 
  )
where

import Ampersand.Basics
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Transformers
import Ampersand.Misc
import Ampersand.Core.A2P_Converters
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
import Ampersand.Input
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.ADL1.Parser
import Data.List
import Data.Maybe

-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: FSpec -> FSpec -> P_Context
grind formalAmpersand userFspec =
  PCtx{ ctx_nm     = "Grinded_"++name userFspec
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_thms   = []
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
                          Errors err -> fatal $
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++showDcl True (popRelation pop)
                            ++intercalate (replicate 30 '=') (map showErr err)
            Errors err -> fatal $ 
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++showDcl True (popRelation pop)
                            ++intercalate (replicate 30 '=') (map showErr err)
      checkAtomValues :: Relation -> [PAtomPair] -> Guarded [AAtomPair]
      checkAtomValues rel pps = sequence $ map fun pps
            where
              fun pp = mkAtomPair 
                <$> pAtomValue2aAtomValue (source rel) (ppLeft  pp)
                <*> pAtomValue2aAtomValue (target rel) (ppRight pp)
            
              pAtomValue2aAtomValue :: A_Concept -> PAtomValue -> Guarded AAtomValue
              pAtomValue2aAtomValue cpt pav =
                case unsafePAtomVal2AtomValue typ (Just cpt) pav of
                  Left msg -> Errors [mkIncompatibleAtomValueError pav msg]
                  Right av -> pure av
                where typ = cptTType formalAmpersand cpt
            
      string2AValue :: String -> Guarded [PAtomPair]
      string2AValue = runParser pContent "Somewhere in formalAmpersand files"
 
data Pop = Pop { popRelation :: Relation
               , popPairs  :: [(PopAtom,PopAtom)]
               }
         | Comment { comment :: [String]  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   }
showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> showP ((aRelation2pRelation (popRelation pop)) {dec_popu = map foo $ popPairs pop} )
           --   "RELATION "++ showDcl True (popRelation pop)++" ="
           --   ++
           --   if null (popPairs pop)
           --   then "[]"
           --   else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> intercalate "\n" . map ("-- " ++) . comment $ pop
    where foo :: (PopAtom,PopAtom) -> PAtomPair
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
dumpGrindFile :: FSpec -> FSpec -> (FilePath,String)
dumpGrindFile formalAmpersand userFspec
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
        ++ body  ++
        [ ""
        , "ENDCONTEXT"
        ])
    body :: [String]
    body =
         intercalate [""]
       . map (lines . showPop )
       . concatMap (grindedPops formalAmpersand userFspec)
       . sortOn (showDcl True)
       . instances $ formalAmpersand
grindedPops :: FSpec -> FSpec -> Relation -> [Pop]
grindedPops formalAmpersand userFspec rel = 
  case filter (isForRel rel) (transformers userFspec) of
    []  -> fatal . unlines $ 
              ["Every relation in FormalAmpersand.adl must have a transformer in Transformers.hs"
              ,"   Violations:"
              ] ++ map ("      "++) viols
            where 
              viols = map (showDcl True) 
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
                        
