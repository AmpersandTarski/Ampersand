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

import Data.List
import Data.Maybe
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Transformers
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.Core.ParseTree
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Input
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.ADL1.Parser
import Ampersand.Input.Parsing

-- ^ Create a P_Context that contains meta-information from 
--   an FSpec.
grind :: FSpec -> FSpec -> P_Context
grind formalAmpersand userFspec =
  (mkContextOfPopsOnly []) {ctx_ds = mapMaybe (extractFromPop formalAmpersand) metaPops2 }
  where
    metaPops2 :: [Pop]
    metaPops2 = concatMap (grindedPops formalAmpersand userFspec)
              . instances $ formalAmpersand





extractFromPop :: MetaFSpec -> Pop -> Maybe P_Relation
extractFromPop formalAmpersand pop =
  case pop of 
    Comment{}                -> Nothing
    (Pop rel src tgt tuples) -> 
      Just P_Sgn { dec_nm = popName pop
                 , dec_sign = P_Sign { pSrc = PCpt src
                                     , pTgt = PCpt tgt
                                     }
                 , dec_prps   = []
                 , dec_pragma = []
                 , dec_Mean   = []
                 , dec_popu   = dclLookup
                 , pos        = Origin "Extracted by the meatgrinder of Ampersand"
                 , dec_plug   = False
                 }
     where
      dclLookup :: [PAtomPair]
      dclLookup =
         case string2AValue . unwords . words . show $ tuples of
            Checked x -> case checkAtomValues aRel x of
                          Checked _ -> x
                          Errors err -> fatal $
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++rel++"["++src++"*"++tgt++"]"
                            ++intercalate (replicate 30 '=') (map showErr err)
            Errors err -> fatal $ 
                              "ERROR in tupels that are generated in the meatgrinder for relation\n"
                            ++"  "++rel++"["++src++"*"++tgt++"]"
                            ++intercalate (replicate 30 '=') (map showErr err)
      checkAtomValues :: Relation -> [PAtomPair] -> Guarded [AAtomPair]
      checkAtomValues dcl pps = sequence $ map fun pps
            where
              fun pp = mkAtomPair 
                <$> pAtomValue2aAtomValue (source dcl) (ppLeft  pp)
                <*> pAtomValue2aAtomValue (target dcl) (ppRight pp)
            
              pAtomValue2aAtomValue :: A_Concept -> PAtomValue -> Guarded AAtomValue
              pAtomValue2aAtomValue cpt pav =
                case unsafePAtomVal2AtomValue typ (Just cpt) pav of
                  Left msg -> Errors [mkIncompatibleAtomValueError pav msg]
                  Right av -> pure av
                where typ = cptTType formalAmpersand cpt
            
      aRel = case [r | r <- vrels formalAmpersand
                     , name r == rel
                     , name (source r) == src
                     , name (target r) == tgt
                  ] of
         []  -> fatal $ "A relation populated by the meatgrinder must be defined in Formalampersand adl files.\n"
                      ++"   Violation: `"++rel++"["++src++"*"++tgt++"]`"
         [r] -> r
         rs  -> fatal $ "Multiple relations that match?? Impossible!"++
                           concatMap (\r -> "\n  "++show r) rs
      string2AValue :: String -> Guarded [PAtomPair]
      string2AValue = runParser pContent "Somewhere in formalAmpersand files"
 
data Pop = Pop { popName   :: String
               , popSource :: String
               , popTarget :: String
               , popPairs  :: [(PopAtom,PopAtom)]
               }
         | Comment { comment :: String  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                   }
showPop :: Pop -> String
showPop pop =
  case pop of
      Pop{} -> "POPULATION "++ popNameSignature pop++" CONTAINS"
              ++
              if null (popPairs pop)
              then "[]"
              else "\n"++indentA++"[ "++intercalate ("\n"++indentA++"; ") showContent++indentA++"]"
      Comment{} -> intercalate "\n" . map ("-- " ++) . lines . comment $ pop
    where indentA = "   "
          showContent = map showPaire (popPairs pop)
          showPaire :: Show a => (a,a) -> String
          showPaire (s,t) = "( "++show s++" , "++show t++" )"

popNameSignature :: Pop -> String
popNameSignature pop =
   case pop of
     Pop{}     -> popName pop++" ["++popSource pop++" * "++popTarget pop++"]"
     Comment{} -> fatal "Must not call popName on a Comment-combinator."

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
        , "CONTEXT "++name userFspec
        , "" ]
        ++ body  ++
        [ ""
        , "ENDCONTEXT"
        ])
    body :: [String]
    body =
         intercalate []
       . map (lines . showPop )
       . concatMap (grindedPops formalAmpersand userFspec)
       . sortOn (showDcl True)
       . instances $ formalAmpersand
grindedPops :: FSpec -> FSpec -> Relation -> [Pop]
grindedPops formalAmpersand userFspec dcl = headerComment ++ pops
  where
    headerComment :: [Pop]
    headerComment = 
      [Comment . unlines $
        [ "  "++(name) dcl++"["
                          ++(name . source) dcl++" * "
                          ++(name . target) dcl++"]"
        ]
      ]
    pops = case filter (isForDcl dcl) (transformers userFspec) of
            []  -> fatal . unlines $ 
                      ["Every relation in FormalAmpersand.adl must have a transformer in Transformers.hs"
                      ,"   Violations:"
                      ] ++ map ("      "++) viols
                    where 
                      viols = map (showDcl True) 
                            . filter hasNoTransformer 
                            . instances $ formalAmpersand
                      hasNoTransformer :: Relation -> Bool
                      hasNoTransformer d = null (filter (isForDcl d) (transformers userFspec))
            ts  -> map transformer2Pop ts 
    transformer2Pop (Transformer n s t ps) = Pop n s t ps      
isForDcl :: Relation -> Transformer -> Bool
isForDcl dcl (Transformer n s t _ ) =
    and [ name dcl == n
        , name (source dcl) == s
        , name (target dcl) == t]
                        
