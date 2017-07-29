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
import Data.Typeable
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Motivations
import Ampersand.FSpec.Transformers
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.Core.ShowPStruct
import Ampersand.Core.ShowAStruct
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




instance MetaPopulations Signature where
 metaPops _ userFspec sgn =
      [ Pop "src" "Signature" "Concept"
             [(dirtyId ctx sgn, dirtyId ctx (source sgn))]
      , Pop "tgt" "Signature" "Concept"
             [(dirtyId ctx sgn, dirtyId ctx (target sgn))]
      ]
  where
    ctx = originalContext userFspec

instance MetaPopulations Declaration where
 metaPops formalAmpersand userFspec dcl =
   (case dcl of
     Sgn{} ->
      [ Comment " "
      , Comment $ " Relation `"++name dcl++" ["++(name.source.decsgn) dcl++" * "++(name.target.decsgn) dcl++"]"++"` "
      , Pop "context" "Relation" "Context"
             [(dirtyId ctx dcl,dirtyId ctx ctx)] 
      , Pop "name" "Relation" "RelationName"
             [(dirtyId ctx dcl, (PopAlphaNumeric . name) dcl)]
      , Pop "sign" "Relation" "Signature"
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "source" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      , Pop "prop" "Relation" "Property"
             [(dirtyId ctx dcl, dirtyId ctx x) | x <- decprps dcl]  -- decprps gives the user defined properties; not the derived properties.
      , Pop "decprL" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprL) dcl)]
      , Pop "decprM" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprM) dcl)]
      , Pop "decprR" "Relation" "String"
             [(dirtyId ctx dcl,(PopAlphaNumeric . decprR) dcl)]
 --     , Pop "decmean" "Relation" "Meaning"
 --            [(dirtyId ctx dcl, (show.concatMap showP.ameaMrk.decMean) dcl)]
      ]
     Isn{} -> fatal "Isn should not be populated by the meatgrinder."
{- SJ sept 2nd, 2016: I don't think we should populate the I-relation from the meatgrinder,
but I'm not sure why. -}
{- HJ july 22, 2017: This is because Isn{} must be removed from relation. It is an expression, not a relation.
      [ Comment " "
      , Comment $ " Relation `I["++name (source dcl)++"]`"
      , Pop "sign" "Relation" "Signature"
             [(dirtyId ctx dcl,dirtyId ctx (sign dcl))]
      , Pop "context" "Relation" "Context"
             [(dirtyId ctx dcl,dirtyId ctx ctx)]
      , Pop "name" "Relation" "RelationName"
             [(dirtyId ctx dcl, (PopAlphaNumeric . name) dcl)]
      , Pop "source" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (source dcl))]
      , Pop "target" "Relation" "Concept"
             [(dirtyId ctx dcl,dirtyId ctx (target dcl))]
      ] -}

     Vs{}  -> fatal "Vs should not be populated by the meatgrinder."
   )++
   metaPops formalAmpersand userFspec (sign dcl)
   where
    ctx = originalContext userFspec

instance MetaPopulations A_Pair where
 metaPops _ userFspec pair =
      [ Pop "in" "Pair" "Relation"
             [(dirtyId ctx pair, dirtyId ctx (lnkDcl pair))]
      , Pop "lAtom" "Pair" "Atom"
             [(dirtyId ctx pair, dirtyId ctx (lnkLeft pair))]
      , Pop "rAtom" "Pair" "Atom"
             [(dirtyId ctx pair, dirtyId ctx (lnkRight pair))]
      ]
  where
    ctx = originalContext userFspec

instance MetaPopulations Expression where
 metaPops formalAmpersand userFspec expr =
  case expr of 
    EBrk e -> metaPops formalAmpersand userFspec e
    _      ->
      [ Comment $ "Expression: "++showA expr++" ("++show (sign expr)++")"
      , Pop "sign" "Expression" "Signature"
             [(dirtyId ctx expr, dirtyId ctx (sign expr))]
-- SJ20170721: The following two are redundant. They must not be populated, so I have commented them away.
--      , Pop "src" "Expression" "Concept" [Uni,Tot]
--             [(dirtyId ctx expr, dirtyId ctx (source expr))]
--      , Pop "tgt" "Expression" "Concept" [Uni,Tot]
--             [(dirtyId ctx expr, dirtyId ctx (target expr))]
      , Pop "showADL" "Expression" "ShowADL"
             [(dirtyId ctx expr, PopAlphaNumeric (showA expr))]
      ]++
      ( case skipEpsilon expr of
            (EEqu (l,r)) -> makeBinaryTerm Equivalence l r
            (EInc (l,r)) -> makeBinaryTerm Inclusion l r
            (EIsc (l,r)) -> makeBinaryTerm Intersection l r
            (EUni (l,r)) -> makeBinaryTerm Union l r
            (EDif (l,r)) -> makeBinaryTerm Difference l r
            (ELrs (l,r)) -> makeBinaryTerm LeftResidu l r   
            (ERrs (l,r)) -> makeBinaryTerm RightResidu l r
            (EDia (l,r)) -> makeBinaryTerm Diamond l r
            (ECps (l,r)) -> makeBinaryTerm Composition l r
            (ERad (l,r)) -> makeBinaryTerm RelativeAddition l r
            (EPrd (l,r)) -> makeBinaryTerm CartesianProduct l r
            (EKl0 e)     -> makeUnaryTerm  KleeneStar e
            (EKl1 e)     -> makeUnaryTerm  KleenePlus e
            (EFlp e)     -> makeUnaryTerm  Converse   e
            (ECpl e)     -> makeUnaryTerm  UnaryMinus e
            (EBrk _)     -> fatal "This should not happen, because EBrk has been handled before"
            (EDcD dcl)   -> [Pop "bind" "BindedRelation" "Relation"
                              [(dirtyId ctx expr,dirtyId ctx dcl)]
                            ]
            EDcI{}       -> []
            EEps{}       -> fatal $ "EEps is not an expression in FormalAmpersand.\n"++
                                    "  Expression: "++showA expr++" ("++show (sign expr)++")" 
            EDcV{}       -> []
            (EMp1 v _)   -> [ Pop "singleton" "Singleton" "AtomValue"
                              [(dirtyId ctx expr,(PopAlphaNumeric . showP) v)]
                            ]
       ) 
  where
    ctx = originalContext userFspec
    makeBinaryTerm :: BinOp -> Expression -> Expression -> [Pop]
    makeBinaryTerm op lhs rhs = 
      [ Comment $ "BinOperator: "++show op
      , Comment $ "  First : "++showA lhs
      , Comment $ "  Second: "++showA rhs
      , Pop "first"  "BinaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx lhs)]
      , Pop "second" "BinaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx rhs)]
      , Pop "operator"  "BinaryTerm" "Operator"
             [(dirtyId ctx expr,dirtyId ctx op)]
      ]++metaPops formalAmpersand userFspec lhs
       ++metaPops formalAmpersand userFspec rhs
    makeUnaryTerm :: UnaryOp -> Expression -> [Pop]
    makeUnaryTerm op arg =
      [ Comment $ "UnaOperator: "++show op
      , Comment $ "  Arg : "++showA arg
      , Pop "arg" "UnaryTerm" "Expression"
             [(dirtyId ctx expr,dirtyId ctx arg)]
      , Pop "operator"  "UnaryTerm" "Operator"
             [(dirtyId ctx expr,dirtyId ctx op)]
      ]++metaPops formalAmpersand userFspec arg

    -- | As long as FormalAmpersand doesn't need/know about Epsilons, 
    --   we will not inject epsilon expressions into it.
    --   So the epsilon expression must be skipped over.
    --   This goes for brackets as well.   
    skipEpsilon :: Expression -> Expression
    skipEpsilon e =
      case e of
        (ECps (EEps{}, e') ) -> skipEpsilon e'
        (ECps (e', EEps{}) ) -> skipEpsilon e'
        (EBrk e'           ) -> skipEpsilon e'
        _                    -> e

data UnaryOp = 
             KleeneStar
           | KleenePlus
           | Converse
           | UnaryMinus deriving (Eq, Show, Typeable)
instance Unique UnaryOp where
  showUnique = show

data BinOp = CartesianProduct
           | Composition
           | Diamond
           | Difference
           | Equivalence 
           | Inclusion 
           | Intersection 
           | LeftResidu
           | RightResidu
           | RelativeAddition 
           | Union deriving (Eq, Show, Typeable)
instance Unique BinOp where
  showUnique = show


instance MetaPopulations Rule where
 metaPops formalAmpersand userFspec rul =
      [ Comment " "
      , Comment $ " Rule `"++name rul++"` "
      , Pop "name"  "Rule" "RuleName"
             [(dirtyId ctx rul, (PopAlphaNumeric . name) rul)]
      , Pop "urlEncodedName" "Rule" "EncodedName"
             [(dirtyId ctx rul, (PopAlphaNumeric . escapeNonAlphaNum . name) rul) 
             | rul `elem` vrules userFspec --Rule must be user defined to show graphic 
             ]
      , Pop "origin"  "Rule" "Origin"
             [(dirtyId ctx rul, (PopAlphaNumeric . show . origin) rul)]
      , Pop "message"  "Rule" "Message"
             [(dirtyId ctx rul, PopAlphaNumeric (aMarkup2String ReST m)) | m <- rrmsg rul, amLang m == fsLang userFspec ]
      , Pop "formalExpression"  "Rule" "Expression"
             [(dirtyId ctx rul, dirtyId ctx (rrexp rul))]
      , Pop "meaning"  "Rule" "Meaning"
             [(dirtyId ctx rul, PopAlphaNumeric (aMarkup2String ReST m)) | m <- (maybeToList . meaning (fsLang userFspec)) rul ]
      , Pop "sign" "Rule" "Signature"
             [(dirtyId ctx rul, dirtyId ctx (sign rul))]
      , Pop "declaredthrough" "PropertyRule" "Property"
             [(dirtyId ctx rul, dirtyId ctx prp) | Just(prp,_) <- [rrdcl rul]]
      , Pop "propertyRule" "Relation" "PropertyRule"
             [(dirtyId ctx dcl, dirtyId ctx rul) | Just(_,dcl) <- [rrdcl rul]]
      ] ++ 
      metaPops formalAmpersand userFspec (sign rul) ++
      metaPops formalAmpersand userFspec (rrexp rul)
  where
    ctx = originalContext userFspec


instance MetaPopulations a => MetaPopulations [a] where
 metaPops formalAmpersand userFspec = concatMap $ metaPops formalAmpersand userFspec
 

extractFromPop :: MetaFSpec -> Pop -> Maybe P_Declaration
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
      checkAtomValues :: Declaration -> [PAtomPair] -> Guarded [AAtomPair]
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
class MetaPopulations a where
 metaPops :: MetaFSpec -> FSpec -> a -> [Pop]



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
grindedPops :: FSpec -> FSpec -> Declaration -> [Pop]
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
                      hasNoTransformer :: Declaration -> Bool
                      hasNoTransformer d = null (filter (isForDcl d) (transformers userFspec))
            ts  -> map transformer2Pop ts 
    transformer2Pop (Transformer n s t ps) = Pop n s t ps      
isForDcl :: Declaration -> Transformer -> Bool
isForDcl dcl (Transformer n s t _ ) =
    and [ name dcl == n
        , name (source dcl) == s
        , name (target dcl) == t]
                        

dirtyId = fatal "Meatgrinder is tijdelijk stuk. @Stef, als je wat wilt doen, bekijk dan Transformers.hs" 