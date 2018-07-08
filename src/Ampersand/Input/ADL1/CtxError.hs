{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , showErr, makeError
  , cannotDisamb, cannotDisambRel
  , mustBeOrdered, mustBeOrderedLst, mustBeOrderedConcLst
  , mustBeBound
  , GetOneGuarded(..), uniqueNames, uniqueBy
  , unexpectedType
  , mkErrorReadingINCLUDE
  , mkDanglingPurposeError
  , mkUndeclaredError, mkMultipleInterfaceError, mkInterfaceRefCycleError, mkIncompatibleInterfaceError
  , mkCyclesInGensError
  , mkMultipleDefaultError, mkDanglingRefError
  , mkIncompatibleViewError, mkOtherAtomInSessionError, mkOtherTupleInSessionError
  , mkInvalidCRUDError
  , mkMultipleRepresentTypesError, nonMatchingRepresentTypes
  , mkEndoPropertyError
  , mkMultipleTypesInTypologyError
  , mkIncompatibleAtomValueError
  , mkTypeMismatchError
  , mkMultipleRootsError
  , mkCrudForRefInterfaceError
  , Guarded(..) -- If you use Guarded in a monad, make sure you use "ApplicativeDo" in order to get error messages in parallel.
  , whenCheckedIO, whenChecked, whenError
  )
-- SJC: I consider it ill practice to export any CtxError constructors
-- Reason: All error messages should pass through the CtxError module
-- By not exporting anything that takes a string, we prevent other modules from containing error message
-- If you build a function that must generate an error, put it in CtxError and call it instead
-- see `getOneExactly' / `GetOneGuarded' as a nice example
-- Although I also consider it ill practice to export PE
-- for the same reasons, I did this as a quick fix for the parse errors
where
--import Control.Applicative
import Ampersand.ADL1
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct
import Ampersand.Basics
import Data.Maybe
import qualified Data.List as L   (intercalate)
import qualified Data.List.NonEmpty as NEL (NonEmpty(..),head,toList)
import GHC.Exts (groupWith)
import Text.Parsec.Error (Message(..), messageString)
import Ampersand.Input.ADL1.FilePos()
import Data.Monoid


data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE Message

instance Show CtxError where
    show (CTXE o s) = "CTXE " ++ show o ++ " " ++ show s
    show (PE msg)   = "PE " ++ messageString msg

errors :: Guarded t -> Maybe (NEL.NonEmpty CtxError)
errors (Checked _) = Nothing
errors (Errors lst) = Just lst

makeError :: String -> Guarded a
makeError msg = Errors (PE (Message msg) NEL.:| [])

unexpectedType :: Origin -> Maybe TType -> Guarded A_Concept
unexpectedType o x = 
   Errors (CTXE o ((case x of 
                     Nothing   -> "The Generic Built-in type was unexpeced. "
                     Just ttyp -> "Unexpected built-in type: "<>show ttyp
                   )<>"\n  expecting a concept.")
           NEL.:| []
          )

mkErrorReadingINCLUDE :: Maybe Origin -> FilePath -> String -> Guarded a
mkErrorReadingINCLUDE mo file str
 = Errors . pure $ CTXE (fromMaybe (Origin "command line argument") mo) msg
    where 
      msg = L.intercalate "\n    " $
             ("While looking for file '"++file++"':" )
             : lines str

mkMultipleRepresentTypesError :: A_Concept -> [(TType,Origin)] -> Guarded a
mkMultipleRepresentTypesError cpt rs
 = Errors . pure $ CTXE o msg
    where 
      o = case rs of
           []  -> fatal "Call of mkMultipleRepresentTypesError with no Representations"
           (_,x):_ -> x
      msg = L.intercalate "\n" $
             [ "The Concept "++showA cpt++" was shown to be representable with multiple types."
             , "The following TYPEs are defined for it:"
             ]++
             [ "  - "++show t++" at "++showFullOrig orig | (t,orig)<-rs]
mkMultipleTypesInTypologyError :: [(A_Concept,TType,[Origin])] -> Guarded a
mkMultipleTypesInTypologyError tripls 
 = Errors . pure $ CTXE o msg
    where
      o = case tripls of
            (_,_,x:_):_ -> x
            _  -> fatal "No origin in list."
      msg = L.intercalate "\n" $
             [ "Concepts in the same typology must have the same TYPE."
             , "The following concepts are in the same typology, but not all"
             , "of them have the same TYPE:"
             ]++
             [ "  - REPRESENT "++name c++" TYPE "++show t++" at "++showFullOrig orig | (c,t,origs) <- tripls, orig <- origs]
mkCyclesInGensError :: NEL.NonEmpty [A_Gen] -> Guarded a
mkCyclesInGensError cycles = Errors (fmap mkErr cycles)
 where 
  mkErr :: [A_Gen] -> CtxError
  mkErr [] = fatal "Nothing to report about!" 
  mkErr gs = CTXE o msg
    where
      o = origin (head gs)
      msg = L.intercalate "\n" $
             [ "Classifications must not contain cycles."
             , "The following CLASSIFY statements are cyclic:"
             ]++
             [ "  - "++showA gn++" at "++showFullOrig (origin gn) | gn <- gs]

mkMultipleRootsError :: [A_Concept] -> NEL.NonEmpty A_Gen -> Guarded a
mkMultipleRootsError roots gs
 = Errors . pure $ CTXE o msg
    where 
      o = origin (NEL.head gs)
      msg = L.intercalate "\n" $
             [ "A typology must have at most one root concept."
             , "The following CLASSIFY statements define a typology with multiple root concepts: "
             ] ++
             [ "  - "++showA x++" at "++showFullOrig (origin x) | x<-NEL.toList gs]++
             [ "Parhaps you could add the following statements:"]++
             [ "  CLASSIFY "++name cpt++" ISA "++show rootName    | cpt<-roots]
          
       where rootName = L.intercalate "_Or_" . map name $ roots 
nonMatchingRepresentTypes :: Origin -> TType -> TType -> Guarded a
nonMatchingRepresentTypes orig wrongType rightType
 = Errors . pure $
      CTXE orig
           $ "A CLASSIFY statement is only allowed between Concepts that are represented by the same type, but "
             ++show wrongType++" is not the same as "++show rightType

class GetOneGuarded a b | b -> a where
  {-# MINIMAL getOneExactly | hasNone #-}  -- we don't want endless loops, do we?
  getOneExactly :: b -> [a] -> Guarded a
  getOneExactly _ [a] = Checked a
  getOneExactly o []  = hasNone o
  getOneExactly o _ = 
    case errors (hasNone o :: Guarded a) of
      Nothing -> fatal "No error message!"
      Just (CTXE o' s NEL.:| _) -> Errors . pure $ CTXE o' $ "Found too many:\n  "++s
      Just (PE _      NEL.:| _) -> fatal "Didn't expect a PE constructor here"
  hasNone :: b  -- the object where the problem is arising
             -> Guarded a
  hasNone o = getOneExactly o []

instance GetOneGuarded SubInterface (P_SubIfc a) where
  hasNone o = Errors . pure $
    CTXE (origin o)$ "Required: one A-subinterface in "++showP o

instance GetOneGuarded Expression P_NamedRel where
  getOneExactly _ [d] = Checked d
  getOneExactly o []  = Errors . pure $ CTXE (origin o) $
      "No relation for "++showP o
  getOneExactly o lst = Errors . pure $ CTXE (origin o) $
      "An ambiguity arises in trying to match "++showP o
    ++".\n  Be more specific by using one of the following matching expressions:"
    ++concat ["\n  - "++showA l | l<-lst]

mkTypeMismatchError :: (Traced a2, Named a) => a2 -> Relation -> SrcOrTgt -> a -> Guarded a1
mkTypeMismatchError o decl sot conc
 = Errors . pure $ CTXE (origin o) message
 where
  message = "The "++showP sot++" for the population pairs, namely "++name conc
            ++"\n  must be more specific or equal to that of the "
            ++"relation you wish to populate, namely: "
            ++showEC (sot,decl)

cannotDisambRel :: TermPrim -> [Expression] -> Guarded Expression
cannotDisambRel o exprs
 = Errors . pure $ CTXE (origin o) message
  where
   message =
    case exprs of
     [] -> "No relations match the relation: "++showP o
     _  -> case o of
             (PNamedR(PNamedRel _ _ Nothing))
                -> L.intercalate "\n" $
                       ["Cannot disambiguate the relation: "++showP o
                       ,"  Please add a signature (e.g. [A*B]) to the relation."
                       ,"  Relations you may have intended:"
                       ]++
                       ["  "++showA e++"["++name (source e)++"*"++name (target e)++"]"
                       |e<-exprs]
             _  -> L.intercalate "\n" $
                       ["Cannot disambiguate: "++showP o
                       ,"  Please add a signature."
                       ,"  You may have intended one of these:"
                       ]++
                       ["  "++showA e|e<-exprs]

cannotDisamb :: TermPrim -> Guarded Expression
cannotDisamb o = Errors . pure $ CTXE (origin o) $
  "Cannot disambiguate: "++showP o++"\n  Please add a signature to it"

uniqueNames :: (Named a, Traced a) =>
                     [a] -> Guarded ()
uniqueNames = uniqueBy name
uniqueBy :: (Traced a, Show b, Ord b) => (a -> b) -> [a] -> Guarded ()
uniqueBy fun a = case (filter moreThanOne . groupWith fun) a of
                  []   -> pure ()
                  x:xs -> Errors $ (messageFor x) NEL.:| (fmap messageFor xs)
    where
     moreThanOne (_:_:_) = True
     moreThanOne  _      = False
     messageFor (x:xs) = CTXE (origin x)
                      ("Names / labels must be unique. "++(show . fun) x++", however, is used at:"++
                        concatMap (("\n    "++ ) . show . origin) (x:xs)
                        ++"."
                       )
     messageFor _ = fatal "messageFor must only be used on lists with more that one element!"

mkDanglingPurposeError :: Purpose -> CtxError
mkDanglingPurposeError p = CTXE (origin p) $ "Purpose refers to non-existent " ++ showA (explObj p)
-- Unfortunately, we cannot use position of the explanation object itself because it is not an instance of Trace.
mkDanglingRefError :: String -- The type of thing that dangles. eg. "Rule"
                   -> String -- the reference itself. eg. "Rule 42"
                   -> Origin -- The place where the thing is found.
                   -> CtxError
mkDanglingRefError entity ref orig =
  CTXE orig $ "Reference to non-existent " ++ entity ++ ": "++show ref
mkUndeclaredError :: String -> P_BoxItem a -> String -> CtxError
mkUndeclaredError entity objDef ref =
  case objDef of
    P_BxExpr{} -> CTXE (origin objDef) $ 
       "Undeclared " ++ entity ++ " " ++ show ref ++ " referenced at field " ++ show (obj_nm objDef)
    _       -> fatal "Unexpected use of mkUndeclaredError."

mkEndoPropertyError :: Origin -> [Prop] -> CtxError
mkEndoPropertyError orig ps =
  CTXE orig msg
  where 
    msg = L.intercalate "\n" $
       case ps of
         []  -> fatal "What property is causing this error???"
         [p] -> ["Property "++show p++" can only be used for relations where"
                ,"  source and target are equal."]
         _   -> ["Properties "++showAnd++" can only be used for relations where"
                ,"  source and target are equal."]
     where showAnd = L.intercalate ", " (map show . init $ ps)++" and "++(show . last) ps

mkMultipleInterfaceError :: String -> Interface -> [Interface] -> CtxError
mkMultipleInterfaceError role' ifc duplicateIfcs =
  CTXE (origin ifc) $ "Multiple interfaces named " ++ show (name ifc) ++ " for role " ++ show role' ++ ":" ++
                      concatMap (("\n    "++ ) . show . origin) (ifc:duplicateIfcs)

mkInvalidCRUDError :: Origin -> String -> CtxError
mkInvalidCRUDError o str = CTXE o $ "Invalid CRUD annotation. (doubles and other characters than crud are not allowed): `"++str++"`."

mkCrudForRefInterfaceError :: Origin -> CtxError
mkCrudForRefInterfaceError o = CTXE o $ "Crud specification is not allowed in combination with a reference to an interface."

mkIncompatibleAtomValueError :: PAtomValue -> String -> CtxError
mkIncompatibleAtomValueError pav msg = CTXE (origin pav) (case msg of 
                                                            "" -> fatal "Error message must not be empty."
                                                            _  -> msg)

mkInterfaceRefCycleError :: NEL.NonEmpty Interface -> CtxError
mkInterfaceRefCycleError cyclicIfcs =
  CTXE (origin (NEL.head cyclicIfcs)) $
             "Interfaces form a reference cycle:\n" ++
             (unlines . NEL.toList $ fmap showIfc cyclicIfcs)
    where
      showIfc :: Interface -> String
      showIfc i = "- " ++ show (name i) ++ " at position " ++ show (origin i)
mkIncompatibleInterfaceError :: P_BoxItem a -> A_Concept -> A_Concept -> String -> CtxError
mkIncompatibleInterfaceError objDef expTgt refSrc ref =
  case objDef of
    P_BxExpr{} -> CTXE (origin objDef) $ 
        "Incompatible interface reference "++ show ref ++ " at field " ++ show (obj_nm objDef) ++
        ":\nReferenced interface "++show ref++" has type " ++ show (name refSrc) ++
        ", which is not comparable to the target " ++ show (name expTgt) ++ " of the expression at this field."
    _ -> fatal "Improper use of mkIncompatibleInterfaceError"
  
mkMultipleDefaultError :: (A_Concept, [ViewDef]) -> CtxError
mkMultipleDefaultError (_, [])              = fatal "mkMultipleDefaultError called on []"
mkMultipleDefaultError (c, vds@(vd0:_)) =
  CTXE (origin vd0) $ "Multiple default views for concept " ++ show (name c) ++ ":" ++
                      concat ["\n    VIEW " ++ name vd ++ " (at " ++ show (origin vd) ++ ")"
                             | vd <- vds ]

mkIncompatibleViewError :: (Named b,Named c) => P_BoxItem a -> String -> b -> c -> CtxError
mkIncompatibleViewError objDef viewId viewRefCptStr viewCptStr =
  case objDef of
    P_BxExpr{} -> CTXE (origin objDef) $
      "Incompatible view annotation <"++viewId++"> at field " ++ show (obj_nm objDef) ++ ":"++
      "\nView " ++ show viewId ++ " has type " ++ name viewCptStr ++
      ", which should be equal to or more general than the target " ++ name viewRefCptStr ++ " of the expression at this field."
    _       -> fatal "Improper use of mkIncompatibleViewError."

mkOtherAtomInSessionError :: AAtomValue -> CtxError
mkOtherAtomInSessionError atomValue =
  CTXE OriginUnknown $ "The special concept `SESSION` cannot contain an initial population. However it is populated with `"++showA atomValue++"`."

mkOtherTupleInSessionError :: Relation -> AAtomPair -> CtxError
mkOtherTupleInSessionError r pr =
  CTXE OriginUnknown $ "The special concept `SESSION` cannot contain an initial population. However it is populated with `"++showA pr++"` by populating the relation `"++showA r++"`."

class ErrorConcept a where
  showEC :: a -> String

instance ErrorConcept (P_ViewD a) where
  showEC x = showP (vd_cpt x) ++" given in VIEW "++vd_lbl x
instance ErrorConcept P_IdentDef where
  showEC x = showP (ix_cpt x) ++" given in Identity "++ix_lbl x

instance (AStruct a2) => ErrorConcept (SrcOrTgt, A_Concept, a2) where
  showEC (p1,c1,e1) = showEC' (p1,c1,showA e1)

showEC' :: (SrcOrTgt, A_Concept, String) -> String
showEC' (p1,c1,e1) = showA c1++" ("++show p1++" of "++e1++")"

instance (AStruct declOrExpr, HasSignature declOrExpr) => ErrorConcept (SrcOrTgt, declOrExpr) where
  showEC (p1,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showA e1)
      Tgt -> showEC' (p1,target e1,showA e1)

instance (AStruct declOrExpr, HasSignature declOrExpr) => ErrorConcept (SrcOrTgt, Origin, declOrExpr) where
  showEC (p1,o,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showA e1 ++ ", "++showMinorOrigin o)
      Tgt -> showEC' (p1,target e1,showA e1 ++ ", "++showMinorOrigin o)

mustBeOrdered :: (ErrorConcept t1, ErrorConcept t2) => Origin -> t1 -> t2 -> Guarded a
mustBeOrdered o a b
 = Errors . pure . CTXE (origin o) . unlines $
     [ "Type error, cannot match:"
     , "  the concept "++showEC a
     , "  and concept "++showEC b
     ]

mustBeOrderedLst :: P_SubIfc (TermPrim, x) -> [(A_Concept, SrcOrTgt, P_BoxItem TermPrim)] -> Guarded b
mustBeOrderedLst o lst
 = Errors . pure . CTXE (origin o) . unlines $
     [ "Type error in "++showP o
     , "  Cannot match:"
     ]++
     [ "  - concept "++showA c++", "++show st++" of "++showP a
     | (c,st,a) <- lst
     ]++ 
     [ "  if you think there is no type error, add an order between the mismatched concepts."
     , "  You can do so by using a CLASSIFY statement."
     ]

mustBeOrderedConcLst :: Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> [[A_Concept]] -> Guarded (A_Concept, [A_Concept])
mustBeOrderedConcLst o (p1,e1) (p2,e2) cs
 = Errors . pure . CTXE (origin o) . unlines $
    [ "Ambiguous type when matching: "++show p1++" of "++showA e1
    , " and "++show p2++" of "++showA e2++"."
    , "  The type can be "++L.intercalate " or " (map (L.intercalate "/" . map name) cs)
    , "  None of these concepts is known to be the smallest, you may want to add an order between them."
    ]

mustBeBound :: Origin -> [(SrcOrTgt, Expression)] -> Guarded a
mustBeBound o [(p,e)]
 = Errors . pure . CTXE (origin o) . unlines $
    [ "An ambiguity arises in type checking. Be more specific by binding the "++show p++" of the expression"
    , "  "++showA e++"."
    , "  You could add more types inside the expression, or just write"
    , "  "++writeBind e++"."
    ]
mustBeBound o lst
 = Errors . pure . CTXE (origin o) . unlines $
    [ "An ambiguity arises in type checking. Be more specific in the expressions "
    , "  "++L.intercalate " and " (map (showA . snd) lst) ++"."
    , "  You could add more types inside the expression, or write:"
    ]++
    ["  "++writeBind e| (_,e)<-lst]

writeBind :: Expression -> String
writeBind (ECpl e)
 = "("++showA (EDcV (sign e))++" - "++showA e++")"
writeBind e
 = "("++showA e++") /\\ "++showA (EDcV (sign e))

data Guarded a = Errors (NEL.NonEmpty CtxError) | Checked a deriving Show

instance Functor Guarded where
 fmap _ (Errors a)  = Errors a
 fmap f (Checked a) = Checked (f a)
instance Applicative Guarded where
 pure = Checked
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked _) = Errors a
 (<*>) (Checked _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a >> b)
instance Monad Guarded where
 (>>=) (Checked a) f = f a
 (>>=) (Errors x) _ = Errors x

-- Shorthand for working with Guarded in IO
whenCheckedIO :: IO  (Guarded a) -> (a -> IO (Guarded b)) -> IO (Guarded b)
whenCheckedIO ioGA fIOGB =
   do gA <- ioGA
      case gA of
         Errors err -> return (Errors err)
         Checked a  -> fIOGB a

whenChecked :: Guarded a -> (a -> Guarded b) -> Guarded b
whenChecked ga fgb =
      case ga of
         Checked a  -> fgb a
         Errors err -> Errors err

whenError :: Guarded a -> Guarded a -> Guarded a
whenError (Errors _) a = a
whenError a@(Checked _) _ = a


showErr :: CtxError -> String
showErr (CTXE o s) = showFullOrig o ++ "\n  " ++ s
showErr (PE msg)   = messageString msg

showFullOrig :: Origin -> String
showFullOrig (FileLoc (FilePos filename line column) t)
              = "Error at symbol " ++ t ++
                " in file " ++ filename ++
                " at line " ++ show line ++
                " : " ++ show column

showFullOrig x = show x
showMinorOrigin :: Origin -> String
showMinorOrigin (FileLoc (FilePos _ line column) _) = "line " ++ show line ++" : "++show column
showMinorOrigin v = show v
