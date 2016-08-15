{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , showErr, makeError, addError
  , cannotDisamb, cannotDisambRel
  , mustBeOrdered, mustBeOrderedLst, mustBeOrderedConcLst
  , mustBeBound
  , GetOneGuarded(..), uniqueNames
  , TypeAware(..), unexpectedType
  , mkErrorReadingINCLUDE
  , mkDanglingPurposeError
  , mkUndeclaredError, mkMultipleInterfaceError, mkInterfaceRefCycleError, mkIncompatibleInterfaceError
  , mkMultipleDefaultError, mkDanglingRefError
  , mkIncompatibleViewError, mkOtherAtomInSessionError
  , mkInvalidCRUDError
  , mkMultipleRepresentTypesError, nonMatchingRepresentTypes
  , mkEndoPropertyError
  , mkMultipleTypesInTypologyError
  , mkIncompatibleAtomValueError
  , mkTypeMismatchError
  , mkMultipleRootsError
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
import Ampersand.FSpec.ShowADL
import Ampersand.Basics
import Data.Maybe
import Data.List  (intercalate)
import GHC.Exts (groupWith)
import Ampersand.Core.ParseTree
import Text.Parsec.Error (Message(..), messageString)
import Ampersand.Input.ADL1.FilePos()
import Data.Monoid


_notUsed :: a
_notUsed = undefined fatal

data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE Message

instance Show CtxError where
    show (CTXE o s) = "CTXE " ++ show o ++ " " ++ show s
    show (PE msg)   = "PE " ++ messageString msg

errors :: Guarded t -> [CtxError]
errors (Checked _) = []
errors (Errors lst) = lst

makeError :: String -> Guarded a
makeError msg = Errors [PE (Message msg)]

addError :: String -> Guarded a -> Guarded b
addError msg guard = Errors (PE (Message msg):errors guard)

class TypeAware x where
  -- Need something of type "p x"? Use on of these: [x], Maybe x, Guarded x
  -- They are all of the type "p x".
  -- The reason for us to write "p x" (without constraints on p, thus not specifying p) is that we want to disallow getADLType to inspect its argument
  -- This way, we have no information about x, except for its type
  getADLType :: p x -> String
  getADLTypes :: p x -> String
  getADLTypes p = getADLType p<>"s"
  getADLType_A :: p x -> String
  getADLType_A p = "A "<>getADLType p
  getADLType_a :: p x -> String
  getADLType_a p = "a "<>getADLType p

instance TypeAware TType where
  getADLType _ = "built-in type"
instance TypeAware (Maybe TType) where
  getADLType _ = "built-in type"
instance TypeAware A_Concept where
  getADLType _ = "concept"

-- SJC, Note about Haskell: I'm using scoped type variables here, via the flag "ScopedTypeVariables"
-- the result is that the b is bound by the type signature, so I can use `getADLType_a' with exactly that type
unexpectedType :: forall a b. (TypeAware a, TypeAware b, ShowADL a) => Origin -> a -> Guarded b
unexpectedType o x = Errors [CTXE o$ "Unexpected "<>getADLType [x]<>": "<>showADL x<>"\n  expecting "<>getADLType_a ([]::[b])]
-- There is a way to work around this without ScopedTypeVariables, but in my (SJC) view, this is less readable:
-- unexpectedType :: (TypeAware a, TypeAware b, ShowADL a) => Origin -> a -> Guarded b
-- unexpectedType o x = res
--   where res = Errors [CTXE o$ "Unexpected "<>getADLType [x]<>": "<>showADL x<>"\n  expecting "<>getADLType_a res]
-- There is no loop, since getADLType_a cannot inspect its first argument (res), and the chain of constructors: "Errors", (:) and CTXE, contains a lazy one (in fact, they are all lazy). In case all occurences of "getADLType_a" are non-strict in their first argument, that would already break a loop.
mkErrorReadingINCLUDE :: Maybe Origin -> FilePath -> String -> Guarded a
mkErrorReadingINCLUDE mo file str
 = Errors [CTXE (fromMaybe (Origin "command line argument") mo) msg]
    where 
      msg = intercalate "\n    " $
             [ "While looking for file '"++file++"':" 
             ]++lines str

mkMultipleRepresentTypesError :: A_Concept -> [(TType,Origin)] -> Guarded a
mkMultipleRepresentTypesError cpt rs
 = Errors [CTXE o msg]
    where 
      o = case rs of
           []  -> fatal 97 "Call of mkMultipleRepresentTypesError with no Representations"
           (_,x):_ -> x
      msg = intercalate "\n" $
             [ "The Concept "++showADL cpt++" was shown to be representable with multiple types."
             , "The following TYPEs are defined for it:"
             ]++
             [ "  - "++show t++" at "++showFullOrig orig | (t,orig)<-rs]
mkMultipleTypesInTypologyError :: [(A_Concept,TType,[Origin])] -> Guarded a
mkMultipleTypesInTypologyError tripls 
 = Errors [CTXE o msg]
    where
      o = case tripls of
            (_,_,(x:_)):_ -> x
            _  -> fatal 112 "No origin in list."
      msg = intercalate "\n" $
             [ "Concepts in the same typology must have the same TYPE."
             , "The following concepts are in the same typology, but not all"
             , "of them have the same TYPE:"
             ]++
             [ "  - REPRESENT "++name c++" TYPE "++show t++" at "++showFullOrig orig | (c,t,origs) <- tripls, orig <- origs]
mkMultipleRootsError :: [A_Concept] -> [A_Gen] -> Guarded a
mkMultipleRootsError roots gs
 = Errors [CTXE o msg]
    where 
      o = case gs of
           [] -> fatal 103 "mkMultipleRootsError called with no A_Gen."
           g:_ -> origin g
      msg = intercalate "\n" $
             [ "A typology must have at most one root concept."
             , "The following CLASSIFY statements define a typology with multiple root concepts: "
             ] ++
             [ "  - "++showADL x++" at "++showFullOrig (origin x) | x<-gs]++
             [ "Parhaps you could add the following statements:"]++
             [ "  CLASSIFY "++name cpt++" ISA "++show rootName    | cpt<-roots]
          
       where rootName = intercalate "_Or_" . map name $ roots 
nonMatchingRepresentTypes :: (Traced a1, Show a2, Show a3) => a1 -> a2 -> a3 -> Guarded a
nonMatchingRepresentTypes genStmt wrongType rightType
 = Errors [CTXE (origin genStmt)$ "A CLASSIFY statement is only allowed between Concepts that are represented by the same type, but "++show wrongType++" is not the same as "++show rightType]

class GetOneGuarded a where
  getOneExactly :: (Traced a1, ShowADL a1) => a1 -> [a] -> Guarded a
  getOneExactly _ [a] = Checked a
  getOneExactly o []  = hasNone o
  getOneExactly o _ = Errors [CTXE o'$ "Found too many:\n  "++s | CTXE o' s <- errors (hasNone o :: Guarded a)]
  hasNone :: (Traced a1, ShowADL a1) => a1  -- the object where the problem is arising
                                     -> Guarded a
  hasNone o = getOneExactly o []

instance GetOneGuarded (P_SubIfc a) where
  hasNone o = Errors [CTXE (origin o)$ "Required: one P-subinterface in "++showADL o]

instance GetOneGuarded (SubInterface) where
  hasNone o = Errors [CTXE (origin o)$ "Required: one A-subinterface in "++showADL o]

instance GetOneGuarded Declaration where
  getOneExactly _ [d] = Checked d
  getOneExactly o []  = Errors [CTXE (origin o)$ "No declaration for "++showADL o]
  getOneExactly o lst = Errors [CTXE (origin o)$ "Too many declarations match "++showADL o++".\n  Be more specific. These are the matching declarations:"++concat ["\n  - "++showADL l++" at "++showFullOrig (origin l) | l<-lst]]

mkTypeMismatchError :: (ShowADL t, Association t, Traced a2, Named a) => a2 -> t -> SrcOrTgt -> a -> Guarded a1
mkTypeMismatchError o decl sot conc
 = Errors [CTXE (origin o) message]
 where
  message = "The "++show sot++" for the population pairs, namely "++name conc
            ++"\n  must be more specific or equal to that of the relation you wish to populate, namely: "++showEC (sot,decl)


cannotDisambRel :: TermPrim -> [Expression] -> Guarded Expression
cannotDisambRel o exprs
 = Errors [CTXE (origin o) message]
  where
   message =
    case exprs of
     [] -> "No declarations match the relation: "++showADL o
     _  -> case o of
             (PNamedR(PNamedRel _ _ Nothing))
                -> intercalate "\n" $
                       ["Cannot disambiguate the relation: "++showADL o
                       ,"  Please add a signature (e.g. [A*B]) to the relation."
                       ,"  Relations you may have intended:"
                       ]++
                       ["  "++showADL e++"["++name (source e)++"*"++name (target e)++"]"
                       |e<-exprs]
             _  -> intercalate "\n" $
                       ["Cannot disambiguate: "++showADL o
                       ,"  Please add a signature."
                       ,"  You may have intended one of these:"
                       ]++
                       ["  "++showADL e|e<-exprs]

cannotDisamb :: TermPrim -> Guarded Expression
cannotDisamb o = Errors [CTXE (origin o)$ "Cannot disambiguate: "++showADL o++"\n  Please add a signature to it"]

uniqueNames :: (Named a, Traced a) =>
                     [a] -> Guarded ()
uniqueNames a = case (filter moreThanOne . groupWith name)  a of
                  [] -> pure ()
                  xs -> Errors (map messageFor xs)
    where
     moreThanOne (_:_:_) = True
     moreThanOne  _      = False
     messageFor :: (Named a, Traced a) => [a] -> CtxError
     messageFor (x:xs) = CTXE (origin x)
                      ("Names / labels must be unique. "++(show . name) x++", however, is used at:"++
                        concatMap (("\n    "++ ) . show . origin) (x:xs)
                        ++"."
                       )
     messageFor _ = fatal 90 "messageFor must only be used on lists with more that one element!"

mkDanglingPurposeError :: Purpose -> CtxError
mkDanglingPurposeError p = CTXE (origin p) $ "Purpose refers to non-existent " ++ showADL (explObj p)
-- Unfortunately, we cannot use position of the explanation object itself because it is not an instance of Trace.
mkDanglingRefError :: String -- The type of thing that dangles. eg. "Rule"
                   -> String -- the reference itself. eg. "Rule 42"
                   -> Origin -- The place where the thing is found.
                   -> CtxError
mkDanglingRefError entity ref orig =
  CTXE orig $ "Reference to non-existent " ++ entity ++ ": "++show ref
mkUndeclaredError :: (Traced e, Named e) => String -> e -> String -> CtxError
mkUndeclaredError entity objDef ref =
  CTXE (origin objDef) $ "Undeclared " ++ entity ++ " " ++ show ref ++ " referenced at field " ++ show (name objDef)

mkEndoPropertyError :: Origin -> [Prop] -> CtxError
mkEndoPropertyError orig ps =
  CTXE orig msg
  where 
    msg = intercalate "\n" $
       case ps of
         []  -> fatal 227 $ "What property is causing this error???"
         [p] -> ["Property "++show p++" can only be used for relations where"
                ,"  source and target are equal."]
         _   -> ["Properties "++showAnd++" can only be used for relations where"
                ,"  source and target are equal."]
     where showAnd = intercalate ", " (map show . init $ ps)++" and "++(show . last) ps

mkMultipleInterfaceError :: String -> Interface -> [Interface] -> CtxError
mkMultipleInterfaceError role ifc duplicateIfcs =
  CTXE (origin ifc) $ "Multiple interfaces named " ++ show (name ifc) ++ " for role " ++ show role ++ ":" ++
                      concatMap (("\n    "++ ) . show . origin) (ifc:duplicateIfcs)

mkInvalidCRUDError :: Origin -> String -> CtxError
mkInvalidCRUDError o str = CTXE o $ "Invalid CRUD annotation. (doubles and other characters than crud are not allowed): `"++str++"`."

mkIncompatibleAtomValueError :: PAtomValue -> String -> CtxError
mkIncompatibleAtomValueError pav msg= CTXE (origin pav) msg

mkInterfaceRefCycleError :: [Interface] -> CtxError
mkInterfaceRefCycleError []                 = fatal 108 "mkInterfaceRefCycleError called on []"
mkInterfaceRefCycleError cyclicIfcs@(ifc:_) = -- take the first one (there will be at least one) as the origin of the error
  CTXE (origin ifc) $ "Interfaces form a reference cycle:\n" ++
                      unlines [ "- " ++ show (name i) ++ " at position " ++ show (origin i) | i <- cyclicIfcs ]

mkIncompatibleInterfaceError :: P_ObjDef a -> A_Concept -> A_Concept -> String -> CtxError
mkIncompatibleInterfaceError objDef expTgt refSrc ref =
  CTXE (origin objDef) $ "Incompatible interface reference "++ show ref ++ " at field " ++ show (name objDef) ++
                         ":\nReferenced interface "++show ref++" has type " ++ show (name refSrc) ++
                         ", which is not comparable to the target " ++ show (name expTgt) ++ " of the expression at this field."

mkMultipleDefaultError :: (A_Concept, [ViewDef]) -> CtxError
mkMultipleDefaultError (_, [])              = fatal 118 "mkMultipleDefaultError called on []"
mkMultipleDefaultError (c, viewDefs@(vd0:_)) =
  CTXE (origin vd0) $ "Multiple default views for concept " ++ show (name c) ++ ":" ++
                      concat ["\n    VIEW " ++ name vd ++ " (at " ++ show (origin vd) ++ ")"
                             | vd <- viewDefs ]

mkIncompatibleViewError :: (Named b,Named c) => P_ObjDef a -> String -> b -> c -> CtxError
mkIncompatibleViewError objDef viewId viewRefCptStr viewCptStr =
  CTXE (origin objDef) $ "Incompatible view annotation <"++viewId++"> at field " ++ show (name objDef) ++ ":\nView " ++ show viewId ++ " has type " ++ name viewCptStr ++
                         ", which should be equal to or more general than the target " ++ name viewRefCptStr ++ " of the expression at this field."

mkOtherAtomInSessionError :: AAtomValue -> CtxError
mkOtherAtomInSessionError atomValue =
  CTXE OriginUnknown $ "The special concept `SESSION` must not contain anything else then `_SESSION`. However it is populated with `"++showADL atomValue++"`."

class ErrorConcept a where
  showEC :: a -> String
  showMini :: a -> String

instance ErrorConcept (P_ViewD a) where
  showEC x = showADL (vd_cpt x) ++" given in VIEW "++vd_lbl x
  showMini x = showADL (vd_cpt x)
instance ErrorConcept (P_IdentDef) where
  showEC x = showADL (ix_cpt x) ++" given in Identity "++ix_lbl x
  showMini x = showADL (ix_cpt x)

instance (ShowADL a2) => ErrorConcept (SrcOrTgt, A_Concept, a2) where
  showEC (p1,c1,e1) = showEC' (p1,c1,showADL e1)
  showMini (_,c1,_) = showADL c1

showEC' :: (SrcOrTgt, A_Concept, String) -> String
showEC' (p1,c1,e1) = showADL c1++" ("++show p1++" of "++e1++")"

instance (ShowADL a2, Association a2) => ErrorConcept (SrcOrTgt, a2) where
  showEC (p1,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showADL e1)
      Tgt -> showEC' (p1,target e1,showADL e1)
  showMini (p1,e1)
   = case p1 of
      Src -> showADL (source e1)
      Tgt -> showADL (target e1)

instance (ShowADL a2, Association a2) => ErrorConcept (SrcOrTgt, Origin, a2) where
  showEC (p1,o,e1)
   = case p1 of
      Src -> showEC' (p1,source e1,showADL e1 ++ ", "++showMinorOrigin o)
      Tgt -> showEC' (p1,target e1,showADL e1 ++ ", "++showMinorOrigin o)
  showMini (p1,_,e1)
   = case p1 of
      Src -> showADL (source e1)
      Tgt -> showADL (target e1)

mustBeOrdered :: (Traced a1, ErrorConcept a2, ErrorConcept a3) => a1 -> a2 -> a3 -> Guarded a
mustBeOrdered o a b
 = Errors [CTXE (origin o)$ "Type error, cannot match:\n  the concept "++showEC a
                                          ++"\n  and concept "++showEC b
                   ++"\n  if you think there is no type error, add an order between concepts "++showMini a++" and "++showMini b++"."]

mustBeOrderedLst :: (Traced o, ShowADL o, ShowADL a) => o -> [(A_Concept, SrcOrTgt, a)] -> Guarded b
mustBeOrderedLst o lst
 = Errors [CTXE (origin o)$ "Type error in "++showADL o++"\n  Cannot match:"++ concat
             [ "\n  - concept "++showADL c++", "++show st++" of "++showADL a
             | (c,st,a) <- lst ] ++
             "\n  if you think there is no type error, add an order between the mismatched concepts."
          ]

mustBeOrderedConcLst :: (Named a_conc) => Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> [[a_conc]] -> Guarded a
mustBeOrderedConcLst o (p1,e1) (p2,e2) cs
 = Errors [CTXE o$ "Ambiguous type when matching: "++show p1++" of "++showADL e1++"\n"
                                          ++" and "++show p2++" of "++showADL e2++".\n"
                   ++"  The type can be "++intercalate " or " (map (showADL . Slash) cs)
                   ++"\n  None of these concepts is known to be the smallest, you may want to add an order between them."]

newtype Slash a = Slash [a]
instance Named a => ShowADL (Slash a) where
  showADL (Slash x) = intercalate "/" (map name x)

mustBeBound :: Origin -> [(SrcOrTgt, Expression)] -> Guarded a
mustBeBound o [(p,e)]
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific by binding the "++show p++" of the expression "++showADL e++".\n"++
                   "  You could add more types inside the expression, or just write "++writeBind e++"."]
mustBeBound o lst
 = Errors [CTXE o$ "An ambiguity arises in type checking. Be more specific in the expressions "++intercalate " and " (map (showADL . snd) lst) ++".\n"++
                   "  You could add more types inside the expression, or write:"++
                   concat ["\n  "++writeBind e| (_,e)<-lst]]

writeBind :: Expression -> String
writeBind (ECpl e)
 = "("++showADL (EDcV (sign e))++" - "++showADL e++")"
writeBind e
 = "("++showADL e++") /\\ "++showADL (EDcV (sign e))

data Guarded a = Errors [CtxError] | Checked a deriving Show

instance Functor Guarded where
 fmap _ (Errors a)  = Errors a
 fmap f (Checked a) = Checked (f a)
instance Applicative Guarded where
 pure = Checked
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked _) = Errors a
 (<*>) (Checked _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a ++ b) -- this line makes Guarded violate some (potential) applicative/monad laws
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
