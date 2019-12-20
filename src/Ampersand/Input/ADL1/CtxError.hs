{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ampersand.Input.ADL1.CtxError
  ( CtxError(PE)
  , Warning
  , cannotDisambiguate
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
  , mkInterfaceMustBeDefinedOnObject
  , mkSubInterfaceMustBeDefinedOnObject
  , lexerWarning2Warning
  , lexerError2CtxError
  , addWarning, addWarnings
  , mkCrudWarning
  , mkBOX_ROWSNH_Warning
  , mkCaseProblemWarning
  , mkNoBoxItemsWarning
  , Guarded(..) -- If you use Guarded in a monad, make sure you use "ApplicativeDo" in order to get error messages in parallel.
  , whenCheckedM, whenChecked, whenError
  )
-- SJC: I consider it ill practice to export any CtxError constructors
-- Reason: All error messages should pass through the CtxError module
-- By not exporting anything that takes a string, we prevent other modules from containing error message
-- If you build a function that must generate an error, put it in CtxError and call it instead
-- see `getOneExactly' / `GetOneGuarded' as a nice example
-- Although I also consider it ill practice to export PE
-- for the same reasons, I did this as a quick fix for the parse errors
-- HJO: I consider it ill practice to export any Warning constructors as well, for the same reasons as SJC stated above.

where

import           Ampersand.ADL1
import           Ampersand.ADL1.Disambiguate(DisambPrim(..))
import           Ampersand.Basics
import           Ampersand.Core.AbstractSyntaxTree (Type)
import           Ampersand.Core.ShowAStruct
import           Ampersand.Core.ShowPStruct
import           Ampersand.Input.ADL1.FilePos()
import           Ampersand.Input.ADL1.LexerMessage
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import           Data.Typeable
import           GHC.Exts (groupWith)
import           Text.Parsec

data CtxError = CTXE Origin String -- SJC: I consider it ill practice to export CTXE, see remark at top
              | PE ParseError 
              | LE LexerError
instance Show CtxError where
  -- The vscode extension expects errors and warnings
  -- to be in a standardized format. The show function
  -- complies to that. Iff for whatever reason 
  -- this function is changed, please verify 
  -- the proper working of the ampersand-language-extension
  show err = L.intercalate "\n  " $
    [show (origin err) ++ " error:"] ++ 
    (case err of
       CTXE _ s               -> lines s
       PE e    -> -- The first line of a parse error allways contains
                  -- the filename and position of the error. However,
                  -- these are in a wrong format. So we strip the first 
                  -- line of the error:
                  case lines (show e) of
                     []       -> fatal "Whoh! the impolssible just happend! (triggered by a parse error somewhere in your script)"
                     _:xs     -> xs
       LE (LexerError _ info) -> lines (show info)
    )

data Warning = Warning Origin String
instance Show Warning where
  -- The vscode extension expects errors and warnings
  -- to be in a standardized format. The show function
  -- complies to that. Iff for whatever reason 
  -- this function is changed, please verify 
  -- the proper working of the ampersand-language-extension
  show (Warning o msg) = L.intercalate "\n  " $
       [show o ++ " warning: "]
    ++ lines msg




instance Traced CtxError where
    origin (CTXE o _) = o
    origin (PE perr)  = let sourcePos = errorPos perr 
                        in FileLoc (FilePos (sourceName sourcePos) (sourceLine sourcePos) (sourceColumn sourcePos)) ""
    origin (LE (LexerError fp info)) = FileLoc fp (show info)

--TODO: Give the errors in a better way
lexerError2CtxError :: LexerError -> CtxError
lexerError2CtxError err = LE err

errors :: Guarded t -> Maybe (NE.NonEmpty CtxError)
errors (Checked _ _) = Nothing
errors (Errors lst) = Just lst

unexpectedType :: Origin -> Maybe TType -> Guarded A_Concept
unexpectedType o x = 
   Errors (CTXE o ((case x of 
                     Nothing   -> "The Generic Built-in type was unexpeced. "
                     Just ttyp -> "Unexpected built-in type: "<>show ttyp
                   )<>"\n  expecting a concept.")
           NE.:| []
          )

mkErrorReadingINCLUDE :: Maybe Origin -> [String] -> Guarded a
mkErrorReadingINCLUDE mo msg
 = Errors . pure $ CTXE (fromMaybe (Origin "command line argument") mo) (L.intercalate "\n    " msg)

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
mkCyclesInGensError :: NE.NonEmpty [AClassify] -> Guarded a
mkCyclesInGensError cycles = Errors (fmap mkErr cycles)
 where 
  mkErr :: [AClassify] -> CtxError
  mkErr gs = 
    case gs of 
      []    -> fatal "Nothing to report about!" 
      (g:_) -> CTXE (origin g) 
             . L.intercalate "\n" $
             [ "Classifications must not contain cycles."
             , "The following CLASSIFY statements are cyclic:"
             ]++
             [ "  - "++showA gn++" at "++showFullOrig (origin gn) | gn <- gs]

mkMultipleRootsError :: [A_Concept] -> NE.NonEmpty AClassify -> Guarded a
mkMultipleRootsError roots gs
 = Errors . pure $ CTXE o msg
    where 
      o = origin (NE.head gs)
      msg = L.intercalate "\n" $
             [ "A typology must have at most one root concept."
             , "The following CLASSIFY statements define a typology with multiple root concepts: "
             ] ++
             [ "  - "++showA x++" at "++showFullOrig (origin x) | x<-NE.toList gs]++
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
  getOneExactly _ [a] = pure a 
  getOneExactly o []  = hasNone o
  getOneExactly o _ = 
    case errors (hasNone o :: Guarded a) of
      Nothing -> fatal "No error message!"
      Just (CTXE o' s NE.:| _) -> Errors . pure $ CTXE o' $ "Found too many:\n  "++s
      Just (PE _      NE.:| _) -> fatal "Didn't expect a PE constructor here"
      Just (LE _      NE.:| _) -> fatal "Didn't expect a LE constructor here"
      
  hasNone :: b  -- the object where the problem is arising
             -> Guarded a
  hasNone o = getOneExactly o []

instance GetOneGuarded Expression P_NamedRel where
  getOneExactly _ [d] = pure d
  getOneExactly o []  = Errors . pure $ CTXE (origin o) $
      "No relation for "++showP o
  getOneExactly o lst = Errors . pure $ CTXE (origin o) $
      "An ambiguity arises in trying to match "++showP o
    ++".\n  Be more specific by using one of the following matching expressions:"
    ++concat ["\n  - "++showA l | l<-lst]

mkTypeMismatchError :: Origin -> Relation -> SrcOrTgt -> Type -> Guarded Type
mkTypeMismatchError o rel sot typ
 = Errors . pure $ CTXE (origin o) message
 where
  message = "The "++(case sot of
                       Src -> "source"
                       Tgt -> "target"
                    )++"("++name typ++") for the population pairs "
            ++"\n  must be more specific or equal to that of the "
            ++"relation you wish to populate ("++name rel++show (sign rel)++" found at "++show (origin rel)++")."


cannotDisambiguate :: TermPrim -> DisambPrim -> Guarded Expression
cannotDisambiguate o x = Errors . pure $ CTXE (origin o) message
  where
    message = 
      case x of 
        Rel [] -> "A relation is used that is not defined: "++showP o
        Rel exprs -> case o of
             (PNamedR(PNamedRel _ _ Nothing))
                -> L.intercalate "\n" $
                       ["Cannot disambiguate the relation: "++showP o
                       ,"  Please add a signature (e.g. [A*B]) to the relation."
                       ,"  Relations you may have intended:"
                       ]++
                       map (("  "++) . showA) exprs
             _  -> L.intercalate "\n" $
                       ["Cannot disambiguate: "++showP o
                       ,"  Please add a signature."
                       ,"  You may have intended one of these:"
                       ]++
                       ["  "++showA e|e<-exprs]
        Known _ -> fatal "We have a known expression, so it is allready disambiguated."
        _ -> "Cannot disambiguate: "++showP o++"\n  Please add a signature to it"

uniqueNames :: (Named a, Traced a) =>
                     [a] -> Guarded ()
uniqueNames = uniqueBy name
uniqueBy :: (Traced a, Show b, Ord b) => (a -> b) -> [a] -> Guarded ()
uniqueBy fun a = case (filter moreThanOne . groupWith fun) a of
                  []   -> pure ()
                  x:xs -> Errors $ (messageFor x) NE.:| (fmap messageFor xs)
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
     where showAnd = commaEng "and" (map show ps)

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

mkInterfaceRefCycleError :: NE.NonEmpty Interface -> CtxError
mkInterfaceRefCycleError cyclicIfcs =
  CTXE (origin (NE.head cyclicIfcs)) $
             "Interfaces form a reference cycle:\n" ++
             (unlines . NE.toList $ fmap showIfc cyclicIfcs)
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
  
mkMultipleDefaultError :: NE.NonEmpty ViewDef -> CtxError
mkMultipleDefaultError vds =
  CTXE (origin . NE.head $ vds) $ 
      "Multiple default views for concept " <> show (name cpt) <> ":" <>
        (concatMap (\vd -> "\n    VIEW " ++ name vd ++ " (at " ++ show (origin vd) ++ ")") $ vds)
     where
       cpt = case nubOrd . NE.toList . fmap vdcpt $ vds of
             [] -> fatal "There should be at least one concept found in a nonempty list of viewdefs."
             [c] -> c 
             _  -> fatal "Different concepts are not acceptable in calling mkMultipleDefaultError"
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

mkInterfaceMustBeDefinedOnObject :: P_Interface -> A_Concept -> TType -> CtxError
mkInterfaceMustBeDefinedOnObject ifc cpt tt =
  CTXE (origin ifc) . L.intercalate "\n  " $
      ["The TYPE of the concept for which an INTERFACE is defined must be OBJECT."
      ,"The TYPE of the concept `"++name cpt++"`, for interface `"++name ifc++"`, however is "++show tt++"."
      ]
mkSubInterfaceMustBeDefinedOnObject :: P_SubIfc (TermPrim, DisambPrim) -> A_Concept -> TType -> CtxError
mkSubInterfaceMustBeDefinedOnObject x cpt tt =
  CTXE (origin x). L.intercalate "\n  " $
      ["The TYPE of the concept for which a "++boxClass++" is defined must be OBJECT."
      ,"The TYPE of the concept `"++name cpt++"`, for this "++boxClass++", however is "++show tt++"."
      ]
    where boxClass = fromMaybe "BOX" (si_class x)
                       
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

mustBeOrderedLst :: P_SubIfc (TermPrim, DisambPrim) -> [(A_Concept, SrcOrTgt, P_BoxItem TermPrim)] -> Guarded b
mustBeOrderedLst o lst
 = Errors . pure . CTXE (origin o) . unlines $
     [ "Type error in BOX"
     , "  Cannot match:"
     ]++
     [ "  - concept "++showA c++" , "++showP st++" of: "++showP (exprOf a)
     | (c,st,a) <- lst
     ]++ 
     [ "  if you think there is no type error, add an order between the mismatched concepts."
     , "  You can do so by using a CLASSIFY statement."
     ]
     where exprOf :: P_BoxItem TermPrim -> Term TermPrim
           exprOf x = 
             case x of 
               P_BxExpr{} -> obj_ctx x
               P_BxTxt{}  -> fatal "How can a type error occur with a TXT field???"
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

lexerWarning2Warning :: LexerWarning -> Warning 
lexerWarning2Warning (LexerWarning a b) = 
  Warning (FileLoc a "") (L.intercalate "\n" $ showLexerWarningInfo b)

instance Traced Warning where
    origin (Warning o _) = o
mkBOX_ROWSNH_Warning :: Origin -> Warning
mkBOX_ROWSNH_Warning orig =
  Warning orig $ L.intercalate "\n   "
     ["The common use of BOX <ROWSNH> has become obsolete. It was used to be able"
     ,   "to have rows without header."
     ,   "In that case, please use ROWS for this purpose."
     ,   "If you still want to use this class for some reason, you have to provide"
     ,   "the template for youself. Failing to do so will cause an error when you"
     ,   "generate your prototype."
     ]
mkNoBoxItemsWarning :: Origin -> Warning
mkNoBoxItemsWarning orig = 
  Warning orig $ L.intercalate "\n    "
     ["This list of BOX-items is empty."
     ]
mkCrudWarning :: P_Cruds -> [String] -> Warning
mkCrudWarning (P_Cruds o _ ) msg = Warning o (unlines msg)
mkCaseProblemWarning :: (Typeable a, Named a) => a -> a -> Warning
mkCaseProblemWarning x y = Warning orig $ L.intercalate "\n    " 
      ["Ampersand is case sensitive. you might have meant that the following are equal:"
      ,    show (typeOf x) ++"`"++name x++"` and `"++name y++"`."
      ]
    where orig :: Origin 
          orig = OriginUnknown
addWarning :: Warning -> Guarded a -> Guarded a
addWarning _ (Errors a) = Errors a
addWarning w (Checked a ws) = Checked a (ws <> [w])
addWarnings :: [Warning] -> Guarded a -> Guarded a
addWarnings ws ga = 
  case ga of
    Checked a ws' -> Checked a (ws <> ws')
    Errors a      -> Errors a
   
data Guarded a = 
   Errors (NE.NonEmpty CtxError) 
 | Checked a [Warning]
--   deriving Show

instance Functor Guarded where
 fmap _ (Errors a)  = Errors a
 fmap f (Checked a ws) = Checked (f a) ws
instance Applicative Guarded where
 pure x = Checked x []
 (<*>) (Checked f ws) (Checked a ws') = Checked (f a) (ws<>ws')
 (<*>) (Errors  a) (Checked _ _) = Errors a
 (<*>) (Checked _ _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a >> b)
instance Monad Guarded where
 (>>=) (Checked a ws) f = addWarnings ws (f a)
 (>>=) (Errors x) _ = Errors x

-- Shorthand for working with Guarded in a monad 
whenCheckedM :: Monad m => m (Guarded a) -> (a -> m (Guarded b)) -> m (Guarded b)
whenCheckedM ioGA fIOGB =
   do gA <- ioGA
      case gA of
         Errors err -> return $ Errors err
         Checked a ws1 -> do gb <- fIOGB a
                             return $ addWarnings ws1 gb

whenChecked :: Guarded a -> (a -> Guarded b) -> Guarded b
whenChecked ga fgb =
      case ga of
         Checked a ws -> addWarnings ws $ fgb a
         Errors err -> Errors err

whenError :: Guarded a -> Guarded a -> Guarded a
whenError (Errors _) a = a
whenError a@(Checked _ _) _ = a


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


