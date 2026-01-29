{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# HLINT ignore "Redundant bracket" #-}

module Ampersand.Input.ADL1.CtxError
  ( CtxError (..),
    Warning,
    addWarning,
    addWarnings,
    GetOneGuarded (..),
    Guarded (..), -- If you use Guarded in a monad, make sure you use "ApplicativeDo" in order to get error messages in parallel.
    lexerError2CtxError,
    lexerWarning2Warning,
    mkBoxRowsnhWarning,
    mkCaseProblemWarning,
    mkCrudForRefInterfaceError,
    mkCrudWarning,
    mkDanglingPurposeError,
    mkDanglingRefError,
    mkEndoPropertyError,
    mkErrorReadingINCLUDE,
    mkGenericParserError,
    mkIncompatibleAtomValueError,
    mkIncompatibleInterfaceError,
    mkIncompatibleViewError,
    mkInterfaceMustBeDefinedOnObject,
    mkInterfaceRefNarrowerError,
    mkInterfaceRefCycleError,
    mkInvalidCRUDError,
    mkInvariantViolationsError,
    mkMultipleDefaultError,
    mkMultipleInterfaceError,
    mkMultipleRepresentTypesError,
    mkMultipleRootsError,
    mkMultipleTypesInTypologyError,
    mkNoBoxItemsWarning,
    mkOperatorError,
    mkConceptNotInSchemaError,
    mkRelationTooNarrowForViewError,
    mkViewTooSpecificError,
    mkViewExpressionMismatchError,
    mkOtherAtomInSessionError,
    mkOtherTupleInSessionError,
    mkParserStateWarning,
    mkRoundTripError,
    mkRoundTripTextError,
    mkSubInterfaceMustBeDefinedOnObject,
    mkTurtleWarning,
    mkUndeclaredError,
    mkUnusedCptDefWarning,
    mustBeBound,
    mustBeOrdered,
    mustBeOrderedConcLst,
    mustBeOrderedLst,
    mustBeValidName,
    nonMatchingRepresentTypes,
    unexpectedType,
    uniqueLabels,
    uniqueNames,
    whenCheckedM,
  )
where

-- SJC: I consider it ill practice to export any CtxError constructors
-- Reason: All error messages should pass through the CtxError module
-- By not exporting anything that takes a string, we prevent other modules from containing error message
-- If you build a function that must generate an error, put it in CtxError and call it instead
-- see `getOneExactly' / `GetOneGuarded' as a nice example
-- Although I also consider it ill practice to export PE
-- for the same reasons, I did this as a quick fix for the parse errors
-- HJO: I consider it ill practice to export any Warning constructors as well, for the same reasons as SJC stated above.

import Ampersand.ADL1
-- import Ampersand.ADL1.Disambiguate (DisambPrim (..))
import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree (showWithAliases, Guarded(..), CtxError(..), Warning(..))
import Ampersand.Core.ShowAStruct
import Ampersand.Core.ShowPStruct
import Ampersand.Input.ADL1.FilePos ()
import Ampersand.Input.ADL1.LexerMessage
import Data.Typeable
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

mkRoundTripError :: Text -> NonEmpty CtxError -> Guarded a
mkRoundTripError parsestring err =
  Errors (RoundTripError parsestring (Left err) NE.:| [])

mkRoundTripTextError :: Text -> Text -> Guarded a
mkRoundTripTextError parsestring msg =
  Errors (RoundTripError parsestring (Right msg) NE.:| [])

lexerError2CtxError :: LexerError -> CtxError
lexerError2CtxError = LE

errors :: Guarded t -> Maybe (NE.NonEmpty CtxError)
errors (Checked _ _) = Nothing
errors (Errors lst) = Just lst

unexpectedType :: Origin -> Maybe TType -> Guarded A_Concept
unexpectedType o x =
  Errors
    ( CTXE
        o
        ( ( case x of
              Nothing -> "The Generic Built-in type was unexpeced. "
              Just ttyp -> "Unexpected built-in type: " <> tshow ttyp
          )
            <> "\n  expecting a concept."
        )
        NE.:| []
    )

mkErrorReadingINCLUDE :: Maybe Origin -> [Text] -> Guarded a
mkErrorReadingINCLUDE mo msg =
  Errors . pure $ CTXE (fromMaybe (Origin "command line argument") mo) (T.intercalate "\n    " msg)

mkMultipleRepresentTypesError :: ([A_Concept], [(TType, [Origin])]) -> Guarded a
mkMultipleRepresentTypesError (cpts, tts@((_, o : _) : _)) =
  Errors . pure $ CTXE o msg
  where
    msg =
      T.intercalate "\n"
        $ [ case cpts of
              [cpt] -> "The Concept " <> showWithAliases cpt <> " is representable with multiple types."
              _ -> "The Concepts " <> (T.intercalate "," . map showWithAliases) cpts <> " are representable with multiple types.",
            "The following TYPEs are defined for it:"
          ]
        <> fmap showOrigs tts
    showOrigs :: (TType, [Origin]) -> Text
    showOrigs (t, origs) = "  - " <> tshow t <> " at " <> T.intercalate "," (fmap showMinorOrigin origs)
mkMultipleRepresentTypesError (cpt, _) = fatal ("mkMultipleRepresentTypesError trips over concept " <> tshow cpt)

mkMultipleTypesInTypologyError :: [(A_Concept, TType, [Origin])] -> Guarded a
mkMultipleTypesInTypologyError tripls =
  Errors . pure $ CTXE o msg
  where
    o = case tripls of
      (_, _, x : _) : _ -> x
      _ -> fatal "No origin in list."
    msg =
      T.intercalate "\n"
        $ [ "Concepts in the same typology must have the same TYPE.",
            "The following concepts are in the same typology, but not all",
            "of them have the same TYPE:"
          ]
        <> ["  - REPRESENT " <> showWithAliases c <> " TYPE " <> tshow t <> " at " <> showFullOrig orig | (c, t, origs) <- tripls, orig <- origs]

mkMultipleRootsError :: [A_Concept] -> NE.NonEmpty AClassify -> Guarded a
mkMultipleRootsError roots gs =
  Errors . pure $ CTXE o msg
  where
    o = origin (NE.head gs)
    msg =
      T.intercalate "\n"
        $ [ "A typology must have at most one root concept.",
            "The following CLASSIFY statements define a typology with multiple root concepts: "
          ]
        <> ["  - " <> showA x <> " at " <> showFullOrig (origin x) | x <- NE.toList gs]
        <> ["The following concepts do not comply:"]
        <> ["  Concept " <> showWithAliases cpt <> " has roots " <> rootNames | cpt <- roots]
      where
        rootNames = T.intercalate ", " . map showWithAliases $ roots

nonMatchingRepresentTypes :: Origin -> TType -> TType -> Guarded a
nonMatchingRepresentTypes orig wrongType rightType =
  Errors
    . pure
    $ CTXE orig
    $ "A CLASSIFY statement is only allowed between Concepts that are represented by the same type, but "
    <> tshow wrongType
    <> " is not the same as "
    <> tshow rightType

class GetOneGuarded a b | b -> a where
  {-# MINIMAL getOneExactly | hasNone #-} -- we don't want endless loops, do we?
  getOneExactly :: b -> [a] -> Guarded a
  getOneExactly _ [a] = pure a
  getOneExactly o [] = hasNone o
  getOneExactly o _ =
    case errors (hasNone o :: Guarded a) of
      Nothing -> fatal "No error message!"
      Just (CTXE o' s NE.:| _) -> Errors . pure $ CTXE o' $ "Found too many:\n  " <> s
      Just (PE _ NE.:| _) -> fatal "Didn't expect a PE constructor here"
      Just (LE _ NE.:| _) -> fatal "Didn't expect a LE constructor here"
      Just (RoundTripError _ _ NE.:| _) -> fatal "Didn't expect a RoundTripError constructor here"
  hasNone ::
    b -> -- the object where the problem is arising
    Guarded a
  hasNone o = getOneExactly o []

instance GetOneGuarded Expression P_NamedRel where
  getOneExactly _ [d] = pure d
  getOneExactly o [] =
    Errors
      . pure
      $ CTXE (origin o)
      $ "A relation is used that is not explicitly declared: "
      <> showP o
  getOneExactly o lst =
    Errors
      . pure
      $ CTXE (origin o)
      $ "The use of \""
      <> tshow (p_nrnm o)
      <> "\" is ambiguous."
      <> "\n  Make the type explicit by specifying one of the following:"
      <> T.concat ["\n  - " <> showA l | l <- lst]

instance GetOneGuarded Expression (P_NamedRel, (A_Concept, A_Concept)) where
  getOneExactly (o, (sr, tg)) = getOneExactly (o, (Just sr, Just tg))

instance GetOneGuarded Expression (P_NamedRel, (Maybe A_Concept, Maybe A_Concept)) where
  getOneExactly o lst = case lst of
    [d] -> pure d
    [] ->
      Errors
        . pure
        $ (CTXE . origin . fst) o
        $ "A relation is used that is not explicitly declared: "
        <> showP_T o
    _ ->
      Errors
        . pure
        $ (CTXE . origin . fst) o
        $ "The use of \""
        <> tshow (p_nrnm (fst o))
        <> "\" is ambiguous."
        <> "\n  Make the type explicit by specifying one of the following:"
        <> T.concat ["\n  - " <> showA l | l <- lst]
    where
      showP_T :: (P_NamedRel, (Maybe A_Concept, Maybe A_Concept)) -> Text
      showP_T (p, (src, tgt)) = fullName p <> "[" <> showC src <> "*" <> showC tgt <> "]"
        where
          showC Nothing = "???"
          showC (Just tp) = showA tp

-- | Rules, identity statements, view definitions, interfaces, and box labels
--   need unique names.
uniqueNames ::
  (Named a, Traced a) =>
  -- | The `nameclass` ("rule", "interface", etc.) is used to
  --   provide a more meaningful error message.
  Text ->
  -- | List of things that need to have unique names.
  [a] ->
  Guarded ()
uniqueNames nameclass = uniqueBy name messageFor
  where
    messageFor x =
      CTXE
        (origin $ NE.head x)
        ( "Every "
            <> nameclass
            <> " must have a unique name. "
            <> fullName (NE.head x)
            <> ", however, is used at:"
            <> T.intercalate "\n    " (NE.toList $ fmap (tshow . origin) x)
            <> "."
        )

uniqueLabels :: Origin -> (a -> Text1) -> [a] -> Text -> Guarded ()
uniqueLabels orig toLabel items thing = uniqueBy toLabel (messageFor . fmap toLabel) items
  where
    messageFor :: NonEmpty Text1 -> CtxError
    messageFor x =
      CTXE orig
        . T.intercalate "\n    "
        $ [ "The label `" <> text1ToText (NE.head x) <> "` occurs " <> tshow (NE.length x) <>
            " times in the " <> thing <> " defined at: ",
            tshow orig <> "."
          ]

-- | Helper function to check for uniqueness.
uniqueBy ::
  (Ord b) =>
  -- | user supplied function to project something out of each element
  (a -> b) ->
  -- | user supplied function to generate the error for a nonempty list
  (NonEmpty a -> CtxError) ->
  -- | List of things that need to have some unique property
  [a] ->
  Guarded ()
uniqueBy fun messageFor a = case (filter moreThanOne . NE.groupAllWith fun) a of
  [] -> pure ()
  x : xs -> Errors . fmap messageFor $ x NE.:| xs
  where
    moreThanOne = not . null . NE.tail

mkDanglingPurposeError :: Purpose -> CtxError
mkDanglingPurposeError p = CTXE (origin p) $ "Purpose refers to non-existent " <> showA (explObj p)

-- Unfortunately, we cannot use position of the explanation object itself because it is not an instance of Trace.
mkDanglingRefError ::
  Text -> -- The type of thing that dangles. eg. "Rule"
  Name -> -- the reference itself. eg. "Rule 42"
  Origin -> -- The place where the thing is found.
  CtxError
mkDanglingRefError entity ref orig =
  CTXE orig $ "Reference to non-existent " <> entity <> ": " <> tshow ref

mkUndeclaredError :: Text -> P_BoxItem a -> Name -> CtxError
mkUndeclaredError entity objDef ref =
  case objDef of
    P_BoxItemTerm {} ->
      CTXE (origin objDef)
        $ "Undeclared "
        <> entity
        <> " "
        <> tshow ref
        <> " referenced at field "
        <> tshow (obj_PlainName objDef)
    _ -> fatal "Unexpected use of mkUndeclaredError."

mkEndoPropertyError :: Origin -> [PProp] -> CtxError
mkEndoPropertyError orig ps =
  CTXE orig msg
  where
    msg = T.intercalate "\n"
      $ case ps of
        [] -> fatal "What property is causing this error???"
        [p] ->
          [ "Property " <> tshow p <> " can only be used for relations where",
            "  source and target are equal."
          ]
        _ ->
          [ "Properties " <> showAnd <> " can only be used for relations where",
            "  source and target are equal."
          ]
      where
        showAnd = commaEng "and" (map tshow ps)

mkMultipleInterfaceError :: Text -> Interface -> [Interface] -> CtxError
mkMultipleInterfaceError role' ifc duplicateIfcs =
  CTXE (origin ifc)
    $ "Multiple interfaces named "
    <> fullName ifc
    <> " for role "
    <> tshow role'
    <> ":"
    <> T.intercalate "\n    " (map (tshow . origin) (ifc : duplicateIfcs))

mkInvalidCRUDError :: Origin -> Text1 -> CtxError
mkInvalidCRUDError o x = CTXE o $ "Invalid CRUD annotation. (doubles and other characters than crud are not allowed): `" <> text1ToText x <> "`."

mkCrudForRefInterfaceError :: Origin -> CtxError
mkCrudForRefInterfaceError o = CTXE o "Crud specification is not allowed in combination with a reference to an interface."

mkIncompatibleAtomValueError :: PAtomValue -> Text -> CtxError
mkIncompatibleAtomValueError pav msg =
  CTXE
    (origin pav)
    ( case msg of
        "" -> fatal "Error message must not be empty."
        _ -> msg
    )

mkInvariantViolationsError :: (Rule -> AAtomPair -> Text) -> (Rule, AAtomPairs) -> CtxError
mkInvariantViolationsError applyViolText (r, ps) =
  CTXE (origin r) violationMessage
  where
    violationMessage :: Text
    violationMessage =
      T.unlines
        $ [ if length ps == 1
              then "There is a violation of RULE " <> fullName r <> ":"
              else "There are " <> tshow (length ps) <> " violations of RULE " <> fullName r <> ":"
          ]
        <> (map ("  " <>) . listPairs 10 . toList $ ps)
    listPairs :: Int -> [AAtomPair] -> [Text]
    listPairs i xs =
      case xs of
        [] -> []
        h : tl
          | i == 0 -> ["  ... (" <> tshow (length xs) <> " more)"]
          | otherwise -> applyViolText r h : listPairs (i - 1) tl

mkInterfaceRefCycleError :: NE.NonEmpty Interface -> CtxError
mkInterfaceRefCycleError cyclicIfcs =
  CTXE (origin (NE.head cyclicIfcs))
    $ "Interfaces form a reference cycle:\n"
    <> (T.unlines . NE.toList $ fmap showIfc cyclicIfcs)
  where
    showIfc :: Interface -> Text
    showIfc i = "- " <> fullName i <> " at position " <> tshow (origin i)

mkIncompatibleInterfaceError :: P_BoxItem a -> A_Concept -> A_Concept -> Name -> CtxError
mkIncompatibleInterfaceError objDef expTgt refSrc ref =
  case objDef of
    P_BoxItemTerm {} ->
      CTXE (origin objDef)
        $ "Incompatible interface reference "
        <> fullName ref
        <> " at field "
        <> maybe "without a label" tshow (obj_PlainName objDef)
        <> ":\nReferenced interface "
        <> fullName ref
        <> " has type "
        <> showWithAliases refSrc
        <> ", which is not comparable to the target "
        <> showWithAliases expTgt
        <> " of the term at this field."
    _ -> fatal "Improper use of mkIncompatibleInterfaceError"

mkInterfaceRefNarrowerError :: Origin -> Name -> Expression -> A_Concept -> A_Concept -> CtxError
mkInterfaceRefNarrowerError refOrigin ifcName parentExpr ifcConcept expectedConcept =
  CTXE refOrigin
    $ "The interface "
    <> fullName ifcName
    <> " works on concept "
    <> showWithAliases ifcConcept
    <> ", which is narrower (i.e. more specific) than "
    <> showWithAliases expectedConcept
    <> ", being the target of "
    <> showA parentExpr
    <> ". Please refer to an interface with a wider (or equal) interface-concept."

mkMultipleDefaultError :: NE.NonEmpty ViewDef -> CtxError
mkMultipleDefaultError vds =
  CTXE (origin . NE.head $ vds)
    $ "Multiple default views for concept "
    <> showWithAliases cpt
    <> ":"
    <> T.intercalate "\n    " (fmap showViewDef (NE.toList vds))
  where
    showViewDef :: ViewDef -> Text
    showViewDef vd = "VIEW " <> fullName vd <> " (at " <> tshow (origin vd) <> ")"
    cpt = case nubOrd . NE.toList . fmap vdcpt $ vds of
      [] -> fatal "There should be at least one concept found in a nonempty list of viewdefs."
      [c] -> c
      _ -> fatal "Different concepts are not acceptable in calling mkMultipleDefaultError"

mkOperatorError :: Origin -> PBinOp -> A_Concept -> TType -> CtxError
mkOperatorError orig oper cpt typ =
  CTXE orig
    $ T.intercalate
      "\n  "
      [ "Illegal use of `" <> tshow oper <> "`",
        "`" <> tshow oper <> "` is used in combination with the concept " <> tshow cpt <> ".",
        "However, " <> tshow cpt <> " is of TYPE " <> tshow typ <> ", which doesn't support " <> tshow oper <> "."
      ]

mkIncompatibleViewError :: (Named b, Named c) => P_BoxItem a -> Name -> b -> c -> CtxError
mkIncompatibleViewError objDef viewId viewRefCptStr viewCptStr =
  case objDef of
    P_BoxItemTerm {} ->
      CTXE (origin objDef)
        $ "Incompatible view annotation <"
        <> fullName viewId
        <> "> at field "
        <> maybe "without a label" tshow (obj_PlainName objDef)
        <> ":"
        <> "\nView "
        <> tshow viewId
        <> " has type "
        <> fullName viewCptStr
        <> ", which should be equal to or more general than the target "
        <> fullName viewRefCptStr
        <> " of the term at this field."
    _ -> fatal "Improper use of mkIncompatibleViewError."

mkOtherAtomInSessionError :: AAtomValue -> CtxError
mkOtherAtomInSessionError atomValue =
  CTXE OriginUnknown $ "The special concept `SESSION` cannot contain an initial population. However it is populated with `" <> showA atomValue <> "`."

mkOtherTupleInSessionError :: Relation -> AAtomPair -> CtxError
mkOtherTupleInSessionError r pr =
  CTXE OriginUnknown $ "The special concept `SESSION` cannot contain an initial population. However it is populated with `" <> showA pr <> "` by populating the relation `" <> showA r <> "`."

mkConceptNotInSchemaError :: Origin -> Name -> Text -> CtxError
mkConceptNotInSchemaError orig cptName contextName =
  CTXE orig
    $ "Cannot compile "
    <> contextName
    <> " because "
    <> fullName cptName
    <> " does not occur in a RELATION, CLASSIFY, REPRESENT, or RULE statement."

mkRelationTooNarrowForViewError :: Origin -> Expression -> A_Concept -> CtxError
mkRelationTooNarrowForViewError orig expr viewConcept =
  CTXE orig
    $ T.intercalate "\n  "
    $ [ "The expression in a VIEW segment is too specific for the VIEW concept.",
        "Expression: " <> showA expr,
        "VIEW concept: " <> showWithAliases viewConcept,
        "The source of the expression (" <> (showWithAliases . source) expr <> ") is more specific than the VIEW concept (" <> showWithAliases viewConcept <> ").",
        "A VIEW expression must work for all instances of the VIEW concept, not just a subset."
      ]

mkViewTooSpecificError :: Origin -> P_ViewDef -> Expression -> CtxError
mkViewTooSpecificError orig vd objExpr =
  CTXE orig
    $ T.intercalate "\n  "
    $ [ "The VIEW " <> tshow (name vd) <> " is defined for " <> tshow (vd_cpt vd) <> ".",
        "This is too specific for the target of " <> showA objExpr <> ", which is " <> showA (target objExpr) <> "."
      ]

mkViewExpressionMismatchError :: Origin -> Expression -> A_Concept -> CtxError
mkViewExpressionMismatchError orig expr viewConcept =
  CTXE orig
    $ T.intercalate "\n  "
    $ [ "The expression in a VIEW segment has an incompatible type.",
        "Expression: " <> showA expr,
        "Expression source: " <> (showWithAliases . source) expr,
        "VIEW concept: " <> showWithAliases viewConcept,
        "The source of the expression and the VIEW concept have no ISA relationship.",
        "The expression source must be equal to or more general than the VIEW concept."
      ]

mkInterfaceMustBeDefinedOnObject :: P_Interface -> A_Concept -> TType -> CtxError
mkInterfaceMustBeDefinedOnObject ifc cpt tt =
  CTXE (origin ifc)
    . T.intercalate "\n  "
    $ [ "The TYPE of the concept for which an INTERFACE is defined must be OBJECT.",
        "The TYPE of the concept `" <> showWithAliases cpt <> "`, for interface `" <> fullName ifc <> "`, however is " <> tshow tt <> "."
      ]

mkSubInterfaceMustBeDefinedOnObject :: P_SubIfc TermPrim -> A_Concept -> TType -> CtxError
mkSubInterfaceMustBeDefinedOnObject x cpt tt =
  CTXE (origin x)
    . T.intercalate "\n  "
    $ [ "The TYPE of the concept for which a " <> tshow boxTemplate <> " is defined must be OBJECT.",
        "The TYPE of the concept `" <> showWithAliases cpt <> "`, for this " <> tshow boxTemplate <> ", however is " <> tshow tt <> "."
      ]
  where
    boxTemplate = btType . si_header $ x

class ErrorConcept a where
  showEC :: a -> Text

instance ErrorConcept (P_ViewD a) where
  showEC x = showP (vd_cpt x) <> " given in VIEW " <> fullName x

instance ErrorConcept P_IdentDef where
  showEC x = showP (ix_cpt x) <> " given in IDENT rule " <> fullName x

instance (AStruct a2) => ErrorConcept (SrcOrTgt, A_Concept, a2) where
  showEC (p1, c1, e1) = showEC' (p1, c1, showA e1)

showEC' :: (SrcOrTgt, A_Concept, Text) -> Text
showEC' (p1, c1, e1) = showWithAliases c1 <> " (" <> tshow p1 <> " of " <> e1 <> ")"

instance (AStruct declOrExpr, HasSignature declOrExpr) => ErrorConcept (SrcOrTgt, declOrExpr) where
  showEC (p1, e1) =
    case p1 of
      Src -> showEC' (p1, source e1, showA e1)
      Tgt -> showEC' (p1, target e1, showA e1)

instance (AStruct declOrExpr, HasSignature declOrExpr) => ErrorConcept (SrcOrTgt, Origin, declOrExpr) where
  showEC (p1, o, e1) =
    case p1 of
      Src -> showEC' (p1, source e1, showA e1 <> ", " <> showMinorOrigin o)
      Tgt -> showEC' (p1, target e1, showA e1 <> ", " <> showMinorOrigin o)

mustBeOrdered :: (ErrorConcept t1, ErrorConcept t2) => Origin -> t1 -> t2 -> Guarded a
mustBeOrdered o a b =
  Errors
    . pure
    . CTXE (origin o)
    . T.unlines
    $ [ "Type error, cannot match:",
        "  the concept " <> showEC a,
        "  and concept " <> showEC b
      ]

mustBeOrderedLst :: P_SubIfc TermPrim -> Expression -> ObjectDef -> Guarded b
mustBeOrderedLst o objExpr ojd =
  (Errors . pure . CTXE (origin o) . T.unlines)
    [ "Type error in BOX",
      "  Cannot match "
        <> (tshow . target) objExpr
        <> " (the target of "
        <> showA objExpr
        <> ") with "
        <> (showWithAliases . source . objExpression) ojd
        <> " (the source of: "
        <> (showA . objExpression) ojd
        <> " at "
        <> showMinorOrigin (origin ojd)
        <> ")."
    ]

mustBeOrderedConcLst :: Origin -> (SrcOrTgt, Expression) -> (SrcOrTgt, Expression) -> [[A_Concept]] -> Guarded (A_Concept, [A_Concept])
mustBeOrderedConcLst o (p1, e1) (p2, e2) cs =
  Errors
    . pure
    . CTXE (origin o)
    . T.unlines
    $ [ "Ambiguous type when matching: " <> tshow p1 <> " of " <> showA e1,
        " and " <> tshow p2 <> " of " <> showA e2 <> ".",
        "  The type can be " <> T.intercalate " or " (map (T.intercalate "/" . map showWithAliases) cs),
        "  None of these concepts is known to be the smallest, you may want to add an order between them."
      ]

mustBeBound :: Origin -> [(SrcOrTgt, Expression)] -> Guarded a
mustBeBound o [(p, e)] =
  Errors
    . pure
    . CTXE (origin o)
    . T.unlines
    $ [ "An ambiguity arises in type checking. Be more specific by binding the " <> tshow p <> " of the term",
        "  " <> showA e <> ".",
        "  You could add more types inside the term, or just write",
        "  " <> writeBind e <> "."
      ]
mustBeBound o lst =
  Errors
    . pure
    . CTXE (origin o)
    . T.unlines
    $ [ "An ambiguity arises in type checking. Be more specific in the terms ",
        "  " <> T.intercalate " and " (map (showA . snd) lst) <> ".",
        "  You could add more types inside the term, or write:"
      ]
    <> ["  " <> writeBind e | (_, e) <- lst]

mustBeValidName :: Origin -> Text -> Guarded Name
mustBeValidName orig t1 =
  Errors
    . pure
    . CTXE orig
    . T.unlines
    $ [ "A single word is expected as name, which must start with a letter and may contain only alphanumerical letters and digits.",
        "  the following was found: `" <> tshow t1 <> "`."
      ]

writeBind :: Expression -> Text
writeBind (ECpl e) =
  "(" <> showA (EDcV (sign e)) <> " - " <> showA e <> ")"
writeBind e =
  "(" <> showA e <> ") /\\ " <> showA (EDcV (sign e))

lexerWarning2Warning :: LexerWarning -> Warning
lexerWarning2Warning (LexerWarning a b) =
  Warning (FileLoc a "") (T.intercalate "\n" . fmap T.pack $ showLexerWarningInfo b)

mkBoxRowsnhWarning :: Origin -> Warning
mkBoxRowsnhWarning orig =
  Warning orig
    $ T.intercalate
      "\n   "
      [ "The common use of BOX <ROWSNH> has become obsolete. It was used to be able",
        "to have rows without header.",
        "In that case, please use BOX <FORM hideLabels> for this purpose.",
        "If you still want to use this class for some reason, you have to provide",
        "the ROWSNH.html template youself. Failing to do so will cause an error when you",
        "generate your prototype."
      ]

mkNoBoxItemsWarning :: Origin -> Warning
mkNoBoxItemsWarning orig =
  Warning orig
    $ T.intercalate
      "\n    "
      [ "This list of BOX-items is empty."
      ]

mkTurtleWarning :: Origin -> [Text] -> Warning
mkTurtleWarning orig msg =
  Warning orig (T.unlines msg)

mkCrudWarning :: P_Cruds -> [Text] -> Warning
mkCrudWarning (P_Cruds o _) msg = Warning o (T.unlines msg)

mkUnusedCptDefWarning :: AConceptDef -> Warning
mkUnusedCptDefWarning cptDef =
  Warning (origin cptDef) $ "The concept '" <> fullName cptDef <> "' is not used in any relation."

mkCaseProblemWarning :: (Typeable a, Named a) => a -> a -> Warning
mkCaseProblemWarning x y =
  Warning OriginUnknown
    $ T.intercalate
      "\n    "
      [ "Ampersand is case sensitive. you might have meant that the following are equal:",
        tshow (typeOf x) <> " `" <> fullName x <> "` and `" <> fullName y <> "`."
      ]

mkGenericParserError :: Origin -> Text -> Guarded a
mkGenericParserError orig msg = Errors . pure $ CTXE orig msg

mkParserStateWarning :: Origin -> Text -> Warning
mkParserStateWarning = Warning

addWarning :: Warning -> Guarded a -> Guarded a
addWarning _ (Errors a) = Errors a
addWarning w (Checked a ws) = Checked a (ws <> [w])

addWarnings :: [Warning] -> Guarded a -> Guarded a
addWarnings ws ga =
  case ga of
    Checked a ws' -> Checked a (ws <> ws')
    Errors a -> Errors a

-- Shorthand for working with Guarded in a monad
whenCheckedM :: (Monad m) => m (Guarded a) -> (a -> m (Guarded b)) -> m (Guarded b)
whenCheckedM ioGA fIOGB =
  do
    gA <- ioGA
    case gA of
      Errors err -> return $ Errors err
      Checked a ws1 -> do
        gb <- fIOGB a
        return $ addWarnings ws1 gb

showFullOrig :: Origin -> Text
showFullOrig (FileLoc (FilePos filename line column) t) =
  "Error at symbol "
    <> t
    <> " in file "
    <> T.pack filename
    <> " at line "
    <> tshow line
    <> " : "
    <> tshow column
showFullOrig x = tshow x

showMinorOrigin :: Origin -> Text
showMinorOrigin (FileLoc (FilePos _ line column) _) = "line " <> tshow line <> " : " <> tshow column
showMinorOrigin v = tshow v
