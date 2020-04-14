{-# OPTIONS_GHC -Wall -Wno-inline-rule-shadowing #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveTraversable #-}
-- | This module defines the basic Data Structure (and instances)
--   for graph-based data-structures.
module GRDT.Data where
import Data.Void
import Text.Megaparsec
import Data.List.NonEmpty (NonEmpty((:|)))

-- | This is the type of the parser state.
-- Keeping track of this gives us a file position for displaying error messages
type PState = State String Void

-- | Cardinatlities: UNIvalent and INJective stand for 0..1,
--   and TOTal and SURjective stand for 1..omega.
data Card = UNI | TOT | INJ | SUR deriving (Show,Eq)

-- | The type of a relation after parsing.
-- This is replaced by pairs of types through the Typed class later,
-- so we don't need to make this parametric.
data RelationType = RelationType InpName InpName deriving (Show)

-- | The declaration of a relation (aka binary predicate) after parsing.
data Declaration = Decl InpName RelationType [Card] deriving (Show)
-- Name, Relation, Card

-- | A string that the user typed, these should only be created at parse-time
type InpName = UserProvided String

-- | Something that the user typed, created at parse-time,
--   so it makes no sense to make this a Functor.
--   If you feel the need to map over this, turn it into a tuple first!
data UserProvided a = UP PState a deriving Show

-- | We can use the thing the user typed in an error message about a certain position.
--   This is a convenient wrapper for that
fail' :: MonadFail m => UserProvided a1 -> (a1 -> [Char]) -> m a2
fail' (UP src v) f
 = fail (errorBundlePretty unknown)
 where unknown :: ParseErrorBundle String Void
       unknown = ParseErrorBundle (err:|[]) (statePosState src)
       err = TrivialError (stateOffset src) (Just (Label (h:|msg))) mempty
       (h:msg) = f v

-- | For the purpose of extracting the user-provided value
runUP :: UserProvided a -> a
runUP (UP _ a) = a

-- | Parses something like:
-- INTERFACE ClientOnly :: Client [name: clientName NameInterface, address: clientAddress , city: clientCity ]
data Interface ifc_info com_info tp t
 = Intf InpName ifc_info [Component com_info tp t] deriving (Show,Functor,Foldable,Traversable)

getInterfaceName :: Interface ifc_info com_info tp t -> InpName
getInterfaceName (Intf nm _ _) = nm

onComponents :: Applicative f
             => (nm -> Component ifc1 tp1 t1 -> f (Component ifc2 tp2 t2))
             -> Interface nm ifc1 tp1 t1 -> f (Interface nm ifc2 tp2 t2)
onComponents f (Intf nm a cps) = Intf nm a <$> traverse (f a) cps

onRAIdent :: Applicative f -- just need fmap and pure, but there's no class for that
          => (tp1 -> f tp2) -> RAExpr tp1 t -> f (RAExpr tp2 t)
onRAIdent _ (RAVar t) = pure $ RAVar t
onRAIdent f (RAConverse t) = RAConverse <$> onRAIdent f t
onRAIdent f (RAIdent t) = RAIdent <$> f t

onRAType :: (Applicative f, Traversable t) => (a -> f tp)
         -> RAExpr a (t a) -> f (RAExpr tp (t tp))
onRAType f (RAVar t) = RAVar <$> traverse f t
onRAType f (RAConverse t) = RAConverse <$> onRAType f t
onRAType f (RAIdent t) = RAIdent <$> f t

onCPType :: (Applicative f, Traversable t1) => (t2 -> f tp)
         -> Component (a, t2) t2 (t1 t2) -> f (Component (a, tp) tp (t1 tp))
onCPType f (CP (nm,tp) e) = (\t e' -> CP (nm,t) e') <$> f tp <*> onRAType f e

-- | Like declaration, but mappable
data FoundDecl a b = FoundDecl a (b, b) deriving Functor
fdeclName :: FoundDecl a b -> a
fdeclName (FoundDecl a _) = a

instance Traversable (FoundDecl a) where
  traverse f (FoundDecl a (b1,b2)) = (\b1' b2' -> FoundDecl a (b1',b2')) <$> f b1 <*> f b2
instance Foldable (FoundDecl a) where
  foldr f e (FoundDecl _ (b1,b2)) = foldr f e [b1,b2]
  foldMap f (FoundDecl _ (b1,b2)) = foldMap f [b1,b2]
instance Typed (FoundDecl a b) b where
  source_target (FoundDecl _ b) = b
instance Cards (FoundDecl (a, [Card]) b) where
  cards (FoundDecl (_,cards') _) = cards'

-- | reletion algebraic expression (for supported operators).
-- Currently, this means the (typed) identity relation,
-- as well as a 
data RAExpr tp t = RAVar t
                 | RAConverse (RAExpr tp t)
                 | RAIdent !tp
                 deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Check if a *correctly typed* expression comparison yields equal expressions.
--   By our correctly typed assumption, Identity relations are equal.
--   We assume all variables are equal by their comparison
eqExpr :: (t1 -> t2 -> Bool)
       -> (tp1 -> tp2 -> Bool)
       -> RAExpr tp1 t1
       -> RAExpr tp2 t2
       -> Bool
eqExpr eqD _   (RAVar t1) (RAVar t2) = eqD t1 t2
eqExpr eqD eqT (RAConverse t1) (RAConverse t2) = eqExpr eqD eqT t1 t2
eqExpr eqD eqT (RAConverse (RAConverse t1)) t2 = eqExpr eqD eqT t1 t2
eqExpr eqD eqT t1 (RAConverse (RAConverse t2)) = eqExpr eqD eqT t1 t2
eqExpr eqD eqT (RAConverse (RAIdent tp)) t2    = eqExpr eqD eqT (RAIdent tp) t2
eqExpr eqD eqT t1 (RAConverse (RAIdent tp))    = eqExpr eqD eqT t1 (RAIdent tp)
eqExpr _   eqT (RAIdent t1) (RAIdent t2) = eqT t1 t2
eqExpr _ _ _ _ = False -- todo: this can be made more precise when given enough information.

-- Component name, expression, interface to be used
data Component info tp t
 = CP info (RAExpr tp t) deriving (Show,Eq,Functor,Foldable,Traversable)

onInfo :: Functor f => (a -> f b) -> Component a x t -> f (Component b x t)
onInfo f (CP info e) = flip CP e <$> f info
onIfInfo :: Functor f => (a -> f b) -> Interface a c x t -> f (Interface b c x t)
onIfInfo f (Intf nm info e) = flip (Intf nm) e <$> f info

class Typed a b where
  source :: a -> b
  source = fst . source_target
  target :: a -> b
  target = snd . source_target
  source_target :: a -> (b,b)
  source_target x = (source x,target x)
  {-# MINIMAL source_target | (source, target) #-}

instance Typed b c => Typed (RAExpr c b) c where
  source_target (RAVar t) = source_target t
  source_target (RAConverse t) = let (x,y) = source_target t in (y,x)
  source_target (RAIdent b) = (b, b)
instance Typed b c => Typed (Component i c b) c where
  source_target (CP _ e) = source_target e

instance Typed Declaration InpName where
  source_target (Decl _ tp _) = source_target tp
instance Typed RelationType InpName where
  source_target (RelationType s t) = (s,t)

class Cards a where
  cards :: a -> [Card]

instance Cards Declaration where
  cards (Decl _ _ c) = c
instance Cards t => Cards (RAExpr x t) where
  cards (RAVar t) = cards t
  cards (RAConverse t) = map conv_card (cards t)
  cards (RAIdent _) = [UNI,INJ,TOT,SUR]

{-@ {conv_card . conv_card == id} @-}
conv_card :: Card -> Card
conv_card UNI = INJ
conv_card INJ = UNI
conv_card SUR = TOT
conv_card TOT = SUR
