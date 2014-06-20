{- COPIED FROM http://hackage.haskell.org/package/altfloat-0.3.1
 - Disabled Sortable instances for instances of Prelude.Ord -}
{-
 - Copyright (C) 2009-2010 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

-- | 'Poset' and 'Sortable' instances for instances of 'Prelude.Ord'
{-# LANGUAGE CPP #-}
module DatabaseDesign.Ampersand.Core.Poset.Instances where

import qualified DatabaseDesign.Ampersand.Core.Poset.Internal as Poset
import DatabaseDesign.Ampersand.Core.Poset.Internal (Poset, Sortable, partialOrder, totalOrder)

import Data.Ratio
import Data.List
import Data.Word
import Data.Int

#define POSET_ORD_INSTANCE(ctx, v) instance ctx Poset (v) where { \
    compare = (partialOrder .) . compare; \
    (<)     = (<); \
    (<=)    = (<=); \
    (>=)    = (>=); \
    (>)     = (>); \
    (<==>)  = const $ const True; \
    (</=>)  = const $ const False }

#define SORTABLE_ORD_INSTANCE(ctx, v) instance ctx Sortable (v) where { \
    isOrdered = const True; \
    sortBy f = sortBy $ (totalOrder .) . f; \
    max    = max; \
    min    = min; }

#define ORD_INSTANCE(ctx, v) \
    POSET_ORD_INSTANCE(ctx, v); \
 {- SORTABLE_ORD_INSTANCE(ctx, v); -} \
    instance ctx Poset.Ord (v)

ORD_INSTANCE(, Bool)
ORD_INSTANCE(, Char)
ORD_INSTANCE(, Int)
ORD_INSTANCE(, Int8)
ORD_INSTANCE(, Int16)
ORD_INSTANCE(, Int32)
ORD_INSTANCE(, Int64)
ORD_INSTANCE(, Word)
ORD_INSTANCE(, Word8)
ORD_INSTANCE(, Word16)
ORD_INSTANCE(, Word32)
ORD_INSTANCE(, Word64)
ORD_INSTANCE(, Integer)

ORD_INSTANCE(Integral a =>, Ratio a)
