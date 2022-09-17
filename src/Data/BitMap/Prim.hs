{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitMap.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.BitMap.Prim
  ( BitMap# (BM#),

    -- * TODO
    singleton#,

    -- * Comparison
    gt#,
    ge#,
    eq#,
    ne#,
    lt#,
    le#,

    -- * Bitwise
    clz#,
    ctz#,

    -- ** Logical
    and#,
    xor#,
    or#,
    not#,

    -- ** Shifts
    shift#,
    (<<#),
    (>>#),
    (<<!#),
    (>>!#),

    -- ** Rotations
    rotate#,
    rotateL#,
    rotateR#,

    -- * Write
    insert#,
    alter#,
    write#,
    clear#,
    toggle#,
    popback#,

    -- * Index
    index#,

    -- * Query
    null#,
    full#,
    size#,
    bounds#,

    -- * Folds
    foldl#,
    foldr#,
    foldMap#,

    -- ** Strict Folds
    foldl'#,
    foldr'#,
    foldMap'#,

    -- * Conversion
    fromList#,
  )
where

import Data.Bool.Prim (Bool# (False#, True#))
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)
import Data.Coerce (coerce)

import GHC.Exception (overflowException)
import GHC.Exts (Int (I#), Word (W#))
import GHC.Exts qualified as GHC
import GHC.Magic (oneShot)

-- include WORD_SIZE_IN_BITS
#include "MachDeps.h" 

#if (WORD_SIZE_IN_BITS == 64)
# define MAX_BITMAP_INDEX 63
#else
# define MAX_BITMAP_INDEX 31
#endif

--------------------------------------------------------------------------------

import Data.BitMap.Prim.Core (BitMap# (BM#), bitmask#)
import Data.BitMap.Prim.Unsafe qualified as Unsafe

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
singleton# :: Int# -> BitMap#
singleton# i# =
  case bounds# i# of
    True# -> BM# (bitmask# i#)
    _ -> BM# 0##

-- Comparison ------------------------------------------------------------------

infix 4 `gt#`, `ge#`, `eq#`, `ne#`, `lt#`, `le#`

-- | "Greater than" comparison on two 'Int#' values.
--
-- @since 1.0.0
gt# :: BitMap# -> BitMap# -> Bool#
gt# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.gtWord# bmp0# bmp1#)

-- | "Greater than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
ge# :: BitMap# -> BitMap# -> Bool#
ge# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.geWord# bmp0# bmp1#)

-- | "Equal to" comparison on two 'BitMap#' values.
--
-- @since 1.0.0
eq# :: BitMap# -> BitMap# -> Bool#
eq# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.eqWord# bmp0# bmp1#)

-- | "Not equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
ne# :: BitMap# -> BitMap# -> Bool#
ne# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.neWord# bmp0# bmp1#)

-- | "Less than" comparison on two 'Int#' values.
--
-- @since 1.0.0
lt# :: BitMap# -> BitMap# -> Bool#
lt# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.ltWord# bmp0# bmp1#)

-- | "Less than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
le# :: BitMap# -> BitMap# -> Bool#
le# (BM# bmp0#) (BM# bmp1#) = Bool.fromInt# (GHC.leWord# bmp0# bmp1#)

-- Bitwise ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
clz# :: BitMap# -> Int#
clz# (BM# bmp#) = GHC.word2Int# (GHC.clz# bmp#)

-- | TODO
--
-- @since 1.0.0
ctz# :: BitMap# -> Int#
ctz# (BM# bmp#) = GHC.word2Int# (GHC.ctz# bmp#)

-- Bitwise - Logical -----------------------------------------------------------

infixl 7 `and#`
infixl 6 `xor#`
infixl 5 `or#`

-- | TODO
--
-- @since 1.0.0
and# :: BitMap# -> BitMap# -> BitMap#
and# (BM# bmp0#) (BM# bmp1#) = BM# (GHC.and# bmp0# bmp1#)

-- | TODO
--
-- @since 1.0.0
xor# :: BitMap# -> BitMap# -> BitMap#
xor# (BM# bmp0#) (BM# bmp1#) = BM# (GHC.xor# bmp0# bmp1#)

-- | TODO
--
-- @since 1.0.0
or# :: BitMap# -> BitMap# -> BitMap#
or# (BM# bmp0#) (BM# bmp1#) = BM# (GHC.or# bmp0# bmp1#)

-- | TODO
--
-- @since 1.0.0
not# :: BitMap# -> BitMap#
not# (BM# bmp#) = BM# (GHC.not# bmp#)

-- Bitwise - Shifts ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
shift# :: BitMap# -> Int# -> BitMap#
shift# bmp# i# =
  case 0# GHC.<=# i# of
    1# -> bmp# >># i#
    _ -> GHC.negateInt# i# <<# bmp#

-- | TODO
--
-- @since 1.0.0
(<<#) :: Int# -> BitMap# -> BitMap#
(<<#) i# (BM# bmp#) =
  case 0# GHC.<=# i# of
    1# -> BM# (GHC.shiftL# bmp# i#)
    _ -> GHC.raise# overflowException

-- | TODO
--
-- @since 1.0.0
(>>#) :: BitMap# -> Int# -> BitMap#
(>>#) (BM# bmp#) i# =
  case 0# GHC.<=# i# of
    1# -> BM# (GHC.shiftRL# bmp# i#)
    _ -> GHC.raise# overflowException

-- | TODO
--
-- @since 1.0.0
(<<!#) :: Int# -> BitMap# -> BitMap#
(<<!#) i# (BM# bmp#) = BM# (GHC.uncheckedShiftL# bmp# i#)

-- | TODO
--
-- @since 1.0.0
(>>!#) :: BitMap# -> Int# -> BitMap#
(>>!#) (BM# bmp#) i# = BM# (GHC.uncheckedShiftRL# bmp# i#)

-- Bitwise - Rotations ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rotate# :: BitMap# -> Int# -> BitMap#
rotate# bmp# 0# = bmp#
rotate# bmp# i# =
  let wsib# = WORD_SIZE_IN_BITS#
      tmp0# = GHC.andI# i# (wsib# GHC.-# 1#)
      tmp1# = tmp0# <<!# bmp#
      tmp2# = bmp# >>!# (wsib# GHC.-# tmp0#)
   in or# tmp1# tmp2#

-- | TODO
--
-- @since 1.0.0
rotateL# :: BitMap# -> Int# -> BitMap#
rotateL# bmp# i# = or# (i# <<!# bmp#) (bmp# >>!# (WORD_SIZE_IN_BITS# GHC.-# i#))

-- rotateL# bmp# 0# = bmp#

-- | TODO
--
-- @since 1.0.0
rotateR# :: BitMap# -> Int# -> BitMap#
rotateR# bmp# i# = or# (bmp# >>!# i#) ((WORD_SIZE_IN_BITS# GHC.-# i#) <<!# bmp#)

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
insert# :: Int# -> Bool# -> BitMap# -> BitMap#
insert# i# x# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeInsert# i# x# bmp#
    _ -> bmp#

-- | TODO
--
-- @since 1.0.0
alter# :: (Bool# -> Bool#) -> Int# -> BitMap# -> BitMap#
alter# op# i# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeInsert# i# (op# (Unsafe.unsafeIndex# i# bmp#)) bmp#
    _ -> bmp#

-- | TODO
--
-- @since 1.0.0
write# :: Int# -> BitMap# -> BitMap#
write# i# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeWrite# i# bmp#
    _ -> bmp#

-- | TODO
--
-- @since 1.0.0
clear# :: Int# -> BitMap# -> BitMap#
clear# i# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeClear# i# bmp#
    _ -> bmp#

-- | TODO
--
-- @since 1.0.0
toggle# :: Int# -> BitMap# -> BitMap#
toggle# i# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeToggle# i# bmp#
    _ -> bmp#

-- | TODO
--
-- @since 1.0.0
popback# :: BitMap# -> BitMap#
popback# (BM# bmp#) = BM# (GHC.and# (GHC.minusWord# bmp# 1##) bmp#)

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index# :: Int# -> BitMap# -> Bool#
index# i# bmp# =
  case bounds# i# of
    True# -> Unsafe.unsafeIndex# i# bmp#
    _ -> False#

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
null# :: BitMap# -> Int#
null# = coerce GHC.eqWord# 0##
{-# INLINE null# #-}

-- | TODO
--
-- @since 1.0.0
full# :: BitMap# -> Int#
full# (BM# bmp#) = case maxBound of W# max# -> GHC.eqWord# bmp# max#
{-# INLINE full# #-}

-- | TODO
--
-- @since 1.0.0
size# :: BitMap# -> Int#
size# (BM# bmp#) = GHC.word2Int# (GHC.popCnt# bmp#)

-- | TODO
--
-- @since 1.0.0
bounds# :: Int# -> Bool#
bounds# i# =
  Bool.fromInt# (GHC.andI# (0# GHC.<=# i#) (i# GHC.<# WORD_SIZE_IN_BITS#))

-- Folds -----------------------------------------------------------------------

slp# :: BitMap# -> Int# -> BitMap#
#if (WORD_SIZE_IN_BITS == 64)
slp# bmp# i# = and# (i# <<!# bmp#) (BM# 0x7FFFFFFFFFFFFFFF##)
#else
slp# bmp# i# = and# (i# <<!# bmp#) (BM# 0x7FFFFFFF##)
#endif

srp# :: BitMap# -> Int# -> BitMap#
#if (WORD_SIZE_IN_BITS == 64)
srp# bmp# i# = and# (bmp# >>!# i#) (BM# 0xFFFFFFFFFFFFFFFE##)
#else
srp# bmp# i# = and# (bmp# >>!# i#) (BM# 0xFFFFFFFE##)
#endif

-- | TODO
--
-- @since 1.0.0
foldl# :: (a -> Int -> a) -> a -> BitMap# -> a
foldl# con# nil# bmp# = loop# (MAX_BITMAP_INDEX# GHC.-# ctz# bmp#)
  where
    dec# :: Int# -> Int#
    dec# i# = i# GHC.-# ctz# (srp# bmp# (MAX_BITMAP_INDEX# GHC.-# i#))

    loop# i# =
      case i# GHC.>=# 0# of
        1# -> con# (loop# (dec# i#)) (I# i#)
        _ -> nil#

-- | TODO
--
-- @since 1.0.0
foldr# :: (Int -> a -> a) -> a -> BitMap# -> a
foldr# con# nil# bmp# = loop# (clz# bmp#)
  where
    inc# :: Int# -> Int#
    inc# i# = clz# (slp# bmp# i#) GHC.+# i#

    loop# i# =
      case i# GHC.<# WORD_SIZE_IN_BITS# of
        1# -> con# (I# i#) (loop# (inc# i#))
        _ -> nil#

-- | TODO
--
-- @since 1.0.0
foldMap# :: Monoid m => (Int -> m) -> BitMap# -> m
foldMap# f = foldl# con# mempty
  where
    con# xs = mappend xs . f
    {-# INLINE con# #-}

-- Folds - Strict Folds --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl'# :: (a -> Int -> a) -> a -> BitMap# -> a
foldl'# con# nil# bmp# = foldl# cps# id bmp# nil#
  where
    cps# k x# = oneShot \xs -> xs `seq` k (con# xs x#)
    {-# INLINE cps# #-}

-- | TODO
--
-- @since 1.0.0
foldr'# :: (Int -> a -> a) -> a -> BitMap# -> a
foldr'# con# nil# bmp# = foldr# cps# id bmp# nil#
  where
    cps# x# k = oneShot \xs -> xs `seq` k (con# x# xs)
    {-# INLINE cps# #-}

-- | TODO
--
-- @since 1.0.0
foldMap'# :: Monoid m => (Int -> m) -> BitMap# -> m
foldMap'# f =
  -- see Note [Inlining foldMap'#]
  foldl'# con# mempty
  where
    con# xs = mappend xs . f
{-# INLINE foldMap'# #-}

-- Note [Inlining foldMap'#]
--
-- Without an explicit INLINE pragma, GHC will apply a worker-wrapper
-- transformation on the 'Monoid' dictionary foldMap'# depends on. It would be
-- wasteful to split unwrapping the arguments to foldMap'# between two seperate
-- functions since foldMap'# is already manually worker-wrappered. Specifying
-- the INLINE pragma for foldMap'# collapses the worker wrapper transformation
-- by pushing the unwrapping of 'Monoid' inside foldMap'#.

-- Conversion ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fromList# :: [Int] -> BitMap#
fromList# [] = BM# 0##
fromList# (I# x# : xs) = write# x# (fromList# xs)