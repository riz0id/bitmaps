{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitMap
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- == Complexity
--
-- The amortized running time is given for each operation, with \(n\) referring
-- to the number of set bits in the 'BitMap' and \(W\) referring to the width 
-- of a platform word.
--
module Data.BitMap
  ( BitMap,
    capacity,

    -- * Construction
    empty,
    filled,
    singleton,

    -- * Write
    insert,
    alter,
    write,
    clear,
    toggle,
    popback,

    -- * Index
    (!),
    index,

    -- * Query
    null,
    full,
    size,
    bounds,

    -- * Folds
    foldl,
    foldr,
    foldMap,

    -- ** Strict Folds
    foldl',
    foldr',
    foldMap',

    -- * Traversals
    for_,
    traverse_,

    -- * Conversion
    fromMask,
    toMask,
    assocs,

    -- ** List
    fromList,
    toAscList,
    toDescList,

    -- ** Maps
    toIntMap,
    toMap,

    -- ** Sets
    toIntSet,
    toSet,
  )
where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Exts (Int (I#), Word (W#))
import GHC.Exts qualified as GHC

import Prelude hiding (foldMap, foldl, foldr, lookup, null)

-- import WORD_SIZE_IN_BITS
#include "MachDeps.h" 

--------------------------------------------------------------------------------

import Data.BitMap.Core (BitMap (BM))
import Data.BitMap.Prim (BitMap# (BM#))
import Data.BitMap.Prim qualified as Prim
import Data.BitMap.Unsafe qualified as Unsafe

-- | 'capacity' is the number of bits that can be stored in a 'BitMap'. This is
-- always the width of a platform word on the host machine.
--
-- * For a 64-bit machine:
--
--   @ 'capacity' == 64 @
--
-- * For a 32-bit machine:
--
--   @ 'capacity' == 32 @
--
-- @since 1.0.0
capacity :: Int
capacity = WORD_SIZE_IN_BITS
{-# INLINE capacity #-}

-- Construction ----------------------------------------------------------------

-- | \(\mathcal{O}(1)\). 'empty' constructs a 'BitMap' with all its bits
-- cleared.
--
-- prop> fromList empty []
--
-- @since 1.0.0
empty :: BitMap
empty = fromMask 0
{-# INLINE empty #-}

-- | \(\mathcal{O}(1)\). 'filled' constructs a 'BitMap' with all its bits set.
--
-- prop> fromList filled == [0 .. capacity - 1]
--
-- @since 1.0.0
filled :: BitMap
filled = fromMask maxBound 
{-# INLINE filled #-}

-- | \(\mathcal{O}(1)\). 'singleton' constructs a 'BitMap' containing a single
-- bit set at the location specified by the index, and all other bits cleared.
--
-- >>> singleton 7
-- fromList [7]
--
-- The location specified must be a positive integer less than 'capacity'.
-- Otherwise, the location will be ignored and 'singleton' will return an empty
-- bitmap.
--
-- >>> singleton (- 5)
-- fromList []
--
-- >>> singleton 1337
-- fromList []
--
-- @since 1.0.0
singleton :: Int -> BitMap
singleton (I# i#) = BM (Prim.singleton# i#)
{-# INLINE singleton #-}

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
insert :: Int -> Bool -> BitMap -> BitMap
insert (I# i#) x (BM bmp#) = BM (Prim.insert# i# (Bool.fromBool x) bmp#)
{-# INLINE insert #-}

-- | TODO
--
-- @since 1.0.0
alter :: (Bool -> Bool) -> Int -> BitMap -> BitMap
alter op (I# i#) (BM bmp#) =
  let op# :: Bool# -> Bool#
      op# x# = Bool.fromBool (op (Bool.toBool x#))
   in BM (Prim.alter# op# i# bmp#)
{-# INLINE alter #-}

-- | TODO
--
-- @since 1.0.0
write :: Int -> BitMap -> BitMap
write (I# i#) (BM bmp#) = BM (Prim.write# i# bmp#)
{-# INLINE write #-}

-- | TODO
--
-- @since 1.0.0
clear :: Int -> BitMap -> BitMap
clear (I# i#) (BM bmp#) = BM (Prim.clear# i# bmp#)
{-# INLINE clear #-}

-- | TODO
--
-- @since 1.0.0
toggle :: Int -> BitMap -> BitMap
toggle (I# i#) (BM bmp#) = BM (Prim.toggle# i# bmp#)
{-# INLINE toggle #-}

-- | TODO
--
-- @since 1.0.0
popback :: BitMap -> BitMap
popback (BM bmp#) = BM (Prim.popback# bmp#)
{-# INLINE popback #-}

-- Index -----------------------------------------------------------------------

infixl 9 !
infixr 9 `index`

-- | TODO
--
-- @since 1.0.0
(!) :: BitMap -> Int -> Bool
(!) = flip index

-- | TODO
--
-- @since 1.0.0
index :: Int -> BitMap -> Bool
index (I# i#) (BM bmp#) = Bool.toBool (Prim.index# i# bmp#)
{-# INLINE index #-}

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). 'null' tests if all bits in the 'BitMap' are cleared.
--
-- >>> null empty
-- True
-- >>> null (singleton 0)
-- False
--
-- @since 1.0.0
null :: BitMap -> Bool
null (BM bmp#) = GHC.tagToEnum# (Prim.null# bmp#)
{-# INLINE null #-}

-- | \(\mathcal{O}(1)\). 'null' tests if all bits in the 'BitMap' are set.
--
-- >>> full filled
-- True
-- >>> full empty
-- False
--
-- @since 1.0.0
full :: BitMap -> Bool
full (BM bmp#) = GHC.tagToEnum# (Prim.full# bmp#)
{-# INLINE full #-}

-- | \(\mathcal{O}(1)\). 'size' returns the number of bits that are set in a
-- 'BitMap'.
--
-- >>> size (fromList [0 .. 7])
-- 8
--
-- @since 1.0.0
size :: BitMap -> Int
size (BM bmp#) = I# (Prim.size# bmp#)
{-# INLINE size #-}

-- | \(\mathcal{O}(1)\). 'bounds' tests if the index refers to a valid location
-- within a 'BitMap'. That is, 'bounds' will return true for non-negative
-- integers that are less than 'capacity'.
--
-- @since 1.0.0
bounds :: Int -> Bool
bounds (I# i#) = Bool.toBool (Prim.bounds# i#)
{-# INLINE bounds #-}

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl :: (a -> Int -> a) -> a -> BitMap -> a
foldl con nil (BM bmp#) = Prim.foldl# con nil bmp#
{-# INLINE foldl #-}

-- | TODO
--
-- @since 1.0.0
foldr :: (Int -> a -> a) -> a -> BitMap -> a
foldr con nil (BM bmp#) = Prim.foldr# con nil bmp#
{-# INLINE foldr #-}

-- | TODO
--
-- @since 1.0.0
foldMap :: Monoid m => (Int -> m) -> BitMap -> m
foldMap f (BM bmp#) = Prim.foldMap# f bmp#
{-# INLINE foldMap #-}

-- Folds - Strict Folds --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl' :: (a -> Int -> a) -> a -> BitMap -> a
foldl' con nil (BM bmp#) = Prim.foldl'# con nil bmp#
{-# INLINE foldl' #-}

-- | TODO
--
-- @since 1.0.0
foldr' :: (Int -> a -> a) -> a -> BitMap -> a
foldr' con nil (BM bmp#) = Prim.foldr'# con nil bmp#
{-# INLINE foldr' #-}

-- | TODO
--
-- @since 1.0.0
foldMap' :: Monoid m => (Int -> m) -> BitMap -> m
foldMap' con (BM bmp#) = Prim.foldMap'# con bmp#
{-# INLINE foldMap' #-}

-- Traversals ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
for_ :: Applicative f => BitMap -> (Int -> f b) -> f ()
for_ = flip traverse_
{-# INLINE for_ #-}

-- | TODO
--
-- @since 1.0.0
traverse_ :: Applicative f => (Int -> f b) -> BitMap -> f ()
traverse_ f = foldr con (pure ())
  where
    con x xs = f x *> xs
    {-# INLINE con #-}

-- Conversion ------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). The inverse of 'toMask'. Produces a mask of the set 
-- bits in a 'BitMap'.
-- 
-- @since 1.0.0
fromMask :: Word -> BitMap 
fromMask (W# x#) = BM (BM# x#)
{-# INLINE [0] fromMask #-}

-- | \(\mathcal{O}(1)\). 'fromMask' constructs a 'BitMap' with bits set in the
-- locations specified by the mask.
--
-- * For a 64-bit machine:
--
-- >>> fromMask 0x8000800080008000 
-- fromList [0,8,16,24,32,40,48,56]
--
-- * For a 32-bit machine:
--
-- >>> fromMask 0x80008000 
-- fromList [0,8,16,24]
-- 
-- @since 1.0.0
toMask :: BitMap -> Word 
toMask (BM (BM# x#)) = W# x#
{-# INLINE [0] toMask #-}

-- | TODO
--
-- @since 1.0.0
assocs :: BitMap -> [(Int, Bool)]
assocs bmp = loop (WORD_SIZE_IN_BITS - 1) []
  where
    loop :: Int -> [(Int, Bool)] -> [(Int, Bool)]
    loop 0 xs = (0, Unsafe.unsafeIndex 0 bmp) : xs
    loop i xs = loop (i - 1) $ (i, Unsafe.unsafeIndex i bmp) : xs

-- Conversion - List -----------------------------------------------------------

-- | \(\mathcal{O}(length(xs))\). Constructs a 'BitMap' containing bits set for each 
-- location in a list of indices specified.
--
-- >>> fromList [0 .. 7]
-- fromList [0,1,2,3,4,5,6,7]
--
-- Locations in the list must refer to valid locations within the resulting 
-- 'BitMap'. Otherwise, the location will be omitting from the resulting 
-- 'BitMap':
--
-- >>> fromList [0, -1, 1, -2, 2, -3, 3, 1337]
-- fromList [0,1,2,3]
--
-- @since 1.0.0
fromList :: [Int] -> BitMap
fromList xs = BM (Prim.fromList# xs) 
{-# INLINE fromList #-}

-- | \(\mathcal{O}(n)\). 'toAscList' produces a list of locations for set bits
-- in the 'BitMap' in order of least to greatest.
--
-- >>> toAscList (fromList [0, 7, 1, 6, 2, 5, 3, 4])
-- [0,1,2,3,4,5,6,7]
--
-- @since 1.0.0
toAscList :: Integral a => BitMap -> [a]
toAscList = foldl' (flip ((:) . fromIntegral)) []
{-# INLINE toAscList #-}
{-# SPECIALIZE INLINE toAscList :: BitMap -> [Int] #-}

-- | \(\mathcal{O}(n)\). 'toAscList' produces a list of locations for set bits
-- in the 'BitMap' in order of greatest to least.
--
-- >>> toDescList (fromList [0, 7, 1, 6, 2, 5, 3, 4])
-- [7,6,5,4,3,2,1,0]
--
-- @since 1.0.0
toDescList :: Integral a => BitMap -> [a]
toDescList = foldr' ((:) . fromIntegral) []
{-# INLINE toDescList #-}
{-# SPECIALIZE INLINE toDescList :: BitMap -> [Int] #-}

-- Conversion - Maps -----------------------------------------------------------

-- | \(\mathcal{O}(n)\). Constructs an 'IntMap' associating the locations of 
-- each set bit in the 'BitMap' with 'True'. 
--
-- >>> toMap (fromList [4, 0, 2, 1, 3])
-- fromList [(0,True),(1,True),(2,True),(3,True),(4,True)]
--
-- @since 1.0.0
toIntMap :: BitMap -> IntMap Bool
toIntMap = foldr' (`IntMap.insert` True) IntMap.empty

-- | \(\mathcal{O}(n)\). Constructs a 'Map' associating the locations of each
-- set bit in the 'BitMap' with 'True'. 
--
-- >>> toMap (fromList [4, 0, 2, 1, 3])
-- fromList [(0,True),(1,True),(2,True),(3,True),(4,True)]
--
-- @since 1.0.0
toMap :: Integral a => BitMap -> Map a Bool
toMap = foldr' (\x -> Map.insert (fromIntegral x) True) Map.empty
{-# SPECIALIZE toMap :: BitMap -> Map Int Bool #-}

-- Conversion - Maps -----------------------------------------------------------

-- | \(\mathcal{O}(n)\). Constructs an 'IntSet' containing the location of each 
-- set bit in a 'BitMap'.
--
-- >>> toIntSet (fromList [4, 0, 2, 1, 3])
-- fromList [0,1,2,3,4]
--
-- @since 1.0.0
toIntSet :: BitMap -> IntSet
toIntSet = foldr' IntSet.insert IntSet.empty

-- | \(\mathcal{O}(n)\). Constructs a 'Set' containing the location of each set
-- bit in a 'BitMap'.
--
-- >>> toSet (fromList [4, 0, 2, 1, 3])
-- fromList [0,1,2,3,4]
--
-- @since 1.0.0
toSet :: Integral a => BitMap -> Set a
toSet = foldr' (Set.insert . fromIntegral) Set.empty
{-# SPECIALIZE toSet :: BitMap -> Set Int #-}
