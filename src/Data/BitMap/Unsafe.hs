{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitMap.Unsafe
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
module Data.BitMap.Unsafe
  ( -- * Write
    unsafeInsert,
    unsafeWrite,
    unsafeClear,
    unsafeToggle,

    -- * Index
    unsafeIndex,
  )
where

import Data.Bool.Prim qualified as Bool

import GHC.Exts (Int (I#))

--------------------------------------------------------------------------------

import Data.BitMap.Core (BitMap (BM))
import Data.BitMap.Prim.Unsafe qualified as Prim.Unsafe

-- Unsafe Writes ---------------------------------------------------------------

-- | todo
--
-- @since 1.0.0
unsafeInsert :: Int -> Bool -> BitMap -> BitMap
unsafeInsert (I# i#) x (BM bmp#) = 
  BM (Prim.Unsafe.unsafeInsert# i# (Bool.fromBool x) bmp#)
{-# INLINE unsafeInsert #-}

-- | TODO
--
-- @since 1.0.0
unsafeWrite :: Int -> BitMap -> BitMap
unsafeWrite (I# i#) (BM bmp#) = BM (Prim.Unsafe.unsafeWrite# i# bmp#)
{-# INLINE unsafeWrite #-}

-- | TODO
--
-- @since 1.0.0
unsafeClear :: Int -> BitMap -> BitMap
unsafeClear (I# i#) (BM bmp#) = BM (Prim.Unsafe.unsafeClear# i# bmp#)
{-# INLINE unsafeClear #-}

-- | TODO
--
-- @since 1.0.0
unsafeToggle :: Int -> BitMap -> BitMap
unsafeToggle (I# i#) (BM bmp#) = BM (Prim.Unsafe.unsafeToggle# i# bmp#)
{-# INLINE unsafeToggle #-}

-- Unsafe Indexing -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex :: Int -> BitMap -> Bool
unsafeIndex (I# i#) (BM bmp#) = Bool.toBool (Prim.Unsafe.unsafeIndex# i# bmp#)
{-# INLINE unsafeIndex #-}
