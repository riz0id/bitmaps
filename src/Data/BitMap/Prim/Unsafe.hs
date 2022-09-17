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
module Data.BitMap.Prim.Unsafe
  ( -- * Write
    unsafeInsert#,
    unsafeWrite#,
    unsafeClear#,
    unsafeToggle#,

    -- * Index
    unsafeIndex#,
  )
where

import Data.Bool.Prim (Bool# (True#))
import Data.Bool.Prim qualified as Bool
import Data.Coerce (coerce)

import GHC.Exts (Int#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Data.BitMap.Prim.Core (BitMap# (BM#), bitmask#)

-- Unsafe Writes ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeInsert# :: Int# -> Bool# -> BitMap# -> BitMap#
unsafeInsert# i# True# = unsafeWrite# i#
unsafeInsert# i# _ = unsafeClear# i#

-- | TODO
--
-- @since 1.0.0
unsafeWrite# :: Int# -> BitMap# -> BitMap#
unsafeWrite# i# = coerce GHC.or# (bitmask# i#)

-- | TODO
--
-- @since 1.0.0
unsafeClear# :: Int# -> BitMap# -> BitMap#
unsafeClear# i# = coerce GHC.and# (GHC.not# (bitmask# i#))

-- | TODO
--
-- @since 1.0.0
unsafeToggle# :: Int# -> BitMap# -> BitMap#
unsafeToggle# i# = coerce GHC.xor# (bitmask# i#)

-- Unsafe Indexing -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex# :: Int# -> BitMap# -> Bool#
unsafeIndex# i# (BM# bmp#) = Bool.fromWord# (GHC.pext# bmp# (bitmask# i#))