{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitMap.Core
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
module Data.BitMap.Core
  ( -- * BitMap
    BitMap (BM),
  )
where

import Control.DeepSeq (NFData (..), rwhnf)

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Bool.Prim qualified as Bool
import Data.Foldable (foldl')
import Data.Hashable (Hashable (..))

import GHC.Exts (Int (I#), IsList (fromList, toList), Word (W#))
import GHC.Exts qualified as GHC

import Language.Haskell.TH.Syntax (Lift, lift)
import Language.Haskell.TH.Syntax qualified as TH

-- include WORD_SIZE_IN_BITS
#include "MachDeps.h" 

--------------------------------------------------------------------------------

import Data.BitMap.Prim (BitMap# (BM#))
import Data.BitMap.Prim qualified as Prim

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data BitMap = BM BitMap#

-- | @since 1.0.0
instance IsList BitMap where
  type Item BitMap = Int

  fromList = foldl' (\(BM bmp#) (I# i#) -> BM (Prim.write# i# bmp#)) (BM (BM# 0##))
  {-# INLINE fromList #-}

  toList (BM bmp#) = Prim.foldr# (:) [] bmp#
  {-# INLINE toList #-}

-- | @since 1.0.0
instance Show BitMap where
  show (BM bmp#) = "fromList " ++ show (Prim.foldr'# (:) [] bmp#)
  {-# INLINE show #-}

-- | @since 1.0.0
instance Eq BitMap where
  BM x# == BM y# = Bool.toBool (Prim.eq# x# y#)
  {-# INLINE (==) #-}

  BM x# /= BM y# = Bool.toBool (Prim.ne# x# y#)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Ord BitMap where
  BM x# > BM y# = Bool.toBool (Prim.gt# x# y#)
  {-# INLINE (>) #-}

  BM x# >= BM y# = Bool.toBool (Prim.ge# x# y#)
  {-# INLINE (>=) #-}

  BM x# < BM y# = Bool.toBool (Prim.lt# x# y#)
  {-# INLINE (<) #-}

  BM x# <= BM y# = Bool.toBool (Prim.le# x# y#)
  {-# INLINE (<=) #-}

-- | @since 1.0.0
instance Bounded BitMap where
  minBound = BM (BM# 0##)
  {-# INLINE CONLIKE minBound #-}

  maxBound = case maxBound of W# bmp# -> BM (BM# bmp#)
  {-# INLINE CONLIKE maxBound #-}

-- | @since 1.0.0
instance Semigroup BitMap where
  BM x# <> BM y# = BM (Prim.or# x# y#)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid BitMap where
  mempty = BM (BM# 0##)
  {-# INLINE CONLIKE mempty #-}

-- | @since 1.0.0
instance Bits BitMap where
  bit (I# i#) = BM (Prim.singleton# i#)
  {-# INLINE bit #-}

  testBit (BM bmp#) (I# i#) = Bool.toBool (Prim.index# i# bmp#)
  {-# INLINE testBit #-}

  BM bmp0# .&. BM bmp1# = BM (Prim.and# bmp0# bmp1#)
  {-# INLINE (.&.) #-}

  BM bmp0# .|. BM bmp1# = BM (Prim.or# bmp0# bmp1#)
  {-# INLINE (.|.) #-}

  xor (BM bmp0#) (BM bmp1#) = BM (Prim.xor# bmp0# bmp1#)
  {-# INLINE xor #-}

  complement (BM bmp#) = BM (Prim.not# bmp#)
  {-# INLINE complement #-}

  shift (BM bmp#) (I# i#) = BM (Prim.shift# bmp# i#)
  {-# INLINE shift #-}

  shiftL (BM bmp#) (I# i#) = BM (i# Prim.<<# bmp#)
  {-# INLINE shiftL #-}

  shiftR (BM bmp#) (I# i#) = BM (bmp# Prim.>># i#)
  {-# INLINE shiftR #-}

  unsafeShiftL (BM bmp#) (I# i#) = BM (i# Prim.<<!# bmp#)
  {-# INLINE unsafeShiftL #-}

  unsafeShiftR (BM bmp#) (I# i#) = BM (bmp# Prim.>>!# i#)
  {-# INLINE unsafeShiftR #-}

  rotate (BM bmp#) (I# i#) = BM (Prim.rotate# bmp# i#)
  {-# INLINE rotate #-}

  rotateL (BM bmp#) (I# i#) = BM (Prim.rotateL# bmp# i#)
  {-# INLINE rotateL #-}

  rotateR (BM bmp#) (I# i#) = BM (Prim.rotateR# bmp# i#)
  {-# INLINE rotateR #-}

  popCount (BM bmp#) = I# (Prim.size# bmp#)
  {-# INLINE popCount #-}

  bitSizeMaybe _ = Just WORD_SIZE_IN_BITS

  isSigned _ = False

  bitSize = finiteBitSize

-- | @since 1.0.0
instance FiniteBits BitMap where
  finiteBitSize _ = WORD_SIZE_IN_BITS

  countLeadingZeros (BM bmp#) = I# (Prim.clz# bmp#)
  {-# INLINE countLeadingZeros #-}

  countTrailingZeros (BM bmp#) = I# (Prim.ctz# bmp#)
  {-# INLINE countTrailingZeros #-}

-- | @since 1.0.0
instance Hashable BitMap where
  hash (BM (BM# bmp#)) = I# (GHC.word2Int# bmp#)
  {-# INLINE hash #-}

  hashWithSalt s (BM (BM# bmp#)) = hashWithSalt s (W# bmp#)
  {-# INLINE hashWithSalt #-}

-- | @since 1.0.0
instance Lift BitMap where
  lift (BM (BM# bmp#)) =
    let conE = TH.VarE 'BM
        litE = TH.LitE (TH.WordPrimL (fromIntegral (W# bmp#)))
     in pure (TH.AppE conE litE)

  liftTyped x = TH.unsafeCodeCoerce (lift x)

-- | @since 1.0.0
instance NFData BitMap where
  rnf = rwhnf