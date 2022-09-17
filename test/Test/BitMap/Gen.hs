{-# LANGUAGE MagicHash #-}

module Test.BitMap.Gen
  ( index,
    bitmap,
  )
where

import Data.BitMap (BitMap)
import Data.BitMap qualified as BitMap

import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

index :: MonadGen m => m Int
index = Gen.int (Range.constant 0 (BitMap.capacity - 1))
{-# SPECIALIZE index :: Gen Int #-}

bitmap :: MonadGen m => m BitMap
bitmap = BitMap.fromMask <$> Gen.word Range.constantBounded
{-# SPECIALIZE bitmap :: Gen BitMap #-}