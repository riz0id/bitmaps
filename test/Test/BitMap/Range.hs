{-# LANGUAGE MagicHash #-}

module Test.BitMap.Range
  ( index,
  )
where

import Data.BitMap qualified as BitMap

import Hedgehog (Range)
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

index :: Range Int
index = Range.constant 0 (BitMap.capacity - 1)
