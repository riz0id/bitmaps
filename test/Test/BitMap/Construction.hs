{-# LANGUAGE MagicHash #-}

module Test.BitMap.Construction (testTree) where

import Data.BitMap qualified as BitMap
import Data.Bits (bit)

import Hedgehog (forAll, property, withTests, (===))

import Test.Compat (TestTree, testGroup, testProp)
import Test.BitMap.Gen qualified as Gen.BitMap 

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Construction"
    [ testProp "empty" $ withTests 1 $ property do 
        BitMap.toMask BitMap.empty === 0
    , testProp "singleton" $ property do 
        i <- forAll Gen.BitMap.index
        BitMap.toMask (BitMap.singleton i) === bit ((BitMap.capacity - 1) - i)
    ]
