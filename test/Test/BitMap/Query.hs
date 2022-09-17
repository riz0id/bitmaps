{-# LANGUAGE MagicHash #-}

module Test.BitMap.Query (testTree) where

import Control.Monad ( when )

import Data.BitMap qualified as BitMap

import Hedgehog (forAll, property, withTests, (===))

import Test.Compat (TestTree, testGroup, testProp)
import Test.BitMap.Gen qualified as Gen.BitMap 

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Query"
    [ testTreeNull
    , testTreeFull
    , testTreeSize
    ]

testTreeNull :: TestTree 
testTreeNull = 
  testGroup
    "Null"
    [ testProp "empty" $ withTests 1 $ property do 
        BitMap.null BitMap.empty === True 
    , testProp "minBound" $ withTests 1 $ property do 
        BitMap.null minBound === True 
    , testProp "mempty" $ withTests 1 $ property do 
        BitMap.null mempty === True 
    , testProp "fuzz" $ withTests 10000 $ property do 
        bmp <- forAll Gen.BitMap.bitmap
        when (BitMap.null bmp) do
          bmp === BitMap.empty
    ]

testTreeFull :: TestTree 
testTreeFull = 
  testGroup
    "Full"
    [ testProp "filled" $ withTests 1 $ property do 
        BitMap.full BitMap.filled === True 
    , testProp "maxBound" $ withTests 1 $ property do 
        BitMap.full maxBound === True 
    , testProp "fuzz" $ withTests 10000 $ property do 
        bmp <- forAll Gen.BitMap.bitmap
        when (BitMap.full bmp) do
          bmp === BitMap.filled
    ]

testTreeSize :: TestTree 
testTreeSize = 
  testGroup
    "Size"
    [ testProp "empty" $ withTests 1 $ property do 
        BitMap.size BitMap.empty === 0
    , testProp "filled" $ withTests 1 $ property do 
        BitMap.size BitMap.filled === BitMap.capacity
    , testProp "singleton" $ property do 
        i <- forAll Gen.BitMap.index
        BitMap.size (BitMap.singleton i) === 1
    ]