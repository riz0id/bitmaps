module Test.BitMap.Class.Eq (testTree) where

import Data.BitMap qualified as BitMap

import Hedgehog (forAll, property, withTests, (===))

import Test.BitMap.Gen qualified as Gen.BitMap
import Test.Core (TestTree, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Eq"
    [ testProp "(==)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 == bmp1) === (BitMap.toMask bmp0 == BitMap.toMask bmp1)
    , testProp "(/=)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 /= bmp1) === (BitMap.toMask bmp0 /= BitMap.toMask bmp1)
    ]
