module Test.BitMap.Class.Ord (testTree) where

import Data.BitMap qualified as BitMap

import Hedgehog (forAll, property, withTests, (===))

import Test.BitMap.Gen qualified as Gen.BitMap
import Test.Core (TestTree, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Ord"
    [ testProp "compare" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (compare bmp0 bmp1) === compare (BitMap.toMask bmp0) (BitMap.toMask bmp1)
    , testProp "(>)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 > bmp1) === (BitMap.toMask bmp0 > BitMap.toMask bmp1)
    , testProp "(>=)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 >= bmp1) === (BitMap.toMask bmp0 >= BitMap.toMask bmp1)
    , testProp "(<)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 < bmp1) === (BitMap.toMask bmp0 < BitMap.toMask bmp1)
    , testProp "(<=)" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bmp1 <- forAll Gen.BitMap.bitmap
        (bmp0 <= bmp1) === (BitMap.toMask bmp0 <= BitMap.toMask bmp1)
    ]
