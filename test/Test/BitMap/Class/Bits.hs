module Test.BitMap.Class.Bits (testTree) where

import Data.BitMap qualified as BitMap
import Data.Bits

import Hedgehog (forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.BitMap.Gen qualified as Gen.BitMap
import Test.Core (TestTree, homomorphic1, homomorphic2, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Bits"
    [ testProp "bitSizeMaybe" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        bitSizeMaybe bmp0 === bitSizeMaybe (BitMap.toMask bmp0)
    , testProp "isSigned" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        isSigned bmp0 === isSigned (BitMap.toMask bmp0)
    , testProp "popCount" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        popCount bmp0 === popCount (BitMap.toMask bmp0)
    , testTreeLogical
    , testTreeShifts
    , testTreeRotations
    , testTreeFinite
    ]

testTreeLogical :: TestTree
testTreeLogical =
  testGroup
    "Logical"
    [ testProp "and" . withTests 10000 $ property do
        homomorphic2 Gen.BitMap.bitmap (.&.) (.&.) BitMap.toMask
    , testProp "xor" . withTests 10000 $ property do
        homomorphic2 Gen.BitMap.bitmap xor xor BitMap.toMask
    , testProp "or" . withTests 10000 $ property do
        homomorphic2 Gen.BitMap.bitmap (.|.) (.|.) BitMap.toMask
    , testProp "not" . withTests 10000 $ property do
        homomorphic1 Gen.BitMap.bitmap complement complement BitMap.toMask
    ]

testTreeShifts :: TestTree
testTreeShifts =
  testGroup
    "Shifts"
    [ testProp "shift" . withTests 10000 $ property do
        idx <- forAll (Gen.int Range.constantBounded)
        homomorphic1 Gen.BitMap.bitmap (`shift` idx) (`shift` idx) BitMap.toMask
    , testProp "shiftL" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 maxBound)
        homomorphic1 Gen.BitMap.bitmap (`shiftL` idx) (`shiftL` idx) BitMap.toMask
    , testProp "shiftR" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 maxBound)
        homomorphic1 Gen.BitMap.bitmap (`shiftR` idx) (`shiftR` idx) BitMap.toMask
    , testTreeUnsafeShifts
    ]

testTreeUnsafeShifts :: TestTree
testTreeUnsafeShifts =
  testGroup
    "Unsafe.Shift"
    [ testProp "shiftL" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 BitMap.capacity)
        homomorphic1 Gen.BitMap.bitmap (`unsafeShiftL` idx) (`unsafeShiftL` idx) BitMap.toMask
    , testProp "shiftR" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 BitMap.capacity)
        homomorphic1 Gen.BitMap.bitmap (`unsafeShiftR` idx) (`unsafeShiftR` idx) BitMap.toMask
    ]

testTreeRotations :: TestTree
testTreeRotations =
  testGroup
    "Shifts"
    [ testProp "rotate" . withTests 10000 $ property do
        idx <- forAll (Gen.int Range.constantBounded)
        homomorphic1 Gen.BitMap.bitmap (`rotate` idx) (`rotate` idx) BitMap.toMask
    , testProp "rotateL" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 maxBound)
        homomorphic1 Gen.BitMap.bitmap (`rotateL` idx) (`rotateL` idx) BitMap.toMask
    , testProp "rotateR" . withTests 10000 $ property do
        idx <- forAll (Gen.int $ Range.constant 0 maxBound)
        homomorphic1 Gen.BitMap.bitmap (`rotateR` idx) (`rotateR` idx) BitMap.toMask
    ]

testTreeFinite :: TestTree
testTreeFinite =
  testGroup
    "Finite"
    [ testProp "finiteBiteSize" . withTests 1 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        finiteBitSize bmp0 === finiteBitSize (BitMap.toMask bmp0)
    , testProp "countLeadingZeros" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        countLeadingZeros bmp0 === countLeadingZeros (BitMap.toMask bmp0)
    , testProp "countTrailingZeros" . withTests 10000 $ property do
        bmp0 <- forAll Gen.BitMap.bitmap
        countTrailingZeros bmp0 === countTrailingZeros (BitMap.toMask bmp0)
    ]
