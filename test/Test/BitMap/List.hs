module Test.BitMap.List (testTree) where

import Data.BitMap qualified as BitMap

import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen

import Test.Compat (TestTree, testGroup, testProp)
import Test.BitMap.Gen qualified as Gen.BitMap 
import Test.BitMap.Range qualified as Range.BitMap 
import qualified Data.List as List


--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "List"
    [ testProp "empty" propEmpty 
    , testProp "singleton" propSingleton 
    , testProp "roundtrip" propRoundTrip
    ]

propEmpty :: Property 
propEmpty = 
  withTests 1 $ property do 
    BitMap.toAscList BitMap.empty === ([] :: [Int])
    BitMap.toDescList BitMap.empty === ([] :: [Int])

propSingleton :: Property 
propSingleton = property do 
  i <- forAll Gen.BitMap.index
  BitMap.toAscList (BitMap.singleton i) === [i]
  BitMap.toDescList (BitMap.singleton i) === [i]

propRoundTrip :: Property 
propRoundTrip = property do 
  ixs <- forAll (List.nub <$> Gen.list Range.BitMap.index Gen.BitMap.index)
  BitMap.toAscList (BitMap.fromList ixs) === List.sort ixs
  BitMap.toDescList (BitMap.fromList ixs) === List.reverse (List.sort ixs)