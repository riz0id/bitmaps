module Test.BitMap.Class (testTree) where

import Test.Compat (TestTree, testGroup)

import qualified Test.BitMap.Class.Bits
import qualified Test.BitMap.Class.Eq
import qualified Test.BitMap.Class.Ord

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Class"
    [ Test.BitMap.Class.Bits.testTree
    , Test.BitMap.Class.Eq.testTree
    , Test.BitMap.Class.Ord.testTree
    ]