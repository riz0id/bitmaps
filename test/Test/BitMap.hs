module Test.BitMap (testTree) where

import Test.Compat (TestTree, testGroup)

import qualified Test.BitMap.Class
import qualified Test.BitMap.Construction 
import qualified Test.BitMap.List
import qualified Test.BitMap.Query

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "BitMap"
    [ Test.BitMap.Class.testTree
    , Test.BitMap.Construction.testTree
    , Test.BitMap.List.testTree
    , Test.BitMap.Query.testTree
    ]