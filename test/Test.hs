module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.BitMap

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree 

testTree :: TestTree
testTree = 
  testGroup 
    "Test"
    [ Test.BitMap.testTree
    ]